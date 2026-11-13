//! Heap: one large region mmap'd at a fixed base address.
//!
//! Layout of the region:
//!   [HEAP_BASE, +QV_BYTES)        QV (globals vector)
//!   [heap_start, +HEAP_BYTES)     Lisp objects, in BLOCK-sized blocks
//!
//! The object area is divided into blocks of `BLOCK` bytes. Small objects
//! are bump allocated inside a block; the allocation pointer and limit of
//! the current block live in the QV `THREAD_HEAP`/`THREAD_HEAP_END` slots.
//! Objects larger than `LARGE` get a run of whole blocks to
//! themselves. Per-block metadata (space, run, bytes used) is kept in Rust,
//! see `HeapState`. The collector is in `gc.rs`.
//!
//! Every block holds a sequence of objects from its start up to its `used`
//! mark, so a block can be parsed: every object is a uvector starting
//! with its header.


use std::cell::UnsafeCell;

use crate::object::*;
use crate::qv::{qv, set_qv};
use crate::qv_index::*;

pub const HEAP_BASE: usize = 0x2000_0000_0000;
const PAGE: usize = 4096;
pub const QV_BYTES: usize = (QV_MAX * CELL + PAGE - 1) & !(PAGE - 1);
/// Reserved (not committed) size of the object heap.
pub const HEAP_BYTES: usize = 8 << 30;

/// Block size. Blocks are the unit of pinning in the collector.
pub const BLOCK: usize = 4096;
/// Objects larger than this get their own run of blocks.
pub const LARGE: usize = BLOCK / 2;
pub const MAX_BLOCKS: usize = HEAP_BYTES / BLOCK;

/// Default number of bytes allocated between collections (at least; the
/// collector also waits for as much as survived the last collection).
const DEFAULT_GC_THRESHOLD: usize = 32 << 20;

const _: () = assert!(QV_BYTES.is_multiple_of(BLOCK));

pub fn heap_start() -> usize {
    HEAP_BASE + QV_BYTES
}

#[inline]
pub fn block_addr(i: usize) -> usize {
    heap_start() + i * BLOCK
}

/// Metadata of one block.
#[derive(Clone, Copy, Default, Debug)]
pub struct Block {
    /// 0 = free, otherwise the space (see `HeapState::space`).
    pub space: u8,
    /// The block starts a run holding one large object.
    pub large: bool,
    /// Index of the first block of the run this block belongs to.
    pub start: u32,
    /// Number of blocks in the run (first block only; 1 for small blocks).
    pub len: u32,
    /// Bytes in use from the start of the block (for a large run, the size
    /// of its object). Not maintained for the current allocation block.
    pub used: u32,
}

pub struct HeapState {
    pub blocks: Vec<Block>,
    /// Space id of all allocated blocks between collections (1 or 2).
    pub space: u8,
    /// Current small-object allocation block, if any.
    pub cur: Option<usize>,
    /// Next block index to look at when searching for a free block.
    pub rover: usize,
    pub allocated_since_gc: usize,
    pub threshold: usize,
    /// Fixed threshold from the environment (GC stress testing).
    pub fixed_threshold: bool,
    pub inhibit: usize,
    pub verify: bool,
    pub live_after_gc: usize,
}

struct StateCell(UnsafeCell<Option<HeapState>>);
// Only one Lisp thread is supported; `with_lisp` serializes access.
unsafe impl Sync for StateCell {}
static STATE: StateCell = StateCell(UnsafeCell::new(None));

/// The heap state. Callers must not keep the reference across calls that
/// may allocate.
#[allow(clippy::mut_from_ref)]
pub fn state() -> &'static mut HeapState {
    unsafe { (*STATE.0.get()).as_mut().expect("heap not initialized") }
}

/// Map the region. Returns the QV pointer. Panics if the fixed address is
/// unavailable.
pub(crate) fn map_region() -> *mut LispObj {
    let total = QV_BYTES + HEAP_BYTES;
    let p = unsafe {
        libc::mmap(
            HEAP_BASE as *mut libc::c_void,
            total,
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
            libc::MAP_PRIVATE
                | libc::MAP_ANONYMOUS
                | libc::MAP_NORESERVE
                | libc::MAP_FIXED_NOREPLACE,
            -1,
            0,
        )
    };
    if p == libc::MAP_FAILED || p as usize != HEAP_BASE {
        panic!(
            "cannot map Lisp heap at {:#x}: {}",
            HEAP_BASE,
            std::io::Error::last_os_error()
        );
    }
    p as *mut LispObj
}

fn env_size(name: &str) -> Option<usize> {
    let s = std::env::var(name).ok()?;
    let s = s.trim().to_ascii_lowercase();
    let (num, mul) = match s.chars().last()? {
        'k' => (&s[..s.len() - 1], 1 << 10),
        'm' => (&s[..s.len() - 1], 1 << 20),
        'g' => (&s[..s.len() - 1], 1 << 30),
        _ => (&s[..], 1),
    };
    num.parse::<usize>().ok().map(|n| n * mul)
}

/// Create the heap state (empty heap). `CORTEN_GC_THRESHOLD` (bytes,
/// optional k/m/g suffix) fixes the number of bytes allocated between
/// collections; `CORTEN_GC_VERIFY=1` checks the heap after each one.
pub(crate) fn init_state() {
    let fixed = env_size("CORTEN_GC_THRESHOLD");
    let st = HeapState {
        blocks: Vec::new(),
        space: 1,
        cur: None,
        rover: 0,
        allocated_since_gc: 0,
        threshold: fixed.unwrap_or(DEFAULT_GC_THRESHOLD).max(BLOCK),
        fixed_threshold: fixed.is_some(),
        inhibit: 0,
        verify: std::env::var("CORTEN_GC_VERIFY").is_ok_and(|v| v != "0"),
        live_after_gc: 0,
    };
    unsafe { *STATE.0.get() = Some(st) };
    set_qv(THREAD_HEAP_Index, LispObj(0));
    set_qv(THREAD_HEAP_END_Index, LispObj(0));
}

/// Set the number of bytes allocated between collections (fixed, as with
/// `CORTEN_GC_THRESHOLD`) and whether to verify the heap after each
/// collection.
pub fn set_gc_params(threshold: usize, verify: bool) {
    let st = state();
    st.threshold = threshold.max(BLOCK);
    st.fixed_threshold = true;
    st.verify = verify;
}

/// Called once QV exists: set up the allocation state.
pub(crate) fn init_alloc() {
    init_state();
}

/// Record how much of the current allocation block is used and drop it as
/// the allocation block.
pub(crate) fn retire_current_block() {
    let st = state();
    if let Some(b) = st.cur.take() {
        st.blocks[b].used = (qv(THREAD_HEAP_Index).0 - block_addr(b)) as u32;
    }
    set_qv(THREAD_HEAP_Index, LispObj(0));
    set_qv(THREAD_HEAP_END_Index, LispObj(0));
}

/// Find `n` contiguous free blocks, extending the used part of the heap
/// if needed, and mark them as a run in `space`.
pub(crate) fn take_blocks(n: usize, space: u8, large: bool) -> usize {
    let st = state();
    let total = st.blocks.len();
    let mut found = None;
    if n == 1 {
        for k in 0..total {
            let i = (st.rover + k) % total;
            if st.blocks[i].space == 0 {
                found = Some(i);
                st.rover = i + 1;
                break;
            }
        }
    } else {
        let mut i = 0;
        while i + n <= total {
            match (i..i + n).find(|&j| st.blocks[j].space != 0) {
                None => {
                    found = Some(i);
                    break;
                }
                Some(j) => i = j + 1,
            }
        }
    }
    let first = match found {
        Some(i) => i,
        None => {
            // Extend; a trailing free run is reused.
            let mut i = total;
            while i > 0 && st.blocks[i - 1].space == 0 {
                i -= 1;
            }
            if i + n > MAX_BLOCKS {
                eprintln!("Lisp heap exhausted");
                std::process::abort();
            }
            if st.blocks.len() < i + n {
                st.blocks.resize(i + n, Block::default());
            }
            i
        }
    };
    for j in first..first + n {
        st.blocks[j] = Block {
            space,
            large: false,
            start: first as u32,
            len: 0,
            used: 0,
        };
    }
    st.blocks[first].len = n as u32;
    st.blocks[first].large = large;
    first
}

#[inline]
fn bump(bytes: usize) -> usize {
    debug_assert!(bytes.is_multiple_of(16));
    let cur = qv(THREAD_HEAP_Index).0;
    let next = cur + bytes;
    if next <= qv(THREAD_HEAP_END_Index).0 {
        set_qv(THREAD_HEAP_Index, LispObj(next));
        return cur;
    }
    alloc_slow(bytes)
}

#[inline(never)]
fn alloc_slow(bytes: usize) -> usize {
    retire_current_block();
    {
        let st = state();
        if st.inhibit == 0 && st.allocated_since_gc >= st.threshold {
            crate::gc::collect();
        }
    }
    let st = state();
    if bytes > LARGE {
        let n = bytes.div_ceil(BLOCK);
        let space = st.space;
        let b = take_blocks(n, space, true);
        let st = state();
        st.blocks[b].used = bytes as u32;
        st.allocated_since_gc += n * BLOCK;
        return block_addr(b);
    }
    let space = st.space;
    let b = take_blocks(1, space, false);
    let st = state();
    st.cur = Some(b);
    st.allocated_since_gc += BLOCK;
    let addr = block_addr(b);
    set_qv(THREAD_HEAP_Index, LispObj(addr + bytes));
    set_qv(THREAD_HEAP_END_Index, LispObj(addr + BLOCK));
    addr
}

/// Bytes in use by objects (including garbage not yet collected).
pub fn bytes_used() -> usize {
    let st = state();
    let mut n = 0;
    for (i, b) in st.blocks.iter().enumerate() {
        if b.space != 0 && b.start as usize == i {
            n += if st.cur == Some(i) {
                qv(THREAD_HEAP_Index).0 - block_addr(i)
            } else {
                b.used as usize
            };
        }
    }
    n
}

/// Disable collection while `inhibit` is non-zero (see
/// `runtime::without_gc`).
pub(crate) fn inhibit_gc(delta: isize) {
    let st = state();
    st.inhibit = (st.inhibit as isize + delta) as usize;
}

/// A uvector with `num` cells after the
/// header, zero filled, type 0. Total size is `num + 1` cells rounded up to
/// a 16-byte unit.
pub fn alloc_vector(num: usize) -> LispObj {
    let units = (num + 2) >> 1;
    let bytes = units * 16;
    let addr = bump(bytes);
    unsafe {
        std::ptr::write_bytes(addr as *mut u8, 0, bytes);
        *(addr as *mut LispObj) = uvector_header(units, 0);
    }
    LispObj(addr + UVECTOR_TAG)
}

/// Allocate a uvector of `num` cells (after header) with type `ty`.
pub fn alloc_uvector(num: usize, ty: usize) -> LispObj {
    let v = alloc_vector(num);
    v.set_uvector_type(ty);
    v
}

/// Simple general vector of `len` elements, initialized to NIL.
pub fn vector_node(len: usize) -> LispObj {
    let v = alloc_uvector(len + 1, SIMPLE_VECTOR_TYPE);
    v.uset(ARRAY_SIMPLE_VECTOR_LENGTH, LispObj::fixnum(len as i64));
    let nil = NIL;
    for i in 0..len {
        v.svset(i, nil);
    }
    v
}

/// Simple general vector of `items`.
pub fn vector_of(items: &[LispObj]) -> LispObj {
    let v = vector_node(items.len());
    for (i, &x) in items.iter().enumerate() {
        v.svset(i, x);
    }
    v
}

/// Character vector of `len` UTF-16 units (zero filled).
pub fn char_vector(len: usize) -> LispObj {
    // 4 LispChars per 8-byte cell.
    let v = alloc_uvector(((len + 3) >> 2) + 1, SIMPLE_CHAR_VECTOR_TYPE);
    v.uset(ARRAY_SIMPLE_VECTOR_LENGTH, LispObj::fixnum(len as i64));
    v
}

pub fn string_node(s: &str) -> LispObj {
    let units: Vec<u16> = s.encode_utf16().collect();
    string_from_units(&units)
}

pub fn string_from_units(units: &[LispChar]) -> LispObj {
    let v = char_vector(units.len());
    unsafe {
        std::ptr::copy_nonoverlapping(units.as_ptr(), v.char_array_start(), units.len());
    }
    v
}

pub fn double_float_node(d: f64) -> LispObj {
    let f = alloc_uvector(DOUBLE_FLOAT_SIZE, DOUBLE_FLOAT_TYPE);
    unsafe { *(f.uvector_ptr().add(DOUBLE_FLOAT_OFFSET) as *mut f64) = d };
    f
}

/// Single float: f32 in the low half of cell 1.
pub fn single_float_node(f: f32) -> LispObj {
    let v = alloc_uvector(1, SINGLE_FLOAT_TYPE);
    unsafe { *(v.uvector_ptr().add(SINGLE_FLOAT_OFFSET) as *mut f32) = f };
    v
}
