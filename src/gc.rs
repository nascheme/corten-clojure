//! Mostly-copying garbage collector (Bartlett style).
//!
//! Roots:
//! - The stack of the current thread and the callee-saved registers are
//!   scanned conservatively. Any word pointing into an allocated block
//!   (including interior pointers such as return addresses into code
//!   objects, or Rust pointers into a string's data) pins that block: the
//!   block is promoted to the new space as a whole and its objects do not
//!   move. Rust code may therefore keep `LispObj`s in locals across
//!   allocations, but NOT in the Rust heap (`Vec`, `Box`): those are not
//!   roots. The compiler, which does keep objects in Rust data structures,
//!   runs with collection inhibited (`runtime::without_gc`).
//! - The QV is scanned precisely (only tagged pointers into the heap are
//!   updated; the raw addresses stored there are 16-byte aligned or point
//!   outside the heap, so they are ignored).
//!
//! Everything reachable from the roots is copied (Cheney scan) to blocks
//! of the new space, except objects in pinned blocks and large objects,
//! whose blocks are promoted in place. All objects in a promoted block are
//! scanned, live or not. Afterwards the blocks still in the old space are
//! free.
//!
//! Objects move, so address-based hashing must rehash: every collection
//! increments the GC count (`gc_count`).

use crate::heap::{self, BLOCK, block_addr, heap_start, state};
use crate::object::*;
use crate::qv::{qv, set_qv};
use crate::qv_index::*;

/// The GC id: number of collections so far, counting those in the
/// processes that built the current image.
pub fn gc_count() -> usize {
    qv(GC_COUNT_Index).integer() as usize
}

/// Collect garbage now, unless collection is inhibited.
#[inline(never)]
pub fn collect() {
    if state().inhibit > 0 {
        return;
    }
    // Spill the callee-saved registers: they may hold the only reference
    // to an object (in a Rust or Lisp frame above us).
    let mut regs = [0usize; 6];
    unsafe {
        std::arch::asm!(
            "mov [{p}], rbx",
            "mov [{p} + 8], rbp",
            "mov [{p} + 16], r12",
            "mov [{p} + 24], r13",
            "mov [{p} + 32], r14",
            "mov [{p} + 40], r15",
            p = in(reg) regs.as_mut_ptr(),
            options(nostack, preserves_flags)
        );
    }
    collect_with_registers(std::hint::black_box(&regs));
    std::hint::black_box(&regs);
}

fn stack_top() -> usize {
    thread_local! {
        static TOP: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    }
    TOP.with(|t| {
        if t.get() == 0 {
            unsafe {
                let mut attr: libc::pthread_attr_t = std::mem::zeroed();
                let r = libc::pthread_getattr_np(libc::pthread_self(), &mut attr);
                assert_eq!(r, 0, "pthread_getattr_np failed");
                let mut addr: *mut libc::c_void = std::ptr::null_mut();
                let mut size: libc::size_t = 0;
                libc::pthread_attr_getstack(&attr, &mut addr, &mut size);
                libc::pthread_attr_destroy(&mut attr);
                t.set(addr as usize + size);
            }
        }
        t.get()
    })
}

#[inline(never)]
fn collect_with_registers(regs: &[usize; 6]) {
    let marker = 0usize;
    let lo = std::hint::black_box(&marker) as *const usize as usize;
    let hi = stack_top();
    heap::retire_current_block();
    let from = state().space;
    let mut gc = Gc {
        from,
        to: 3 - from,
        promoted: Vec::new(),
        copy_blocks: Vec::new(),
        copy_block: None,
        copy_ptr: 0,
        copy_end: 0,
    };
    for &w in regs {
        gc.pin(w);
    }
    let mut p = lo & !7;
    while p < hi {
        gc.pin(unsafe { *(p as *const usize) });
        p += CELL;
    }
    for i in 0..QV_MAX {
        let v = qv(i);
        let nv = gc.forward(v);
        if nv != v {
            set_qv(i, nv);
        }
    }
    gc.trace();
    if let Some(b) = gc.copy_block {
        state().blocks[b].used = (gc.copy_ptr - block_addr(b)) as u32;
    }

    let st = state();
    for b in st.blocks.iter_mut() {
        if b.space == from {
            *b = heap::Block::default();
        }
    }
    st.space = gc.to;
    st.rover = 0;
    st.allocated_since_gc = 0;
    // The count continues from the value in the heap (which may come
    // from an image): hash tables compare it with the count they last saw.
    let count = qv(GC_COUNT_Index).integer() + 1;
    let live = heap::bytes_used();
    let st = state();
    st.live_after_gc = live;
    if !st.fixed_threshold {
        st.threshold = st.threshold.max(live);
    }
    set_qv(GC_COUNT_Index, LispObj::fixnum(count));
    if state().verify {
        verify_heap();
    }
    crate::perfmap::heap_snapshot();
}

struct Gc {
    from: u8,
    to: u8,
    /// Run starts promoted in place, waiting to be scanned.
    promoted: Vec<usize>,
    /// Blocks objects were copied to, in order.
    copy_blocks: Vec<usize>,
    copy_block: Option<usize>,
    copy_ptr: usize,
    copy_end: usize,
}

/// Block index of `addr` if it is in the used part of the heap.
#[inline]
fn block_of(addr: usize) -> Option<usize> {
    let hs = heap_start();
    if addr < hs {
        return None;
    }
    let i = (addr - hs) / BLOCK;
    (i < state().blocks.len()).then_some(i)
}

impl Gc {
    /// Conservative root: pin the run containing `w`, if any.
    fn pin(&mut self, w: usize) {
        if let Some(i) = block_of(w) {
            let b = state().blocks[i];
            if b.space == self.from {
                self.promote(b.start as usize);
            }
        }
    }

    fn promote(&mut self, start: usize) {
        let st = state();
        let len = st.blocks[start].len as usize;
        for j in start..start + len {
            st.blocks[j].space = self.to;
        }
        self.promoted.push(start);
    }

    fn copy_alloc(&mut self, size: usize) -> usize {
        if self.copy_ptr + size > self.copy_end {
            if let Some(b) = self.copy_block {
                state().blocks[b].used = (self.copy_ptr - block_addr(b)) as u32;
            }
            let b = heap::take_blocks(1, self.to, false);
            self.copy_blocks.push(b);
            self.copy_block = Some(b);
            self.copy_ptr = block_addr(b);
            self.copy_end = self.copy_ptr + BLOCK;
        }
        let p = self.copy_ptr;
        self.copy_ptr += size;
        p
    }

    /// The new location of object `x` (copying it if needed).
    fn forward(&mut self, x: LispObj) -> LispObj {
        if !x.is_uvector() {
            return x;
        }
        let addr = x.strip_tag();
        let Some(i) = block_of(addr) else { return x };
        let b = state().blocks[i];
        if b.space != self.from {
            return x;
        }
        let start = b.start as usize;
        if state().blocks[start].large {
            self.promote(start);
            return x;
        }
        let w = unsafe { *(addr as *const usize) };
        if w & 7 == FORWARD_TAG {
            return LispObj((w & !7) | UVECTOR_TAG);
        }
        if w & 7 != UVECTOR_LENGTH_TAG {
            panic!("GC: uvector pointer {:#x} to a non-header {w:#x}", x.raw());
        }
        let size = (w >> 8) * 16;
        let new = self.copy_alloc(size);
        unsafe {
            std::ptr::copy_nonoverlapping(addr as *const u8, new as *mut u8, size);
            *(addr as *mut usize) = new | FORWARD_TAG;
        }
        LispObj(new | UVECTOR_TAG)
    }

    fn scan_run(&mut self, start: usize) {
        let b = state().blocks[start];
        let addr = block_addr(start);
        let end = addr + b.used as usize;
        let mut p = addr;
        while p < end {
            p += for_each_field(p, &mut |v| *v = self.forward(*v));
        }
    }

    fn trace(&mut self) {
        let mut scan_i = 0;
        let mut scan_ptr = 0;
        loop {
            if let Some(s) = self.promoted.pop() {
                self.scan_run(s);
                continue;
            }
            if scan_i < self.copy_blocks.len() {
                let b = self.copy_blocks[scan_i];
                if scan_ptr == 0 {
                    scan_ptr = block_addr(b);
                }
                let current = self.copy_block == Some(b);
                let limit = if current {
                    self.copy_ptr
                } else {
                    block_addr(b) + state().blocks[b].used as usize
                };
                if scan_ptr < limit {
                    scan_ptr += for_each_field(scan_ptr, &mut |v| *v = self.forward(*v));
                    continue;
                }
                if !current {
                    scan_i += 1;
                    scan_ptr = 0;
                    continue;
                }
            }
            break;
        }
    }
}

/// Uvector types whose cells after the header hold no Lisp objects.
fn is_raw_type(ty: usize) -> bool {
    matches!(
        ty,
        DOUBLE_FLOAT_TYPE | BIGNUM_TYPE | SIMPLE_CHAR_VECTOR_TYPE | SINGLE_FLOAT_TYPE
    )
}

/// Call `f` on every object field of the object at `addr` (untagged).
/// Returns the size of the object in bytes.
fn for_each_field(addr: usize, f: &mut dyn FnMut(&mut LispObj)) -> usize {
    use crate::runtime::{CODE_INFO, CODE_LITERALS_OFFSET, CODE_NAME, CODE_NUM_LITERALS};
    let cell = |i: usize| unsafe { &mut *((addr + i * CELL) as *mut LispObj) };
    let w = cell(0).raw();
    assert!(w & 7 == UVECTOR_LENGTH_TAG, "GC: object at {addr:#x} has no header ({w:#x})");
    let units = w >> 8;
    let ty = (w >> 3) & 0x1f;
    let ncells = units * 2;
    if ty == COMPILED_CODE_TYPE {
        f(cell(CODE_NAME));
        f(cell(CODE_INFO));
        let off = cell(CODE_LITERALS_OFFSET).integer() as usize;
        let n = cell(CODE_NUM_LITERALS).integer() as usize;
        for k in 0..n {
            let p = (addr + off + k * CELL) as *mut LispObj;
            let mut v = unsafe { p.read_unaligned() };
            let old = v;
            f(&mut v);
            if v != old {
                unsafe { p.write_unaligned(v) };
            }
        }
    } else if !is_raw_type(ty) {
        for i in 1..ncells {
            f(cell(i));
        }
    }
    units * 16
}

/// Check that every object field in the heap and the QV refers to a valid
/// object in an allocated block. Panics with a description otherwise.
pub fn verify_heap() {
    let space = state().space;
    let check = |v: &mut LispObj, whence: &dyn Fn() -> String| {
        if !v.is_uvector() {
            return;
        }
        let addr = v.strip_tag();
        let Some(i) = block_of(addr) else { return };
        let st = state();
        let b = st.blocks[i];
        if b.space != space {
            panic!("GC verify: {} refers to {:#x} in a free block", whence(), v.raw());
        }
        let start = b.start as usize;
        let used = st.blocks[start].used as usize;
        let cur_limit = if st.cur == Some(start) {
            qv(THREAD_HEAP_Index).raw()
        } else {
            block_addr(start) + used
        };
        if addr >= cur_limit {
            panic!("GC verify: {} refers to {:#x} past the used part of its block", whence(), v.raw());
        }
        let w = unsafe { *(addr as *const usize) };
        if w & 7 != UVECTOR_LENGTH_TAG {
            panic!("GC verify: {} refers to {:#x}, first word {w:#x}", whence(), v.raw());
        }
    };
    for i in 0..QV_MAX {
        let mut v = qv(i);
        check(&mut v, &|| format!("QV[{i}]"));
    }
    for_each_object(&mut |obj| {
        for_each_field(obj, &mut |v| check(v, &|| format!("object at {obj:#x}")));
    });
}

/// Call `f` with the address of every object in the heap.
pub fn for_each_object(f: &mut dyn FnMut(usize)) {
    let nblocks = state().blocks.len();
    for i in 0..nblocks {
        let b = state().blocks[i];
        if b.space == 0 || b.start as usize != i {
            continue;
        }
        let addr = block_addr(i);
        let end = if state().cur == Some(i) {
            qv(THREAD_HEAP_Index).raw()
        } else {
            addr + b.used as usize
        };
        let mut p = addr;
        while p < end {
            f(p);
            p += for_each_field(p, &mut |_| {});
        }
        if p != end {
            panic!("GC verify: block {i} objects overrun the used mark");
        }
    }
}
