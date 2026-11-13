//! Heap images.
//!
//! The heap is mapped at a fixed address, so an image is a raw copy of the
//! QV and of the allocated heap blocks; no relocation is needed. A garbage
//! collection runs first. Format (integers little endian):
//!
//! ```text
//!   magic "CLRSIMG2", kernel id (u64), heap base, QV bytes, block size,
//!   number of blocks N (all u64)
//!   QV contents
//!   N block descriptors: allocated (u8), large (u8), 2 bytes padding,
//!     run start, run length, bytes used (u32 each)
//!   contents of each allocated block, in order
//! ```
//!
//! The kernel id hashes everything compiled code and kernel function
//! objects depend on (primitive table, helper table, ABI version); an
//! image from a different kernel build is rejected.
//!
//! Process state outside the heap is recreated after loading: the runtime
//! stubs, the dynamic frame chain and the console stream handle.

use std::io::{Read, Write};

use crate::error::lisp_error;
use crate::heap::{self, BLOCK, HEAP_BASE, QV_BYTES};
use crate::object::*;
use crate::prims::{self};
use crate::qv::{qv, set_qv};
use crate::qv_index::*;
use crate::runtime::{self, DF_KIND, DF_LANDING, DF_PREV, DF_SPECIAL, DF_TAG};
use crate::streams;

const MAGIC: &[u8; 8] = b"CLRSIMG2";

/// Bump when code generation or runtime conventions change in a way that
/// makes existing compiled code invalid.
const ABI_VERSION: u64 = 9;

/// Identifies the kernel build an image depends on.
pub fn kernel_id() -> u64 {
    // FNV-1a over the ABI version, primitive names and helper names.
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    let mut feed = |bytes: &[u8]| {
        for &b in bytes {
            h ^= b as u64;
            h = h.wrapping_mul(0x0100_0000_01b3);
        }
        h ^= 0xff;
        h = h.wrapping_mul(0x0100_0000_01b3);
    };
    feed(&ABI_VERSION.to_le_bytes());
    feed(&(QV_MAX as u64).to_le_bytes());
    for p in prims::PRIMS {
        feed(p.name.as_bytes());
    }
    for i in 0..crate::clj::native::count() {
        feed(crate::clj::native::name(i).as_bytes());
    }
    for name in runtime::helper_names() {
        feed(name.as_bytes());
    }
    h
}

fn bytes_of(addr: usize, len: usize) -> &'static [u8] {
    unsafe { std::slice::from_raw_parts(addr as *const u8, len) }
}

/// Special variables currently rebound by dynamic frames, with their
/// global (outermost) values.
fn global_values_of_bound_specials() -> Vec<(LispObj, LispObj)> {
    let mut out: Vec<(LispObj, LispObj)> = Vec::new();
    let mut f = qv(DYN_FRAME_Index).raw();
    while f != 0 {
        let cell = |i: usize| unsafe { *((f + i * CELL) as *const LispObj) };
        if cell(DF_KIND).integer() == DF_SPECIAL {
            let sym = cell(DF_TAG);
            // Frames are visited innermost first: the last one seen for a
            // symbol holds its global value.
            match out.iter_mut().find(|(s, _)| *s == sym) {
                Some(e) => e.1 = cell(DF_LANDING),
                None => out.push((sym, cell(DF_LANDING))),
            }
        }
        f = cell(DF_PREV).raw();
    }
    out
}

/// Write an image of the current heap to `path`.
pub fn save_image(path: &str) {
    crate::gc::collect();
    streams::flush_out();
    // Save global values, not the bindings in effect inside this call.
    let globals = global_values_of_bound_specials();
    let current: Vec<(LispObj, LispObj)> =
        globals.iter().map(|&(s, _)| (s, crate::clj::binding_value(s))).collect();
    let saved_dyn = qv(DYN_FRAME_Index);
    for &(s, v) in &globals {
        crate::clj::set_binding_value(s, v);
    }
    set_qv(DYN_FRAME_Index, LispObj(0));
    heap::retire_current_block();
    let result = (|| -> std::io::Result<()> {
        let blocks = &heap::state().blocks;
        let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
        f.write_all(MAGIC)?;
        for v in [
            kernel_id(),
            HEAP_BASE as u64,
            QV_BYTES as u64,
            BLOCK as u64,
            blocks.len() as u64,
        ] {
            f.write_all(&v.to_le_bytes())?;
        }
        f.write_all(bytes_of(HEAP_BASE, QV_BYTES))?;
        for b in blocks {
            f.write_all(&[(b.space != 0) as u8, b.large as u8, 0, 0])?;
            for v in [b.start, b.len, b.used] {
                f.write_all(&v.to_le_bytes())?;
            }
        }
        for (i, b) in blocks.iter().enumerate() {
            if b.space != 0 {
                f.write_all(bytes_of(heap::block_addr(i), BLOCK))?;
            }
        }
        f.flush()
    })();
    set_qv(DYN_FRAME_Index, saved_dyn);
    for &(s, v) in &current {
        crate::clj::set_binding_value(s, v);
    }
    if let Err(e) = result {
        lisp_error(format!("Could not write image {path}: {e}"));
    }
}

/// Load an image into the (just mapped, otherwise uninitialized) heap.
/// Returns an error message on failure.
pub(crate) fn load_image_into_heap(path: &str) -> Result<(), String> {
    let mut f = std::fs::File::open(path).map_err(|e| format!("cannot open {path}: {e}"))?;
    let mut magic = [0u8; 8];
    f.read_exact(&mut magic).map_err(|e| format!("{path}: {e}"))?;
    if &magic != MAGIC {
        return Err(format!("{path} is not a Corten Clojure image"));
    }
    let mut hdr = [0u64; 5];
    for v in &mut hdr {
        let mut b = [0u8; 8];
        f.read_exact(&mut b).map_err(|e| format!("{path}: {e}"))?;
        *v = u64::from_le_bytes(b);
    }
    let [id, base, qv_bytes, block, nblocks] = hdr;
    if id != kernel_id() {
        return Err(format!(
            "{path} was made by a different kernel build; rebuild it with --boot"
        ));
    }
    if base != HEAP_BASE as u64 || qv_bytes != QV_BYTES as u64 || block != BLOCK as u64 {
        return Err(format!("{path} has an incompatible heap layout"));
    }
    let nblocks = nblocks as usize;
    if nblocks > heap::MAX_BLOCKS {
        return Err(format!("{path}: heap too large"));
    }
    let mut f = std::io::BufReader::new(f);
    let read_into = |f: &mut std::io::BufReader<std::fs::File>, addr: usize, len: usize| {
        let buf = unsafe { std::slice::from_raw_parts_mut(addr as *mut u8, len) };
        f.read_exact(buf).map_err(|e| format!("{path}: truncated image: {e}"))
    };
    read_into(&mut f, HEAP_BASE, QV_BYTES)?;
    heap::init_state();
    let space = heap::state().space;
    let mut blocks = Vec::with_capacity(nblocks);
    for _ in 0..nblocks {
        let mut d = [0u8; 16];
        read_into(&mut f, d.as_mut_ptr() as usize, 16)?;
        let word = |k: usize| u32::from_le_bytes(d[k..k + 4].try_into().unwrap());
        blocks.push(heap::Block {
            space: if d[0] != 0 { space } else { 0 },
            large: d[1] != 0,
            start: word(4),
            len: word(8),
            used: word(12),
        });
    }
    for (i, b) in blocks.iter().enumerate() {
        if b.space != 0 {
            read_into(&mut f, heap::block_addr(i), BLOCK)?;
        }
    }
    heap::state().blocks = blocks;
    Ok(())
}

/// Recreate process state after an image was loaded.
pub(crate) fn restore_after_load() {
    runtime::init_runtime();
    streams::reattach_console();
}
