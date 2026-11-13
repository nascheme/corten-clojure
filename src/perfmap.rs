//! Profiling aid. With `CORTEN_PERF_LOG=FILE`, log where compiled
//! code objects are, with CLOCK_MONOTONIC times: each new code object,
//! and all of them after each GC (code moves). Each code object gets an
//! id in its CODE_INFO cell, logged once with its name. `scripts/perf-jit.py`
//! attributes `perf record -k mono` samples with this log.

use std::fs::File;
use std::io::{BufWriter, Write};
use std::sync::{Mutex, OnceLock};

use crate::object::*;
use crate::runtime::{CODE_INFO, CODE_NAME};

static LOG: OnceLock<Option<Mutex<BufWriter<File>>>> = OnceLock::new();

fn log() -> Option<&'static Mutex<BufWriter<File>>> {
    LOG.get_or_init(|| {
        let path = std::env::var("CORTEN_PERF_LOG").ok()?;
        let f = File::create(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
        let log = Mutex::new(BufWriter::new(f));
        // The code objects that are already there (from an image).
        write_snapshot(&mut log.lock().unwrap());
        Some(log)
    })
    .as_ref()
}

fn now() -> u64 {
    let mut ts = libc::timespec { tv_sec: 0, tv_nsec: 0 };
    unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) };
    ts.tv_sec as u64 * 1_000_000_000 + ts.tv_nsec as u64
}

static NEXT_ID: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(0);

/// `start size id`, preceded by `N id name` the first time.
fn entry(w: &mut BufWriter<File>, code: LispObj) {
    let mut id = code.uref(CODE_INFO);
    if !id.is_fixnum() {
        id = LispObj::fixnum(NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
        code.uset(CODE_INFO, id);
        let name = crate::clj::printer::print_str(code.uref(CODE_NAME));
        let _ = writeln!(w, "N {} {name}", id.integer());
    }
    let start = code.strip_tag();
    let size = code.uvector_size() * 16;
    let _ = writeln!(w, "{start:x} {size:x} {}", id.integer());
}

fn write_snapshot(w: &mut BufWriter<File>) {
    let _ = writeln!(w, "G {}", now());
    crate::gc::for_each_object(&mut |p| {
        let h = unsafe { *(p as *const usize) };
        if h & 7 == UVECTOR_LENGTH_TAG && (h >> 3) & 0x1f == COMPILED_CODE_TYPE {
            entry(w, LispObj(p + UVECTOR_TAG));
        }
    });
    let _ = w.flush();
}

/// A new code object.
pub fn code_created(code: LispObj) {
    if let Some(l) = log() {
        let w = &mut *l.lock().unwrap();
        let _ = writeln!(w, "C {}", now());
        entry(w, code);
        let _ = w.flush();
    }
}

/// After a GC: where all code objects are now.
pub fn heap_snapshot() {
    if let Some(l) = log() {
        write_snapshot(&mut l.lock().unwrap());
    }
}
