//! Rust kernel of Corten Clojure (x86-64 Linux). See `DESIGN.md`.

pub mod clj;
pub mod compiler;
pub mod error;
pub mod gc;
pub mod heap;
pub mod image;
pub mod numbers;
pub mod object;
pub mod perfmap;
pub mod prims;
pub mod qv;
pub mod qv_index;
pub mod runtime;
pub mod streams;

use std::sync::{Mutex, MutexGuard, Once};

use qv_index::*;

static INIT: Once = Once::new();
static LISP_LOCK: Mutex<()> = Mutex::new(());

/// Map the heap and create the kernel symbols, kernel functions, Clojure
/// globals and the console stream. Idempotent.
pub fn init_lisp() {
    INIT.call_once(|| {
        let base = heap::map_region();
        qv::set_qv_base(base);
        heap::init_alloc();
        // The world is not consistent until initialization is done.
        heap::inhibit_gc(1);
        qv::set_qv(GC_COUNT_Index, object::LispObj::fixnum(0));
        runtime::init_runtime();
        clj::init();
        heap::inhibit_gc(-1);
    });
}

/// Initialize the Lisp world from an image file instead of building it.
/// Must be called before anything else touches the heap.
pub fn init_lisp_from_image(path: &str) -> Result<(), String> {
    let mut r = Err("Lisp already initialized".to_string());
    INIT.call_once(|| {
        let base = heap::map_region();
        qv::set_qv_base(base);
        r = image::load_image_into_heap(path);
        if r.is_ok() {
            image::restore_after_load();
        }
    });
    r
}

/// Run `f` with exclusive access to the (single-threaded) Lisp world,
/// initializing it first if needed. Used by tests and embedders.
pub fn with_lisp<R>(f: impl FnOnce() -> R) -> R {
    let _guard: MutexGuard<'_, ()> = LISP_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    init_lisp();
    f()
}
