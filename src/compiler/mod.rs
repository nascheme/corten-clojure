//! Kernel compiler and the Clojure `eval` / `load` built on it.

pub mod codegen;
pub mod frontend;

use crate::error::lisp_error;
use crate::heap;
use crate::object::*;
use crate::runtime::{lisp_call, make_function, without_gc};
use crate::streams;

/// Compile Clojure `form` as the body of a function of no arguments.
pub fn compile_clj_form(form: LispObj) -> LispObj {
    without_gc(|| {
        let mut fe = frontend::Frontend::new();
        let id = fe.convert_clj_toplevel(form);
        let code = codegen::compile_lambda(&fe, id);
        make_function(code, NIL)
    })
}

/// Evaluate a Clojure form. Top-level `do` forms are evaluated one
/// subform at a time, after macroexpansion, so that macros defined by
/// earlier subforms apply to later ones.
pub fn clj_eval(form: LispObj) -> LispObj {
    let mut x = form;
    while let Some(e) = frontend::clj::macroexpand_1(x) {
        x = e;
    }
    if frontend::clj::is_do_form(x) {
        // (Walked on the heap: a collection may move.)
        let mut body = crate::clj::coll::next(x);
        let mut r = NIL;
        while body != NIL {
            r = clj_eval(crate::clj::coll::first(body));
            body = crate::clj::coll::next(body);
        }
        return r;
    }
    let f = compile_clj_form(x);
    lisp_call(f, &[])
}

/// Load a Clojure source file: read and evaluate every form, with
/// `*ns*` restored afterwards.
pub fn clj_load(path: LispObj) -> LispObj {
    let ns_var = crate::clj::global(crate::clj::G::NsVar);
    crate::runtime::rebinding(ns_var, || clj_load_tracked(path))
}

/// Load a Clojure boot file: as `clj_load`, but `*ns*` is not rebound,
/// so the namespace it ends in stays current (the image's default).
pub fn clj_boot(path: LispObj) -> LispObj {
    clj_load_tracked(path)
}

fn clj_load_tracked(path: LispObj) -> LispObj {
    let depth = CLJ_LOADING.with(|l| {
        let mut l = l.borrow_mut();
        l.push(path.string_to_rust());
        l.len() - 1
    });
    let r = clj_load_1(path);
    // A non-local exit can skip this; the next load to finish below it
    // drops the stale entries.
    CLJ_LOADING.with(|l| l.borrow_mut().truncate(depth));
    r
}

thread_local! {
    /// The files being loaded by `clj_load`, outermost first.
    static CLJ_LOADING: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
    /// The file and line of the top-level form being loaded (for the
    /// `:file` and `:line` metadata of the vars it defines).
    static LOAD_POS: std::cell::RefCell<Option<(String, i64)>> = const { std::cell::RefCell::new(None) };
}

/// The file and line of the top-level form being loaded, if any.
pub fn load_pos() -> Option<(String, i64)> {
    LOAD_POS.with(|p| p.borrow().clone())
}

/// Whether `clj_load` is loading file `path`.
pub fn clj_loading(path: &str) -> bool {
    CLJ_LOADING.with(|l| l.borrow().iter().any(|p| p == path))
}

fn clj_load_1(path: LispObj) -> LispObj {
    if !path.is_string() {
        lisp_error("load: not a string");
    }
    let p = path.string_to_rust();
    if std::fs::metadata(&p).is_err() {
        lisp_error(format!("Could not locate {p}"));
    }
    let s = streams::make_file_input_stream(&p);
    let trace = std::env::var_os("CLJ_LOAD_TRACE").is_some();
    let outer = load_pos();
    let abs = std::fs::canonicalize(&p).map_or_else(|_| p.clone(), |a| a.to_string_lossy().into_owned());
    let r = crate::error::catch_lisp_error(|| {
        loop {
            let line = streams::line_number(s) + 1;
            if trace {
                eprintln!(";;; {p}:{line}");
            }
            let r = crate::error::catch_lisp_error(|| match crate::clj::reader::read(s) {
                Some(x) => {
                    let pos = (abs.clone(), crate::clj::reader::form_line());
                    LOAD_POS.with(|l| *l.borrow_mut() = Some(pos));
                    clj_eval(x);
                    true
                }
                None => false,
            });
            match r {
                Ok(true) => {}
                Ok(false) => break,
                Err(e) => crate::error::error_with_cause(format!("{e}\n  while loading {p} (form at line {line})"), e.value),
            }
        }
    });
    streams::close_stream(s);
    LOAD_POS.with(|l| *l.borrow_mut() = outer);
    if let Err(e) = r {
        crate::error::throw(e.value);
    }
    NIL
}

/// Read and evaluate all Clojure forms in `text`; returns the last value.
pub fn clj_eval_string(text: &str) -> LispObj {
    let s = streams::make_string_input_stream(heap::string_node(text));
    let mut r = NIL;
    while let Some(x) = crate::clj::reader::read(s) {
        r = clj_eval(x);
    }
    r
}

