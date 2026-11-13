//! Kernel error signalling.
//!
//! A kernel error is an exception like any other: an instance of a
//! `corten.lang` error type (made by the kernel at start-up, see
//! `ERROR_TYPES`), thrown to the innermost catcher, a Clojure `try` or
//! the loader / REPL (`catch_lisp_error`). Like any throw, an error
//! discards intervening Rust frames without unwinding.

use crate::object::{LispObj, NIL};
use crate::qv::qv;
use crate::runtime;

/// An exception caught by `catch_lisp_error`: its description and the
/// thrown value.
#[derive(Debug, Clone)]
pub struct LispError {
    pub msg: String,
    pub value: LispObj,
}

impl std::fmt::Display for LispError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

/// The `corten.lang` error types the kernel defines (names and the index
/// of the supertype): first one per `ErrorKind`, in order.
pub const ERROR_TYPES: [(&str, Option<usize>); 9] = [
    ("Error", None),
    ("ArgumentError", Some(0)),
    ("IndexError", Some(1)),
    ("TypeError", Some(1)),
    ("ArithmeticError", Some(0)),
    ("StateError", Some(0)),
    ("UnsupportedError", Some(0)),
    ("ArityError", Some(1)),
    ("AssertionError", Some(0)),
];

/// The kind of a kernel error: which `corten.lang` error type it is
/// (an index into `ERROR_TYPES`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Error = 0,
    /// A bad argument value (`ArgumentError`).
    Argument = 1,
    /// An index out of range (`IndexError`).
    Index = 2,
    /// A value of the wrong type (`TypeError`).
    Type = 3,
    /// Division by zero, overflow (`ArithmeticError`).
    Arithmetic = 4,
    /// An invalid operation in the current state (`StateError`).
    State = 5,
    /// An operation the value does not support (`UnsupportedError`).
    Unsupported = 6,
    /// A wrong number of arguments (`ArityError`).
    Arity = 7,
}

/// Signal an error with message `msg` (not a format control string).
pub fn lisp_error(msg: impl Into<String>) -> ! {
    error_of_kind(ErrorKind::Error, msg)
}

/// Signal a `TypeError`.
pub fn type_error(msg: impl Into<String>) -> ! {
    error_of_kind(ErrorKind::Type, msg)
}

/// Signal an `ArgumentError`.
pub fn argument_error(msg: impl Into<String>) -> ! {
    error_of_kind(ErrorKind::Argument, msg)
}

/// Signal an `IndexError`.
pub fn index_error(msg: impl Into<String>) -> ! {
    error_of_kind(ErrorKind::Index, msg)
}

/// Signal an `ArithmeticError`.
pub fn arithmetic_error(msg: impl Into<String>) -> ! {
    error_of_kind(ErrorKind::Arithmetic, msg)
}

/// Signal an `ArityError`: `nargs` arguments passed to function `name`.
pub fn arity_error(nargs: usize, name: &str) -> ! {
    // Past 20 arguments, as in Clojure's ArityException.
    let n = if nargs > 20 { "> 20".to_string() } else { nargs.to_string() };
    signal(ErrorKind::Arity, format!("Wrong number of args ({n}) passed to: {name}"), Some((nargs, name)))
}

/// Signal an error of `kind`.
pub fn error_of_kind(kind: ErrorKind, msg: impl Into<String>) -> ! {
    signal(kind, msg.into(), None)
}

fn signal(kind: ErrorKind, msg: String, arity: Option<(usize, &str)>) -> ! {
    throw(crate::clj::exceptions::make_error(kind, &msg, arity, NIL))
}

/// Signal an `Error` with message `msg` caused by exception `cause`.
pub fn error_with_cause(msg: impl Into<String>, cause: LispObj) -> ! {
    throw(crate::clj::exceptions::make_error(ErrorKind::Error, &msg.into(), None, cause))
}

/// Throw `x` to the innermost catcher (a `try`, or `catch_lisp_error`).
/// With none, the error is reported and the process exits.
pub fn throw(x: LispObj) -> ! {
    if !runtime::has_catcher() {
        eprintln!("Unhandled error: {}", crate::clj::exceptions::describe(x));
        std::process::exit(1);
    }
    runtime::throw_to(x)
}

/// Run `f`, converting an exception thrown out of it into `Err`.
pub fn catch_lisp_error<R>(f: impl FnOnce() -> R) -> Result<R, LispError> {
    match runtime::rust_catch(f) {
        Ok(v) => Ok(v),
        Err(_) => {
            let v = qv(crate::qv_index::THROWN_VALUE_Index);
            Err(LispError { msg: crate::clj::exceptions::describe(v), value: v })
        }
    }
}
