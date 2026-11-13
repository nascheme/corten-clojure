//! Clojure exceptions on the kernel's catch/throw.
//!
//! `(try body (catch T e h) (finally f))` compiles to an IR `Try` (see
//! `clj_try` in `src/compiler/frontend/clj.rs`): `body` runs under a
//! catch frame, whose handler is `(if (k/instance? T c) (let [e c] h)
//! (k/throw c))` on the thrown value `c`, all under an unwind-protect
//! frame running `f`. A throw goes to the innermost catch frame: a
//! `try`, or a Rust catcher (`error::catch_lisp_error`) of the loader,
//! the REPL or `main`. A kernel error (Rust `lisp_error`) is thrown as
//! an instance of the `corten.lang` error type of its kind.

use super::*;
use crate::error::ErrorKind;

/// The error type of `kind`.
fn error_type(kind: usize) -> LispObj {
    global(G::ErrorTypes).svref(kind)
}

/// The exception for a kernel error of `kind` with message `msg` and
/// cause `cause`; `arity` is the argument count and function name of an
/// arity error. Before the Clojure globals exist (kernel start-up), the
/// message string.
pub fn make_error(kind: ErrorKind, msg: &str, arity: Option<(usize, &str)>, cause: LispObj) -> LispObj {
    if !qv(CLJ_GLOBALS_Index).is_simple_generic_vector() {
        return string_node(msg);
    }
    let t = error_type(kind as usize);
    let m = string_node(msg);
    match (kind, arity) {
        (ErrorKind::Arity, Some((n, name))) => {
            make_instance(t, &[m, cause, LispObj::fixnum(n as i64), string_node(name)])
        }
        (ErrorKind::Arity, None) => make_instance(t, &[m, cause, NIL, NIL]),
        _ => make_instance(t, &[m, cause]),
    }
}

/// The message of a thrown value: its `message` field if it has one (an
/// error or ex-info), else how it prints.
fn message(x: LispObj) -> String {
    if is_instance(x) && !is_type(x) {
        let ty = instance_type(x);
        if let Some(i) = field_index(ty, string_node("message")) {
            let m = x.uref(INSTANCE_FIELDS + i);
            if m.is_string() {
                return m.string_to_rust();
            }
        }
    }
    printer::pr_str(x)
}

/// How an uncaught exception is reported: the message of a kernel error
/// type's instance, else "Clojure exception: " and its message.
pub fn describe(x: LispObj) -> String {
    if x.is_string() && !qv(CLJ_GLOBALS_Index).is_simple_generic_vector() {
        return x.string_to_rust();
    }
    let types = global(G::ErrorTypes);
    let kernel = is_instance(x) && (0..crate::error::ERROR_TYPES.len()).any(|i| types.svref(i) == instance_type(x));
    if kernel { message(x) } else { format!("Clojure exception: {}", message(x)) }
}

/// `(k/throw x)`.
pub fn throw(a: &[LispObj]) -> LispObj {
    crate::error::throw(a[0])
}

/// `(k/kernel-type name)`: the kernel-defined type that `deftype` of
/// `name` (namespace-qualified) extends, or nil.
pub fn kernel_type(a: &[LispObj]) -> LispObj {
    let n = a[0].string_to_rust();
    if let Some(e) = n.strip_prefix("corten.lang/")
        && let Some(i) = crate::error::ERROR_TYPES.iter().position(|&(t, _)| t == e)
    {
        return error_type(i);
    }
    match n.as_str() {
        "clojure.core/Symbol" => global(G::SymbolType),
        "clojure.core/Keyword" => global(G::KeywordType),
        _ => super::coll::kernel_type(&n).unwrap_or(NIL),
    }
}

/// `(k/instance? type x)`: true if `x`'s type is `type`
/// or a subtype of it.
pub fn instance_p(a: &[LispObj]) -> LispObj {
    cbool(is_type(a[0]) && is_subtype(type_of(a[1]), a[0]))
}

/// `(k/type-super type)`: the supertype of `type`, or NIL.
pub fn type_super_prim(a: &[LispObj]) -> LispObj {
    if !is_type(a[0]) {
        crate::error::lisp_error(format!("Not a type: {}", printer::pr_str(a[0])));
    }
    type_super(a[0])
}

/// `(k/set-super type super)`: make `type` a subtype of `super`.
pub fn set_super(a: &[LispObj]) -> LispObj {
    for &t in a {
        if !is_type(t) {
            crate::error::lisp_error(format!("Not a type: {}", printer::pr_str(t)));
        }
    }
    a[0].uset(TYPE_SUPER, a[1]);
    bump_method_epoch();
    a[0]
}
