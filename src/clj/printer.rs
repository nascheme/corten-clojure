//! Kernel Clojure printer for reader data and kernel objects (error
//! messages, and before `pr-str` exists). Once `cljs.core` is loaded the
//! print hook (`G::PrintHook`) takes over for objects the kernel does not
//! know.

use super::*;
use crate::object::*;

/// `pr-str`: readable representation.
pub fn pr_str(x: LispObj) -> String {
    let mut s = String::new();
    pr(x, true, &mut s);
    s
}

/// `pr_str` for an error message: a lazy seq or collection printed by
/// the print hook is cut short (see `bounded`).
pub fn error_str(x: LispObj) -> String {
    bounded(|| pr_str(x))
}

/// `print-str` / `str` style: strings and characters unescaped.
pub fn print_str(x: LispObj) -> String {
    let mut s = String::new();
    pr(x, false, &mut s);
    s
}

fn pr_string(x: LispObj, readably: bool, out: &mut String) {
    let s = x.string_to_rust();
    if !readably {
        out.push_str(&s);
        return;
    }
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            c => out.push(c),
        }
    }
    out.push('"');
}

fn pr_char(code: u32, readably: bool, out: &mut String) {
    let c = char::from_u32(code).unwrap_or('\u{fffd}');
    if !readably {
        out.push(c);
        return;
    }
    out.push('\\');
    match code {
        10 => out.push_str("newline"),
        32 => out.push_str("space"),
        9 => out.push_str("tab"),
        8 => out.push_str("backspace"),
        12 => out.push_str("formfeed"),
        13 => out.push_str("return"),
        _ => out.push(c),
    }
}

fn pr_seq(items: impl Iterator<Item = LispObj>, open: &str, close: &str, out: &mut String) {
    out.push_str(open);
    for (i, x) in items.enumerate() {
        if i > 0 {
            out.push(' ');
        }
        pr(x, true, out);
    }
    out.push_str(close);
}

fn vector_items(v: LispObj) -> impl Iterator<Item = LispObj> {
    let n = v.vector_length().integer() as usize;
    (0..n).map(move |i| v.svref(i))
}

/// A finite float as Java's `Double.toString`/`Float.toString`: plain
/// decimal for magnitudes in [1e-3, 1e7), else `d.dddE[-]n`, with the
/// shortest digits that read back. `plain` and `sci` are Rust's
/// shortest `{}` and `{:e}` forms of the value.
fn java_float_string(x: f64, plain: String, sci: String, out: &mut String) {
    let a = x.abs();
    if a == 0.0 || (1e-3..1e7).contains(&a) {
        out.push_str(&plain);
        if !plain.contains('.') {
            out.push_str(".0");
        }
    } else {
        let (m, e) = sci.split_once('e').unwrap();
        out.push_str(m);
        if !m.contains('.') {
            out.push_str(".0");
        }
        out.push('E');
        out.push_str(e);
    }
}

fn pr_double(d: f64, out: &mut String) {
    if d.is_nan() {
        out.push_str("##NaN");
    } else if d.is_infinite() {
        out.push_str(if d > 0.0 { "##Inf" } else { "##-Inf" });
    } else {
        // Java gives at least two digits: when the shortest has one, the
        // two-digit decimal nearest the exact value if it reads back
        // (Double.MIN_VALUE is 4.9E-324, not 5.0E-324).
        let mut sci = format!("{d:e}");
        if !sci.split_once('e').unwrap().0.contains('.') {
            let two = format!("{d:.1e}");
            if two.parse::<f64>() == Ok(d) {
                sci = two;
            }
        }
        java_float_string(d, format!("{d}"), sci, out);
    }
}

fn pr_single(f: f32, out: &mut String) {
    if f.is_nan() || f.is_infinite() {
        pr_double(f as f64, out);
    } else {
        java_float_string(f as f64, format!("{f}"), format!("{f:e}"), out);
    }
}

/// Print `x` to `out`. (GC is inhibited: the elements of a collection
/// are held in a Rust `Vec` while the print hook may run.)
pub fn pr(x: LispObj, readably: bool, out: &mut String) {
    crate::runtime::without_gc(|| pr1(x, readably, out))
}

fn pr1(x: LispObj, readably: bool, out: &mut String) {
    if x == NIL {
        out.push_str("nil");
    } else if x == TRUE {
        out.push_str("true");
    } else if x == FALSE {
        out.push_str("false");
    } else if x.is_fixnum() {
        out.push_str(&x.integer().to_string());
    } else if x.is_character() {
        pr_char(x.char_code(), readably, out);
    } else if x.is_string() {
        pr_string(x, readably, out);
    } else if x.is_double_float() {
        pr_double(x.double_float(), out);
    } else if crate::numbers::is_bignum(x) || crate::numbers::is_ratio(x) {
        out.push_str(&crate::numbers::exact_string(x).unwrap_or_default());
        if crate::numbers::is_bigint(x) {
            out.push('N');
        }
    } else if x.is_single_float() {
        pr_single(x.single_float(), out);
    } else if x.is_simple_generic_vector() {
        pr_seq(vector_items(x), "[", "]", out);
    } else if coll::is_list(x) || coll::is_empty_list(x) || coll::is_cons(x) {
        pr_seq(coll::items(x).into_iter(), "(", ")", out);
    } else if coll::is_pvector(x) {
        pr_seq(coll::items(x).into_iter(), "[", "]", out);
    } else if coll::is_array_map(x) {
        out.push('{');
        for (i, e) in coll::items(x).into_iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            pr(coll::nth(e, 0), true, out);
            out.push(' ');
            pr(coll::nth(e, 1), true, out);
        }
        out.push('}');
    } else if coll::is_hash_set(x) && coll::is_array_map(x.uref(coll::PHS_MAP)) {
        pr_seq(coll::items(x).into_iter(), "#{", "}", out);
    } else if is_symbol(x) {
        if sym_ns(x) != NIL {
            out.push_str(&sym_ns(x).string_to_rust());
            out.push('/');
        }
        out.push_str(&sym_name(x).string_to_rust());
    } else if is_keyword(x) {
        out.push(':');
        if x.uref(KW_NS) != NIL {
            out.push_str(&x.uref(KW_NS).string_to_rust());
            out.push('/');
        }
        out.push_str(&x.uref(KW_NAME).string_to_rust());
    } else if is_var(x) {
        out.push_str("#'");
        out.push_str(&public_ns_name(var_ns(x)).string_to_rust());
        out.push('/');
        out.push_str(&var_name(x).string_to_rust());
    } else if is_ns(x) {
        out.push_str("#namespace[");
        out.push_str(&ns_name(x).string_to_rust());
        out.push(']');
    } else {
        let hook = global(G::PrintHook);
        if hook != NIL && !NO_HOOK.get() {
            let s = crate::runtime::lisp_call(hook, &[x, cbool(readably), cbool(BOUNDED.get())]);
            if s.is_string() {
                out.push_str(&s.string_to_rust());
                return;
            }
        }
        if is_instance(x) {
            out.push_str("#object[");
            out.push_str(&type_name(instance_type(x)).string_to_rust());
            out.push(']');
        } else if x.is_function() {
            out.push_str("#object[fn]");
        } else {
            pr_other(x, out);
        }
    }
}

/// Objects only the kernel sees: arrays, code, uvector headers, ...
fn pr_other(x: LispObj, out: &mut String) {
    let what = match x.tag() {
        UVECTOR_TAG => match x.uvector_type() {
            ARRAY_TYPE | SIMPLE_VECTOR_TYPE => "Array".to_string(),
            COMPILED_CODE_TYPE => "Code".to_string(),
            ty => format!("uvector-type-{ty}"),
        },
        UVECTOR_LENGTH_TAG => "uvector-header".to_string(),
        FORWARD_TAG => "forward".to_string(),
        _ => "unknown".to_string(),
    };
    out.push_str(&format!("#object[{what} {:#x}]", x.raw()));
}

/// `(k/pr-str x &optional readably no-hook)`: with `no-hook` true the
/// print hook is not called (cljs.core's printer uses this for objects
/// it does not know, which would otherwise come back to it).
pub fn pr_str_prim(a: &[LispObj]) -> LispObj {
    let readably = a.len() < 2 || truthy(a[1]);
    let no_hook = a.len() > 2 && truthy(a[2]);
    struct Restore(bool);
    impl Drop for Restore {
        fn drop(&mut self) {
            NO_HOOK.set(self.0);
        }
    }
    let _restore = Restore(NO_HOOK.replace(no_hook));
    let mut s = String::new();
    pr(a[0], readably, &mut s);
    crate::heap::string_node(&s)
}

/// Run `f` with objects printed by the print hook bounded in length and
/// depth (for error messages).
pub fn bounded<T>(f: impl FnOnce() -> T) -> T {
    struct Restore(bool);
    impl Drop for Restore {
        fn drop(&mut self) {
            BOUNDED.set(self.0);
        }
    }
    let _restore = Restore(BOUNDED.replace(true));
    f()
}

thread_local! {
    static BOUNDED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static NO_HOOK: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}
