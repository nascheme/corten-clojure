//! Host primitives for the Clojure side: list, string, number and output
//! functions.

use super::*;
use crate::numbers::{self, Num};
use crate::streams;

fn units(s: LispObj) -> &'static [LispChar] {
    let len = s.vector_length().integer() as usize;
    unsafe { std::slice::from_raw_parts(s.char_array_start(), len) }
}

fn check_string(x: LispObj) -> LispObj {
    if !x.is_string() {
        crate::error::type_error(format!("Not a string: {}", printer::pr_str(x)));
    }
    x
}

fn check_index(x: LispObj) -> usize {
    if !x.is_fixnum() || x.integer() < 0 {
        crate::error::type_error(format!("Not a valid index: {}", printer::pr_str(x)));
    }
    x.integer() as usize
}

/// `(k/simple-vector? x)`: a raw `[...]` (CL simple-vector).
pub fn simple_vector_p(a: &[LispObj]) -> LispObj {
    cbool(a[0].is_simple_generic_vector())
}

/// `(k/length x)`: length of a string or simple vector.
pub fn length(a: &[LispObj]) -> LispObj {
    let x = a[0];
    let n = if x.is_vector() {
        x.vector_length().integer() as usize
    } else {
        crate::error::argument_error(format!("Not a sequence: {}", printer::pr_str(x)))
    };
    LispObj::fixnum(n as i64)
}

/// `(k/subseq x start [end])` of a string or simple vector.
pub fn subseq(a: &[LispObj]) -> LispObj {
    let x = a[0];
    let start = check_index(a[1]);
    let len = length(&a[..1]).integer() as usize;
    let end = if a.len() > 2 && a[2] != NIL { check_index(a[2]) } else { len };
    if start > end || end > len {
        crate::error::index_error(format!("Bad subsequence bounds {start} {end} of length {len}"));
    }
    if x.is_string() {
        heap::string_from_units(&units(x)[start..end])
    } else if x.is_simple_generic_vector() {
        let v = heap::vector_node(end - start);
        for i in start..end {
            v.svset(i - start, x.svref(i));
        }
        v
    } else {
        crate::error::argument_error(format!("Not a sequence: {}", printer::pr_str(x)))
    }
}

/// `(k/char s i)`: character `i` of string `s`.
pub fn char_at(a: &[LispObj]) -> LispObj {
    let u = units(check_string(a[0]));
    let i = check_index(a[1]);
    if i >= u.len() {
        crate::error::index_error(format!("String index out of range: {i}"));
    }
    LispObj::character(u[i] as u32)
}




/// `(k/reverse s)`: a reversed copy of string `s` (by code points, so
/// surrogate pairs stay in order).
pub fn reverse(a: &[LispObj]) -> LispObj {
    let s: String = String::from_utf16_lossy(units(check_string(a[0]))).chars().rev().collect();
    string_node(&s)
}


/// `(k/string-index-of s sub [from])`: index of `sub` in `s`, or -1.
pub fn string_index_of(a: &[LispObj]) -> LispObj {
    let s = units(check_string(a[0]));
    let sub = units(check_string(a[1]));
    let from = if a.len() > 2 { a[2].integer().clamp(0, s.len() as i64) as usize } else { 0 };
    let r = if sub.is_empty() {
        Some(from)
    } else {
        s[from..].windows(sub.len()).position(|w| w == sub).map(|i| i + from)
    };
    LispObj::fixnum(r.map_or(-1, |i| i as i64))
}

/// `(k/string-concat x...)`: the strings and characters `x`
/// concatenated.
pub fn string_concat(a: &[LispObj]) -> LispObj {
    let mut v: Vec<LispChar> = Vec::new();
    for &x in a {
        if x.is_character() {
            v.push(x.char_code() as LispChar);
        } else {
            v.extend_from_slice(units(check_string(x)));
        }
    }
    heap::string_from_units(&v)
}

fn check_var(x: LispObj) -> LispObj {
    if !is_var(x) {
        crate::error::type_error(format!("Not a var: {}", printer::pr_str(x)));
    }
    x
}

/// `(k/var-ns var)`: the namespace a var is interned in.
pub fn var_ns_prim(a: &[LispObj]) -> LispObj {
    var_ns(check_var(a[0]))
}

/// `(k/var-meta var)`: the raw metadata of a var.
///
/// The metadata `def` gave it, or none, has `:ns` and `:name` added.
pub fn var_meta_prim(a: &[LispObj]) -> LispObj {
    let v = check_var(a[0]);
    let m = var_meta(v);
    if m != NIL && !var_flag(v, DEF_META_FLAG) {
        return m;
    }
    let ns = symbol(NIL, public_ns_name(var_ns(v)));
    let name = symbol(NIL, var_name(v));
    let m = coll::assoc(m, keyword_from(None, "ns"), ns);
    coll::assoc(m, keyword_from(None, "name"), name)
}

/// `(k/ns-name ns)`: the name string of a namespace.
pub fn ns_name_prim(a: &[LispObj]) -> LispObj {
    if !is_ns(a[0]) {
        crate::error::type_error(format!("Not a namespace: {}", printer::pr_str(a[0])));
    }
    ns_name(a[0])
}

fn check_real(x: LispObj) -> Num {
    if !numbers::is_number(x) {
        crate::error::type_error(format!("Not a number: {}", printer::pr_str(x)));
    }
    numbers::num(x)
}

fn is_nan(x: LispObj) -> bool {
    match check_real(x) {
        Num::Double(d) => d.is_nan(),
        Num::Single(f) => f.is_nan(),
        _ => false,
    }
}

/// The first NaN of `a`, else the extremum; on a tie the later argument
/// (as `(if (> x y) x y)`).
fn extremum(a: &[LispObj], skip: std::cmp::Ordering) -> LispObj {
    let mut r = a[0];
    check_real(r);
    for &x in &a[1..] {
        if is_nan(r) {
            break;
        }
        if is_nan(x) || numbers::compare(x, r) != skip {
            r = x;
        }
    }
    r
}

/// `(k/max x y...)`: the largest number; NaN if any is NaN.
pub fn max(a: &[LispObj]) -> LispObj {
    extremum(a, std::cmp::Ordering::Less)
}

/// `(k/min x y...)`: the smallest number; NaN if any is NaN.
pub fn min(a: &[LispObj]) -> LispObj {
    extremum(a, std::cmp::Ordering::Greater)
}

/// `(k/write-string s err)`: write `s` to standard output, or to
/// error output when `err` is true.
pub fn write_string(a: &[LispObj]) -> LispObj {
    let s = check_string(a[0]);
    let text = String::from_utf16_lossy(units(s));
    if a.len() > 1 && truthy(a[1]) {
        streams::write_err(&text);
    } else {
        streams::write_out(&text);
    }
    s
}

/// `(k/flush)`: flush standard output.
pub fn flush(_a: &[LispObj]) -> LispObj {
    streams::flush_out();
    NIL
}

/// `(k/string-input-stream string)`: a stream reading `string`
/// (`load-string`).
pub fn string_input_stream(a: &[LispObj]) -> LispObj {
    streams::make_string_input_stream(check_string(a[0]))
}

/// The input stream `x`, or standard input for nil.
fn input_stream(x: LispObj) -> LispObj {
    if x == NIL { streams::console() } else { streams::check_stream(x) }
}

/// `(k/read-char stream)`: the next UTF-16 unit of `stream` (nil:
/// standard input) as an integer, or -1 at the end (java.io.Reader.read).
pub fn read_char(a: &[LispObj]) -> LispObj {
    let c = streams::get_character(input_stream(a[0]));
    LispObj::fixnum(c.map_or(-1, |c| c as i64))
}

/// `(k/unread-char stream)`: put back the character last read.
pub fn unread_char(a: &[LispObj]) -> LispObj {
    streams::unread_character(input_stream(a[0]));
    NIL
}

/// `(k/read-line stream)`: the next line of `stream` (nil: standard
/// input) without its terminator (\n, \r or \r\n), or nil at the end.
pub fn read_line(a: &[LispObj]) -> LispObj {
    let s = input_stream(a[0]);
    let mut line: Vec<LispChar> = Vec::new();
    let mut any = false;
    while let Some(c) = streams::get_character(s) {
        any = true;
        match c {
            0x0a => break,
            0x0d => {
                if streams::get_character(s).is_some_and(|d| d != 0x0a) {
                    streams::unread_character(s);
                }
                break;
            }
            u => line.push(u as LispChar),
        }
    }
    if any { crate::heap::string_from_units(&line) } else { NIL }
}

/// `(k/clear-input &optional stream)`: discard the buffered input of
/// `stream` (default standard input), the rest of the line after a REPL
/// error.
pub fn clear_input(a: &[LispObj]) -> LispObj {
    streams::clear_input(input_stream(a.first().copied().unwrap_or(NIL)));
    NIL
}

/// `(k/open-input-file path)`: a stream reading file `path`.
pub fn open_input_file(a: &[LispObj]) -> LispObj {
    streams::make_file_input_stream(&check_string(a[0]).string_to_rust())
}

/// `(k/close stream)`: close `stream`.
pub fn close(a: &[LispObj]) -> LispObj {
    streams::close_stream(streams::check_stream(a[0]));
    NIL
}


/// `(k/type? x)`: a type descriptor (deftype, protocol host type).
pub fn type_p(a: &[LispObj]) -> LispObj {
    cbool(is_instance(a[0]) && instance_type(a[0]) == global(G::TypeType))
}

/// `(k/var-name var)`: the name string of a var.
pub fn var_name_prim(a: &[LispObj]) -> LispObj {
    var_name(check_var(a[0]))
}

/// Java's `Character.isWhitespace`: Unicode space, line and paragraph
/// separators other than the no-break spaces, and the ASCII controls
/// tab to return and file to unit separator.
pub fn is_java_whitespace(c: u32) -> bool {
    match c {
        0x09..=0x0d | 0x1c..=0x1f => true,
        0xa0 | 0x2007 | 0x202f => false,
        // Unicode White_Space beyond the controls is Zs, Zl and Zp.
        _ => c != 0x85 && char::from_u32(c).is_some_and(char::is_whitespace),
    }
}

/// `(k/string-trim s mode)`: `s` without leading (mode 1), trailing
/// (mode 2) or both (mode 0) JavaScript whitespace: space, tab, newline,
/// return, form feed, vertical tab, no-break space. Modes 3 to 5 are
/// modes 0 to 2 with Java whitespace (`is_java_whitespace`).
pub fn string_trim(a: &[LispObj]) -> LispObj {
    let u = units(check_string(a[0])).to_vec();
    let java = a[1].integer() >= 3;
    let mode = a[1].integer() % 3;
    let ws = |c: &LispChar| {
        if java {
            is_java_whitespace(*c as u32)
        } else {
            matches!(*c, 0x20 | 0x09 | 0x0a | 0x0d | 0x0c | 0x0b | 0xa0)
        }
    };
    let start = if mode == 2 { 0 } else { u.iter().position(|c| !ws(c)).unwrap_or(u.len()) };
    let end = if mode == 1 { u.len() } else { u.iter().rposition(|c| !ws(c)).map_or(start, |i| i + 1) };
    heap::string_from_units(&u[start..end.max(start)])
}

fn as_f64(x: LispObj) -> f64 {
    numbers::as_f64(&check_real(x))
}

/// `(k/object-is a b)`: JavaScript `Object.is` (SameValue): NaN is
/// itself, 0.0 is not -0.0. Clojure boolean.
pub fn object_is(a: &[LispObj]) -> LispObj {
    let (x, y) = (a[0], a[1]);
    if x == y {
        return TRUE;
    }
    if x.is_float() && y.is_float() {
        let (p, q) = (as_f64(x), as_f64(y));
        return cbool(p.to_bits() == q.to_bits() || (p.is_nan() && q.is_nan()));
    }
    if numbers::is_number(x) && numbers::is_number(y) && !x.is_float() && !y.is_float() {
        return cbool(numbers::compare(x, y) == std::cmp::Ordering::Equal);
    }
    FALSE
}

/// `(k/safe-integer? x)`: `Number.isSafeInteger`. Clojure boolean.
pub fn safe_integer_p(a: &[LispObj]) -> LispObj {
    if !numbers::is_number(a[0]) {
        return FALSE;
    }
    let d = as_f64(a[0]);
    cbool(d.is_finite() && d.abs() <= 9007199254740991.0 && d == d.trunc())
}

fn float_or_int(d: f64) -> LispObj {
    heap::double_float_node(d)
}

/// `(k/math-fn name x)`: `Math.sqrt`, `log`, `exp`, `sin`, `cos`, `tan`, `abs` (doubles,
/// except `abs` of an integer).
pub fn math_fn(a: &[LispObj]) -> LispObj {
    let x = a[1];
    let name = a[0].string_to_rust();
    if name == "abs" {
        return match check_real(x) {
            Num::Int(i) => match i.checked_abs().and_then(|v| LispObj::fixnum_checked(v)) {
                Some(v) => v,
                None => float_or_int((i as f64).abs()),
            },
            _ => float_or_int(as_f64(x).abs()),
        };
    }
    let d = as_f64(x);
    float_or_int(match name.as_str() {
        "sqrt" => d.sqrt(),
        "log" => d.ln(),
        "exp" => d.exp(),
        "sin" => d.sin(),
        "cos" => d.cos(),
        "tan" => d.tan(),
        _ => lisp_error(format!("No Math function {name}")),
    })
}

/// `(k/math-pow x y)`: a double, as in JavaScript, except an exact
/// integer power that fits (there are no bignums).
pub fn math_pow(a: &[LispObj]) -> LispObj {
    let d = as_f64(a[0]).powf(as_f64(a[1]));
    if let (Num::Int(x), Num::Int(y)) = (numbers::num(a[0]), numbers::num(a[1]))
        && y >= 0
        && d.abs() < 9007199254740992.0
        && let Some(v) = u32::try_from(y).ok().and_then(|y| x.checked_pow(y))
    {
        return LispObj::fixnum(v);
    }
    float_or_int(d)
}

/// `(k/format-timestamp y mo d h mi s ms)`: an RFC 3339 UTC timestamp
/// string (month `mo` counts from 1).
pub fn format_timestamp(a: &[LispObj]) -> LispObj {
    let v: Vec<i64> = a.iter().map(|x| numbers::integer_value(*x)).collect();
    string_node(&format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
        v[0], v[1], v[2], v[3], v[4], v[5], v[6]
    ))
}
