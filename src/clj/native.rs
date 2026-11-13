//! Operations on native values: the functions of `corten.string`,
//! `corten.array` and `corten.number` (`FUNCTIONS`), which the ported
//! libraries use, and JavaScript methods (`(.m x args...)` on strings,
//! characters, arrays, numbers and functions, and static methods of their
//! types such as `String/fromCharCode`), kept for cljs code.
//!
//! `METHODS` lists methods by type descriptor and name. Each is a kernel
//! function (`prims::kernel_function_node`, at index `PRIMS.len() + i`),
//! installed in its type's method table (`TYPE_OBJ_METHODS`) or static
//! table (`TYPE_STATICS`) by `install`, so `(.m x)` finds it as it finds
//! a deftype's methods. An instance method takes the receiver first.
//! Names and semantics are JavaScript's (cljs.core calls them); a
//! character is a one-character string.

use super::*;
use crate::numbers::{self, Num};
use crate::prims::{PrimFn, funcall};
use std::cmp::Ordering;

pub struct Method {
    pub ty: G,
    pub name: &'static str,
    pub func: PrimFn,
    pub is_static: bool,
}

const fn m(ty: G, name: &'static str, func: PrimFn) -> Method {
    Method { ty, name, func, is_static: false }
}

const fn s(ty: G, name: &'static str, func: PrimFn) -> Method {
    Method { ty, name, func, is_static: true }
}

/// String methods, also installed on characters.
const STRING_METHODS: &[(&str, PrimFn)] = &[
    ("charAt", str_char_at),
    ("charCodeAt", str_char_code_at),
    ("substring", str_substring),
    ("slice", str_slice),
    ("indexOf", str_index_of),
    ("lastIndexOf", str_last_index_of),
    ("toLowerCase", str_to_lower_case),
    ("toUpperCase", str_to_upper_case),
    ("toString", str_to_string),
    ("valueOf", str_to_string),
    ("trim", str_trim),
    ("startsWith", str_starts_with),
    ("endsWith", str_ends_with),
    ("split", str_split),
    ("concat", str_concat),
    ("replace", str_replace),
    ("match", str_match),
    ("search", str_search),
];

pub static METHODS: &[Method] = &[
    s(G::StringType, "fromCharCode", str_from_char_code),
    m(G::ArrayType, "push", arr_push),
    m(G::ArrayType, "pop", arr_pop),
    m(G::ArrayType, "shift", arr_shift),
    m(G::ArrayType, "slice", arr_slice),
    m(G::ArrayType, "splice", arr_splice),
    m(G::ArrayType, "join", arr_join),
    m(G::ArrayType, "toString", arr_to_string),
    m(G::ArrayType, "indexOf", arr_index_of),
    m(G::ArrayType, "forEach", arr_for_each),
    m(G::ArrayType, "map", arr_map),
    m(G::ArrayType, "sort", arr_sort),
    m(G::ArrayType, "concat", arr_concat),
    m(G::NumberType, "toString", num_to_string),
    m(G::NumberType, "valueOf", identity),
    m(G::FunctionType, "call", fn_call),
    m(G::FunctionType, "apply", fn_apply),
];

/// Functions of the `corten.*` namespaces: (namespace, name, function,
/// min args, max args). They take the receiver first, as methods do.
pub static FUNCTIONS: &[(&str, &str, PrimFn, usize, usize)] = &[
    ("corten.string", "length", cs_length, 1, 1),
    ("corten.string", "char-code-at", cs_char_code_at, 2, 2),
    ("corten.string", "substring", cs_substring, 2, 3),
    ("corten.string", "index-of", cs_index_of, 2, 3),
    ("corten.string", "last-index-of", cs_last_index_of, 2, 3),
    ("corten.string", "lower-case", cs_lower_case, 1, 1),
    ("corten.string", "upper-case", cs_upper_case, 1, 1),
    ("corten.string", "trim", cs_trim, 1, 1),
    ("corten.string", "starts-with?", cs_starts_with, 2, 2),
    ("corten.string", "ends-with?", cs_ends_with, 2, 2),
    ("corten.string", "split", cs_split, 2, 2),
    ("corten.string", "replace", cs_replace, 3, 3),
    ("corten.array", "push!", ca_push, 2, usize::MAX),
    ("corten.array", "pop!", ca_pop, 1, 1),
    ("corten.array", "shift!", ca_shift, 1, 1),
    ("corten.array", "slice", ca_slice, 1, 3),
    ("corten.array", "splice!", ca_splice, 3, 3),
    ("corten.array", "join", ca_join, 1, 2),
    ("corten.array", "sort!", ca_sort, 1, 2),
    ("corten.array", "map", ca_map, 2, 2),
    ("corten.array", "for-each", ca_for_each, 2, 2),
    ("corten.number", "to-string", cn_to_string, 1, 2),
];

/// Number of native kernel functions: `METHODS`, `STRING_METHODS`, then
/// `FUNCTIONS`.
pub fn count() -> usize {
    METHODS.len() + STRING_METHODS.len() + FUNCTIONS.len()
}

/// Call native kernel function `i` (`prims::call`).
pub fn call(i: usize, a: &[LispObj]) -> LispObj {
    let n = METHODS.len();
    let k = STRING_METHODS.len();
    if i < n {
        (METHODS[i].func)(a)
    } else if i < n + k {
        (STRING_METHODS[i - n].1)(a)
    } else {
        let (ns, name, f, min, max) = FUNCTIONS[i - n - k];
        if a.len() < min || a.len() > max {
            crate::error::arity_error(a.len(), &format!("{ns}/{name}"));
        }
        f(a)
    }
}

/// Name of native kernel function `i` (for the image's kernel signature).
pub fn name(i: usize) -> String {
    let n = METHODS.len();
    let k = STRING_METHODS.len();
    if i < n {
        format!("{}.{}", METHODS[i].ty as usize, METHODS[i].name)
    } else if i < n + k {
        format!("string.{}", STRING_METHODS[i - n].0)
    } else {
        format!("{}/{}", FUNCTIONS[i - n - k].0, FUNCTIONS[i - n - k].1)
    }
}

/// Install the native methods in their types' tables and the `corten.*`
/// functions as vars (`init`).
pub(crate) fn install() {
    let base = crate::prims::PRIMS.len();
    let f = |i: usize| crate::prims::kernel_function_node(base + i);
    for (i, me) in METHODS.iter().enumerate() {
        let ty = global(me.ty);
        let name = string_node(me.name);
        if me.is_static {
            set_type_static(ty, name, f(i));
        } else {
            add_obj_method(ty, name, f(i));
        }
    }
    let n = METHODS.len();
    for (i, (name, _)) in STRING_METHODS.iter().enumerate() {
        let k = f(n + i);
        add_obj_method(global(G::StringType), string_node(name), k);
        add_obj_method(global(G::CharType), string_node(name), k);
    }
    let n = n + STRING_METHODS.len();
    for (i, (ns, name, ..)) in FUNCTIONS.iter().enumerate() {
        let ns = find_ns(ns).unwrap_or_else(|| create_ns(ns));
        let v = intern_var(ns, string_node(name));
        set_var_value(v, f(n + i));
    }
}

// ---------------------------------------------------------------------------
// Helpers

pub(super) fn units_of(s: LispObj) -> Vec<LispChar> {
    let len = s.vector_length().integer() as usize;
    unsafe { std::slice::from_raw_parts(s.char_array_start(), len) }.to_vec()
}

pub(super) fn from_units(u: &[LispChar]) -> LispObj {
    heap::string_from_units(u)
}

pub(super) fn to_str(x: LispObj) -> LispObj {
    super::prims::str_prim(&[x])
}

pub(super) fn nan() -> LispObj {
    heap::double_float_node(f64::NAN)
}

/// Argument `i` of a method (after the receiver), if passed.
fn arg(a: &[LispObj], i: usize) -> Option<LispObj> {
    a.get(i + 1).copied()
}

fn arg_or_nil(a: &[LispObj], i: usize) -> LispObj {
    arg(a, i).unwrap_or(NIL)
}

/// A JavaScript slice index: negative counts from the end.
fn js_index(i: Option<LispObj>, n: usize) -> usize {
    match i {
        None => n,
        Some(i) if i == NIL => n,
        Some(i) => {
            let i = int_of(i);
            if i < 0 { (n as i64 + i).max(0) as usize } else { (i as usize).min(n) }
        }
    }
}

fn int_of(x: LispObj) -> i64 {
    match numbers::num(x) {
        Num::Int(i) => i,
        Num::Double(d) => d as i64,
        Num::Single(f) => f as i64,
        _ => crate::error::type_error(format!("Not an integer: {}", printer::pr_str(x))),
    }
}

fn find_units(s: &[LispChar], sub: &[LispChar], from: usize) -> Option<usize> {
    let from = from.min(s.len());
    if sub.is_empty() {
        return Some(from);
    }
    s[from..].windows(sub.len()).position(|w| w == sub).map(|i| i + from)
}

/// Last index of `sub` in `s` starting at or before `from`.
fn rfind_units(s: &[LispChar], sub: &[LispChar], from: usize) -> Option<usize> {
    let from = from.min(s.len());
    if sub.is_empty() {
        return Some(from);
    }
    s.windows(sub.len()).take(from + 1).rposition(|w| w == sub)
}

/// A string argument of a string method: JavaScript ToString (nil is
/// "null").
fn string_arg(x: LispObj) -> Vec<LispChar> {
    if x == NIL {
        "null".encode_utf16().collect()
    } else if x.is_string() {
        units_of(x)
    } else if x.is_character() {
        vec![x.char_code() as LispChar]
    } else {
        units_of(to_str(x))
    }
}

fn identity(a: &[LispObj]) -> LispObj {
    a[0]
}

// ---------------------------------------------------------------------------
// Strings (and characters)

/// The receiver string's code units.
fn this_units(a: &[LispObj]) -> Vec<LispChar> {
    string_arg(a[0])
}

fn str_from_char_code(a: &[LispObj]) -> LispObj {
    let c = int_of(a.first().copied().unwrap_or(NIL));
    from_units(&[c as LispChar])
}

fn str_char_at(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let i = int_of(arg(a, 0).unwrap_or(LispObj::fixnum(0)));
    if i >= 0 && (i as usize) < s.len() { from_units(&s[i as usize..=i as usize]) } else { string_node("") }
}

fn str_char_code_at(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let i = arg(a, 0).filter(|&i| i != NIL).map_or(0, int_of);
    if i >= 0 && (i as usize) < s.len() { LispObj::fixnum(s[i as usize] as i64) } else { nan() }
}

fn str_substring(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let n = s.len();
    let clamp = |v: LispObj| if v == NIL { 0 } else { int_of(v).clamp(0, n as i64) as usize };
    let x = arg(a, 0).map_or(0, clamp);
    let y = arg(a, 1).map_or(n, clamp);
    from_units(&s[x.min(y)..x.max(y)])
}

fn str_slice(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let n = s.len();
    let x = js_index(Some(arg(a, 0).unwrap_or(LispObj::fixnum(0))), n);
    let y = js_index(arg(a, 1), n);
    if x < y { from_units(&s[x..y]) } else { string_node("") }
}

fn str_index_of(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let sub = string_arg(arg_or_nil(a, 0));
    let from = arg(a, 1).map_or(0, |f| int_of(f).max(0) as usize);
    LispObj::fixnum(find_units(&s, &sub, from).map_or(-1, |i| i as i64))
}

fn str_last_index_of(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let sub = string_arg(arg_or_nil(a, 0));
    let from = arg(a, 1).filter(|&f| f != NIL).map_or(s.len() as i64, int_of).max(0) as usize;
    LispObj::fixnum(rfind_units(&s, &sub, from).map_or(-1, |i| i as i64))
}

fn str_to_lower_case(a: &[LispObj]) -> LispObj {
    string_node(&String::from_utf16_lossy(&this_units(a)).to_lowercase())
}

fn str_to_upper_case(a: &[LispObj]) -> LispObj {
    string_node(&String::from_utf16_lossy(&this_units(a)).to_uppercase())
}

fn str_to_string(a: &[LispObj]) -> LispObj {
    if a[0].is_string() { a[0] } else { from_units(&this_units(a)) }
}

fn str_trim(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    let ws = |c: &LispChar| matches!(*c, 0x20 | 0x09 | 0x0a | 0x0d | 0x0c);
    let x = s.iter().position(|c| !ws(c)).unwrap_or(s.len());
    let y = s.iter().rposition(|c| !ws(c)).map_or(x, |i| i + 1);
    from_units(&s[x..y.max(x)])
}

fn str_starts_with(a: &[LispObj]) -> LispObj {
    cbool(this_units(a).starts_with(&string_arg(arg_or_nil(a, 0))))
}

fn str_ends_with(a: &[LispObj]) -> LispObj {
    cbool(this_units(a).ends_with(&string_arg(arg_or_nil(a, 0))))
}

fn regexp_arg(a: &[LispObj]) -> Option<LispObj> {
    arg(a, 0).filter(|&r| super::js::is_regexp(r))
}

fn str_split(a: &[LispObj]) -> LispObj {
    let s = this_units(a);
    if let Some(re) = regexp_arg(a) {
        return super::js::regex_split(from_units(&s), re);
    }
    let sep = string_arg(arg_or_nil(a, 0));
    let r = array::new_array(0, 4);
    if sep.is_empty() {
        for &c in &s {
            array::push(r, from_units(&[c]));
        }
        return r;
    }
    let mut start = 0;
    loop {
        match find_units(&s, &sep, start) {
            Some(p) => {
                array::push(r, from_units(&s[start..p]));
                start = p + sep.len();
            }
            None => {
                array::push(r, from_units(&s[start..]));
                return r;
            }
        }
    }
}

fn str_concat(a: &[LispObj]) -> LispObj {
    let mut v = this_units(a);
    for &x in &a[1..] {
        v.extend(units_of(to_str(x)));
    }
    from_units(&v)
}

fn str_replace(a: &[LispObj]) -> LispObj {
    if let Some(re) = regexp_arg(a) {
        return super::js::regex_replace(a[0], re, arg_or_nil(a, 1));
    }
    // A string pattern: the first match only, as in JavaScript.
    let s = this_units(a);
    let p = string_arg(arg_or_nil(a, 0));
    let r = arg_or_nil(a, 1);
    match find_units(&s, &p, 0) {
        None => from_units(&s),
        Some(i) => {
            let rep = if r.is_function() { units_of(to_str(funcall(r, &[from_units(&p)]))) } else { string_arg(r) };
            let mut v = s[..i].to_vec();
            v.extend(rep);
            v.extend_from_slice(&s[i + p.len()..]);
            from_units(&v)
        }
    }
}

fn no_regexp(a: &[LispObj], name: &str) -> ! {
    crate::error::type_error(format!("{name}: not a RegExp: {}", printer::pr_str(arg_or_nil(a, 0))))
}

fn str_match(a: &[LispObj]) -> LispObj {
    let Some(re) = regexp_arg(a) else { no_regexp(a, "match") };
    let s = from_units(&this_units(a));
    super::prims::invoke_method(&[re, string_node("exec"), s])
}

fn str_search(a: &[LispObj]) -> LispObj {
    let Some(re) = regexp_arg(a) else { no_regexp(a, "search") };
    let s = from_units(&this_units(a));
    LispObj::fixnum(super::js::exec(re, s, 0).map_or(-1, |(i, _)| i as i64))
}

// ---------------------------------------------------------------------------
// Arrays

pub(super) fn default_compare(x: LispObj, y: LispObj) -> Ordering {
    units_of(to_str(x)).cmp(&units_of(to_str(y)))
}

/// Stable merge sort of array `a` with `cmp` (a function returning a
/// number, negative for less), or by string order when `cmp` is NIL.
/// The elements stay in Lisp arrays, so the comparison may allocate.
pub(super) fn stable_sort(a: LispObj, cmp: LispObj) -> LispObj {
    let n = array::length(a);
    let less = |x: LispObj, y: LispObj| -> bool {
        if cmp == NIL {
            default_compare(x, y) == Ordering::Less
        } else {
            let r = funcall(cmp, &[x, y]);
            numbers::partial_compare(r, LispObj::fixnum(0)) == Some(Ordering::Less)
        }
    };
    let mut src = array::slice(a, 0, None);
    let mut dst = array::slice(a, 0, None);
    let mut width = 1;
    while width < n {
        let mut lo = 0;
        while lo < n {
            let mid = (lo + width).min(n);
            let hi = (lo + 2 * width).min(n);
            let (mut i, mut j) = (lo, mid);
            for k in lo..hi {
                let take_right = j < hi && (i >= mid || less(array::get(src, j), array::get(src, i)));
                if take_right {
                    array::set(dst, k, array::get(src, j));
                    j += 1;
                } else {
                    array::set(dst, k, array::get(src, i));
                    i += 1;
                }
            }
            lo = hi;
        }
        std::mem::swap(&mut src, &mut dst);
        width *= 2;
    }
    for i in 0..n {
        array::set(a, i, array::get(src, i));
    }
    a
}

fn arr_push(a: &[LispObj]) -> LispObj {
    for &x in &a[1..] {
        array::push(a[0], x);
    }
    LispObj::fixnum(array::length(a[0]) as i64)
}

fn arr_pop(a: &[LispObj]) -> LispObj {
    array::apop(&a[..1])
}

fn arr_shift(a: &[LispObj]) -> LispObj {
    if array::length(a[0]) == 0 {
        return NIL;
    }
    let x = array::get(a[0], 0);
    array::splice(a[0], 0, 1);
    x
}

fn arr_slice(a: &[LispObj]) -> LispObj {
    let n = array::length(a[0]);
    let s = js_index(Some(arg(a, 0).unwrap_or(LispObj::fixnum(0))), n);
    let e = js_index(arg(a, 1), n);
    array::slice(a[0], s as i64, Some(s.max(e) as i64))
}

fn arr_splice(a: &[LispObj]) -> LispObj {
    let n = array::length(a[0]);
    let s = js_index(Some(arg(a, 0).unwrap_or(LispObj::fixnum(0))), n);
    let count = arg(a, 1).map_or(n as i64 - s as i64, int_of);
    array::splice(a[0], s as i64, count)
}

fn arr_join(a: &[LispObj]) -> LispObj {
    let sep = match arg(a, 0) {
        None => vec![b',' as LispChar],
        Some(s) => string_arg(s),
    };
    let mut v: Vec<LispChar> = Vec::new();
    for i in 0..array::length(a[0]) {
        if i > 0 {
            v.extend_from_slice(&sep);
        }
        let x = array::get(a[0], i);
        if x != NIL {
            v.extend(units_of(to_str(x)));
        }
    }
    from_units(&v)
}

fn arr_to_string(a: &[LispObj]) -> LispObj {
    arr_join(&a[..1])
}

fn arr_index_of(a: &[LispObj]) -> LispObj {
    let x = arg_or_nil(a, 0);
    let from = arg(a, 1).map_or(0, |f| int_of(f).max(0) as usize);
    let eql = |y: LispObj| {
        y == x
            || (numbers::is_number(x)
                && numbers::is_number(y)
                && !x.is_fixnum()
                && numbers::partial_compare(x, y) == Some(Ordering::Equal)
                && x.is_double_float() == y.is_double_float())
    };
    let n = array::length(a[0]);
    LispObj::fixnum((from..n).find(|&i| eql(array::get(a[0], i))).map_or(-1, |i| i as i64))
}

fn arr_for_each(a: &[LispObj]) -> LispObj {
    let f = arg_or_nil(a, 0);
    for i in 0..array::length(a[0]) {
        funcall(f, &[array::get(a[0], i)]);
    }
    NIL
}

fn arr_map(a: &[LispObj]) -> LispObj {
    let f = arg_or_nil(a, 0);
    let n = array::length(a[0]);
    let r = array::new_array(n, n);
    for i in 0..n {
        let v = funcall(f, &[array::get(a[0], i)]);
        array::set(r, i, v);
    }
    r
}

fn arr_sort(a: &[LispObj]) -> LispObj {
    stable_sort(a[0], arg_or_nil(a, 0))
}

fn arr_concat(a: &[LispObj]) -> LispObj {
    let r = array::slice(a[0], 0, None);
    for &x in &a[1..] {
        if array::is_array(x) {
            for i in 0..array::length(x) {
                array::push(r, array::get(x, i));
            }
        } else {
            array::push(r, x);
        }
    }
    r
}

// ---------------------------------------------------------------------------
// Numbers and functions

fn radix_string(mut n: i64, radix: u32) -> String {
    if !(2..=36).contains(&radix) {
        crate::error::argument_error(format!("Bad radix: {radix}"));
    }
    if n == 0 {
        return "0".into();
    }
    let neg = n < 0;
    let mut digits = Vec::new();
    while n != 0 {
        let d = (n % radix as i64).unsigned_abs() as u32;
        digits.push(std::char::from_digit(d, radix).unwrap());
        n /= radix as i64;
    }
    if neg {
        digits.push('-');
    }
    digits.iter().rev().collect()
}

fn num_to_string(a: &[LispObj]) -> LispObj {
    match (arg(a, 0), numbers::num(a[0])) {
        (Some(r), Num::Int(i)) if r != NIL => string_node(&radix_string(i, int_of(r) as u32)),
        _ => to_str(a[0]),
    }
}

/// `(.call f this args...)`.
pub(super) fn fn_call(a: &[LispObj]) -> LispObj {
    funcall(a[0], a.get(2..).unwrap_or(&[]))
}

/// `(.apply f this array)`.
pub(super) fn fn_apply(a: &[LispObj]) -> LispObj {
    let arr = arg_or_nil(a, 1);
    let items: Vec<LispObj> =
        if arr == NIL { vec![] } else { (0..array::length(arr)).map(|i| array::get(arr, i)).collect() };
    // `items` is not a GC root, but lisp_call copies the arguments
    // before anything allocates.
    funcall(a[0], &items)
}

// ---------------------------------------------------------------------------
// corten.string, corten.array, corten.number

fn check_string(x: LispObj) {
    if !(x.is_string() || x.is_character()) {
        crate::error::type_error(format!("Not a string: {}", printer::pr_str(x)));
    }
}

fn check_array(x: LispObj) {
    if !array::is_array(x) {
        crate::error::type_error(format!("Not an array: {}", printer::pr_str(x)));
    }
}

/// `(length s)`: the number of UTF-16 code units.
fn cs_length(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    if a[0].is_character() { LispObj::fixnum(1) } else { a[0].vector_length() }
}

/// `(char-code-at s i)`: the code unit at `i` (an error out of range).
fn cs_char_code_at(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    let s = this_units(a);
    let i = int_of(a[1]);
    if i < 0 || i as usize >= s.len() {
        crate::error::index_error(format!("String index out of range: {i}"));
    }
    LispObj::fixnum(s[i as usize] as i64)
}

/// `(substring s start [end])`: indexes clamped to the string, swapped
/// when `start` > `end`.
fn cs_substring(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_substring(a)
}

/// Index result: the position, or nil.
fn index_or_nil(r: LispObj) -> LispObj {
    if r.integer() < 0 { NIL } else { r }
}

/// `(index-of s sub [from])`: the first index of `sub` at or after
/// `from`, or nil.
fn cs_index_of(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    index_or_nil(str_index_of(a))
}

/// `(last-index-of s sub [from])`: the last index of `sub` at or before
/// `from`, or nil.
fn cs_last_index_of(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    index_or_nil(str_last_index_of(a))
}

fn cs_lower_case(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_to_lower_case(a)
}

fn cs_upper_case(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_to_upper_case(a)
}

/// `(trim s)`: without leading and trailing ASCII whitespace.
fn cs_trim(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_trim(a)
}

fn cs_starts_with(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_starts_with(a)
}

fn cs_ends_with(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_ends_with(a)
}

/// `(split s sep)`: an array of the parts of `s` between matches of
/// `sep` (a string or a regex; groups are included); "" splits into
/// one-character strings.
fn cs_split(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_split(a)
}

/// `(replace s match replacement)`: a string `match` replaces the first
/// occurrence; a regex every match if it has the `g` flag, else the
/// first. `replacement` is a string (`$1` .. `$9`, `$&`, `$$` with a
/// regex) or a function of the match (and groups).
fn cs_replace(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    str_replace(a)
}

/// `(push! arr x & xs)`: append to `arr`; returns `arr`.
fn ca_push(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_push(a);
    a[0]
}

/// `(pop! arr)`: remove and return the last element (nil if empty).
fn ca_pop(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_pop(a)
}

/// `(shift! arr)`: remove and return the first element (nil if empty).
fn ca_shift(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_shift(a)
}

/// `(slice arr [start [end]])`: a new array; negative indexes count from
/// the end.
fn ca_slice(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_slice(a)
}

/// `(splice! arr start n)`: remove `n` elements at `start`; returns them
/// as an array.
fn ca_splice(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_splice(a)
}

/// `(join arr [sep])`: the elements as strings (nil as ""), separated by
/// `sep` (default "").
fn ca_join(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    if a.len() == 1 { arr_join(&[a[0], string_node("")]) } else { arr_join(a) }
}

/// `(sort! arr [cmp])`: stable in-place sort with comparator `cmp`
/// (default `compare` of the elements as strings); returns `arr`.
fn ca_sort(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_sort(a)
}

/// `(map arr f)`: a new array of `(f x)` for each element.
fn ca_map(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_map(a)
}

/// `(for-each arr f)`: call `f` on each element; returns nil.
fn ca_for_each(a: &[LispObj]) -> LispObj {
    check_array(a[0]);
    arr_for_each(a)
}

/// `(to-string n [radix])`: an integer in base `radix` (2 to 36), or `n`
/// as by `str`.
fn cn_to_string(a: &[LispObj]) -> LispObj {
    if !numbers::is_number(a[0]) {
        crate::error::type_error(format!("Not a number: {}", printer::pr_str(a[0])));
    }
    num_to_string(a)
}
