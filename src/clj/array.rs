//! JavaScript-style arrays for the ported cljs.core: growable vectors.
//!
//! An array is a CL vector with a fill pointer (uvector type ARRAY_TYPE:
//! backing simple-vector, fill pointer = length, offset, rank 1,
//! capacity), so CL sees an ordinary adjustable vector. Plain
//! simple-vectors are accepted wherever an array is read.

use super::*;

/// A new array of `len` NILs with room for `cap` elements.
pub fn new_array(len: usize, cap: usize) -> LispObj {
    let cap = cap.max(len);
    let v = vector_node(cap);
    let a = heap::alloc_uvector(5, ARRAY_TYPE);
    a.uset(ARRAY_VECTOR, v);
    a.uset(ARRAY_FILL_POINTER, LispObj::fixnum(len as i64));
    a.uset(ARRAY_OFFSET, LispObj::fixnum(0));
    a.uset(ARRAY_DIMENSIONS, LispObj::fixnum(1));
    a.uset(ARRAY_DIM1_LENGTH, LispObj::fixnum(cap as i64));
    a
}

/// Typed arrays (`long-array` etc.) have a larger header (4 units, not
/// 3) whose cell 6 is the element kind: one of the `KIND_` values, 0
/// (the zero-filled cell of a plain array) for any value. They have a
/// fixed size, and `aset` checks and coerces what is stored.
const ARRAY_ELEMENT_KIND: usize = 6;
const KIND_BOOLEAN: i64 = 1;
const KIND_BYTE: i64 = 2;
const KIND_SHORT: i64 = 3;
const KIND_CHAR: i64 = 4;
const KIND_INT: i64 = 5;
const KIND_LONG: i64 = 6;
const KIND_FLOAT: i64 = 7;
const KIND_DOUBLE: i64 = 8;

/// The element kind of array `x` (0 for a plain array).
pub fn element_kind(x: LispObj) -> i64 {
    if x.is_adjustable_array() && x.uvector_size() >= 4 {
        x.uref(ARRAY_ELEMENT_KIND).integer()
    } else {
        0
    }
}

fn kind_name(kind: i64) -> &'static str {
    ["object", "boolean", "byte", "short", "char", "int", "long", "float", "double"][kind as usize]
}

/// `v` as an element of an array of `kind`: integers in range for the
/// integer kinds, any real number for the float kinds (converted), a
/// character or a boolean.
fn coerce_element(kind: i64, v: LispObj) -> LispObj {
    let cast = |k: i64| super::number::num_cast(&[LispObj::fixnum(k), v]);
    let ok = match kind {
        0 => return v,
        KIND_BOOLEAN => super::is_boolean(v),
        KIND_CHAR => v.is_character(),
        KIND_BYTE | KIND_SHORT | KIND_INT | KIND_LONG => crate::numbers::is_integer(v),
        _ => crate::numbers::is_number(v),
    };
    if !ok {
        crate::error::argument_error(format!("Cannot store {} in a {} array", printer::pr_str(v), kind_name(kind)));
    }
    match kind {
        KIND_BYTE => cast(3),
        KIND_SHORT => cast(2),
        KIND_INT => cast(1),
        KIND_LONG => cast(0),
        KIND_FLOAT => cast(11),
        KIND_DOUBLE => cast(10),
        _ => v,
    }
}

/// A new array of `len` elements of `kind`, each `zero`.
fn new_typed_array(kind: i64, len: usize, zero: LispObj) -> LispObj {
    let v = vector_node(len);
    for i in 0..len {
        v.svset(i, zero);
    }
    let a = heap::alloc_uvector(6, ARRAY_TYPE);
    a.uset(ARRAY_VECTOR, v);
    a.uset(ARRAY_FILL_POINTER, LispObj::fixnum(len as i64));
    a.uset(ARRAY_OFFSET, LispObj::fixnum(0));
    a.uset(ARRAY_DIMENSIONS, LispObj::fixnum(1));
    a.uset(ARRAY_DIM1_LENGTH, LispObj::fixnum(len as i64));
    a.uset(ARRAY_ELEMENT_KIND, LispObj::fixnum(kind));
    a
}

fn kind_zero(kind: i64) -> LispObj {
    match kind {
        KIND_BOOLEAN => super::FALSE,
        KIND_CHAR => LispObj::character(0),
        KIND_FLOAT => heap::single_float_node(0.0),
        KIND_DOUBLE => heap::double_float_node(0.0),
        _ => LispObj::fixnum(0),
    }
}

/// Is `x` an array (a JS-style array or a simple-vector)?
pub fn is_array(x: LispObj) -> bool {
    x.is_simple_generic_vector()
        || (x.is_adjustable_array()
            && x.uref(ARRAY_DIMENSIONS).integer() == 1
            && x.uref(ARRAY_VECTOR).is_simple_generic_vector())
}

fn check(x: LispObj) {
    if !is_array(x) {
        crate::error::type_error(format!("Not an array: {}", printer::pr_str(x)));
    }
}

/// Number of elements.
pub fn length(x: LispObj) -> usize {
    if x.is_simple_generic_vector() {
        return x.vector_length().integer() as usize;
    }
    let fp = x.uref(ARRAY_FILL_POINTER).integer();
    if fp >= 0 { fp as usize } else { x.uref(ARRAY_DIM1_LENGTH).integer() as usize }
}

/// Backing simple-vector and offset of element 0.
fn storage(x: LispObj) -> (LispObj, usize) {
    if x.is_simple_generic_vector() {
        (x, 0)
    } else {
        (x.uref(ARRAY_VECTOR), x.uref(ARRAY_OFFSET).integer() as usize)
    }
}

fn index(x: LispObj, i: LispObj) -> usize {
    check(x);
    let n = length(x) as i64;
    if !i.is_fixnum() || i.integer() < 0 || i.integer() >= n {
        crate::error::index_error(format!("Array index out of bounds: {}", printer::pr_str(i)));
    }
    i.integer() as usize
}

pub fn get(x: LispObj, i: usize) -> LispObj {
    let (v, off) = storage(x);
    v.svref(off + i)
}

pub fn set(x: LispObj, i: usize, val: LispObj) {
    let (v, off) = storage(x);
    v.svset(off + i, val)
}

/// A new array holding `items`.
pub fn from_slice(items: &[LispObj]) -> LispObj {
    let a = new_array(items.len(), items.len());
    for (i, &x) in items.iter().enumerate() {
        set(a, i, x);
    }
    a
}

/// Append `val`, growing the backing vector when full.
pub fn push(x: LispObj, val: LispObj) {
    if !x.is_adjustable_array() || element_kind(x) != 0 {
        crate::error::error_of_kind(crate::error::ErrorKind::Unsupported, "Cannot push onto a fixed-size array");
    }
    let n = length(x);
    let cap = x.uref(ARRAY_DIM1_LENGTH).integer() as usize;
    let off = x.uref(ARRAY_OFFSET).integer() as usize;
    if off + n >= x.uref(ARRAY_VECTOR).vector_length().integer() as usize || n >= cap {
        let ncap = (cap * 2).max(8);
        let nv = vector_node(ncap);
        let (v, off) = storage(x);
        for i in 0..n {
            nv.svset(i, v.svref(off + i));
        }
        x.uset(ARRAY_VECTOR, nv);
        x.uset(ARRAY_OFFSET, LispObj::fixnum(0));
        x.uset(ARRAY_DIM1_LENGTH, LispObj::fixnum(ncap as i64));
    }
    set(x, n, val);
    x.uset(ARRAY_FILL_POINTER, LispObj::fixnum(n as i64 + 1));
}

/// JS `slice`: elements `start` (inclusive) to `end` (exclusive); negative
/// indices count from the end.
pub fn slice(x: LispObj, start: i64, end: Option<i64>) -> LispObj {
    check(x);
    let n = length(x) as i64;
    let norm = |i: i64| if i < 0 { (n + i).max(0) } else { i.min(n) };
    let s = norm(start);
    let e = norm(end.unwrap_or(n));
    let len = (e - s).max(0) as usize;
    let kind = element_kind(x);
    let r = if kind == 0 { new_array(len, len) } else { new_typed_array(kind, len, kind_zero(kind)) };
    for i in 0..len {
        set(r, i, get(x, s as usize + i));
    }
    r
}

/// JS `splice(start, count)` without insertion: remove elements, return
/// them as an array.
pub fn splice(x: LispObj, start: i64, count: i64) -> LispObj {
    check(x);
    if !x.is_adjustable_array() || element_kind(x) != 0 {
        crate::error::error_of_kind(crate::error::ErrorKind::Unsupported, "Cannot splice a fixed-size array");
    }
    let n = length(x) as i64;
    let s = if start < 0 { (n + start).max(0) } else { start.min(n) };
    let c = count.clamp(0, n - s);
    let removed = slice(x, s, Some(s + c));
    for i in s..n - c {
        set(x, i as usize, get(x, (i + c) as usize));
    }
    for i in n - c..n {
        set(x, i as usize, NIL);
    }
    x.uset(ARRAY_FILL_POINTER, LispObj::fixnum(n - c));
    removed
}

fn int_arg(x: LispObj) -> i64 {
    if !x.is_fixnum() {
        crate::error::type_error(format!("Not an integer: {}", printer::pr_str(x)));
    }
    x.integer()
}

// Primitives

/// `(k/aget a i)`. PORT: on a string, as JavaScript's `s[i]`, but the
/// element is a character (cljs.core's IndexedSeq over strings).
pub fn aget(a: &[LispObj]) -> LispObj {
    if a[0].is_simple_char_vector() {
        let n = a[0].vector_length().integer();
        let i = a[1];
        if !i.is_fixnum() || i.integer() < 0 || i.integer() >= n {
            crate::error::index_error(format!("String index out of bounds: {}", printer::pr_str(i)));
        }
        let c = unsafe { *a[0].char_array_start().add(i.integer() as usize) };
        return LispObj::character(c as u32);
    }
    if !is_array(a[0]) {
        return js_object_op(false, &a[..2]);
    }
    let i = index(a[0], a[1]);
    get(a[0], i)
}

/// `aget` / `aset` on a non-array (a JavaScript object): property
/// access.
fn js_object_op(set: bool, a: &[LispObj]) -> LispObj {
    // As a JavaScript property access: an error on nil, else undefined
    // (nil) for a missing property.
    if a[0] == NIL || (set && !super::js::is_js_object(a[0])) {
        crate::error::type_error(format!("Not an array: {}", printer::pr_str(a[0])));
    }
    if set { super::js::oset(a) } else { super::js::oget(a) }
}

pub fn aset(a: &[LispObj]) -> LispObj {
    let kind = element_kind(a[0]);
    if kind != 0 {
        let i = index(a[0], a[1]);
        let v = coerce_element(kind, a[2]);
        set(a[0], i, v);
        return v;
    }
    // As in JavaScript, setting past the end grows a growable array
    // (missing elements are nil).
    if a[0].is_adjustable_array() && is_array(a[0]) && a[1].is_fixnum() {
        let i = a[1].integer();
        if i >= 0 && i as usize >= length(a[0]) {
            while length(a[0]) < i as usize {
                push(a[0], NIL);
            }
            push(a[0], a[2]);
            return a[2];
        }
    }
    if !is_array(a[0]) {
        return js_object_op(true, a);
    }
    let i = index(a[0], a[1]);
    set(a[0], i, a[2]);
    a[2]
}

pub fn alength(a: &[LispObj]) -> LispObj {
    if a[0].is_simple_char_vector() {
        return a[0].vector_length();
    }
    check(a[0]);
    LispObj::fixnum(length(a[0]) as i64)
}

/// `(k/make-array n)`: an array of `n` NILs.
pub fn make_array(a: &[LispObj]) -> LispObj {
    let n = int_arg(a[0]);
    if n < 0 {
        crate::error::argument_error("Bad array size");
    }
    new_array(n as usize, n as usize)
}

/// `(k/typed-array kind n)`: an array of `n` zeros of `kind` (a
/// keyword name: `:long`, `:double`, ...).
pub fn typed_array(a: &[LispObj]) -> LispObj {
    let name = if super::is_keyword(a[0]) { a[0].uref(KW_NAME).string_to_rust() } else { String::new() };
    let Some(kind) = (1..=8).find(|&k| kind_name(k) == name) else {
        lisp_error(format!("Bad array element type: {}", printer::pr_str(a[0])));
    };
    let n = int_arg(a[1]);
    if n < 0 {
        crate::error::argument_error("Bad array size");
    }
    new_typed_array(kind, n as usize, kind_zero(kind))
}

/// `(k/array-kind a)`: the element type keyword of a typed array, or
/// nil.
pub fn array_kind(a: &[LispObj]) -> LispObj {
    match element_kind(a[0]) {
        0 => NIL,
        k => super::keyword_from(None, kind_name(k)),
    }
}

/// `(k/array x...)`.
pub fn array(a: &[LispObj]) -> LispObj {
    from_slice(a)
}

pub fn array_p(a: &[LispObj]) -> LispObj {
    cbool(is_array(a[0]))
}

pub fn apush(a: &[LispObj]) -> LispObj {
    for &v in &a[1..] {
        push(a[0], v);
    }
    LispObj::fixnum(length(a[0]) as i64)
}

pub fn apop(a: &[LispObj]) -> LispObj {
    check(a[0]);
    let n = length(a[0]);
    if n == 0 || !a[0].is_adjustable_array() || element_kind(a[0]) != 0 {
        return NIL;
    }
    let v = get(a[0], n - 1);
    set(a[0], n - 1, NIL);
    a[0].uset(ARRAY_FILL_POINTER, LispObj::fixnum(n as i64 - 1));
    v
}

/// `(k/aslice arr start &optional end)`.
pub fn aslice(a: &[LispObj]) -> LispObj {
    let start = a.get(1).map_or(0, |&s| int_arg(s));
    let end = a.get(2).filter(|&&e| e != NIL).map(|&e| int_arg(e));
    slice(a[0], start, end)
}

/// `(k/asplice arr start count)`.
pub fn asplice(a: &[LispObj]) -> LispObj {
    splice(a[0], int_arg(a[1]), int_arg(a[2]))
}

/// `(k/acopy src src-start dest dest-start len)`.
pub fn acopy(a: &[LispObj]) -> LispObj {
    let (src, dst) = (a[0], a[2]);
    check(src);
    check(dst);
    let (s, d, n) = (int_arg(a[1]), int_arg(a[3]), int_arg(a[4]));
    if s < 0 || d < 0 || n < 0 || (s + n) as usize > length(src) || (d + n) as usize > length(dst) {
        crate::error::index_error("Array copy out of bounds");
    }
    let (s, d, n) = (s as usize, d as usize, n as usize);
    let kind = element_kind(dst);
    if kind != 0 && element_kind(src) != kind {
        // Check every element before storing any.
        for i in 0..n {
            coerce_element(kind, get(src, s + i));
        }
        for i in 0..n {
            let v = coerce_element(kind, get(src, s + i));
            set(dst, d + i, v);
        }
        return NIL;
    }
    if src == dst && s < d {
        for i in (0..n).rev() {
            set(dst, d + i, get(src, s + i));
        }
    } else {
        for i in 0..n {
            set(dst, d + i, get(src, s + i));
        }
    }
    NIL
}
