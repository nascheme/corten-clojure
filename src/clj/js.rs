//! The JavaScript runtime pieces the ported cljs.core expects (Phase 2 of
//! the Clojure conversion: these were CL code in the CL host layer, now deleted).
//!
//! - JavaScript objects (`#js {}`, `js-obj`, goog.object) are instances
//!   of the kernel type `object` (`G::ObjectType`) with fields: keys (an
//!   array), values (an array) and an index (NIL, or a simple-vector of
//!   fixnums: 0 for a free slot, else position + 1). Keys keep insertion
//!   order. Strings compare by contents, numbers by value, other keys by
//!   identity.
//! - Methods and properties of strings, arrays, numbers, functions and
//!   objects: `(.name x args...)`, `(.-name x)`, `(set! (.-name x) v)` and
//!   `(new o args...)` on values that are not Clojure instances.
//! - Regular expression replace and split, `goog/getUid`, array sorting
//!   and shuffling, time, `parseInt`, `Math` helpers.

use super::*;
use crate::numbers::{self, Num};
use crate::prims::funcall;
use super::native::{self, from_units, nan, stable_sort, to_str, units_of};
use std::cmp::Ordering;

const OBJ_KEYS: usize = INSTANCE_FIELDS;
const OBJ_VALS: usize = INSTANCE_FIELDS + 1;
const OBJ_INDEX: usize = INSTANCE_FIELDS + 2;
/// Objects with fewer keys are searched linearly.
const INDEX_MIN: usize = 8;

fn rstr(s: LispObj) -> String {
    s.string_to_rust()
}

// ---------------------------------------------------------------------------
// JavaScript objects

pub fn is_js_object(x: LispObj) -> bool {
    is_instance(x) && instance_type(x) == global(G::ObjectType)
}

fn new_object() -> LispObj {
    let keys = array::new_array(0, 4);
    let vals = array::new_array(0, 4);
    make_instance(global(G::ObjectType), &[keys, vals, NIL])
}

fn key_hash(k: LispObj) -> u64 {
    if k.is_string() {
        let len = k.vector_length().integer() as usize;
        let u = unsafe { std::slice::from_raw_parts(k.char_array_start(), len) };
        let mut h: u64 = 0xcbf2_9ce4_8422_2325;
        for &c in u {
            h = (h ^ c as u64).wrapping_mul(0x100_0000_01b3);
        }
        h
    } else if k.is_fixnum() {
        (k.integer() as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15)
    } else if k.is_character() {
        (k.char_code() as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15)
    } else if k.is_double_float() {
        k.double_float().to_bits().wrapping_mul(0x9e37_79b9_7f4a_7c15)
    } else {
        // Identity keys move with the GC: no stable hash.
        0
    }
}

fn key_eq(a: LispObj, b: LispObj) -> bool {
    if a == b {
        return true;
    }
    if a.is_string() && b.is_string() {
        return units_of(a) == units_of(b);
    }
    if a.is_double_float() && b.is_double_float() {
        return a.double_float().to_bits() == b.double_float().to_bits();
    }
    false
}

fn obj_len(o: LispObj) -> usize {
    array::length(o.uref(OBJ_KEYS))
}

fn rebuild_index(o: LispObj) {
    let n = obj_len(o);
    if n < INDEX_MIN {
        o.uset(OBJ_INDEX, NIL);
        return;
    }
    let size = (n * 2).next_power_of_two().max(16);
    let idx = heap::vector_node(size);
    for i in 0..size {
        idx.svset(i, LispObj::fixnum(0));
    }
    let keys = o.uref(OBJ_KEYS);
    for p in 0..n {
        let mut j = key_hash(array::get(keys, p)) as usize & (size - 1);
        while idx.svref(j).integer() != 0 {
            j = (j + 1) & (size - 1);
        }
        idx.svset(j, LispObj::fixnum(p as i64 + 1));
    }
    o.uset(OBJ_INDEX, idx);
}

/// Position of key `k` in object `o`.
fn obj_find(o: LispObj, k: LispObj) -> Option<usize> {
    let keys = o.uref(OBJ_KEYS);
    let idx = o.uref(OBJ_INDEX);
    if idx == NIL {
        return (0..obj_len(o)).find(|&p| key_eq(array::get(keys, p), k));
    }
    let size = idx.vector_length().integer() as usize;
    let mut j = key_hash(k) as usize & (size - 1);
    loop {
        let p = idx.svref(j).integer();
        if p == 0 {
            return None;
        }
        if key_eq(array::get(keys, p as usize - 1), k) {
            return Some(p as usize - 1);
        }
        j = (j + 1) & (size - 1);
    }
}

pub fn obj_get(o: LispObj, k: LispObj) -> Option<LispObj> {
    obj_find(o, k).map(|p| array::get(o.uref(OBJ_VALS), p))
}

pub fn obj_set(o: LispObj, k: LispObj, v: LispObj) {
    if let Some(p) = obj_find(o, k) {
        array::set(o.uref(OBJ_VALS), p, v);
        return;
    }
    array::push(o.uref(OBJ_KEYS), k);
    array::push(o.uref(OBJ_VALS), v);
    let n = obj_len(o);
    let idx = o.uref(OBJ_INDEX);
    if n >= INDEX_MIN && (idx == NIL || n * 2 > idx.vector_length().integer() as usize) {
        rebuild_index(o);
    } else if idx != NIL {
        let size = idx.vector_length().integer() as usize;
        let mut j = key_hash(k) as usize & (size - 1);
        while idx.svref(j).integer() != 0 {
            j = (j + 1) & (size - 1);
        }
        idx.svset(j, LispObj::fixnum(n as i64));
    }
}

fn obj_delete(o: LispObj, k: LispObj) -> bool {
    let Some(p) = obj_find(o, k) else { return false };
    array::splice(o.uref(OBJ_KEYS), p as i64, 1);
    array::splice(o.uref(OBJ_VALS), p as i64, 1);
    rebuild_index(o);
    true
}

fn check_object(o: LispObj) -> LispObj {
    if !is_js_object(o) {
        lisp_error(format!("Not a JavaScript object: {}", printer::pr_str(o)));
    }
    o
}

/// `(k/js-obj k v ...)`: a new JavaScript object.
pub fn js_obj(a: &[LispObj]) -> LispObj {
    let o = new_object();
    for kv in a.chunks(2) {
        obj_set(o, kv[0], kv.get(1).copied().unwrap_or(NIL));
    }
    o
}

/// `(k/js-object? x)`.
pub fn js_object_p(a: &[LispObj]) -> LispObj {
    cbool(is_js_object(a[0]))
}

/// `(k/oget o k [default])`: property `k` of object `o`, element `k` of
/// an array, else `default`.
pub fn oget(a: &[LispObj]) -> LispObj {
    let default = a.get(2).copied().unwrap_or(NIL);
    let (o, k) = (a[0], a[1]);
    if is_js_object(o) {
        obj_get(o, k).unwrap_or(default)
    } else if array::is_array(o) {
        if k.is_fixnum() && k.integer() >= 0 && (k.integer() as usize) < array::length(o) {
            array::get(o, k.integer() as usize)
        } else {
            default
        }
    } else {
        default
    }
}

/// `(k/oset o k v)`.
pub fn oset(a: &[LispObj]) -> LispObj {
    if is_js_object(a[0]) {
        obj_set(a[0], a[1], a[2]);
        a[2]
    } else if array::is_array(a[0]) {
        array::aset(a)
    } else {
        lisp_error(format!(
            "Cannot set property {} of {}",
            printer::pr_str(a[1]),
            printer::pr_str(a[0])
        ))
    }
}

/// `(k/ocontains o k)`.
pub fn ocontains(a: &[LispObj]) -> LispObj {
    cbool(is_js_object(a[0]) && obj_find(a[0], a[1]).is_some())
}

/// `(k/okeys o)`: an array of the keys (empty for a non-object).
pub fn okeys(a: &[LispObj]) -> LispObj {
    if is_js_object(a[0]) {
        array::slice(a[0].uref(OBJ_KEYS), 0, None)
    } else {
        array::new_array(0, 0)
    }
}

/// `(k/oclone o)`: a shallow copy.
pub fn oclone(a: &[LispObj]) -> LispObj {
    let o = check_object(a[0]);
    let keys = array::slice(o.uref(OBJ_KEYS), 0, None);
    let vals = array::slice(o.uref(OBJ_VALS), 0, None);
    let c = make_instance(global(G::ObjectType), &[keys, vals, NIL]);
    rebuild_index(c);
    c
}

/// `(k/odelete o k)`: remove property `k`.
pub fn odelete(a: &[LispObj]) -> LispObj {
    if is_js_object(a[0]) {
        obj_delete(a[0], a[1]);
    }
    NIL
}

// ---------------------------------------------------------------------------
// Methods and properties of host values

/// `(.name x args...)` on a value whose type has no method `name`
/// (called by `k/invoke-method`): a function property of a
/// JavaScript object, `.call` / `.apply` of a Clojure instance
/// implementing IFn (cljs.core's MetaFn), else `toString`.
pub fn host_method(name: LispObj, x: LispObj, args: &[LispObj]) -> LispObj {
    let name = rstr(name);
    let name = name.as_str();
    if is_js_object(x) {
        if name == "hasOwnProperty" {
            return cbool(obj_find(x, args.first().copied().unwrap_or(NIL)).is_some());
        }
        let f = obj_get(x, string_node(name)).unwrap_or(NIL);
        if f.is_function() || f.is_kfunction() || is_instance(f) {
            return funcall(f, args);
        }
        crate::error::argument_error(format!("No method {name} for object"));
    }
    if is_instance(x) && matches!(name, "call" | "apply") {
        let mut a = vec![x];
        a.extend_from_slice(args);
        return if name == "call" { native::fn_call(&a) } else { native::fn_apply(&a) };
    }
    if name == "toString" {
        return to_str(x);
    }
    // The type, not the value: printing an infinite seq never ends.
    let ty = super::type_name(super::type_of(x)).string_to_rust();
    crate::error::argument_error(format!("No method {name} for {ty}"))
}

/// `(.-name x)` on a value that is not a Clojure instance.
pub fn host_field(name: LispObj, x: LispObj) -> LispObj {
    if lisp_str_is(name, "length") {
        if x.is_string() {
            return x.vector_length();
        }
        if x.is_character() {
            return LispObj::fixnum(1);
        }
        if array::is_array(x) {
            return LispObj::fixnum(array::length(x) as i64);
        }
    }
    if is_js_object(x) {
        return obj_get(x, name).unwrap_or(NIL);
    }
    NIL
}

/// `(set! (.-name x) v)` on a value that is not a Clojure instance.
pub fn host_set_field(name: LispObj, x: LispObj, v: LispObj) -> LispObj {
    if is_js_object(x) {
        obj_set(x, name, v);
        return v;
    }
    crate::error::argument_error(format!("No field {} in {}", rstr(name), printer::pr_str(x)))
}

/// `(new o args...)` for a JavaScript object `o` with a "construct"
/// function (js/Object).
pub fn host_new(o: LispObj, args: &[LispObj]) -> LispObj {
    let c = if is_js_object(o) { obj_get(o, string_node("construct")) } else { None };
    match c {
        Some(c) if c != NIL => funcall(c, args),
        _ => lisp_error(format!("Not a type: {}", printer::pr_str(o))),
    }
}

// ---------------------------------------------------------------------------
// Regular expressions: instances of js/RegExp (source flags lastIndex
// lastMatchIndex).

fn regexp_type() -> LispObj {
    let v = find_ns("js").and_then(|ns| ns_lookup(ns, string_node("RegExp")));
    match v {
        Some(v) if is_bound(v) => var_value(v),
        _ => lisp_error("Regular expressions need js/RegExp (clj/cljs/host.cljs)"),
    }
}

pub(super) fn is_regexp(x: LispObj) -> bool {
    is_instance(x) && {
        let v = find_ns("js").and_then(|ns| ns_lookup(ns, string_node("RegExp")));
        matches!(v, Some(v) if is_bound(v) && var_value(v) == instance_type(x))
    }
}

/// `(k/regexp source [flags])`: what `#"..."` reads as.
pub fn regexp(a: &[LispObj]) -> LispObj {
    let flags = a.get(1).copied().unwrap_or_else(|| string_node(""));
    super::prims::new_instance(&[regexp_type(), a[0], flags])
}

/// `(k/regexp? x)`.
pub fn regexp_p(a: &[LispObj]) -> LispObj {
    cbool(is_regexp(a[0]))
}

/// The match index and the array of the match and its groups, if any.
pub(super) fn exec(re: LispObj, s: LispObj, pos: usize) -> Option<(usize, LispObj)> {
    let r = super::regex::regex_exec(&[re.uref(INSTANCE_FIELDS), re.uref(INSTANCE_FIELDS + 1), s, LispObj::fixnum(pos as i64)]);
    if r == NIL { None } else { Some((super::coll::nth(r, 0).integer() as usize, super::coll::nth(r, 1))) }
}

/// A JavaScript replacement string: `$$ $& $1`..`$9`.
fn expand_replacement(r: &[LispChar], groups: LispObj, out: &mut Vec<LispChar>) {
    let ng = array::length(groups);
    let group = |i: usize, out: &mut Vec<LispChar>| {
        let g = array::get(groups, i);
        if g != NIL {
            out.extend(units_of(g));
        }
    };
    let mut i = 0;
    while i < r.len() {
        let c = r[i];
        if c == b'$' as LispChar && i + 1 < r.len() {
            let d = r[i + 1];
            if d == b'$' as LispChar {
                out.push(d);
                i += 2;
                continue;
            }
            if d == b'&' as LispChar {
                group(0, out);
                i += 2;
                continue;
            }
            if (b'1' as LispChar..=b'9' as LispChar).contains(&d) && ((d - b'0' as LispChar) as usize) < ng {
                group((d - b'0' as LispChar) as usize, out);
                i += 2;
                continue;
            }
        }
        out.push(c);
        i += 1;
    }
}

pub(super) fn regex_replace(s: LispObj, re: LispObj, r: LispObj) -> LispObj {
    let s = if s.is_character() { from_units(&[s.char_code() as LispChar]) } else { s };
    let su = units_of(s);
    let flags = re.uref(INSTANCE_FIELDS + 1);
    let global = flags != NIL && units_of(flags).contains(&(b'g' as LispChar));
    let mut out: Vec<LispChar> = Vec::new();
    let mut pos = 0;
    loop {
        let m = if pos <= su.len() { exec(re, s, pos) } else { None };
        let Some((idx, groups)) = m else {
            out.extend_from_slice(&su[pos.min(su.len())..]);
            break;
        };
        let len = array::get(groups, 0).vector_length().integer() as usize;
        out.extend_from_slice(&su[pos..idx]);
        if r.is_string() {
            expand_replacement(&units_of(r), groups, &mut out);
        } else {
            let items: Vec<LispObj> = (0..array::length(groups)).map(|i| array::get(groups, i)).collect();
            out.extend(units_of(to_str(funcall(r, &items))));
        }
        pos = idx + len;
        if len == 0 {
            if idx < su.len() {
                out.push(su[idx]);
            }
            pos += 1;
        }
        if !global {
            if pos <= su.len() {
                out.extend_from_slice(&su[pos..]);
            }
            break;
        }
    }
    from_units(&out)
}

pub(super) fn regex_split(s: LispObj, re: LispObj) -> LispObj {
    let su = units_of(s);
    let a = array::new_array(0, 4);
    let (mut start, mut pos) = (0, 0);
    loop {
        let m = if pos < su.len() { exec(re, s, pos) } else { None };
        let Some((idx, groups)) = m else {
            array::push(a, from_units(&su[start..]));
            return a;
        };
        let len = array::get(groups, 0).vector_length().integer() as usize;
        if len == 0 {
            if idx >= su.len() {
                array::push(a, from_units(&su[start..]));
                return a;
            }
            if idx > start {
                array::push(a, from_units(&su[start..idx]));
                start = idx;
            }
            pos = idx.max(start) + 1;
        } else {
            array::push(a, from_units(&su[start..idx]));
            for i in 1..array::length(groups) {
                array::push(a, array::get(groups, i));
            }
            start = idx + len;
            pos = start;
        }
    }
}

// ---------------------------------------------------------------------------
// goog/getUid: an EQ table of objects to numbers. Addresses change when
// the GC moves objects, so the table is rehashed after a collection.

fn uid_table() -> LispObj {
    let t = global(G::UidTable);
    if t != NIL {
        return t;
    }
    // [gc-id, count, next-uid, keys, uids]
    let t = heap::vector_node(5);
    t.svset(0, LispObj::fixnum(-1));
    t.svset(1, LispObj::fixnum(0));
    t.svset(2, LispObj::fixnum(0));
    let keys = heap::vector_node(64);
    let uids = heap::vector_node(64);
    for i in 0..64 {
        uids.svset(i, LispObj::fixnum(0));
    }
    t.svset(3, keys);
    t.svset(4, uids);
    set_global(G::UidTable, t);
    t
}

fn addr_hash(x: LispObj) -> usize {
    (x.raw() as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15).rotate_right(29) as usize
}

fn uid_rehash(t: LispObj, size: usize) {
    let keys = heap::vector_node(size);
    let uids = heap::vector_node(size);
    for i in 0..size {
        uids.svset(i, LispObj::fixnum(0));
    }
    // Read the old vectors after allocating (the GC may have moved them).
    let (old_keys, old_uids) = (t.svref(3), t.svref(4));
    let old_n = old_uids.vector_length().integer() as usize;
    for i in 0..old_n {
        let u = old_uids.svref(i);
        if u.integer() != 0 {
            let k = old_keys.svref(i);
            let mut j = addr_hash(k) & (size - 1);
            while uids.svref(j).integer() != 0 {
                j = (j + 1) & (size - 1);
            }
            keys.svset(j, k);
            uids.svset(j, u);
        }
    }
    t.svset(3, keys);
    t.svset(4, uids);
    t.svset(0, LispObj::fixnum(crate::gc::gc_count() as i64));
}

/// `(k/uid x)`: a number unique to object `x`.
pub fn uid(a: &[LispObj]) -> LispObj {
    let x = a[0];
    let t = uid_table();
    let size = t.svref(4).vector_length().integer() as usize;
    if t.svref(0).integer() != crate::gc::gc_count() as i64 {
        uid_rehash(t, size);
    }
    let count = t.svref(1).integer() as usize;
    if (count + 1) * 2 > size {
        uid_rehash(t, size * 2);
    }
    // No allocation from here on: addresses are stable.
    let (keys, uids) = (t.svref(3), t.svref(4));
    let size = uids.vector_length().integer() as usize;
    let mut j = addr_hash(x) & (size - 1);
    loop {
        let u = uids.svref(j);
        if u.integer() == 0 {
            break;
        }
        if keys.svref(j) == x {
            return u;
        }
        j = (j + 1) & (size - 1);
    }
    let next = t.svref(2).integer() + 1;
    t.svset(2, LispObj::fixnum(next));
    t.svset(1, LispObj::fixnum(count as i64 + 1));
    keys.svset(j, x);
    uids.svset(j, LispObj::fixnum(next));
    LispObj::fixnum(next)
}

// ---------------------------------------------------------------------------
// Arrays, numbers, time

/// `(k/js-lt a b)`: JavaScript `<` on numbers, strings, booleans and
/// characters (garray/defaultCompare).
pub fn js_lt(a: &[LispObj]) -> LispObj {
    let (x, y) = (a[0], a[1]);
    let r = if numbers::is_number(x) && numbers::is_number(y) {
        numbers::partial_compare(x, y) == Some(Ordering::Less)
    } else if is_boolean(x) && is_boolean(y) {
        x == FALSE && y == TRUE
    } else if x.is_string() && y.is_string() {
        units_of(x) < units_of(y)
    } else if x.is_character() && y.is_character() {
        x.char_code() < y.char_code()
    } else {
        crate::error::type_error(format!("Cannot compare {} to {}", printer::pr_str(x), printer::pr_str(y)))
    };
    cbool(r)
}

/// `(k/astable-sort a cmp)`.
pub fn astable_sort(a: &[LispObj]) -> LispObj {
    stable_sort(a[0], a[1])
}

static RNG: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn next_random() -> u64 {
    use std::sync::atomic::Ordering::Relaxed;
    let mut s = RNG.load(Relaxed);
    if s == 0 {
        let t = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(1, |d| d.as_nanos() as u64);
        s = t | 1;
    }
    // xorshift64*
    s ^= s >> 12;
    s ^= s << 25;
    s ^= s >> 27;
    RNG.store(s, Relaxed);
    s.wrapping_mul(0x2545_f491_4f6c_dd1d)
}

/// `(k/random)`: a double in [0, 1).
pub fn random(_a: &[LispObj]) -> LispObj {
    heap::double_float_node((next_random() >> 11) as f64 / (1u64 << 53) as f64)
}

/// `(k/ashuffle a)`: shuffle array `a` in place.
pub fn ashuffle(a: &[LispObj]) -> LispObj {
    let x = a[0];
    let n = array::length(x);
    for i in (1..n).rev() {
        let j = (next_random() % (i as u64 + 1)) as usize;
        let tmp = array::get(x, i);
        array::set(x, i, array::get(x, j));
        array::set(x, j, tmp);
    }
    x
}

/// `(k/now-ms)`: milliseconds since the Unix epoch.
pub fn now_ms(_a: &[LispObj]) -> LispObj {
    let ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_millis() as i64);
    LispObj::fixnum(ms)
}

/// `(k/zero? x)`: a number equal to zero (an error on a non-number).
pub fn zerop(a: &[LispObj]) -> LispObj {
    if !numbers::is_number(a[0]) {
        crate::error::type_error(format!("Not a number: {}", printer::pr_str(a[0])));
    }
    cbool(numbers::partial_compare(a[0], LispObj::fixnum(0)) == Some(Ordering::Equal))
}

fn float_to_int(d: f64) -> LispObj {
    if d.is_finite() && d.abs() < (1u64 << 60) as f64 {
        LispObj::fixnum(d as i64)
    } else {
        heap::double_float_node(d)
    }
}

fn round_with(a: &[LispObj], f: fn(f64) -> f64) -> LispObj {
    match numbers::num(a[0]) {
        Num::Int(_) => a[0],
        n if numbers::is_number(a[0]) => float_to_int(f(numbers::as_f64(&n))),
        _ => crate::error::type_error(format!("Not a number: {}", printer::pr_str(a[0]))),
    }
}

/// `(k/math-floor x)`: an integer for a finite number.
pub fn math_floor(a: &[LispObj]) -> LispObj {
    round_with(a, f64::floor)
}

/// `(k/math-ceil x)`.
pub fn math_ceil(a: &[LispObj]) -> LispObj {
    round_with(a, f64::ceil)
}

/// `(k/parse-int s radix)`: JavaScript `parseInt`, NaN when there are
/// no digits.
pub fn parse_int(a: &[LispObj]) -> LispObj {
    let s = a[0];
    if s.is_fixnum() {
        return s;
    }
    if numbers::is_number(s) {
        let d = numbers::to_f64(s);
        return if d.is_finite() { float_to_int(d.trunc()) } else { nan() };
    }
    if !s.is_string() {
        return nan();
    }
    let radix = if a[1].is_fixnum() { a[1].integer() as u32 } else { 10 };
    let text = rstr(s);
    let t = text.trim_start();
    let (neg, t) = match t.as_bytes().first() {
        Some(b'-') => (true, &t[1..]),
        Some(b'+') => (false, &t[1..]),
        _ => (false, t),
    };
    let digits: String = t.chars().take_while(|c| c.is_digit(radix)).collect();
    if digits.is_empty() {
        return nan();
    }
    match i64::from_str_radix(&digits, radix) {
        Ok(n) => {
            let n = if neg { -n } else { n };
            LispObj::fixnum_checked(n).unwrap_or_else(|| heap::double_float_node(n as f64))
        }
        Err(_) => {
            let d = digits.chars().fold(0f64, |acc, c| acc * radix as f64 + c.to_digit(radix).unwrap() as f64);
            heap::double_float_node(if neg { -d } else { d })
        }
    }
}

/// `(k/var-meta-put var k v)`: set key `k` of a var's metadata to `v`.
pub fn var_meta_put(a: &[LispObj]) -> LispObj {
    let (var, k, v) = (a[0], a[1], a[2]);
    set_var_meta(var, super::coll::assoc(var_meta(var), k, v));
    v
}
