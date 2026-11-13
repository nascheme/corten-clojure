//! Kernel-owned collections: the layouts of what the reader produces.
//!
//! The kernel defines the types `List`, `EmptyList`, `Cons`,
//! `PersistentVector` (with `VectorNode`), `PersistentArrayMap` and
//! `PersistentHashSet` with cljs.core's fields, and `IndexedSeq` for rest
//! arguments; cljs.core's `deftype`s of them adopt the kernel descriptors
//! (`k/kernel-type`) and add the methods. The kernel only builds and
//! reads them: the reader, the compiler (which walks forms), rest
//! arguments, `apply` and the sequence functions below, which the
//! bootstrap prelude is written on.
//!
//! The sequence functions (`first`, `next`, `seq`, `get`, ...) work
//! natively on these types, on strings and arrays; for other values
//! (a `LazySeq`, a `PersistentHashMap`, ...) they call the `clojure.core`
//! function of the same name, so they are correct for any collection
//! once cljs.core is loaded.

use super::*;
use crate::runtime::{lisp_call, without_gc};

/// The metadata field, first in all the kernel collection types.
pub const META: usize = 2;
// List [meta first rest count __hash]
pub const LIST_FIRST: usize = 3;
pub const LIST_REST: usize = 4;
pub const LIST_COUNT: usize = 5;
// Cons [meta first rest __hash]
pub const CONS_FIRST: usize = 3;
pub const CONS_REST: usize = 4;
// PersistentVector [meta cnt shift root tail __hash]
pub const PV_CNT: usize = 3;
pub const PV_SHIFT: usize = 4;
pub const PV_ROOT: usize = 5;
pub const PV_TAIL: usize = 6;
// VectorNode [edit arr]
pub const VN_ARR: usize = 3;
// PersistentArrayMap [meta cnt arr __hash]
pub const PAM_CNT: usize = 3;
pub const PAM_ARR: usize = 4;
// PersistentHashSet [meta hash-map __hash]
pub const PHS_MAP: usize = 3;
// IndexedSeq [arr i meta]
pub const IS_ARR: usize = 2;
pub const IS_I: usize = 3;
pub const IS_META: usize = 4;

/// Create the collection types and their empty values (`init`).
pub(crate) fn init() {
    let ty = |g: G, name: &str, fields: &[&str]| {
        let t = make_type(&format!("clojure.core/{name}"), fields);
        set_global(g, t);
        t
    };
    let list = ty(G::ListType, "List", &["meta", "first", "rest", "count", "__hash"]);
    let empty_list = ty(G::EmptyListType, "EmptyList", &["meta"]);
    ty(G::ConsType, "Cons", &["meta", "first", "rest", "__hash"]);
    let vector = ty(G::VectorType, "PersistentVector", &["meta", "cnt", "shift", "root", "tail", "__hash"]);
    let node = ty(G::VectorNodeType, "VectorNode", &["edit", "arr"]);
    let map = ty(G::ArrayMapType, "PersistentArrayMap", &["meta", "cnt", "arr", "__hash"]);
    let set = ty(G::HashSetType, "PersistentHashSet", &["meta", "hash-map", "__hash"]);
    ty(G::IndexedSeqType, "IndexedSeq", &["arr", "i", "meta"]);

    let el = make_instance(empty_list, &[NIL]);
    set_global(G::EmptyList, el);
    set_static(list, "EMPTY", el);
    let en = make_instance(node, &[NIL, heap::vector_node(32)]);
    set_global(G::EmptyVectorNode, en);
    set_static(vector, "EMPTY-NODE", en);
    let ev = make_instance(vector, &[NIL, LispObj::fixnum(0), LispObj::fixnum(5), en, heap::vector_node(0), NIL]);
    set_global(G::EmptyVector, ev);
    set_static(vector, "EMPTY", ev);
    let em = make_instance(map, &[NIL, LispObj::fixnum(0), heap::vector_node(0), NIL]);
    set_global(G::EmptyArrayMap, em);
    set_static(map, "EMPTY", em);
    let es = make_instance(set, &[NIL, em, NIL]);
    set_global(G::EmptyHashSet, es);
    set_static(set, "EMPTY", es);
}

/// An array (a simple-vector: the reader's collections are never pushed
/// onto; cljs.core copies arrays before changing them) of `items`.
fn array_of(items: &[LispObj]) -> LispObj {
    let a = heap::vector_node(items.len());
    for (i, &x) in items.iter().enumerate() {
        a.svset(i, x);
    }
    a
}

fn set_static(ty: LispObj, name: &str, v: LispObj) {
    set_type_static(ty, string_node(name), v);
}

/// The kernel collection type named `name` (`clojure.core/List`, ...).
pub fn kernel_type(name: &str) -> Option<LispObj> {
    let g = match name.strip_prefix("clojure.core/")? {
        "List" => G::ListType,
        "EmptyList" => G::EmptyListType,
        "Cons" => G::ConsType,
        "PersistentVector" => G::VectorType,
        "VectorNode" => G::VectorNodeType,
        "PersistentArrayMap" => G::ArrayMapType,
        "PersistentHashSet" => G::HashSetType,
        "IndexedSeq" => G::IndexedSeqType,
        _ => return None,
    };
    Some(global(g))
}

// ---------------------------------------------------------------------------
// Type tests

fn is_a(x: LispObj, g: G) -> bool {
    is_instance(x) && instance_type(x) == global(g)
}
pub fn is_list(x: LispObj) -> bool {
    is_a(x, G::ListType)
}
pub fn is_empty_list(x: LispObj) -> bool {
    is_a(x, G::EmptyListType)
}
pub fn is_cons(x: LispObj) -> bool {
    is_a(x, G::ConsType)
}
pub fn is_pvector(x: LispObj) -> bool {
    is_a(x, G::VectorType)
}
pub fn is_array_map(x: LispObj) -> bool {
    is_a(x, G::ArrayMapType)
}
pub fn is_hash_set(x: LispObj) -> bool {
    is_a(x, G::HashSetType)
}
pub fn is_indexed_seq(x: LispObj) -> bool {
    is_a(x, G::IndexedSeqType)
}

/// The cell of `x`'s metadata (a kernel collection).
fn meta_cell(x: LispObj) -> usize {
    if is_indexed_seq(x) { IS_META } else { META }
}

/// Is `x` a value whose type the kernel knows completely: not an
/// instance, or a symbol, keyword or kernel collection (for which the
/// native functions below are authoritative)?
pub fn is_kernel_value(x: LispObj) -> bool {
    if !is_instance(x) {
        return true;
    }
    let t = instance_type(x);
    [
        G::SymbolType,
        G::KeywordType,
        G::ListType,
        G::EmptyListType,
        G::ConsType,
        G::VectorType,
        G::ArrayMapType,
        G::HashSetType,
        G::IndexedSeqType,
    ]
    .iter()
    .any(|&g| t == global(g))
}

// ---------------------------------------------------------------------------
// Constructors. GC is inhibited while they run: `items` may be in the
// Rust heap.

/// A list of `items` (the empty list when there are none).
pub fn list_from(items: &[LispObj]) -> LispObj {
    without_gc(|| {
        let mut r = global(G::EmptyList);
        for (i, &x) in items.iter().enumerate().rev() {
            let rest = if i + 1 == items.len() { NIL } else { r };
            r = make_instance(global(G::ListType), &[NIL, x, rest, LispObj::fixnum((items.len() - i) as i64), NIL]);
        }
        r
    })
}

/// A seq of `items` (rest arguments): an IndexedSeq over an array of
/// them, or nil if there are none.
pub fn indexed_seq(items: &[LispObj]) -> LispObj {
    if items.is_empty() {
        return NIL;
    }
    without_gc(|| {
        let arr = array_of(items);
        make_instance(global(G::IndexedSeqType), &[arr, LispObj::fixnum(0), NIL])
    })
}

/// IndexedSeq `x`: its array (or string, as cljs.core's `seq` of a
/// string makes), index and length.
fn indexed_part(x: LispObj) -> (LispObj, usize, usize) {
    let arr = x.uref(IS_ARR);
    let n = if arr.is_string() { arr.vector_length().integer() as usize } else { array::length(arr) };
    (arr, x.uref(IS_I).integer() as usize, n)
}

/// Element `i` of the array or string of an IndexedSeq.
fn indexed_get(arr: LispObj, i: usize) -> LispObj {
    if arr.is_string() {
        crate::clj::host::char_at(&[arr, LispObj::fixnum(i as i64)])
    } else {
        array::get(arr, i)
    }
}

/// `x` consed onto list `l` (a List or the empty list).
fn list_cons(x: LispObj, l: LispObj) -> LispObj {
    if is_list(l) {
        let n = l.uref(LIST_COUNT).integer() + 1;
        make_instance(global(G::ListType), &[NIL, x, l, LispObj::fixnum(n), NIL])
    } else {
        make_instance(global(G::ListType), &[NIL, x, NIL, LispObj::fixnum(1), NIL])
    }
}

/// A vector of `items`.
pub fn vector_from(items: &[LispObj]) -> LispObj {
    without_gc(|| {
        let n = items.len();
        if n == 0 {
            return global(G::EmptyVector);
        }
        let tailoff = if n <= 32 { 0 } else { ((n - 1) >> 5) << 5 };
        let tail = array_of(&items[tailoff..]);
        let mut shift = 5;
        let root = if tailoff == 0 {
            global(G::EmptyVectorNode)
        } else {
            let node = |children: &[LispObj]| {
                let a = heap::vector_node(32);
                for (i, &c) in children.iter().enumerate() {
                    a.svset(i, c);
                }
                make_instance(global(G::VectorNodeType), &[NIL, a])
            };
            let mut level: Vec<LispObj> = items[..tailoff].chunks(32).map(node).collect();
            while level.len() > 32 {
                level = level.chunks(32).map(node).collect();
                shift += 5;
            }
            node(&level)
        };
        let fields = [NIL, LispObj::fixnum(n as i64), LispObj::fixnum(shift), root, tail, NIL];
        make_instance(global(G::VectorType), &fields)
    })
}

/// An array map of `kvs` (k v k v ...), whose keys are all different.
pub fn array_map_from(kvs: &[LispObj]) -> LispObj {
    without_gc(|| {
        if kvs.is_empty() {
            return global(G::EmptyArrayMap);
        }
        let arr = array_of(kvs);
        make_instance(global(G::ArrayMapType), &[NIL, LispObj::fixnum(kvs.len() as i64 / 2), arr, NIL])
    })
}

/// A hash set of `items`, all different (backed by an array map).
pub fn hash_set_from(items: &[LispObj]) -> LispObj {
    without_gc(|| {
        if items.is_empty() {
            return global(G::EmptyHashSet);
        }
        let kvs: Vec<LispObj> = items.iter().flat_map(|&x| [x, NIL]).collect();
        let m = array_map_from(&kvs);
        make_instance(global(G::HashSetType), &[NIL, m, NIL])
    })
}

// ---------------------------------------------------------------------------
// Reading kernel collections

fn vector_count(v: LispObj) -> usize {
    v.uref(PV_CNT).integer() as usize
}

/// Element `i` of vector `v` (in range).
fn vector_nth(v: LispObj, i: usize) -> LispObj {
    let cnt = vector_count(v);
    let tailoff = if cnt <= 32 { 0 } else { ((cnt - 1) >> 5) << 5 };
    if i >= tailoff {
        return array::get(v.uref(PV_TAIL), i - tailoff);
    }
    let mut node = v.uref(PV_ROOT);
    let mut level = v.uref(PV_SHIFT).integer();
    while level > 0 {
        node = array::get(node.uref(VN_ARR), (i >> level) & 31);
        level -= 5;
    }
    array::get(node.uref(VN_ARR), i & 31)
}

fn vector_items(v: LispObj) -> Vec<LispObj> {
    (0..vector_count(v)).map(|i| vector_nth(v, i)).collect()
}

fn map_count(m: LispObj) -> usize {
    m.uref(PAM_CNT).integer() as usize
}

/// The keys and values (k v k v ...) of array map `m`.
fn array_map_kvs(m: LispObj) -> Vec<LispObj> {
    let arr = m.uref(PAM_ARR);
    (0..2 * map_count(m)).map(|i| array::get(arr, i)).collect()
}

// ---------------------------------------------------------------------------
// Falling back to clojure.core

/// `clojure.core/name` called with `args` if it is defined.
fn core_call(name: &str, args: &[LispObj]) -> Option<LispObj> {
    let v = ns_lookup(global(G::CoreNs), string_node(name)).filter(|&v| is_bound(v))?;
    Some(lisp_call(var_value(v), args))
}

fn core(name: &str, args: &[LispObj]) -> LispObj {
    core_call(name, args).unwrap_or_else(|| {
        crate::error::argument_error(format!("{name} not supported on: {}", printer::pr_str(args[0])))
    })
}

/// Is `x` (not a kernel value) true for predicate `clojure.core/name`?
pub fn core_bool(name: &str, x: LispObj) -> bool {
    !is_kernel_value(x) && core_call(name, &[x]).is_some_and(truthy)
}

// ---------------------------------------------------------------------------
// Predicates (as clojure.core's)

/// `seq?`: a List, the empty list, a Cons, or another seq.
pub fn is_seq(x: LispObj) -> bool {
    is_list(x) || is_empty_list(x) || is_cons(x) || is_indexed_seq(x) || core_bool("seq?", x)
}
pub fn is_vector(x: LispObj) -> bool {
    is_pvector(x) || core_bool("vector?", x)
}
pub fn is_map(x: LispObj) -> bool {
    is_array_map(x) || core_bool("map?", x)
}
pub fn is_set(x: LispObj) -> bool {
    is_hash_set(x) || core_bool("set?", x)
}

// ---------------------------------------------------------------------------
// Sequences

/// The elements of seqable `x` (map entries as two-element vectors),
/// realized.
pub fn items(x: LispObj) -> Vec<LispObj> {
    without_gc(|| {
        let mut out = Vec::new();
        let mut x = x;
        loop {
            if x == NIL || is_empty_list(x) {
                return out;
            }
            if is_list(x) {
                out.push(x.uref(LIST_FIRST));
                x = x.uref(LIST_REST);
            } else if is_indexed_seq(x) {
                let (arr, i, n) = indexed_part(x);
                out.extend((i..n).map(|j| indexed_get(arr, j)));
                return out;
            } else if is_cons(x) {
                out.push(x.uref(CONS_FIRST));
                x = x.uref(CONS_REST);
            } else if is_pvector(x) {
                out.extend(vector_items(x));
                return out;
            } else if is_array_map(x) {
                let kvs = array_map_kvs(x);
                out.extend(kvs.chunks(2).map(vector_from));
                return out;
            } else if is_hash_set(x) {
                let m = x.uref(PHS_MAP);
                if is_array_map(m) {
                    out.extend(array_map_kvs(m).into_iter().step_by(2));
                } else {
                    out.extend(items(m).into_iter().map(|e| nth(e, 0)));
                }
                return out;
            } else if x.is_string() {
                out.extend(x.string_to_rust().encode_utf16().map(|c| LispObj::character(c as u32)));
                return out;
            } else if array::is_array(x) {
                out.extend((0..array::length(x)).map(|i| array::get(x, i)));
                return out;
            } else {
                // Any other seqable: clojure.core's seq, first and next.
                let mut s = core("seq", &[x]);
                while s != NIL {
                    out.push(core("first", &[s]));
                    s = core("next", &[s]);
                }
                return out;
            }
        }
    })
}

/// The elements `i`, `i + step`, ... of simple-vector `v`.
fn sv_items(v: LispObj, i: usize, step: usize) -> Vec<LispObj> {
    (i..v.vector_length().integer() as usize).step_by(step).map(|j| v.svref(j)).collect()
}

/// A list of `[key value]` vectors for the simple-vector
/// `[key value key value ...]` `kvs`.
pub fn pairs_list(kvs: LispObj) -> LispObj {
    without_gc(|| {
        let v: Vec<LispObj> =
            sv_items(kvs, 0, 2).into_iter().zip(sv_items(kvs, 1, 2)).map(|(k, v)| vector_from(&[k, v])).collect();
        list_from(&v)
    })
}

/// A list of the keys (`which` 0) or values (1) of the simple-vector
/// `[key value key value ...]` `kvs`.
pub fn pairs_column(kvs: LispObj, which: usize) -> LispObj {
    without_gc(|| list_from(&sv_items(kvs, which, 2)))
}

/// `seq`: nil when empty; a list of the elements of a kernel
/// collection, string or array.
pub fn seq(x: LispObj) -> LispObj {
    if x == NIL || is_empty_list(x) {
        return NIL;
    }
    if is_list(x) || is_cons(x) {
        return x;
    }
    if is_indexed_seq(x) {
        let (_, i, n) = indexed_part(x);
        return if i < n { x } else { NIL };
    }
    if is_kernel_value(x) || x.is_string() || array::is_array(x) {
        let v = items(x);
        return if v.is_empty() { NIL } else { list_from(&v) };
    }
    core("seq", &[x])
}

pub fn first(x: LispObj) -> LispObj {
    if is_list(x) {
        x.uref(LIST_FIRST)
    } else if is_cons(x) {
        x.uref(CONS_FIRST)
    } else if is_indexed_seq(x) {
        let (arr, i, n) = indexed_part(x);
        if i < n { indexed_get(arr, i) } else { NIL }
    } else if x == NIL || is_empty_list(x) {
        NIL
    } else if is_pvector(x) {
        if vector_count(x) == 0 { NIL } else { vector_nth(x, 0) }
    } else if is_kernel_value(x) || x.is_string() || array::is_array(x) {
        first(seq(x))
    } else {
        core("first", &[x])
    }
}

/// `next`: nil when there is nothing after the first element.
pub fn next(x: LispObj) -> LispObj {
    if is_list(x) {
        x.uref(LIST_REST)
    } else if is_cons(x) {
        seq(x.uref(CONS_REST))
    } else if is_indexed_seq(x) {
        let (arr, i, n) = indexed_part(x);
        if i + 1 < n { make_instance(global(G::IndexedSeqType), &[arr, LispObj::fixnum(i as i64 + 1), NIL]) } else { NIL }
    } else if x == NIL || is_empty_list(x) {
        NIL
    } else if is_kernel_value(x) || x.is_string() || array::is_array(x) {
        next(seq(x))
    } else {
        core("next", &[x])
    }
}

/// `rest`: the empty list when there is nothing after the first element.
pub fn rest(x: LispObj) -> LispObj {
    if is_cons(x) {
        let r = x.uref(CONS_REST);
        return if r == NIL { global(G::EmptyList) } else { r };
    }
    if !is_kernel_value(x) && !x.is_string() && !array::is_array(x) {
        return core("rest", &[x]);
    }
    let n = next(x);
    if n == NIL { global(G::EmptyList) } else { n }
}

pub fn count(x: LispObj) -> usize {
    if x == NIL || is_empty_list(x) {
        0
    } else if is_list(x) {
        x.uref(LIST_COUNT).integer() as usize
    } else if is_pvector(x) {
        vector_count(x)
    } else if is_array_map(x) {
        map_count(x)
    } else if is_hash_set(x) {
        count(x.uref(PHS_MAP))
    } else if x.is_string() {
        x.vector_length().integer() as usize
    } else if array::is_array(x) {
        array::length(x)
    } else if is_indexed_seq(x) {
        let (_, i, n) = indexed_part(x);
        n.saturating_sub(i)
    } else if is_cons(x) {
        items(x).len()
    } else {
        let n = core("count", &[x]);
        n.integer() as usize
    }
}

/// `(nth x i)`, an error when out of range.
pub fn nth(x: LispObj, i: usize) -> LispObj {
    if is_pvector(x) {
        if i < vector_count(x) {
            return vector_nth(x, i);
        }
    } else if is_list(x) || is_cons(x) || is_indexed_seq(x) || x == NIL || is_empty_list(x) {
        let mut p = x;
        for _ in 0..i {
            p = next(p);
        }
        if p != NIL && !is_empty_list(p) {
            return first(p);
        }
    } else if !is_kernel_value(x) && !x.is_string() && !array::is_array(x) {
        return core("nth", &[x, LispObj::fixnum(i as i64)]);
    } else {
        let v = items(x);
        if i < v.len() {
            return v[i];
        }
    }
    crate::error::index_error(format!("Index out of bounds: {i}"))
}

/// `(cons x coll)`: a List when `coll` is nil or a list, else a Cons.
pub fn cons_onto(x: LispObj, coll: LispObj) -> LispObj {
    if coll == NIL || is_list(coll) || is_empty_list(coll) {
        return list_cons(x, coll);
    }
    let s = if is_cons(coll) || is_indexed_seq(coll) { coll } else { seq(coll) };
    if s == NIL || is_list(s) {
        return list_cons(x, s);
    }
    make_instance(global(G::ConsType), &[NIL, x, s, NIL])
}

/// `(conj coll x)`.
pub fn conj(coll: LispObj, x: LispObj) -> LispObj {
    without_gc(|| {
        if coll == NIL || is_list(coll) || is_empty_list(coll) {
            return list_cons(x, coll);
        }
        if is_cons(coll) {
            return make_instance(global(G::ConsType), &[NIL, x, coll, NIL]);
        }
        if is_pvector(coll) {
            let mut v = vector_items(coll);
            v.push(x);
            return with_meta(vector_from(&v), meta(coll));
        }
        if is_array_map(coll) {
            if is_map(x) {
                return items(x).into_iter().fold(coll, conj);
            }
            if !is_vector(x) || count(x) != 2 {
                crate::error::argument_error("Vector arg to map conj must be a pair");
            }
            return assoc(coll, nth(x, 0), nth(x, 1));
        }
        if is_hash_set(coll) {
            if contains(coll, x) {
                return coll;
            }
            let mut v = items(coll);
            v.push(x);
            return with_meta(hash_set_from(&v), meta(coll));
        }
        core("conj", &[coll, x])
    })
}

/// `(assoc m k v)` on maps and vectors.
pub fn assoc(m: LispObj, k: LispObj, v: LispObj) -> LispObj {
    without_gc(|| {
        if m == NIL || is_array_map(m) {
            let mut kvs = if m == NIL { Vec::new() } else { array_map_kvs(m) };
            match kvs.iter().step_by(2).position(|&x| equal(x, k)) {
                Some(i) => kvs[2 * i + 1] = v,
                None => kvs.extend([k, v]),
            }
            return with_meta(array_map_from(&kvs), meta(m));
        }
        if is_pvector(m) {
            let mut items = vector_items(m);
            let i = k.is_fixnum().then(|| k.integer()).filter(|&i| i >= 0 && i as usize <= items.len());
            let Some(i) = i else { crate::error::index_error("Index out of bounds") };
            if i as usize == items.len() {
                items.push(v);
            } else {
                items[i as usize] = v;
            }
            return with_meta(vector_from(&items), meta(m));
        }
        core("assoc", &[m, k, v])
    })
}

/// Is `k` a key of map `m` (an element of set `m`, an index of
/// vector `m`)?
pub fn contains(m: LispObj, k: LispObj) -> bool {
    without_gc(|| {
        if m == NIL {
            false
        } else if is_array_map(m) {
            array_map_kvs(m).iter().step_by(2).any(|&x| equal(x, k))
        } else if is_hash_set(m) {
            contains(m.uref(PHS_MAP), k)
        } else if is_pvector(m) {
            k.is_fixnum() && k.integer() >= 0 && (k.integer() as usize) < vector_count(m)
        } else if is_kernel_value(m) {
            false
        } else {
            core("contains?", &[m, k]) != FALSE
        }
    })
}

/// `(get m k not-found)`.
pub fn get(m: LispObj, k: LispObj, not_found: LispObj) -> LispObj {
    without_gc(|| {
        if m == NIL {
            not_found
        } else if is_array_map(m) {
            let kvs = array_map_kvs(m);
            kvs.chunks(2).find(|e| equal(e[0], k)).map_or(not_found, |e| e[1])
        } else if is_hash_set(m) {
            if contains(m, k) { k } else { not_found }
        } else if is_pvector(m) {
            if contains(m, k) { vector_nth(m, k.integer() as usize) } else { not_found }
        } else if is_kernel_value(m) && !is_symbol(m) && !is_keyword(m) {
            not_found
        } else {
            core("get", &[m, k, not_found])
        }
    })
}

/// The metadata of `x` (nil for values without).
pub fn meta(x: LispObj) -> LispObj {
    if is_symbol(x) {
        sym_meta(x)
    } else if is_instance(x) && is_kernel_value(x) && !is_keyword(x) {
        x.uref(meta_cell(x))
    } else if is_kernel_value(x) {
        NIL
    } else {
        core_call("meta", &[x]).unwrap_or(NIL)
    }
}

/// `x` with metadata `m`.
pub fn with_meta(x: LispObj, m: LispObj) -> LispObj {
    if is_symbol(x) {
        return sym_with_meta(x, m);
    }
    if is_instance(x) && is_kernel_value(x) && !is_keyword(x) {
        if x.uref(meta_cell(x)) == m {
            return x;
        }
        return without_gc(|| {
            let ty = instance_type(x);
            let n = field_count(ty);
            let fields: Vec<LispObj> = (0..n).map(|i| x.uref(INSTANCE_FIELDS + i)).collect();
            let y = make_instance(ty, &fields);
            y.uset(meta_cell(x), m);
            y
        });
    }
    core("with-meta", &[x, m])
}

// ---------------------------------------------------------------------------
// Equality and hashing of reader data

/// Clojure `=`. Native for the values the reader produces; other
/// collections use clojure.core's `=`.
pub fn equal(a: LispObj, b: LispObj) -> bool {
    without_gc(|| {
        if a == b {
            return true;
        }
        if !is_kernel_value(a) || !is_kernel_value(b) {
            return core_call("=", &[a, b]).is_some_and(truthy);
        }
        let seqish = |x: LispObj| is_list(x) || is_empty_list(x) || is_cons(x) || is_indexed_seq(x) || is_pvector(x);
        if crate::numbers::is_number(a) {
            crate::numbers::is_number(b) && crate::numbers::equiv(a, b)
        } else if a.is_string() {
            b.is_string() && crate::object::lisp_strings_equal(a, b)
        } else if is_symbol(a) {
            is_symbol(b) && sym_eq(a, b)
        } else if is_keyword(a) {
            is_keyword(b) && crate::object::lisp_strings_equal(a.uref(KW_FQN), b.uref(KW_FQN))
        } else if seqish(a) {
            seqish(b) && {
                let (x, y) = (items(a), items(b));
                x.len() == y.len() && x.iter().zip(&y).all(|(&p, &q)| equal(p, q))
            }
        } else if is_array_map(a) {
            is_array_map(b)
                && map_count(a) == map_count(b)
                && array_map_kvs(a).chunks(2).all(|e| {
                    contains(b, e[0]) && equal(get(b, e[0], NIL), e[1])
                })
        } else if is_hash_set(a) {
            is_hash_set(b) && count(a) == count(b) && items(a).into_iter().all(|x| contains(b, x))
        } else {
            false
        }
    })
}

/// A hash of reader data consistent with `equal` (for finding duplicate
/// keys); `None` for values it does not know.
pub fn data_hash(x: LispObj) -> Option<u64> {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    if x == NIL || is_boolean(x) || x.is_character() {
        x.raw().hash(&mut h);
    } else if crate::numbers::is_number(x) {
        crate::numbers::hash(x).hash(&mut h);
    } else if x.is_string() {
        x.string_to_rust().hash(&mut h);
    } else if is_symbol(x) {
        (1, sym_ns(x).is_string().then(|| sym_ns(x).string_to_rust()), sym_name(x).string_to_rust()).hash(&mut h);
    } else if is_keyword(x) {
        (2, x.uref(KW_FQN).string_to_rust()).hash(&mut h);
    } else if is_list(x) || is_empty_list(x) || is_cons(x) || is_indexed_seq(x) || is_pvector(x) {
        3.hash(&mut h);
        for y in items(x) {
            data_hash(y)?.hash(&mut h);
        }
    } else if is_array_map(x) || is_hash_set(x) {
        // Order-independent.
        let mut sum: u64 = 0;
        for y in items(x) {
            sum = sum.wrapping_add(data_hash(y)?);
        }
        (4, sum).hash(&mut h);
    } else {
        return None;
    }
    Some(h.finish())
}

/// The first element of `xs` equal to an earlier one, if any.
pub fn find_duplicate(xs: &[LispObj]) -> Option<LispObj> {
    if xs.len() <= 16 {
        return (1..xs.len()).find(|&i| xs[..i].iter().any(|&y| equal(xs[i], y))).map(|i| xs[i]);
    }
    let mut seen: std::collections::HashMap<u64, Vec<LispObj>> = std::collections::HashMap::new();
    for &x in xs {
        let Some(h) = data_hash(x) else {
            // Not reader data: compare with everything.
            return find_duplicate_slow(xs);
        };
        let bucket = seen.entry(h).or_default();
        if bucket.iter().any(|&y| equal(x, y)) {
            return Some(x);
        }
        bucket.push(x);
    }
    None
}

fn find_duplicate_slow(xs: &[LispObj]) -> Option<LispObj> {
    (1..xs.len()).find(|&i| xs[..i].iter().any(|&y| equal(xs[i], y))).map(|i| xs[i])
}

// ---------------------------------------------------------------------------
// Kernel functions

fn index_arg(x: LispObj) -> usize {
    if !x.is_fixnum() || x.integer() < 0 {
        crate::error::index_error(format!("Index out of bounds: {}", printer::pr_str(x)));
    }
    x.integer() as usize
}

pub fn first_prim(a: &[LispObj]) -> LispObj {
    first(a[0])
}
pub fn next_prim(a: &[LispObj]) -> LispObj {
    next(a[0])
}
pub fn rest_prim(a: &[LispObj]) -> LispObj {
    rest(a[0])
}
pub fn seq_prim(a: &[LispObj]) -> LispObj {
    seq(a[0])
}
pub fn count_prim(a: &[LispObj]) -> LispObj {
    LispObj::fixnum(count(a[0]) as i64)
}
/// `(k/nth coll i)` or `(k/nth coll i not-found)`.
pub fn nth_prim(a: &[LispObj]) -> LispObj {
    if a.len() == 3 {
        let ok = a[1].is_fixnum() && a[1].integer() >= 0 && (a[1].integer() as usize) < count(a[0]);
        return if ok { nth(a[0], a[1].integer() as usize) } else { a[2] };
    }
    nth(a[0], index_arg(a[1]))
}
pub fn cons_prim(a: &[LispObj]) -> LispObj {
    cons_onto(a[0], a[1])
}
pub fn conj_prim(a: &[LispObj]) -> LispObj {
    conj(a[0], a[1])
}
pub fn assoc_prim(a: &[LispObj]) -> LispObj {
    assoc(a[0], a[1], a[2])
}
pub fn get_prim(a: &[LispObj]) -> LispObj {
    get(a[0], a[1], a.get(2).copied().unwrap_or(NIL))
}
pub fn contains_prim(a: &[LispObj]) -> LispObj {
    cbool(contains(a[0], a[1]))
}
pub fn meta_prim(a: &[LispObj]) -> LispObj {
    meta(a[0])
}
pub fn with_meta_prim(a: &[LispObj]) -> LispObj {
    with_meta(a[0], a[1])
}
pub fn equal_prim(a: &[LispObj]) -> LispObj {
    cbool(equal(a[0], a[1]))
}
/// `(k/list & items)`.
pub fn list_prim(a: &[LispObj]) -> LispObj {
    list_from(a)
}
/// `(k/vector & items)`.
pub fn vector_prim(a: &[LispObj]) -> LispObj {
    vector_from(a)
}
/// `(k/array-map & kvs)`: later keys replace earlier equal ones.
pub fn array_map_prim(a: &[LispObj]) -> LispObj {
    if !a.len().is_multiple_of(2) {
        crate::error::argument_error("No value supplied for key");
    }
    a.chunks(2).fold(global(G::EmptyArrayMap), |m, e| assoc(m, e[0], e[1]))
}
/// `(k/hash-set & items)`: duplicates are dropped.
pub fn hash_set_prim(a: &[LispObj]) -> LispObj {
    a.iter().fold(global(G::EmptyHashSet), |s, &x| conj(s, x))
}
pub fn seq_p(a: &[LispObj]) -> LispObj {
    cbool(is_seq(a[0]))
}
pub fn vector_p(a: &[LispObj]) -> LispObj {
    cbool(is_vector(a[0]))
}
pub fn map_p(a: &[LispObj]) -> LispObj {
    cbool(is_map(a[0]))
}
pub fn set_p(a: &[LispObj]) -> LispObj {
    cbool(is_set(a[0]))
}
/// `(k/vec coll)`: a vector of the elements of `coll`.
pub fn vec_prim(a: &[LispObj]) -> LispObj {
    if is_pvector(a[0]) {
        return with_meta(a[0], NIL);
    }
    vector_from(&items(a[0]))
}
/// `(k/concat & seqables)`: a list of all their elements (syntax-quote's
/// splicing); nil when there are none.
pub fn concat_prim(a: &[LispObj]) -> LispObj {
    let v: Vec<LispObj> = without_gc(|| a.iter().flat_map(|&x| items(x)).collect());
    if v.is_empty() { NIL } else { list_from(&v) }
}

/// The number of fixed parameters of `f`'s variadic arity, if `f` is a
/// compiled function with one.
fn rest_arity(f: LispObj) -> Option<usize> {
    if !f.is_uvector_of(FUNCTION_TYPE) {
        return None;
    }
    let n = f.uref(FUNCTION_ADDRESS).uref(crate::runtime::CODE_REST_ARITY);
    n.is_fixnum().then(|| n.integer() as usize)
}

/// `(k/apply f args... seqable)`: call `f` with `args` and the elements
/// of `seqable`. A compiled variadic function is given the elements past
/// its fixed parameters unrealized, as its rest seq (entered with the
/// rest seq ready, `runtime::REST_READY`), so the seq may be infinite.
/// An instance is applied through its invoke method.
pub fn apply_prim(a: &[LispObj]) -> LispObj {
    let mut f = a[0];
    let mut front: Vec<LispObj> = Vec::new();
    if is_var(f) {
        f = var_value(f);
    }
    if is_instance(f)
        && let Some(g) = crate::clj::prims::invoke_function(f)
    {
        front.push(f);
        f = g;
    }
    let explicit = &a[1..a.len() - 1];
    let tail = a[a.len() - 1];
    let n = rest_arity(f);
    let (mut args, rest) = without_gc(|| {
        let mut args = front;
        args.extend_from_slice(explicit);
        let Some(n) = n else {
            args.extend(items(tail));
            return (args, NIL);
        };
        let mut s = seq(tail);
        if args.len() > n {
            for &x in args[n..].iter().rev() {
                s = cons_onto(x, s);
            }
            args.truncate(n);
        } else {
            while args.len() < n && s != NIL {
                args.push(first(s));
                s = next(s);
            }
        }
        (args, s)
    });
    // (The calls copy the arguments to the stack before anything allocates.)
    if rest == NIL {
        crate::prims::funcall(f, &args)
    } else {
        args.push(rest);
        crate::runtime::lisp_call_rest_ready(f, &args)
    }
}
