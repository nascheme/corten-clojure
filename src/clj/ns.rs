//! Vars and namespaces (as SCI's `sci.lang/Var` and `Namespace`, after
//! JVM Clojure). Both are instances of kernel types:
//!
//! - A Var holds its current value (`VAR_VAL`, the root unless a
//!   `binding` frame has replaced it: shallow binding, the dynamic frame
//!   keeps the outer value), its namespace, name, metadata, flags and
//!   watches. Compiled code keeps the Var in its literal area and loads
//!   `VAR_VAL` directly.
//! - A Namespace has a name and two string-keyed tables: `mappings`
//!   (name -> Var interned here or referred, or a type) and `aliases`
//!   (name -> Namespace).

use super::*;

// ---------------------------------------------------------------------------
// String tables: a simple-vector `[count entries]`, `entries` a
// simple-vector of 2 * capacity cells (key value ...) with linear probing.
// An empty key is NIL, a removed one `TOMBSTONE`. Keys are simple strings,
// hashed by content, so a GC never needs a rehash.

const TOMBSTONE: LispObj = LispObj::fixnum(0);

pub fn strtab_new() -> LispObj {
    let t = vector_node(2);
    t.svset(0, LispObj::fixnum(0));
    t.svset(1, vector_node(2 * 8));
    t
}

fn str_hash(s: &[LispChar]) -> usize {
    let mut h: u64 = 0xcbf29ce484222325;
    for &u in s {
        h ^= u as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h as usize
}

fn strtab_cap(t: LispObj) -> usize {
    t.svref(1).vector_length().integer() as usize / 2
}

/// Index of `key` (UTF-16 units) in `t`, or of the slot where it would
/// go.
fn strtab_find(t: LispObj, key: &[LispChar]) -> (usize, bool) {
    let e = t.svref(1);
    let cap = strtab_cap(t);
    let mut i = str_hash(key) % cap;
    let mut free = None;
    loop {
        let k = e.svref(2 * i);
        if k == NIL {
            return (free.unwrap_or(i), false);
        }
        if k == TOMBSTONE {
            free.get_or_insert(i);
        } else if crate::object::units_of(k) == key {
            return (i, true);
        }
        i = (i + 1) % cap;
    }
}

pub fn strtab_get(t: LispObj, key: LispObj) -> Option<LispObj> {
    strtab_get_units(t, crate::object::units_of(key))
}

/// The value of the key with UTF-16 units `key` in `t`.
pub fn strtab_get_units(t: LispObj, key: &[LispChar]) -> Option<LispObj> {
    match strtab_find(t, key) {
        (i, true) => Some(t.svref(1).svref(2 * i + 1)),
        _ => None,
    }
}

pub fn strtab_put(t: LispObj, key: LispObj, v: LispObj) {
    let (i, found) = strtab_find(t, crate::object::units_of(key));
    if found {
        t.svref(1).svset(2 * i + 1, v);
        return;
    }
    let count = t.svref(0).integer() as usize;
    if 4 * (count + 1) > 3 * strtab_cap(t) {
        strtab_grow(t);
        return strtab_put(t, key, v);
    }
    let key = if key.is_simple_char_vector() { key } else { string_node(&key.string_to_rust()) };
    let e = t.svref(1);
    e.svset(2 * i, key);
    e.svset(2 * i + 1, v);
    t.svset(0, LispObj::fixnum(count as i64 + 1));
}

pub fn strtab_remove(t: LispObj, key: LispObj) {
    if let (i, true) = strtab_find(t, crate::object::units_of(key)) {
        let e = t.svref(1);
        e.svset(2 * i, TOMBSTONE);
        e.svset(2 * i + 1, NIL);
        t.svset(0, LispObj::fixnum(t.svref(0).integer() - 1));
    }
}

fn strtab_grow(t: LispObj) {
    let old = t.svref(1);
    let cap = strtab_cap(t);
    t.svset(1, vector_node(4 * cap));
    t.svset(0, LispObj::fixnum(0));
    for i in 0..cap {
        let k = old.svref(2 * i);
        if k != NIL && k != TOMBSTONE {
            strtab_put(t, k, old.svref(2 * i + 1));
        }
    }
}

/// A copy of `t`.
pub fn strtab_copy(t: LispObj) -> LispObj {
    let c = vector_node(2);
    c.svset(0, t.svref(0));
    let e = t.svref(1);
    let n = e.vector_length().integer() as usize;
    let ce = vector_node(n);
    for i in 0..n {
        ce.svset(i, e.svref(i));
    }
    c.svset(1, ce);
    c
}

/// The entries of `t` as a simple-vector `[key value key value ...]`.
pub fn strtab_entries(t: LispObj) -> LispObj {
    let out = vector_node(2 * t.svref(0).integer() as usize);
    let e = t.svref(1);
    let mut j = 0;
    for i in 0..strtab_cap(t) {
        let k = e.svref(2 * i);
        if k != NIL && k != TOMBSTONE {
            out.svset(j, k);
            out.svset(j + 1, e.svref(2 * i + 1));
            j += 2;
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Vars

pub const VAR_VAL: usize = INSTANCE_FIELDS;
pub const VAR_NS: usize = INSTANCE_FIELDS + 1;
pub const VAR_NAME: usize = INSTANCE_FIELDS + 2;
pub const VAR_META: usize = INSTANCE_FIELDS + 3;
pub const VAR_FLAGS: usize = INSTANCE_FIELDS + 4;
pub const VAR_WATCHES: usize = INSTANCE_FIELDS + 5;
pub const VAR_FIELD_NAMES: [&str; 6] = ["val", "ns", "name", "meta", "flags", "watches"];

/// `^:dynamic`.
pub const DYNAMIC_FLAG: i64 = 1;
/// A Clojure macro.
pub const MACRO_FLAG: i64 = 2;
/// The metadata was given by `def`: reading it adds `:ns` and `:name`.
pub const DEF_META_FLAG: i64 = 4;

pub fn is_var(x: LispObj) -> bool {
    is_a(x, G::VarType)
}

/// A new unbound var named `name` (a simple string) in `ns`.
fn make_var(ns: LispObj, name: LispObj) -> LispObj {
    make_instance(global(G::VarType), &[SYMBOL_UNBOUND, ns, name, NIL, LispObj::fixnum(0), NIL])
}

pub fn var_value(v: LispObj) -> LispObj {
    v.uref(VAR_VAL)
}
pub fn set_var_value(v: LispObj, x: LispObj) {
    v.uset(VAR_VAL, x)
}
pub fn is_bound(v: LispObj) -> bool {
    var_value(v) != SYMBOL_UNBOUND
}
pub fn var_ns(v: LispObj) -> LispObj {
    v.uref(VAR_NS)
}
pub fn var_name(v: LispObj) -> LispObj {
    v.uref(VAR_NAME)
}
pub fn var_meta(v: LispObj) -> LispObj {
    v.uref(VAR_META)
}
pub fn set_var_meta(v: LispObj, m: LispObj) {
    v.uset(VAR_META, m)
}
pub fn var_flag(v: LispObj, f: i64) -> bool {
    v.uref(VAR_FLAGS).integer() & f != 0
}
pub fn set_var_flag(v: LispObj, f: i64, on: bool) {
    let old = v.uref(VAR_FLAGS).integer();
    v.uset(VAR_FLAGS, LispObj::fixnum(if on { old | f } else { old & !f }));
}

/// The outermost binding frame of `v`, if `binding` has rebound it.
fn outermost_frame(v: LispObj) -> Option<usize> {
    crate::runtime::special_frames().into_iter().rev().find(|&(_, t)| t == v).map(|(f, _)| f)
}

/// The root value of `v` (its value outside any `binding`).
pub fn var_root(v: LispObj) -> LispObj {
    match outermost_frame(v) {
        Some(f) => crate::runtime::frame_saved_value(f),
        None => var_value(v),
    }
}

/// Set the root value of `v`: the value outside any `binding`.
pub fn set_var_root(v: LispObj, x: LispObj) {
    match outermost_frame(v) {
        Some(f) => crate::runtime::set_frame_saved_value(f, x),
        None => set_var_value(v, x),
    }
}

/// Is `v` rebound by a `binding` frame?
pub fn is_thread_bound(v: LispObj) -> bool {
    outermost_frame(v).is_some()
}

/// The vars rebound by `binding` frames with their current values, as
/// a simple-vector `[var value var value ...]`.
pub fn thread_bindings() -> LispObj {
    crate::runtime::without_gc(|| {
        let mut out: Vec<LispObj> = Vec::new();
        for (f, _) in crate::runtime::special_frames() {
            let t = crate::runtime::frame_tag(f);
            if is_var(t) && !out.iter().step_by(2).any(|&v| v == t) {
                out.extend([t, var_value(t)]);
            }
        }
        vector_of(&out)
    })
}

/// The value of a binding-frame target, a Var.
pub fn binding_value(x: LispObj) -> LispObj {
    var_value(x)
}
pub fn set_binding_value(x: LispObj, v: LispObj) {
    set_var_value(x, v)
}

// ---------------------------------------------------------------------------
// Namespaces

pub const NS_NAME: usize = INSTANCE_FIELDS;
pub const NS_MAPPINGS: usize = INSTANCE_FIELDS + 1;
pub const NS_ALIASES: usize = INSTANCE_FIELDS + 2;
pub const NS_META: usize = INSTANCE_FIELDS + 3;
pub const NS_FIELD_NAMES: [&str; 4] = ["name", "mappings", "aliases", "meta"];

pub fn is_ns(x: LispObj) -> bool {
    is_a(x, G::NamespaceType)
}

/// A new namespace named `name`.
pub fn create_ns(name: &str) -> LispObj {
    let ns = make_instance(global(G::NamespaceType), &[string_node(name), strtab_new(), strtab_new(), NIL]);
    strtab_put(global(G::Namespaces), ns_name(ns), ns);
    ns
}

/// The namespace named `name`, if any.
pub fn find_ns(name: &str) -> Option<LispObj> {
    let units: Vec<LispChar> = name.encode_utf16().collect();
    strtab_get_units(global(G::Namespaces), &units)
}

/// Remove namespace `ns` from the namespace table.
pub fn remove_ns(ns: LispObj) {
    if strtab_get(global(G::Namespaces), ns_name(ns)) == Some(ns) {
        strtab_remove(global(G::Namespaces), ns_name(ns));
    }
}

/// The namespace named `name`, created if needed.
pub fn find_or_create_ns(name: &str) -> LispObj {
    find_ns(name).unwrap_or_else(|| create_ns(name))
}

pub fn current_ns() -> LispObj {
    var_value(global(G::NsVar))
}

pub fn ns_name(ns: LispObj) -> LispObj {
    ns.uref(NS_NAME)
}

pub fn ns_mappings(ns: LispObj) -> LispObj {
    ns.uref(NS_MAPPINGS)
}

pub fn ns_aliases(ns: LispObj) -> LispObj {
    ns.uref(NS_ALIASES)
}

/// The var named `name` (a string) interned in `ns`, created (unbound) if
/// needed. A var of another namespace referred under that name is
/// replaced, as `def` does in Clojure.
pub fn intern_var(ns: LispObj, name: LispObj) -> LispObj {
    if let Some(v) = strtab_get(ns_mappings(ns), name)
        && is_var(v)
        && var_ns(v) == ns
    {
        return v;
    }
    let name = if name.is_simple_char_vector() { name } else { string_node(&name.string_to_rust()) };
    let v = make_var(ns, name);
    strtab_put(ns_mappings(ns), name, v);
    v
}

/// What `name` maps to in `ns` (a var interned or referred), if present.
pub fn ns_lookup(ns: LispObj, name: LispObj) -> Option<LispObj> {
    strtab_get(ns_mappings(ns), name).filter(|&v| is_var(v))
}

/// Make `var` visible in `ns` under its own name (Clojure `refer`).
pub fn refer_var(ns: LispObj, var: LispObj) {
    refer_var_as(ns, var, var_name(var));
}

/// Refer `var` in `ns` under `name` (a string).
pub fn refer_var_as(ns: LispObj, var: LispObj, name: LispObj) {
    strtab_put(ns_mappings(ns), name, var);
}

/// Record `alias` (a string) for namespace `target` in `ns`.
pub fn add_alias(ns: LispObj, alias: LispObj, target: LispObj) {
    strtab_put(ns_aliases(ns), alias, target);
}

/// The namespace that `name` (a string) denotes in `ns`: an alias or a
/// namespace name.
pub fn resolve_ns(ns: LispObj, name: LispObj) -> Option<LispObj> {
    if let Some(t) = strtab_get(ns_aliases(ns), name) {
        return Some(t);
    }
    let n = name.string_to_rust();
    // The ported cljs.core is clojure.core.
    if n == "cljs.core" {
        return Some(global(G::CoreNs));
    }
    // `a$macros` is `a` when `a` keeps its macros in itself (a .cljc
    // file loaded once, unlike self-hosted ClojureScript).
    find_ns(&n)
        .or_else(|| n.strip_suffix("$macros").and_then(find_ns))
        // `clojure.x` is `cljs.x` when only that exists, as lib loading
        // falls back to it.
        .or_else(|| n.strip_prefix("clojure.").and_then(|s| find_ns(&format!("cljs.{s}"))))
}
