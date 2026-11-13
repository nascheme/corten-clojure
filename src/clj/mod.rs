//! Clojure on the kernel: object representation, namespaces and vars.
//!
//! - `nil`, `false` and `true` are immediates (`object.rs`).
//! - Clojure objects are uvectors of type `CLJ_INSTANCE_TYPE`: cell 1 is a
//!   type descriptor, the following cells are fields. Descriptors are
//!   instances of the root type `Type`.
//! - Symbols are `Symbol` instances (not interned). Keywords are `Keyword`
//!   instances interned in a string table (`G::Keywords`) by full name.
//! - Vars and namespaces are instances of kernel types (`ns.rs`).

pub mod array;
pub mod coll;
pub mod exceptions;
pub mod host;
pub mod format;
pub mod js;
pub mod math;
pub mod native;
pub mod number;
pub mod printer;
pub mod prims;
pub mod reader;
pub mod ns;
pub mod regex;

pub use ns::*;

use crate::error::lisp_error;
use crate::heap::{self, string_node, vector_node, vector_of};
use crate::object::*;
use crate::qv::{qv, set_qv};
use crate::qv_index::*;

// ---------------------------------------------------------------------------
// Instances and types

/// Cell holding the type descriptor of an instance.
pub const INSTANCE_TYPE: usize = 1;
/// First field cell of an instance.
pub const INSTANCE_FIELDS: usize = 2;

// Type descriptor fields (cells).
pub const TYPE_NAME: usize = 2;
/// Field names: a simple-vector of strings.
pub const TYPE_FIELDS: usize = 3;
/// Protocol methods: simple-vector indexed by method id, or NIL.
pub const TYPE_METHODS: usize = 4;
/// Methods called by name (`(.name x)`): a string table (`ns.rs`) of
/// functions, or NIL.
pub const TYPE_OBJ_METHODS: usize = 5;
/// Static properties (`(.-EMPTY T)`): a string table, or NIL. Names are
/// stored as cljs munges them (`-` is `_`), see `type_static`.
pub const TYPE_STATICS: usize = 6;
/// Supertype (exception classes: `Exception` of `RuntimeException`), or
/// NIL. Instances of a type are instances of its supertypes, and inherit
/// their protocol and named methods.
pub const TYPE_SUPER: usize = 7;
pub const TYPE_SIZE: usize = 7;

// Protocol fields: name, marker method id, string table name -> id.
pub const PROTO_NAME: usize = 2;
pub const PROTO_MARKER: usize = 3;
pub const PROTO_METHODS: usize = 4;

// Symbol fields, as in ClojureScript's `(deftype Symbol [ns name str
// _hash _meta])` (cljs.core extends the kernel type). `str` is the
// full name, `_hash` a cache filled by cljs.core.
pub const SYM_NS: usize = 2;
pub const SYM_NAME: usize = 3;
pub const SYM_STR: usize = 4;
pub const SYM_HASH: usize = 5;
pub const SYM_META: usize = 6;
pub const SYM_SIZE: usize = 6;

// Keyword fields, as in `(deftype Keyword [ns name fqn _hash])`.
pub const KW_NS: usize = 2;
pub const KW_NAME: usize = 3;
pub const KW_FQN: usize = 4;
pub const KW_HASH: usize = 5;
pub const KW_SIZE: usize = 5;

/// Slots of the Clojure globals vector (QV `CLJ_GLOBALS_Index`).
#[derive(Clone, Copy)]
#[repr(usize)]
pub enum G {
    TypeType,
    SymbolType,
    KeywordType,
    // The collection types the reader produces (`coll.rs`).
    ListType,
    EmptyListType,
    ConsType,
    VectorType,
    VectorNodeType,
    ArrayMapType,
    HashSetType,
    /// Rest arguments: an IndexedSeq over an array of them.
    IndexedSeqType,
    /// The empty list, vector (and its root node), map and set.
    EmptyList,
    EmptyVector,
    EmptyVectorNode,
    EmptyArrayMap,
    EmptyHashSet,
    /// String table interning keywords by full name.
    Keywords,
    /// The namespace `clojure.core`.
    CoreNs,
    /// The namespace `corten.kernel`, of the kernel functions.
    KernelNs,
    /// The var `clojure.core/*ns*`.
    NsVar,
    /// Function printing an object (`pr-str` style), or NIL.
    PrintHook,
    /// Function `(obj args...)` invoking a non-function object, or NIL.
    InvokeHook,
    /// String table of all namespaces by name.
    Namespaces,
    /// Next protocol method id (fixnum).
    NextMethodId,
    /// Names of protocol methods (simple-vector indexed by id).
    MethodNames,
    /// Method id of IFn's invoke, used to call instances, or NIL.
    InvokeId,
    // Types of kernel objects that are not Clojure instances.
    NilType,
    BooleanType,
    NumberType,
    StringType,
    CharType,
    ArrayType,
    FunctionType,
    /// JavaScript objects (`js.rs`) and other kernel objects.
    ObjectType,
    /// Holds the `default` implementations of protocols.
    DefaultType,
    ProtocolType,
    /// Directories searched by `require` (a simple-vector of strings).
    LoadPath,
    /// The var `clojure.core/*read-tag-fn*`: when bound to a fn, the
    /// reader returns `(f tag-string form)` for every tagged literal
    /// (cljs.reader).
    ReadTagVar,
    /// `goog/getUid` table (see `js::uid`), or NIL.
    UidTable,
    VarType,
    NamespaceType,
    /// Function `(var old new)` calling the watches of a var whose root
    /// changed, or NIL.
    VarWatchHook,
    /// The names of the loaded libs (a string table).
    LoadedLibs,
    // Subtypes of `NumberType`.
    LongType,
    BigIntType,
    RatioType,
    DoubleType,
    /// Fixnum incremented when a type's named methods or supertype
    /// change; method call-site caches hold the epoch they were filled in.
    MethodEpoch,
    /// The `corten.lang` error types, in `ErrorKind` order, then
    /// `AssertionError` (a vector).
    ErrorTypes,
    /// Kernel input streams (`streams.rs`) and the console's.
    StreamType,
    ConsoleIn,
}
const NUM_GLOBALS: usize = G::ConsoleIn as usize + 1;

pub fn global(g: G) -> LispObj {
    qv(CLJ_GLOBALS_Index).svref(g as usize)
}

pub fn set_global(g: G, v: LispObj) {
    qv(CLJ_GLOBALS_Index).svset(g as usize, v)
}

/// A new instance of type `ty` with `fields`.
pub fn make_instance(ty: LispObj, fields: &[LispObj]) -> LispObj {
    let x = heap::alloc_uvector(fields.len() + 1, CLJ_INSTANCE_TYPE);
    x.uset(INSTANCE_TYPE, ty);
    for (i, &f) in fields.iter().enumerate() {
        x.uset(INSTANCE_FIELDS + i, f);
    }
    x
}

pub fn is_instance(x: LispObj) -> bool {
    x.is_uvector_of(CLJ_INSTANCE_TYPE)
}

pub fn instance_type(x: LispObj) -> LispObj {
    x.uref(INSTANCE_TYPE)
}

fn is_a(x: LispObj, g: G) -> bool {
    is_instance(x) && instance_type(x) == global(g)
}

pub fn is_symbol(x: LispObj) -> bool {
    is_a(x, G::SymbolType)
}
pub fn is_keyword(x: LispObj) -> bool {
    is_a(x, G::KeywordType)
}

/// A type descriptor named `name` with field names `fields`.
pub fn make_type(name: &str, fields: &[&str]) -> LispObj {
    let v = vector_node(fields.len());
    for (i, f) in fields.iter().enumerate() {
        v.svset(i, string_node(f));
    }
    make_type_str(string_node(name), v)
}

/// A type descriptor named `name` with field names `fields`, a
/// simple-vector of strings.
pub fn make_type_str(name: LispObj, fields: LispObj) -> LispObj {
    make_instance(global(G::TypeType), &[name, fields, NIL, NIL, NIL, NIL])
}

/// The number of fields of instances of `ty`.
pub fn field_count(ty: LispObj) -> usize {
    ty.uref(TYPE_FIELDS).vector_length().integer() as usize
}

/// Static property names compare as cljs munges them: `-` is `_`, so
/// `(.-HASHMAP-THRESHOLD T)` and `T.HASHMAP_THRESHOLD` are the same.
fn munged(name: &[LispChar]) -> Vec<LispChar> {
    name.iter().map(|&c| if c == b'-' as LispChar { b'_' as LispChar } else { c }).collect()
}

/// Static property `name` (a string) of type `ty`.
pub fn type_static(ty: LispObj, name: LispObj) -> Option<LispObj> {
    type_static_units(ty, crate::object::units_of(name))
}

pub fn type_static_str(ty: LispObj, name: &str) -> Option<LispObj> {
    type_static_units(ty, &name.encode_utf16().collect::<Vec<_>>())
}

fn type_static_units(ty: LispObj, name: &[LispChar]) -> Option<LispObj> {
    let t = ty.uref(TYPE_STATICS);
    if t == NIL { None } else { strtab_get_units(t, &munged(name)) }
}

/// Set static property `name` (a string) of type `ty`.
pub fn set_type_static(ty: LispObj, name: LispObj, v: LispObj) {
    let key = heap::string_from_units(&munged(crate::object::units_of(name)));
    if ty.uref(TYPE_STATICS) == NIL {
        ty.uset(TYPE_STATICS, strtab_new());
    }
    strtab_put(ty.uref(TYPE_STATICS), key, v);
}

pub fn is_type(x: LispObj) -> bool {
    is_instance(x) && instance_type(x) == global(G::TypeType)
}

/// The type descriptor of any object (kernel types for non-instances).
pub fn type_of(x: LispObj) -> LispObj {
    let g = match x.tag() {
        FIXNUM_TAG => G::LongType,
        IMMEDIATE_TAG if x.is_character() => G::CharType,
        IMMEDIATE_TAG if is_boolean(x) => G::BooleanType,
        IMMEDIATE_TAG if x == NIL => G::NilType,
        UVECTOR_TAG => match x.uvector_type() {
            CLJ_INSTANCE_TYPE => return instance_type(x),
            FUNCTION_TYPE | KFUNCTION_TYPE => G::FunctionType,
            DOUBLE_FLOAT_TYPE | SINGLE_FLOAT_TYPE => G::DoubleType,
            RATIO_TYPE => G::RatioType,
            BIGNUM_TYPE if crate::numbers::is_long(x) => G::LongType,
            BIGNUM_TYPE => G::BigIntType,
            SIMPLE_VECTOR_TYPE => G::ArrayType,
            ARRAY_TYPE if array::is_array(x) => G::ArrayType,
            _ if x.is_string() => G::StringType,
            _ => G::ObjectType,
        },
        _ => G::ObjectType,
    };
    global(g)
}

/// Protocol method `id` of `x`'s type (or the default one), or NIL.
pub fn find_method(id: usize, x: LispObj) -> LispObj {
    // As in ClojureScript, where goog/typeOf of an instance is "object",
    // methods extended to `object` apply to instances too.
    let object = if is_instance(x) { global(G::ObjectType) } else { NIL };
    let own = |ty: LispObj| {
        let m = ty.uref(TYPE_METHODS);
        if m != NIL && id < m.vector_length().integer() as usize { m.svref(id) } else { NIL }
    };
    let mut ty = type_of(x);
    while ty != NIL {
        let f = own(ty);
        if f != NIL {
            return f;
        }
        ty = type_super(ty);
    }
    for ty in [object, global(G::DefaultType)] {
        if ty != NIL {
            let f = own(ty);
            if f != NIL {
                return f;
            }
        }
    }
    NIL
}

/// The supertype of type `ty`, or NIL.
pub fn type_super(ty: LispObj) -> LispObj {
    ty.uref(TYPE_SUPER)
}

/// True if type `t` is `ty` or a subtype of it.
pub fn is_subtype(mut t: LispObj, ty: LispObj) -> bool {
    while t != NIL {
        if t == ty {
            return true;
        }
        t = type_super(t);
    }
    false
}

/// Install `f` as protocol method `id` of type `ty`.
pub fn extend_type(ty: LispObj, id: usize, f: LispObj) {
    let m = ty.uref(TYPE_METHODS);
    let len = if m == NIL { 0 } else { m.vector_length().integer() as usize };
    if id >= len {
        let n = (id + 1).max(len * 2).max(8);
        let v = vector_node(n);
        let m = ty.uref(TYPE_METHODS);
        for i in 0..len {
            v.svset(i, m.svref(i));
        }
        ty.uset(TYPE_METHODS, v);
    }
    ty.uref(TYPE_METHODS).svset(id, f);
}

/// A new protocol method id, named `name` (for error messages).
pub fn new_method_id(name: LispObj) -> usize {
    let id = global(G::NextMethodId).integer() as usize;
    set_global(G::NextMethodId, LispObj::fixnum(id as i64 + 1));
    let names = global(G::MethodNames);
    let len = names.vector_length().integer() as usize;
    if id >= len {
        let v = vector_node(len * 2);
        let names = global(G::MethodNames);
        for i in 0..len {
            v.svset(i, names.svref(i));
        }
        set_global(G::MethodNames, v);
    }
    global(G::MethodNames).svset(id, name);
    id
}

/// Method named `name` (a string) callable as `(.name x)` on instances
/// of `ty` (its own or a supertype's), or NIL.
pub fn find_obj_method(mut ty: LispObj, name: LispObj) -> LispObj {
    while ty != NIL {
        let t = ty.uref(TYPE_OBJ_METHODS);
        if t != NIL
            && let Some(f) = strtab_get(t, name)
        {
            return f;
        }
        ty = type_super(ty);
    }
    NIL
}

pub fn add_obj_method(ty: LispObj, name: LispObj, f: LispObj) {
    if ty.uref(TYPE_OBJ_METHODS) == NIL {
        ty.uset(TYPE_OBJ_METHODS, strtab_new());
    }
    strtab_put(ty.uref(TYPE_OBJ_METHODS), name, f);
    bump_method_epoch();
}

/// Invalidate the method call-site caches.
pub fn bump_method_epoch() {
    set_global(G::MethodEpoch, LispObj::fixnum(global(G::MethodEpoch).integer() + 1));
}

/// Index (from 0) of field `name` (a string) of instance type `ty`.
pub fn field_index(ty: LispObj, name: LispObj) -> Option<usize> {
    let f = ty.uref(TYPE_FIELDS);
    (0..field_count(ty)).find(|&i| crate::object::lisp_strings_equal(f.svref(i), name))
}

pub fn type_name(ty: LispObj) -> LispObj {
    ty.uref(TYPE_NAME)
}

/// Create the kernel types. Called from `init_lisp`.
pub(crate) fn init() {
    let g = vector_node(NUM_GLOBALS);
    set_qv(CLJ_GLOBALS_Index, g);
    // The root type is its own type.
    let root = heap::alloc_uvector(TYPE_SIZE, CLJ_INSTANCE_TYPE);
    root.uset(INSTANCE_TYPE, root);
    root.uset(TYPE_NAME, string_node("Type"));
    root.uset(TYPE_FIELDS, vector_node(0));
    root.uset(TYPE_METHODS, NIL);
    root.uset(TYPE_OBJ_METHODS, NIL);
    root.uset(TYPE_STATICS, NIL);
    root.uset(TYPE_SUPER, NIL);
    set_global(G::TypeType, root);
    set_global(G::NextMethodId, LispObj::fixnum(0));
    set_global(G::MethodEpoch, LispObj::fixnum(0));
    set_global(G::MethodNames, vector_node(64));
    for (g, name) in [
        (G::NilType, "nil"),
        (G::BooleanType, "corten.lang/Boolean"),
        (G::NumberType, "corten.lang/Number"),
        (G::StringType, "corten.lang/String"),
        (G::CharType, "corten.lang/Character"),
        (G::ArrayType, "corten.lang/Array"),
        (G::FunctionType, "corten.lang/Function"),
        (G::ObjectType, "object"),
        (G::DefaultType, "default"),
    ] {
        let t = make_type(name, &[]);
        set_global(g, t);
    }
    for (g, name) in [
        (G::LongType, "corten.lang/Long"),
        (G::BigIntType, "corten.lang/BigInt"),
        (G::RatioType, "corten.lang/Ratio"),
        (G::DoubleType, "corten.lang/Double"),
    ] {
        let t = make_type(name, &[]);
        t.uset(TYPE_SUPER, global(G::NumberType));
        set_global(g, t);
    }
    let t = make_type("Symbol", &["ns", "name", "str", "_hash", "_meta"]);
    set_global(G::SymbolType, t);
    let t = make_type("Keyword", &["ns", "name", "fqn", "_hash"]);
    set_global(G::KeywordType, t);
    coll::init();


    let t = make_type("Protocol", &["name", "marker", "methods"]);
    set_global(G::ProtocolType, t);

    // The host's error types (corten.lang extends them): Error, its
    // subtypes, and the ErrorKind of each.
    let types = vector_node(crate::error::ERROR_TYPES.len());
    for (i, &(name, sup)) in crate::error::ERROR_TYPES.iter().enumerate() {
        let fields: &[&str] = if name == "ArityError" { &["message", "cause", "actual", "name"] } else { &["message", "cause"] };
        let t = make_type(&format!("corten.lang/{name}"), fields);
        if let Some(s) = sup {
            t.uset(TYPE_SUPER, types.svref(s));
        }
        types.svset(i, t);
    }
    set_global(G::ErrorTypes, types);

    let t = make_type("Var", &VAR_FIELD_NAMES);
    set_global(G::VarType, t);
    let t = make_type("Namespace", &NS_FIELD_NAMES);
    set_global(G::NamespaceType, t);
    let t = make_type("corten.kernel/Stream", &crate::streams::STREAM_FIELD_NAMES);
    set_global(G::StreamType, t);
    crate::streams::init();

    set_global(G::LoadedLibs, strtab_new());
    set_global(G::Namespaces, strtab_new());
    native::install();

    set_global(G::Keywords, strtab_new());
    let core = create_ns("clojure.core");
    set_global(G::CoreNs, core);
    // The kernel functions, as vars of `corten.kernel` (aliased `k` in
    // clojure.core).
    let kns = create_ns("corten.kernel");
    set_global(G::KernelNs, kns);
    for (i, p) in crate::prims::PRIMS.iter().enumerate() {
        let v = intern_var(kns, string_node(p.name));
        set_var_value(v, crate::prims::kernel_function_node(i));
    }
    strtab_put(global(G::LoadedLibs), string_node("corten.kernel"), TRUE);
    add_alias(core, string_node("k"), kns);
    let ns_var = intern_var(core, string_node("*ns*"));
    set_var_flag(ns_var, DYNAMIC_FLAG, true);
    set_var_value(ns_var, core);
    set_global(G::NsVar, ns_var);
    let tag_var = intern_var(core, string_node("*read-tag-fn*"));
    set_var_flag(tag_var, DYNAMIC_FLAG, true);
    set_var_value(tag_var, NIL);
    set_global(G::ReadTagVar, tag_var);
}

// ---------------------------------------------------------------------------
// Symbols and keywords

/// A Clojure symbol. `ns` is a string or NIL.
pub fn symbol(ns: LispObj, name: LispObj) -> LispObj {
    make_instance(global(G::SymbolType), &[ns, name, full_name(ns, name), NIL, NIL])
}

/// `ns/name`, or `name` when `ns` is NIL.
fn full_name(ns: LispObj, name: LispObj) -> LispObj {
    if ns == NIL {
        name
    } else {
        string_node(&format!("{}/{}", ns.string_to_rust(), name.string_to_rust()))
    }
}

pub fn symbol_from(ns: Option<&str>, name: &str) -> LispObj {
    let n = string_node(name);
    let ns = ns.map_or(NIL, string_node);
    symbol(ns, n)
}

pub fn sym_ns(s: LispObj) -> LispObj {
    s.uref(SYM_NS)
}
pub fn sym_name(s: LispObj) -> LispObj {
    s.uref(SYM_NAME)
}
pub fn sym_meta(s: LispObj) -> LispObj {
    s.uref(SYM_META)
}

/// Symbol `s` with metadata `meta` (a copy).
pub fn sym_with_meta(s: LispObj, meta: LispObj) -> LispObj {
    make_instance(global(G::SymbolType), &[sym_ns(s), sym_name(s), s.uref(SYM_STR), s.uref(SYM_HASH), meta])
}

fn str_eq(a: LispObj, b: LispObj) -> bool {
    if a == NIL || b == NIL {
        return a == b;
    }
    crate::object::lisp_strings_equal(a, b)
}

/// Rust-side equality of two symbols (by namespace and name).
pub fn sym_eq(a: LispObj, b: LispObj) -> bool {
    str_eq(sym_ns(a), sym_ns(b)) && str_eq(sym_name(a), sym_name(b))
}

/// True if `x` is a symbol without namespace named `name`.
pub fn is_sym_named(x: LispObj, name: &str) -> bool {
    is_symbol(x) && sym_ns(x) == NIL && lisp_str_is(sym_name(x), name)
}

/// Compare a Lisp string with a Rust string.
pub fn lisp_str_is(s: LispObj, name: &str) -> bool {
    let len = s.vector_length().integer() as usize;
    if name.is_ascii() && len != name.len() {
        return false;
    }
    let units = unsafe { std::slice::from_raw_parts(s.char_array_start(), len) };
    units.iter().copied().eq(name.encode_utf16())
}

/// The interned keyword `:ns/name` (`ns` a string or NIL).
pub fn keyword(ns: LispObj, name: LispObj) -> LispObj {
    let full = full_name(ns, name);
    if let Some(kw) = strtab_get(global(G::Keywords), full) {
        return kw;
    }
    let kw = make_instance(global(G::KeywordType), &[ns, name, full, NIL]);
    strtab_put(global(G::Keywords), full, kw);
    kw
}

pub fn keyword_from(ns: Option<&str>, name: &str) -> LispObj {
    let n = string_node(name);
    let ns = ns.map_or(NIL, string_node);
    keyword(ns, n)
}

// ---------------------------------------------------------------------------
// Namespaces and vars

/// Resolve global symbol `sym` in the current namespace: its var.
pub fn resolve(sym: LispObj) -> Option<LispObj> {
    let ns = sym_ns(sym);
    let name = sym_name(sym);
    let cur = current_ns();
    if ns == NIL {
        if let Some(v) = ns_lookup(cur, name) {
            return Some(v);
        }
        // `a.b.C` is `C` in namespace `a.b`, as in ClojureScript.
        let n = name.string_to_rust();
        if let Some(i) = n.rfind('.')
            && i > 0
            && i + 1 < n.len()
            && let Some(target) = resolve_ns(cur, string_node(&n[..i]))
        {
            let name = string_node(&n[i + 1..]);
            return Some(ns_lookup(target, name).unwrap_or_else(|| intern_var(target, name)));
        }
        // A private var of clojure.core (not referred): ClojureScript
        // resolves it too (core_test, clojure.core.reducers).
        let core = global(G::CoreNs);
        if cur != core
            && let Some(v) = ns_lookup(core, name)
            && prims::is_private(v)
        {
            return Some(v);
        }
        // The host's types (corten.lang), available in every namespace.
        if let Some(lang) = find_ns("corten.lang")
            && let Some(v) = ns_lookup(lang, name)
            && is_bound(v)
        {
            return Some(v);
        }
        return None;
    }
    let target = resolve_ns(cur, ns)?;
    if let Some(v) = ns_lookup(target, name) {
        return Some(v);
    }
    // `a$macros/f`: a function of a .cljc file defining macros of `a`
    // is in `a` (only the macros are in `a$macros`).
    if let Some(v) = ns_name(target)
        .string_to_rust()
        .strip_suffix("$macros")
        .and_then(find_ns)
        .and_then(|base| ns_lookup(base, name))
    {
        return Some(v);
    }
    // The kernel functions are fixed.
    if target == global(G::KernelNs) {
        return None;
    }
    // PORT: like ClojureScript, a qualified reference to a var not
    // defined yet is allowed (it is unbound until defined).
    Some(intern_var(target, name))
}

/// Like `resolve`, but a macro is found too (`#'when`, `resolve`): an
/// existing var first, then a macro, then what `resolve` gives.
pub fn resolve_var(sym: LispObj) -> Option<LispObj> {
    let ns = sym_ns(sym);
    let name = sym_name(sym);
    if ns == NIL {
        if let Some(v) = ns_lookup(current_ns(), name) {
            return Some(v);
        }
    } else if let Some(v) = resolve_ns(current_ns(), ns).and_then(|t| ns_lookup(t, name))
    {
        return Some(v);
    }
    if let Some(v) = resolve_macro(sym) {
        return Some(v);
    }
    resolve(sym)
}

/// The namespace holding the macros of `ns` (`<ns>$macros`, as in
/// self-hosted ClojureScript); a macro namespace is its own.
pub fn macro_ns_of(ns: LispObj, create: bool) -> Option<LispObj> {
    let name = ns_name(ns).string_to_rust();
    if name.ends_with("$macros") {
        return Some(ns);
    }
    let mname = format!("{name}$macros");
    if create { Some(find_or_create_ns(&mname)) } else { find_ns(&mname) }
}

pub fn is_macro_var(v: LispObj) -> bool {
    var_flag(v, MACRO_FLAG)
}

/// The macro named by symbol `s` in the current namespace, if any:
/// unqualified names are looked up in the current namespace's macros,
/// then in `clojure.core`'s.
pub fn resolve_macro(s: LispObj) -> Option<LispObj> {
    let name = sym_name(s);
    let cur = current_ns();
    let found = |ns: Option<LispObj>| {
        ns.and_then(|ns| ns_lookup(ns, name)).filter(|&v| is_macro_var(v))
    };
    let ns = sym_ns(s);
    if ns == NIL {
        // A macro referred into the current namespace.
        let here = ns_lookup(cur, name);
        if let Some(v) = here.filter(|&v| is_macro_var(v)) {
            return Some(v);
        }
        if let Some(m) = macro_ns_of(cur, false)
            && let Some(v) = found(Some(m))
        {
            // A macro referred into the macros namespace does not hide
            // a var defined here (as a def replaces a referral).
            let own_here = here.is_some_and(|h| var_ns(h) == cur);
            if var_ns(v) == m || !own_here {
                return Some(v);
            }
        }
        // clojure.core's macros, unless the name is another var here or
        // is excluded (:refer-clojure :exclude): a core fn not referred.
        let core = global(G::CoreNs);
        let shadowed = match here {
            Some(v) => var_ns(v) != core,
            None => ns_lookup(core, name).is_some() && cur != core,
        };
        return if shadowed { None } else { found(macro_ns_of(core, false)) };
    }
    let target = resolve_ns(cur, ns)?;
    found(macro_ns_of(target, false))
}

/// Name of the namespace syntax-quote uses for vars of `ns`: macro
/// namespaces and the bootstrap library stand for `clojure.core` / the
/// base namespace.
pub fn public_ns_name(ns: LispObj) -> LispObj {
    let name = ns_name(ns).string_to_rust();
    if name == "clj.boot" {
        return string_node("clojure.core");
    }
    match name.strip_suffix("$macros") {
        Some(base) => string_node(base),
        None => ns_name(ns),
    }
}

