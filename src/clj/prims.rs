//! Kernel primitives for Clojure (called from compiled Clojure code and
//! from the CL host layer).

use super::*;
use crate::compiler::frontend::clj as fe;

fn check_string(x: LispObj) -> LispObj {
    if !x.is_string() {
        crate::error::type_error(format!("Not a string: {}", printer::pr_str(x)));
    }
    x
}

fn opt_string(x: LispObj) -> LispObj {
    if x == NIL { x } else { check_string(x) }
}

/// `(k/def var value)`: set the root value; returns the var.
pub fn def(a: &[LispObj]) -> LispObj {
    set_root(check_var(a[0]), a[1]);
    a[0]
}

/// Set the root of `v` to `x` and call its watches.
fn set_root(v: LispObj, x: LispObj) {
    let old = var_root(v);
    set_var_root(v, x);
    let hook = global(G::VarWatchHook);
    if v.uref(VAR_WATCHES) != NIL && hook != NIL {
        crate::runtime::lisp_call(hook, &[v, old, x]);
    }
}

/// `(k/var-root var)`: the value of `var` outside any `binding`.
pub fn var_root_prim(a: &[LispObj]) -> LispObj {
    let v = var_root(check_var(a[0]));
    if v == SYMBOL_UNBOUND {
        lisp_error(format!("Var {} is unbound", printer::pr_str(a[0])));
    }
    v
}

/// `(k/var-set-root var value)`: set the root value, calling the
/// watches; returns `value`.
pub fn var_set_root(a: &[LispObj]) -> LispObj {
    set_root(check_var(a[0]), a[1]);
    a[1]
}

/// `(k/var-unbind-root var)`: make the root of `var` unbound.
pub fn var_unbind_root(a: &[LispObj]) -> LispObj {
    set_var_root(check_var(a[0]), SYMBOL_UNBOUND);
    a[0]
}

/// `(k/thread-bindings)`: a list of `[var value]` for the vars
/// rebound by `binding`.
pub fn thread_bindings_prim(_a: &[LispObj]) -> LispObj {
    coll::pairs_list(thread_bindings())
}

/// `(k/var-thread-bound? var)`.
pub fn var_thread_bound_p(a: &[LispObj]) -> LispObj {
    cbool(is_thread_bound(check_var(a[0])))
}

/// `(k/var-dynamic var &optional on)`: is `var` dynamic;
/// with `on`, make it dynamic (or not) first.
pub fn var_dynamic(a: &[LispObj]) -> LispObj {
    let v = check_var(a[0]);
    if let Some(&on) = a.get(1) {
        set_var_flag(v, DYNAMIC_FLAG, on != NIL);
    }
    cbool(var_flag(v, DYNAMIC_FLAG))
}

/// `(k/var-set-meta var m)`.
pub fn var_set_meta(a: &[LispObj]) -> LispObj {
    set_var_meta(check_var(a[0]), a[1]);
    set_var_flag(a[0], DEF_META_FLAG, false);
    a[1]
}

/// `(k/ns-unmap ns name)`: remove the mapping of `name` in `ns`.
pub fn ns_unmap(a: &[LispObj]) -> LispObj {
    strtab_remove(ns_mappings(check_ns(a[0])), name_of(a[1]));
    NIL
}

/// `(k/ns-unalias ns name)`: remove the alias `name` in `ns`.
pub fn ns_unalias(a: &[LispObj]) -> LispObj {
    strtab_remove(ns_aliases(check_ns(a[0])), name_of(a[1]));
    NIL
}

/// `(k/ns-aliases ns)`: a list of `[alias namespace]`.
pub fn ns_aliases_prim(a: &[LispObj]) -> LispObj {
    coll::pairs_list(strtab_entries(ns_aliases(check_ns(a[0]))))
}

/// `(k/remove-ns ns)`: remove `ns` from the namespace list.
pub fn remove_ns_prim(a: &[LispObj]) -> LispObj {
    remove_ns(check_ns(a[0]));
    a[0]
}

fn check_var(x: LispObj) -> LispObj {
    if !is_var(x) {
        crate::error::type_error(format!("Not a var: {}", printer::pr_str(x)));
    }
    x
}

fn check_ns(x: LispObj) -> LispObj {
    if !is_ns(x) {
        crate::error::type_error(format!("Not a namespace: {}", printer::pr_str(x)));
    }
    x
}

/// `(k/arity-error nargs name)`.
pub fn arity_error(a: &[LispObj]) -> LispObj {
    let name = if a[1].is_string() { a[1].string_to_rust() } else { "fn".to_string() };
    crate::error::arity_error(a[0].integer() as usize, &name)
}

/// `(k/eval form)`.
pub fn eval(a: &[LispObj]) -> LispObj {
    crate::compiler::clj_eval(a[0])
}

/// `(k/load path)`.
pub fn load(a: &[LispObj]) -> LispObj {
    crate::compiler::clj_load(a[0])
}

fn name_of(x: LispObj) -> LispObj {
    if is_symbol(x) { sym_name(x) } else { check_string(x) }
}

/// `(k/in-ns name &optional (refer t))`: switch to namespace `name`
/// (symbol or string), creating it with `clojure.core` referred unless
/// `refer` is NIL.
pub fn in_ns(a: &[LispObj]) -> LispObj {
    let name = name_of(a[0]).string_to_rust();
    let ns = match find_ns(&name) {
        Some(ns) => ns,
        None => {
            let ns = create_ns(&name);
            if a.len() < 2 || a[1] != NIL {
                refer_all(ns, global(G::CoreNs), NIL);
            }
            ns
        }
    };
    set_var_value(global(G::NsVar), ns);
    ns
}

/// Refer every public var of `from` in `ns`, except those named in
/// `exclude` (a seq of strings).
pub fn refer_all(ns: LispObj, from: LispObj, exclude: LispObj) {
    let exclude = heap::vector_of(&coll::items(exclude));
    let es = strtab_entries(ns_mappings(from));
    for i in (0..es.vector_length().integer() as usize).step_by(2) {
        let (key, v) = (es.svref(i), es.svref(i + 1));
        if is_var(v) && var_ns(v) == from && !is_private(v) {
            let excluded = (0..exclude.vector_length().integer() as usize)
                .any(|j| crate::object::lisp_strings_equal(exclude.svref(j), key));
            // A name interned in `ns` is not replaced.
            let own = ns_lookup(ns, key).is_some_and(|o| var_ns(o) == ns);
            if !excluded && !own {
                refer_var(ns, v);
            }
        }
    }
}

pub fn is_private(v: LispObj) -> bool {
    let m = var_meta(v);
    m != NIL && truthy(coll::get(m, keyword_from(None, "private"), NIL))
}

/// `(k/refer ns from &optional exclude)`: refer the public vars of
/// namespace `from` except those named in `exclude` (strings), and its
/// macros into `ns`'s macros.
pub fn refer(a: &[LispObj]) -> LispObj {
    if !is_ns(a[1]) {
        lisp_error("refer: no such namespace");
    }
    let exclude = a.get(2).copied().unwrap_or(NIL);
    refer_all(a[0], a[1], exclude);
    if let Some(m) = macro_ns_of(a[1], false).filter(|&m| m != a[1]) {
        refer_all(macro_ns_of(a[0], true).unwrap(), m, exclude);
    }
    NIL
}

/// `(k/add-load-path dir)`: a relative `dir` is made absolute.
pub fn add_load_path(a: &[LispObj]) -> LispObj {
    let d = check_string(a[0]).string_to_rust();
    let d = std::fs::canonicalize(&d).map_or(d, |p| p.to_string_lossy().into_owned());
    let d = string_node(&d);
    let old = global(G::LoadPath);
    let n = if old == NIL { 0 } else { old.vector_length().integer() as usize };
    let v = heap::vector_node(n + 1);
    for i in 0..n {
        v.svset(i, old.svref(i));
    }
    v.svset(n, d);
    set_global(G::LoadPath, v);
    NIL
}

/// `(k/set-reader-features k...)`: the reader conditional features,
/// keywords (default `:corten :default`).
pub fn set_reader_features(a: &[LispObj]) -> LispObj {
    let names = a
        .iter()
        .map(|&k| {
            if !is_keyword(k) {
                lisp_error("set-reader-features: not a keyword");
            }
            k.uref(KW_NAME).string_to_rust()
        })
        .collect();
    super::reader::set_features(names);
    NIL
}

/// File extensions tried by `require`: as in ClojureScript, a `.cljs`
/// file comes before a `.clj` one (which holds macros).
const REQUIRE_EXTS: [&str; 3] = ["cljs", "cljc", "clj"];
/// File extensions tried by `require-macros`.
const MACRO_EXTS: [&str; 3] = ["clj", "cljc", "cljs"];

/// `(k/require name &optional reload)`: load lib `name` from the load
/// path (`a.b-c` is `a/b_c.cljs`, `.cljc` or `.clj`) unless it is loaded
/// (or `reload`), and return its namespace (nil if loading did not create
/// it). A namespace that exists without a file counts as loaded. As in
/// ClojureScript, `clojure.x` falls back to `cljs.x`.
pub fn require(a: &[LispObj]) -> LispObj {
    require_lib(&name_of(a[0]).string_to_rust(), a.get(1).is_some_and(|&r| truthy(r)), &REQUIRE_EXTS)
}

/// `(k/require-macros name &optional reload)`: `(:require-macros
/// name)`. As `k/require`, except that for the current namespace (a
/// .cljs file requiring its own macros, as cljs allows) the `.clj` or
/// `.cljc` file of the namespace is loaded, if there is one and it is not
/// loading.
pub fn require_macros(a: &[LispObj]) -> LispObj {
    let name = name_of(a[0]).string_to_rust();
    let reload = a.get(1).is_some_and(|&r| truthy(r));
    let cur = current_ns();
    if find_ns(&name) != Some(cur) {
        return require_lib(&name, reload, &MACRO_EXTS);
    }
    if let Some(f) = lib_file(&name, &["clj", "cljc"]) {
        // A .cljc file requiring its own macros is already loading.
        if !crate::compiler::clj_loading(&f) {
            crate::compiler::clj_load(string_node(&f));
        }
    }
    cur
}

/// Is lib `name` loaded?
fn lib_loaded(name: &str) -> bool {
    strtab_get(global(G::LoadedLibs), string_node(name)).is_some()
}

fn add_loaded_lib(name: &str) {
    strtab_put(global(G::LoadedLibs), string_node(name), TRUE);
}

/// `(k/add-loaded-lib name)`: record lib `name` as loaded.
pub fn add_loaded_lib_prim(a: &[LispObj]) -> LispObj {
    add_loaded_lib(&name_of(a[0]).string_to_rust());
    NIL
}

/// `(k/loaded-libs)`: a list of the names of the loaded libs.
pub fn loaded_libs(_a: &[LispObj]) -> LispObj {
    coll::pairs_column(strtab_entries(global(G::LoadedLibs)), 0)
}

/// The first file `<dir>/<rel>.<ext>` on the load path.
fn find_on_load_path(rel: &str, exts: &[&str]) -> Option<String> {
    let p = global(G::LoadPath);
    let n = if p == NIL { 0 } else { p.vector_length().integer() as usize };
    for i in 0..n {
        let dir = p.svref(i).string_to_rust();
        for ext in exts {
            let f = format!("{dir}/{rel}.{ext}");
            if std::fs::metadata(&f).is_ok() {
                return Some(f);
            }
        }
    }
    None
}

/// The file of lib `name` on the load path.
fn lib_file(name: &str, exts: &[&str]) -> Option<String> {
    find_on_load_path(&name.replace('.', "/").replace('-', "_"), exts)
}

/// Load file `f` (cyclic loads are an error).
fn load_file_once(f: &str) {
    if crate::compiler::clj_loading(f) {
        lisp_error(format!("Cyclic load dependency: {f} is being loaded"));
    }
    crate::compiler::clj_load(string_node(f));
}

fn require_lib(name: &str, reload: bool, exts: &[&str]) -> LispObj {
    // The ported cljs.core is clojure.core (see resolve_ns).
    let name = if name == "cljs.core" { "clojure.core" } else { name };
    let alt = name.strip_prefix("clojure.").map(|rest| format!("cljs.{rest}"));
    for n in std::iter::once(name).chain(alt.as_deref()) {
        if !reload && lib_loaded(n) {
            match find_ns(n) {
                Some(ns) => return ns,
                // Loaded as its `cljs.x` fallback: found under that name.
                None if n == name && alt.is_some() => continue,
                // A file defining other namespaces (cljs/host).
                None => return NIL,
            }
        }
        if let Some(f) = lib_file(n, exts) {
            load_file_once(&f);
            add_loaded_lib(n);
            add_loaded_lib(name);
            // nil if the file defines other namespaces (cljs/host).
            return find_ns(n).unwrap_or(NIL);
        }
        if let Some(ns) = find_ns(n) {
            add_loaded_lib(name);
            return ns;
        }
    }
    let rel = name.replace('.', "/").replace('-', "_");
    lisp_error(format!("Could not locate {rel}.clj, .cljc or .cljs on the load path"))
}

/// `(k/load-resource path)`: load `<dir>/<path>.clj` (or .cljc,
/// .cljs) from the load path.
pub fn load_resource(a: &[LispObj]) -> LispObj {
    let rel = check_string(a[0]).string_to_rust();
    match find_on_load_path(&rel, &["clj", "cljc", "cljs"]) {
        Some(f) => crate::compiler::clj_load(string_node(&f)),
        None => lisp_error(format!("Could not locate {rel}.clj, .cljc or .cljs on the load path")),
    }
}

/// `(k/utc-part ms i)`: part `i` (0 second, 1 minute, 2 hour, 3 day,
/// 4 month, 5 year) of the UTC time `ms` (milliseconds since the Unix
/// epoch).
pub fn utc_part(a: &[LispObj]) -> LispObj {
    let ms = if a[0].is_double_float() { a[0].double_float().floor() as i64 } else { a[0].integer() };
    let secs = ms.div_euclid(1000);
    let days = secs.div_euclid(86400);
    let sod = secs.rem_euclid(86400);
    // Civil from days (Howard Hinnant's algorithm).
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = yoe + era * 400 + i64::from(month <= 2);
    let v = match a[1].integer() {
        0 => sod % 60,
        1 => sod / 60 % 60,
        2 => sod / 3600,
        3 => day,
        4 => month,
        5 => year,
        _ => lisp_error("utc-part: bad part"),
    };
    LispObj::fixnum(v)
}

/// `(k/identical? a b)`: the slow path of `identical?` for values
/// that are not EQ. As in JavaScript (`===`), equal strings and equal
/// double floats are identical.
pub fn identical_p(a: &[LispObj]) -> LispObj {
    let (x, y) = (a[0], a[1]);
    let same = x == y
        || (x.is_string() && y.is_string() && crate::object::lisp_strings_equal(x, y))
        || (x.is_double_float() && y.is_double_float() && x.double_float() == y.double_float());
    if same { TRUE } else { FALSE }
}

/// `(k/refer-name ns from name &optional local)`: refer the var
/// `name` of namespace `from` into `ns` and its macro (if any) into
/// `ns`'s macros, as `:refer` and `:refer-macros` do; under the name
/// `local` if given (`:rename`).
pub fn refer_name(a: &[LispObj]) -> LispObj {
    if !is_ns(a[1]) {
        lisp_error("refer: no such namespace");
    }
    let name = name_of(a[2]);
    let local = a.get(3).map_or(name, |&n| name_of(n));
    // A namespace referring its own names (a .cljc file requiring its
    // own macros, which it has not defined yet).
    if a[0] == a[1] && a.get(3).is_none() {
        return NIL;
    }
    let mut found = false;
    if let Some(v) = ns_lookup(a[1], name) {
        refer_var_as(a[0], v, local);
        found = true;
    }
    if let Some(v) = macro_ns_of(a[1], false).and_then(|m| ns_lookup(m, name)) {
        refer_var_as(macro_ns_of(a[0], true).unwrap(), v, local);
        found = true;
    }
    if !found {
        lisp_error(format!("refer: {} does not exist", name.string_to_rust()));
    }
    NIL
}

/// `(k/refer-var ns var)`.
pub fn refer_var_prim(a: &[LispObj]) -> LispObj {
    refer_var(a[0], a[1]);
    NIL
}

/// `(k/alias ns alias target)`.
pub fn alias(a: &[LispObj]) -> LispObj {
    add_alias(check_ns(a[0]), name_of(a[1]), check_ns(a[2]));
    NIL
}

/// `(k/find-ns name)`: namespace or NIL.
pub fn find_ns_prim(a: &[LispObj]) -> LispObj {
    find_ns(&name_of(a[0]).string_to_rust()).unwrap_or(NIL)
}

/// `(k/resolve-ns name)`: the namespace that `name` (a string or
/// symbol, an alias or a namespace name) names in the current one, or NIL.
pub fn resolve_ns_prim(a: &[LispObj]) -> LispObj {
    resolve_ns(current_ns(), name_of(a[0])).unwrap_or(NIL)
}

/// `(k/all-ns)`: a list of the namespaces.
pub fn all_ns(_a: &[LispObj]) -> LispObj {
    coll::pairs_column(strtab_entries(global(G::Namespaces)), 1)
}

/// `(k/create-ns name)`.
pub fn create_ns_prim(a: &[LispObj]) -> LispObj {
    find_or_create_ns(&name_of(a[0]).string_to_rust())
}

/// `(k/intern ns name)`: the var `name` in `ns`, created if needed.
pub fn intern(a: &[LispObj]) -> LispObj {
    intern_var(a[0], name_of(a[1]))
}

/// `(resolve sym)`: the var `sym` names, or NIL.
pub fn resolve_prim(a: &[LispObj]) -> LispObj {
    resolve_var(a[0]).unwrap_or(NIL)
}

/// `(k/macroexpand-1 form)`.
pub fn macroexpand_1(a: &[LispObj]) -> LispObj {
    fe::macroexpand_1(a[0]).unwrap_or(a[0])
}

/// `(k/set-macro var flag)`.
pub fn set_macro(a: &[LispObj]) -> LispObj {
    fe::set_macro_var(a[0], truthy(a[1]));
    a[0]
}

/// `(k/symbol ns name)`.
pub fn symbol_prim(a: &[LispObj]) -> LispObj {
    if a.len() == 1 {
        return symbol(NIL, check_string(a[0]));
    }
    symbol(opt_string(a[0]), check_string(a[1]))
}

/// `(k/symbol-with-meta sym meta)`.
pub fn symbol_with_meta(a: &[LispObj]) -> LispObj {
    if !is_symbol(a[0]) {
        lisp_error(format!("Not a symbol: {}", printer::pr_str(a[0])));
    }
    sym_with_meta(a[0], a[1])
}

/// `(k/keyword ns name)`.
pub fn keyword_prim(a: &[LispObj]) -> LispObj {
    if a.len() == 1 {
        return keyword(NIL, check_string(a[0]));
    }
    keyword(opt_string(a[0]), check_string(a[1]))
}

/// `(k/set-global index value)`: set a kernel hook (`G`).
pub fn set_global_prim(a: &[LispObj]) -> LispObj {
    let i = a[0].integer() as usize;
    let g = qv(CLJ_GLOBALS_Index);
    if i >= g.vector_length().integer() as usize {
        lisp_error("Bad Clojure global index");
    }
    g.svset(i, a[1]);
    a[1]
}

/// `(k/global index)`.
pub fn global_prim(a: &[LispObj]) -> LispObj {
    let g = qv(CLJ_GLOBALS_Index);
    let i = a[0].integer() as usize;
    if i >= g.vector_length().integer() as usize {
        lisp_error("Bad Clojure global index");
    }
    g.svref(i)
}

/// `(k/instance-type x)`: type descriptor of a Clojure instance, or NIL.
pub fn instance_type_prim(a: &[LispObj]) -> LispObj {
    if is_instance(a[0]) { instance_type(a[0]) } else { NIL }
}

/// `(k/make-instance type field...)`.
pub fn make_instance_prim(a: &[LispObj]) -> LispObj {
    make_instance(a[0], &a[1..])
}

/// `(k/js-fn? x)`: is `x` a function (not an invocable instance)?
pub fn js_fn_p(a: &[LispObj]) -> LispObj {
    let x = a[0];
    lb(x.is_function())
}

/// `(k/type-name type)`: the name string of a type descriptor.
pub fn type_name_prim(a: &[LispObj]) -> LispObj {
    type_name(check_type(a[0]))
}

/// `(k/make-type name fields)`.
pub fn make_type_prim(a: &[LispObj]) -> LispObj {
    let name = check_string(a[0]);
    let fields = crate::runtime::without_gc(|| {
        let items = coll::items(a[1]);
        let v = heap::vector_node(items.len());
        for (i, &f) in items.iter().enumerate() {
            v.svset(i, check_string(f));
        }
        v
    });
    make_type_str(name, fields)
}

fn check_instance_index(x: LispObj, i: LispObj) -> usize {
    if !is_instance(x) {
        lisp_error(format!("Not an instance: {}", printer::pr_str(x)));
    }
    let n = x.uvector_size() * 2 - 1 - INSTANCE_FIELDS;
    let i = i.integer();
    if i < 0 || i as usize > n {
        lisp_error("Instance field index out of range");
    }
    INSTANCE_FIELDS + i as usize
}

/// `(k/field x i)`.
pub fn field(a: &[LispObj]) -> LispObj {
    let i = check_instance_index(a[0], a[1]);
    a[0].uref(i)
}

/// `(k/specify x)`: give instance `x` a type of its own, a copy of
/// its type, so methods can be added to `x` alone (cljs `specify!`).
/// Returns the new type.
pub fn specify(a: &[LispObj]) -> LispObj {
    if !is_instance(a[0]) || is_type(a[0]) {
        lisp_error(format!("specify!: not an instance: {}", printer::pr_str(a[0])));
    }
    let x = a[0];
    crate::runtime::without_gc(|| {
        let old = instance_type(x);
        let ty = make_type_str(old.uref(TYPE_NAME), old.uref(TYPE_FIELDS));
        let m = old.uref(TYPE_METHODS);
        if m != NIL {
            let len = m.vector_length().integer() as usize;
            let v = heap::vector_node(len);
            for i in 0..len {
                v.svset(i, m.svref(i));
            }
            ty.uset(TYPE_METHODS, v);
        }
        for i in [TYPE_OBJ_METHODS, TYPE_STATICS] {
            if old.uref(i) != NIL {
                ty.uset(i, strtab_copy(old.uref(i)));
            }
        }
        ty.uset(TYPE_SUPER, old.uref(TYPE_SUPER));
        x.uset(INSTANCE_TYPE, ty);
        ty
    })
}

/// `(k/with-field x i v)`: a copy of instance `x` with field `i` set
/// to `v`.
pub fn with_field(a: &[LispObj]) -> LispObj {
    let i = check_instance_index(a[0], a[1]) - INSTANCE_FIELDS;
    let n = field_count(instance_type(a[0]));
    if i >= n {
        lisp_error("Instance field index out of range");
    }
    // Allocate first: the field values may move during a collection.
    let x = heap::alloc_uvector(n + 1, CLJ_INSTANCE_TYPE);
    for j in INSTANCE_TYPE..INSTANCE_FIELDS + n {
        x.uset(j, a[0].uref(j));
    }
    x.uset(INSTANCE_FIELDS + i, a[2]);
    x
}

/// `(k/set-field x i v)`.
pub fn set_field(a: &[LispObj]) -> LispObj {
    let i = check_instance_index(a[0], a[1]);
    a[0].uset(i, a[2]);
    a[2]
}

/// `(k/boolean x)`.
pub fn boolean_prim(a: &[LispObj]) -> LispObj {
    cbool(truthy(a[0]))
}

/// `(k/not x)`.
pub fn not_prim(a: &[LispObj]) -> LispObj {
    cbool(!truthy(a[0]))
}

fn check_type(x: LispObj) -> LispObj {
    if !(is_instance(x) && instance_type(x) == global(G::TypeType)) {
        lisp_error(format!("Not a type: {}", printer::pr_str(x)));
    }
    x
}

fn method_id(x: LispObj) -> usize {
    if !x.is_fixnum() || x.integer() < 0 || x.integer() >= global(G::NextMethodId).integer() {
        lisp_error("Bad protocol method id");
    }
    x.integer() as usize
}

/// `(k/type-of x)`: type descriptor of any object.
pub fn type_of_prim(a: &[LispObj]) -> LispObj {
    type_of(a[0])
}

/// `(k/builtin-type name)`: kernel type `nil`, `number`, ... `default`
/// (the cljs base type names), `Long`, `BigInt`, `Ratio`, `Double`.
pub fn builtin_type(a: &[LispObj]) -> LispObj {
    let g = match name_of(a[0]).string_to_rust().as_str() {
        "nil" => G::NilType,
        "boolean" => G::BooleanType,
        "number" => G::NumberType,
        "Long" => G::LongType,
        "BigInt" => G::BigIntType,
        "Ratio" => G::RatioType,
        "Double" => G::DoubleType,
        "string" => G::StringType,
        "char" => G::CharType,
        "array" => G::ArrayType,
        "function" => G::FunctionType,
        "object" => G::ObjectType,
        "default" => G::DefaultType,
        "Symbol" => G::SymbolType,
        "Keyword" => G::KeywordType,
        "Type" => G::TypeType,
        "Var" => G::VarType,
        "Namespace" => G::NamespaceType,
        n => lisp_error(format!("No kernel type {n}")),
    };
    global(g)
}

/// `(k/new-method-id name)`: a fresh protocol method id.
pub fn new_method_id_prim(a: &[LispObj]) -> LispObj {
    LispObj::fixnum(new_method_id(a[0]) as i64)
}

/// `(k/extend type id fn)`.
pub fn extend(a: &[LispObj]) -> LispObj {
    extend_type(check_type(a[0]), method_id(a[1]), a[2]);
    a[0]
}

/// `(k/method id x)`: implementation of protocol method `id` for `x`.
pub fn method(a: &[LispObj]) -> LispObj {
    let id = method_id(a[0]);
    let f = find_method(id, a[1]);
    if f == NIL {
        let name = global(G::MethodNames).svref(id);
        let name = if is_symbol(name) { printer::pr_str(name) } else { printer::print_str(name) };
        let ty = type_name(type_of(a[1])).string_to_rust();
        // Not the object itself: printing it may not terminate.
        crate::error::argument_error(format!("No protocol method {name} defined for type {ty}"));
    }
    f
}

/// `(k/has-method id x)`.
pub fn has_method(a: &[LispObj]) -> LispObj {
    cbool(find_method(method_id(a[0]), a[1]) != NIL)
}

/// `(k/ns-entries ns)`: a list of `[name var]` for the vars
/// interned in or referred to by namespace `ns`.
pub fn ns_entries(a: &[LispObj]) -> LispObj {
    coll::pairs_list(strtab_entries(ns_mappings(check_ns(a[0]))))
}

/// `(k/div a b)`: Clojure `/` (see `numbers::div`).
pub fn div(a: &[LispObj]) -> LispObj {
    crate::numbers::div(a[0], a[1])
}

/// `(k/parse-timestamp s)`: Unix milliseconds of RFC 3339 timestamp
/// `s`, or NaN.
pub fn parse_timestamp_prim(a: &[LispObj]) -> LispObj {
    match crate::clj::reader::parse_timestamp(&check_string(a[0]).string_to_rust()) {
        Some(ms) => LispObj::fixnum(ms),
        None => heap::double_float_node(f64::NAN),
    }
}

/// `(k/parse-float x)`: JavaScript parseFloat: the double of the
/// longest prefix of string `x` (after whitespace) that is a decimal
/// number or Infinity, else NaN. A number is returned as is.
pub fn parse_float(a: &[LispObj]) -> LispObj {
    if a[0].is_fixnum() || a[0].is_double_float() {
        return a[0];
    }
    let f = if a[0].is_string() { js_parse_float(&a[0].string_to_rust()) } else { f64::NAN };
    heap::double_float_node(f)
}

fn js_parse_float(s: &str) -> f64 {
    let s = s.trim_start();
    let b = s.as_bytes();
    let mut i = 0;
    if i < b.len() && (b[i] == b'+' || b[i] == b'-') {
        i += 1;
    }
    if s[i..].starts_with("Infinity") {
        return if b.first() == Some(&b'-') { f64::NEG_INFINITY } else { f64::INFINITY };
    }
    let digits = |i: &mut usize| {
        let start = *i;
        while *i < b.len() && b[*i].is_ascii_digit() {
            *i += 1;
        }
        *i > start
    };
    let mut any = digits(&mut i);
    if i < b.len() && b[i] == b'.' {
        let mut j = i + 1;
        if digits(&mut j) || any {
            any = true;
            i = j;
        }
    }
    if !any {
        return f64::NAN;
    }
    if i < b.len() && (b[i] == b'e' || b[i] == b'E') {
        let mut j = i + 1;
        if j < b.len() && (b[j] == b'+' || b[j] == b'-') {
            j += 1;
        }
        if digits(&mut j) {
            i = j;
        }
    }
    s[..i].parse().unwrap_or(f64::NAN)
}

/// `(k/var-invoke var args...)`: call the value of `var` (calling a
/// var calls its value).
pub fn var_invoke(a: &[LispObj]) -> LispObj {
    let f = var_value(a[0]);
    if f == SYMBOL_UNBOUND {
        crate::error::error_of_kind(crate::error::ErrorKind::State, format!("Attempting to call unbound fn: {}", printer::pr_str(a[0])));
    }
    crate::prims::funcall(f, &a[1..])
}

/// `(k/var-get var)`: the value of `var` (unbound: an error).
pub fn var_get(a: &[LispObj]) -> LispObj {
    let v = var_value(check_var(a[0]));
    if v == SYMBOL_UNBOUND {
        lisp_error(format!("Var {} is unbound", printer::pr_str(a[0])));
    }
    v
}

/// `(k/var-bound? var)`: does `var` have a value?
pub fn var_bound_p(a: &[LispObj]) -> LispObj {
    cbool(is_bound(check_var(a[0])))
}

/// `(k/var? x)`: is `x` a var?
pub fn var_p(a: &[LispObj]) -> LispObj {
    cbool(is_var(a[0]))
}

/// `(k/implements id x)`: true if the type of `x` itself
/// has the method (cljs `implements?`: `default` does not count).
pub fn implements(a: &[LispObj]) -> LispObj {
    let id = method_id(a[0]);
    let m = type_of(a[1]).uref(TYPE_METHODS);
    let r = m != NIL && id < m.vector_length().integer() as usize && m.svref(id) != NIL;
    cbool(r)
}

/// `(k/type-has-method type id)`: ignoring defaults.
pub fn type_has_method(a: &[LispObj]) -> LispObj {
    let m = check_type(a[0]).uref(TYPE_METHODS);
    let id = method_id(a[1]);
    let r = m != NIL && id < m.vector_length().integer() as usize && m.svref(id) != NIL;
    cbool(r)
}

/// `(k/set-invoke-id id)`: protocol method used to call instances.
pub fn set_invoke_id(a: &[LispObj]) -> LispObj {
    method_id(a[0]);
    set_global(G::InvokeId, a[0]);
    a[0]
}

/// `(k/add-obj-method type name fn)`.
pub fn add_obj_method_prim(a: &[LispObj]) -> LispObj {
    add_obj_method(check_type(a[0]), name_of(a[1]), a[2]);
    a[0]
}

/// Cells of a method call-site cache (a simple-vector).
const CACHE_TYPE: usize = 0;
const CACHE_FN: usize = 1;
const CACHE_EPOCH: usize = 2;
pub const METHOD_CACHE_SIZE: usize = 3;

/// `(k/call-method cache x name args...)`: `(.name x args...)` at a
/// call site with its own cache of the method last found (by type and
/// method epoch).
pub fn call_method(a: &[LispObj]) -> LispObj {
    let (cache, x) = (a[0], a[1]);
    if !is_type(x) {
        let ty = type_of(x);
        let epoch = global(G::MethodEpoch);
        let f = if cache.svref(CACHE_TYPE) == ty && cache.svref(CACHE_EPOCH) == epoch {
            cache.svref(CACHE_FN)
        } else {
            let f = find_obj_method(ty, name_of(a[2]));
            if f != NIL {
                cache.svset(CACHE_TYPE, ty);
                cache.svset(CACHE_FN, f);
                cache.svset(CACHE_EPOCH, epoch);
            }
            f
        };
        if f != NIL {
            let mut args = vec![x];
            args.extend_from_slice(&a[3..]);
            return crate::runtime::lisp_call(f, &args);
        }
    }
    invoke_method(&a[1..])
}

/// `(k/invoke-method x name args...)`: `(.name x args...)`. Types
/// without the method go to `js::host_method`.
pub fn invoke_method(a: &[LispObj]) -> LispObj {
    // A static function of a type: `(.fromArray PersistentVector arr)`.
    if is_type(a[0])
        && let Some(f) = type_static(a[0], name_of(a[1]))
    {
        return crate::prims::funcall(f, &a[2..]);
    }
    let f = find_obj_method(type_of(a[0]), name_of(a[1]));
    if f != NIL {
        let mut args = vec![a[0]];
        args.extend_from_slice(&a[2..]);
        return crate::runtime::lisp_call(f, &args);
    }
    super::js::host_method(name_of(a[1]), a[0], &a[2..])
}

fn named_field(x: LispObj, name: LispObj) -> usize {
    let i = is_instance(x).then(|| field_index(instance_type(x), name)).flatten();
    match i {
        Some(i) => INSTANCE_FIELDS + i,
        None => lisp_error(format!(
            "No field {} in {}",
            name.string_to_rust(),
            printer::pr_str(x)
        )),
    }
}

/// Does type `ty` have static property `name` (or its `name`)?
pub fn has_static(ty: LispObj, name: &str) -> bool {
    name == "name" || type_static_str(ty, name).is_some()
}

/// `(k/get-field x name)`: `(.-name x)`. On a type, a static property.
/// As for JavaScript properties, a missing field is NIL; `constructor`
/// of an instance is its type, and `name` / `cljs$lang$ctorStr` of a
/// type its name.
pub fn get_field(a: &[LispObj]) -> LispObj {
    let name = name_of(a[1]);
    if is_type(a[0]) {
        return match type_static(a[0], name) {
            Some(v) => v,
            None if lisp_str_is(name, "name") || lisp_str_is(name, "cljs$lang$ctorStr") => type_name(a[0]),
            None => NIL,
        };
    }
    if !is_instance(a[0]) || super::js::is_js_object(a[0]) {
        return super::js::host_field(name, a[0]);
    }
    match field_index(instance_type(a[0]), name) {
        Some(i) => a[0].uref(INSTANCE_FIELDS + i),
        None if lisp_str_is(name, "constructor") => instance_type(a[0]),
        None => NIL,
    }
}

/// `(k/put-field x name v)`: `(set! (.-name x) v)`.
pub fn put_field(a: &[LispObj]) -> LispObj {
    if is_type(a[0]) {
        set_type_static(a[0], name_of(a[1]), a[2]);
        return a[2];
    }
    if !is_instance(a[0]) || super::js::is_js_object(a[0]) {
        return super::js::host_set_field(name_of(a[1]), a[0], a[2]);
    }
    let i = named_field(a[0], name_of(a[1]));
    a[0].uset(i, a[2]);
    a[2]
}

/// `(k/new type args...)`: a new instance; checks the field count.
/// A type whose static property `optionalFields` is true (JavaScript
/// constructor stand-ins) accepts fewer arguments; the other fields are
/// NIL.
/// Does `(new ty args...)` with `nargs` arguments just fill the fields?
pub fn plain_constructor(ty: LispObj, nargs: usize) -> bool {
    if !is_type(ty) {
        return false;
    }
    type_static_str(ty, "construct").is_none_or(|f| f == NIL) && field_count(ty) == nargs
}

pub fn new_instance(a: &[LispObj]) -> LispObj {
    // A JavaScript object standing in for a constructor (js/Object).
    if !is_type(a[0]) {
        return super::js::host_new(a[0], &a[1..]);
    }
    let ty = check_type(a[0]);
    // A static `construct` function (JavaScript constructor stand-ins
    // such as js/Date) makes the instance from the arguments.
    if let Some(f) = type_static_str(ty, "construct")
        && f != NIL
    {
        return crate::runtime::lisp_call(f, &a[1..]);
    }
    let n = field_count(ty);
    if a.len() - 1 < n && type_static_str(ty, "optionalFields").is_some_and(|v| v != NIL) {
        let mut args = a[1..].to_vec();
        args.resize(n, NIL);
        return make_instance(ty, &args);
    }
    if a.len() - 1 != n {
        lisp_error(format!(
            "Wrong number of args ({}) passed to constructor of {}",
            a.len() - 1,
            type_name(ty).string_to_rust()
        ));
    }
    make_instance(ty, &a[1..])
}

/// Function to call for instance `x` (its IFn invoke method), called by
/// the funcall stub; the stub passes `x` as the first argument.
pub fn invoke_function(x: LispObj) -> Option<LispObj> {
    if is_var(x) {
        let kns = global(G::KernelNs);
        return ns_lookup(kns, string_node("var-invoke")).map(var_value);
    }
    let id = global(G::InvokeId);
    if !id.is_fixnum() {
        return None;
    }
    let f = find_method(id.integer() as usize, x);
    (f != NIL).then_some(f)
}

// Raw forms (used by the bootstrap library clj.boot).

fn lb(b: bool) -> LispObj {
    cbool(b)
}

pub fn symbol_p(a: &[LispObj]) -> LispObj {
    lb(is_symbol(a[0]))
}
pub fn keyword_p(a: &[LispObj]) -> LispObj {
    lb(is_keyword(a[0]))
}
pub fn boolean_p(a: &[LispObj]) -> LispObj {
    lb(is_boolean(a[0]))
}

/// `(k/name x)`: name string of a symbol or keyword.
pub fn name_prim(a: &[LispObj]) -> LispObj {
    if is_symbol(a[0]) {
        sym_name(a[0])
    } else if is_keyword(a[0]) {
        a[0].uref(KW_NAME)
    } else {
        crate::error::type_error(format!("Doesn't support name: {}", printer::pr_str(a[0])))
    }
}

/// `(k/namespace x)`: namespace string (or NIL) of a symbol or keyword.
pub fn namespace_prim(a: &[LispObj]) -> LispObj {
    if is_symbol(a[0]) {
        sym_ns(a[0])
    } else if is_keyword(a[0]) {
        a[0].uref(KW_NS)
    } else {
        crate::error::type_error(format!("Doesn't support namespace: {}", printer::pr_str(a[0])))
    }
}

/// `(k/symbol-meta sym)`.
pub fn symbol_meta(a: &[LispObj]) -> LispObj {
    if !is_symbol(a[0]) {
        lisp_error(format!("Not a symbol: {}", printer::pr_str(a[0])));
    }
    sym_meta(a[0])
}


/// `(k/gensym prefix)`: a fresh symbol `prefix<n>`.
pub fn gensym(a: &[LispObj]) -> LispObj {
    let p = if a.is_empty() { "G__".to_string() } else { printer::print_str(a[0]) };
    symbol_from(None, &format!("{p}{}", reader::next_id()))
}

/// `(k/symbol-equal a b)`.
pub fn symbol_equal(a: &[LispObj]) -> LispObj {
    lb(is_symbol(a[0]) && is_symbol(a[1]) && sym_eq(a[0], a[1]))
}

/// `(k/str x)`: `str` of one object (NIL is "").
pub fn str_prim(a: &[LispObj]) -> LispObj {
    if a[0] == NIL {
        return string_node("");
    }
    if a[0].is_string() {
        return a[0];
    }
    // BigInts without N.
    if let Some(t) = crate::numbers::is_integer(a[0]).then(|| crate::numbers::exact_string(a[0])).flatten() {
        return string_node(&t);
    }
    // As Java's toString: Infinity, -Infinity, NaN (pr gives ##Inf etc.).
    if a[0].is_double_float() || a[0].is_single_float() {
        let d = if a[0].is_double_float() { a[0].double_float() } else { a[0].single_float() as f64 };
        if d.is_nan() {
            return string_node("NaN");
        }
        if d.is_infinite() {
            return string_node(if d > 0.0 { "Infinity" } else { "-Infinity" });
        }
    }
    // As Java's Object.toString: a function, or an instance whose type
    // defines no toString, is Type@HASH (an identity hash).
    let x = a[0];
    // As Java's Namespace.toString: the name.
    if is_ns(x) {
        return ns_name(x);
    }
    if (is_instance(x)
        && !(is_type(x) || is_var(x) || is_ns(x) || is_symbol(x) || is_keyword(x))
        && find_obj_method(type_of(x), string_node("toString")) == NIL)
        || x.is_function()
    {
        let name = type_name(type_of(a[0])).string_to_rust().replace('/', ".");
        let uid = crate::clj::js::uid(&[a[0]]).integer();
        return string_node(&format!("{name}@{uid:x}"));
    }
    string_node(&printer::print_str(a[0]))
}

/// `(k/make-protocol name method-names)`: a protocol with fresh
/// method ids (and a marker id for `satisfies?`).
pub fn make_protocol(a: &[LispObj]) -> LispObj {
    let name = check_string(a[0]);
    let marker = new_method_id(name);
    let methods = strtab_new();
    let names = heap::vector_of(&coll::items(a[1]));
    for i in 0..names.vector_length().integer() as usize {
        let m = check_string(names.svref(i));
        strtab_put(methods, m, LispObj::fixnum(new_method_id(m) as i64));
    }
    make_instance(global(G::ProtocolType), &[name, LispObj::fixnum(marker as i64), methods])
}

fn check_protocol(x: LispObj) -> LispObj {
    if !(is_instance(x) && instance_type(x) == global(G::ProtocolType)) {
        lisp_error(format!("Not a protocol: {}", printer::pr_str(x)));
    }
    x
}

/// `(k/protocol-method-id proto name)`: method id, or NIL.
pub fn protocol_method_id(a: &[LispObj]) -> LispObj {
    strtab_get(check_protocol(a[0]).uref(PROTO_METHODS), name_of(a[1])).unwrap_or(NIL)
}

/// `(k/protocol-marker proto)`: the marker method id.
pub fn protocol_marker(a: &[LispObj]) -> LispObj {
    check_protocol(a[0]).uref(PROTO_MARKER)
}

/// `(k/protocol? x)`.
pub fn protocol_p(a: &[LispObj]) -> LispObj {
    lb(is_instance(a[0]) && instance_type(a[0]) == global(G::ProtocolType))
}

/// `(k/case-equal x c)`: does `x` match `case` constant `c`?
pub fn case_equal(a: &[LispObj]) -> LispObj {
    let (x, c) = (a[0], a[1]);
    let r = x == c
        || crate::numbers::equiv(x, c)
        || (x.is_string() && c.is_string() && crate::object::lisp_strings_equal(x, c))
        || (is_symbol(x) && is_symbol(c) && sym_eq(x, c))
        // Keywords made at run time are not interned: as cljs, compare
        // the fqn.
        || (is_keyword(x)
            && is_keyword(c)
            && crate::object::lisp_strings_equal(x.uref(KW_FQN), c.uref(KW_FQN)));
    lb(r)
}

// JavaScript number semantics used by the ported cljs.core: bit
// operations work on 32-bit integers (ToInt32 / ToUint32).

fn to_i64(x: LispObj) -> i64 {
    if x.is_fixnum() {
        x.integer()
    } else if x.is_double_float() {
        let d = x.double_float();
        if d.is_finite() { (d.trunc() % 4294967296.0) as i64 } else { 0 }
    } else if x.is_single_float() {
        let d = x.single_float() as f64;
        if d.is_finite() { (d.trunc() % 4294967296.0) as i64 } else { 0 }
    } else {
        crate::error::type_error(format!("Not a number: {}", printer::pr_str(x)))
    }
}

fn to_int32(x: LispObj) -> i32 {
    to_i64(x) as i32
}

fn int32(n: i32) -> LispObj {
    LispObj::fixnum(n as i64)
}

/// `(k/bit op a b)`: op 0 and, 1 or, 2 xor, 3 and-not, 4 shl, 5 shr,
/// 6 unsigned shr, 7 imul; JavaScript 32-bit semantics.
pub fn bit_op(a: &[LispObj]) -> LispObj {
    let (x, y) = (to_int32(a[1]), to_int32(a[2]));
    match a[0].integer() {
        0 => int32(x & y),
        1 => int32(x | y),
        2 => int32(x ^ y),
        3 => int32(x & !y),
        4 => int32(x.wrapping_shl(y as u32 & 31)),
        5 => int32(x >> (y as u32 & 31)),
        6 => LispObj::fixnum(((x as u32) >> (y as u32 & 31)) as i64),
        7 => int32(x.wrapping_mul(y)),
        _ => lisp_error("Bad bit operation"),
    }
}

/// `(k/int32 x)`: ToInt32.
pub fn int32_prim(a: &[LispObj]) -> LispObj {
    int32(to_int32(a[0]))
}

/// `(k/bit-not x)`.
pub fn bit_not(a: &[LispObj]) -> LispObj {
    int32(!to_int32(a[0]))
}



/// `(k/set-hook name f)`: set the kernel hook `print` (`(f x
/// readably)` -> string). `f` may be nil.
pub fn set_hook(a: &[LispObj]) -> LispObj {
    let g = match name_of(a[0]).string_to_rust().as_str() {
        "print" => G::PrintHook,
        "var-watch" => G::VarWatchHook,
        n => lisp_error(format!("No kernel hook {n}")),
    };
    set_global(g, a[1]);
    a[1]
}

/// `(k/hash-double f)`: ClojureScript's hash-double, the xor of the
/// high and low 32 bits of the IEEE double (as a signed int32).
pub fn hash_double(a: &[LispObj]) -> LispObj {
    let f = crate::numbers::to_f64(a[0]);
    let bits = f.to_bits();
    let h = ((bits >> 32) as u32 ^ bits as u32) as i32;
    LispObj::fixnum(h as i64)
}

/// `(k/quot n d)` and `(k/rem n d)` (`rem` when `rem` is true):
/// truncating division, as Clojure's `quot` and `rem` (doubles if
/// either is a float, NaN and infinities following IEEE rules).
fn quot_rem(a: &[LispObj], rem: bool) -> LispObj {
    use crate::numbers::{as_f64, int_divide, is_float_num, num, Rounding};
    let (x, y) = (num(a[0]), num(a[1]));
    if is_float_num(&x) || is_float_num(&y) {
        // As Clojure's Numbers.quotient/remainder(double, double).
        let (n, d) = (as_f64(&x), as_f64(&y));
        if d == 0.0 {
            crate::numbers::divide_by_zero();
        }
        let q = n / d;
        if !q.is_finite() {
            crate::error::arithmetic_error(format!("Infinite or NaN: {q}"));
        }
        let q = q.trunc();
        return crate::heap::double_float_node(if rem { n - q * d } else { q });
    }
    let (q, r) = int_divide(a[0], a[1], Rounding::Trunc);
    if rem { r } else { q }
}

pub fn quot_prim(a: &[LispObj]) -> LispObj {
    quot_rem(a, false)
}

pub fn rem_prim(a: &[LispObj]) -> LispObj {
    quot_rem(a, true)
}
