//! Clojure front end: Clojure forms -> IR (see `frontend.rs`). Forms are
//! Clojure data: the kernel collections the reader makes (`clj::coll`),
//! or any seq, vector, map or set a macro returns. Clojure locals are
//! `Scope::CljVar` entries keyed by name (Clojure symbols are not
//! interned).

use super::*;
use crate::clj::coll::{self, list_from};
use crate::clj::{self as c, G};
use crate::heap::{self, string_node};

pub use crate::clj::is_macro_var;
use crate::clj::MACRO_FLAG;

#[derive(Default)]
pub struct State {
    recur: Option<Recur>,
    /// The innermost `recur` target is outside an enclosing `try`.
    across_try: bool,
}

struct Recur {
    id: LoopId,
    lambda: LambdaId,
    used: bool,
}

#[derive(Clone, Copy)]
enum Local {
    Var(VarId),
    /// Instance field: variable holding the instance, cell, mutable.
    Field(VarId, usize, bool),
}

impl Local {
    fn node(self) -> Node {
        match self {
            Local::Var(id) => Node::LexRef(id),
            Local::Field(this, cell, _) => Node::UvRef(Box::new(Node::LexRef(this)), cell),
        }
    }
}

/// Mark `v` as a macro (or not).
pub fn set_macro_var(v: LispObj, on: bool) {
    c::set_var_flag(v, MACRO_FLAG, on);
}

/// The elements of form `x` (a seq or a vector).
fn items(x: LispObj) -> Vec<LispObj> {
    coll::items(x)
}

/// The cache of a `(new T ...)` site: `[type]`, starting with a value
/// that no type is.
fn new_cache() -> LispObj {
    heap::vector_of(&[SYMBOL_UNBOUND])
}

/// Is `x` a non-empty seq (a call or special form)?
fn is_compound(x: LispObj) -> bool {
    coll::is_list(x) || coll::is_cons(x) || (!coll::is_kernel_value(x) && coll::is_seq(x) && coll::seq(x) != NIL)
}

/// The value of keyword `k` in metadata map `m` (nil if `m` is nil).
fn meta_get(m: LispObj, k: &str) -> LispObj {
    if m == NIL { NIL } else { coll::get(m, c::keyword_from(None, k), NIL) }
}

/// `Type.FIELD` (also `ns/Type.FIELD`, `a.b.Type.FIELD`) names the
/// static field FIELD of a type, as in ClojureScript: the form
/// `(.-FIELD Type)`, unless the whole name is a var.
fn static_field_form(s: LispObj) -> Option<LispObj> {
    let name = c::sym_name(s).string_to_rust();
    let i = name.rfind('.')?;
    if i == 0 || i + 1 == name.len() {
        return None;
    }
    let ns = c::sym_ns(s);
    let home = if ns == NIL { Some(c::current_ns()) } else { c::resolve_ns(c::current_ns(), ns) };
    if home.and_then(|h| c::ns_lookup(h, c::sym_name(s))).is_some() {
        return None;
    }
    let ns_str = (ns != NIL).then(|| ns.string_to_rust());
    let prefix = c::symbol_from(ns_str.as_deref(), &name[..i]);
    match c::resolve(prefix) {
        Some(v) if c::is_bound(v) && c::is_type(c::var_value(v)) => {
            let acc = c::symbol_from(None, &format!(".-{}", &name[i + 1..]));
            Some(list_from(&[acc, prefix]))
        }
        _ => None,
    }
}

/// Qualified methods (Clojure 1.12): for `T/m` where `T` names no
/// namespace but a bound var (a type) or a JavaScript global `js/T`, the
/// target symbol and the member name `m`.
fn qualified_member(s: LispObj) -> Option<(LispObj, String)> {
    if !c::is_symbol(s) {
        return None;
    }
    let ns = c::sym_ns(s);
    if ns == NIL || c::resolve_ns(c::current_ns(), ns).is_some() {
        return None;
    }
    let t = ns.string_to_rust();
    let bound = |sym| c::resolve(sym).is_some_and(c::is_bound);
    let target = [c::symbol_from(None, &t), c::symbol_from(Some("js"), &t)].into_iter().find(|&x| bound(x))?;
    Some((target, c::sym_name(s).string_to_rust()))
}

/// `T/m` as a value: `T/.m` is a fn calling method `m` of its first
/// argument, `T/new` a fn making a `T`, other names the static property.
fn qualified_member_value(s: LispObj) -> Option<LispObj> {
    let (t, m) = qualified_member(s)?;
    let l = list_from;
    let apply = c::symbol_from(Some("clojure.core"), "apply");
    let (o, a) = (crate::clj::prims::gensym(&[]), crate::clj::prims::gensym(&[]));
    let amp = csym("&");
    Some(if let Some(m) = m.strip_prefix('.') {
        let call = l(&[apply, kernel_sym("invoke-method"), o, string_node(m), a]);
        l(&[csym("fn*"), coll::vector_from(&[o, amp, a]), call])
    } else if m == "new" {
        let call = l(&[apply, kernel_sym("new"), t, a]);
        l(&[csym("fn*"), coll::vector_from(&[amp, a]), call])
    } else {
        // A type's statics are all set when it is defined (corten.lang
        // types, deftype), so an unknown one is an error, as in Clojure.
        if let Some(v) = c::resolve(t) {
            let ty = c::var_value(v);
            if c::is_type(ty) && !c::prims::has_static(ty, &m) {
                let tn = c::type_name(ty).string_to_rust();
                lisp_error(format!("Unable to find static field: {m} in {tn}"));
            }
        }
        l(&[csym("."), t, c::symbol_from(None, &format!("-{m}"))])
    })
}

/// `[a b] ++ rest`, a list.
fn list_of(head: &[LispObj], rest: &[LispObj]) -> LispObj {
    let mut v = head.to_vec();
    v.extend_from_slice(rest);
    list_from(&v)
}

/// `(T/m args...)`: `(. arg0 m args...)` for `T/.m`, `(new T args...)`
/// for `T/new`, otherwise the static method `(. T m args...)`.
fn qualified_member_call(x: &[LispObj], form: LispObj) -> Option<LispObj> {
    let (t, m) = qualified_member(x[0])?;
    let args = &x[1..];
    Some(if let Some(m) = m.strip_prefix('.') {
        if args.is_empty() {
            clj_err("Malformed member expression, expecting (T/.member target ...)", form);
        }
        list_of(&[csym("."), args[0], csym(m)], &args[1..])
    } else if m == "new" {
        list_of(&[csym("new"), t], args)
    } else {
        list_of(&[csym("."), t, csym(&m)], args)
    })
}

/// The symbol `corten.kernel/name`.
fn kernel_sym(name: &str) -> LispObj {
    crate::clj::reader::kernel_fn(name)
}

/// Callee: kernel function `name`.
fn kernel(name: &str) -> Callee {
    Callee::Kernel(crate::prims::prim_index(name))
}

/// The kernel function index of var `v` if it is one of `corten.kernel`.
fn kernel_index(v: LispObj) -> Option<usize> {
    let f = c::var_value(v);
    (c::var_ns(v) == c::global(G::KernelNs) && f.is_uvector_of(KFUNCTION_TYPE))
        .then(|| f.uref(FUNCTION_ADDRESS).integer() as usize)
}

/// A call of kernel function `idx` (arity checked here): an inline
/// operation for the common primitives with the arity codegen handles,
/// else a direct call.
fn kernel_call(idx: usize, args: Vec<Node>, form: LispObj) -> Node {
    let p = &crate::prims::PRIMS[idx];
    if args.len() < p.min || args.len() > p.max {
        clj_err(&format!("Wrong number of args ({}) passed to", args.len()), form);
    }
    let op = match (p.name, args.len()) {
        ("eq", 2) => Some(InlineOp::Eq),
        ("nil?", 1) => Some(InlineOp::IsNil),
        ("not", 1) => Some(InlineOp::Not),
        ("+", 2) => Some(InlineOp::Add2),
        ("-", 2) => Some(InlineOp::Sub2),
        ("*", 2) => Some(InlineOp::Mul2),
        ("=", 2) => Some(InlineOp::NumEq2),
        ("<", 2) => Some(InlineOp::Lt2),
        (">", 2) => Some(InlineOp::Gt2),
        ("<=", 2) => Some(InlineOp::Le2),
        (">=", 2) => Some(InlineOp::Ge2),
        ("boolean", 1) => Some(InlineOp::Boolean),
        ("aget", 2) => Some(InlineOp::Aget),
        ("alength", 1) => Some(InlineOp::Alength),
        ("method", 2) => Some(InlineOp::Method),
        ("implements", 2) => Some(InlineOp::Implements),
        ("keyword?", 1) => Some(InlineOp::KeywordP),
        ("protocol-marker", 1) => Some(InlineOp::ProtocolMarker),
        ("type-of", 1) => Some(InlineOp::TypeOf),
        ("identical?", 2) => Some(InlineOp::Identical),
        ("array", n) if n <= 32 => Some(InlineOp::Array),
        _ => None,
    };
    if let Some(op) = op {
        return Node::Inline(op, args);
    }
    // The kernel types are fixed objects.
    if p.name == "builtin-type"
        && let [Node::Const(t)] = args.as_slice()
    {
        return Node::Const(crate::clj::prims::builtin_type(&[*t]));
    }
    Node::Call(Callee::Kernel(idx), args)
}

fn clj_err(msg: &str, form: LispObj) -> ! {
    lisp_error(format!("{msg}: {}", c::printer::pr_str(form)))
}

/// An unqualified reference to a var not defined yet: as in
/// ClojureScript, a warning, and the var is interned in the current
/// namespace (unbound until defined).
fn undeclared_var(s: LispObj) -> LispObj {
    if c::sym_ns(s) != NIL {
        if c::resolve_ns(c::current_ns(), c::sym_ns(s)) == Some(c::global(G::KernelNs)) {
            clj_err("No such kernel function", s);
        }
        clj_err("No such namespace", s);
    }
    let ns = c::current_ns();
    let name = c::sym_name(s);
    if c::lisp_str_is(name, "&") || name.string_to_rust().contains('.') {
        clj_err("Unable to resolve symbol", s);
    }
    eprintln!(
        "WARNING: Use of undeclared Var {}/{}",
        c::ns_name(ns).string_to_rust(),
        name.string_to_rust()
    );
    c::intern_var(ns, name)
}

fn csym(name: &str) -> LispObj {
    c::symbol_from(None, name)
}

/// The macro var named by `head`, if any.
fn macro_var(head: LispObj) -> Option<LispObj> {
    // A special form is never a macro call (a namespace may define a
    // macro named `def`, as cljs.spec does).
    if !c::is_symbol(head) || special_name(head).is_some() {
        return None;
    }
    c::resolve_macro(head)
}

/// Call macro `var` on `form` (Clojure convention: `&form &env args...`).
fn expand(var: LispObj, form: LispObj, args: &[LispObj]) -> LispObj {
    let mut a = vec![form, NIL];
    a.extend_from_slice(args);
    lisp_call(c::var_value(var), &a)
}

/// Expand `form` once if it is a macro call (no locals in scope). As
/// when compiling, GC is inhibited: the form's elements are in a `Vec`.
pub fn macroexpand_1(form: LispObj) -> Option<LispObj> {
    if !is_compound(form) {
        return None;
    }
    crate::runtime::without_gc(|| {
        let v = items(form);
        let m = macro_var(v[0])?;
        Some(expand(m, form, &v[1..]))
    })
}

/// Is `x` a call of the special form `do`?
pub fn is_do_form(x: LispObj) -> bool {
    is_compound(x) && c::is_sym_named(coll::first(x), "do")
}

fn special_name(head: LispObj) -> Option<String> {
    if c::is_symbol(head) && c::sym_ns(head) == NIL {
        let n = c::sym_name(head).string_to_rust();
        // catch and finally are only special inside try; & only in
        // parameter lists.
        if crate::clj::reader::SPECIAL_FORMS.contains(&n.as_str()) && !["catch", "finally", "&"].contains(&n.as_str()) {
            return Some(n);
        }
    }
    None
}

/// Does metadata `m` have keys other than the compiler's hints (`:tag`,
/// `:once`, `:line`, `:column`, `:file`)? Then a collection or fn
/// form carrying it evaluates to a value with that metadata.
fn has_user_meta(m: LispObj) -> bool {
    m != NIL
        && coll::items(m).into_iter().any(|e| {
            let k = coll::nth(e, 0);
            !(c::is_keyword(k)
                && c::sym_ns(k) == NIL
                && ["tag", "once", "line", "column", "file"].iter().any(|n| c::lisp_str_is(k.uref(c::KW_NAME), n)))
        })
}

/// Is form `x` a constant: a value that evaluates to itself, or a
/// vector (of at most 32), map or set (of at most 8) of constants without
/// metadata?
fn is_constant(x: LispObj) -> bool {
    if c::is_symbol(x) || is_compound(x) {
        return false;
    }
    if !coll::is_kernel_value(x) {
        return !(coll::is_vector(x) || coll::is_map(x) || coll::is_set(x) || coll::is_seq(x));
    }
    let small = |n: usize| coll::meta(x) == NIL && coll::count(x) <= n;
    if coll::is_pvector(x) {
        small(32) && items(x).into_iter().all(is_constant)
    } else if coll::is_array_map(x) {
        small(8) && items(x).into_iter().all(|e| is_constant(coll::nth(e, 0)) && is_constant(coll::nth(e, 1)))
    } else if coll::is_hash_set(x) {
        small(8) && items(x).into_iter().all(is_constant)
    } else {
        true
    }
}

impl Frontend {
    /// Convert a Clojure form as the body of a lambda of no arguments.
    pub fn convert_clj_toplevel(&mut self, form: LispObj) -> LambdaId {
        let id = self.push_lambda(NIL);
        let saved = self.cur;
        self.cur = id;
        let body = self.clj(form);
        self.lambdas[id].arities.push(Arity { req: Vec::new(), rest: None, body });
        self.cur = saved;
        id
    }

    fn push_lambda(&mut self, name: LispObj) -> LambdaId {
        self.lambdas.push(LambdaInfo {
            parent: if self.lambdas.is_empty() { None } else { Some(self.cur) },
            name,
            free: Vec::new(),
            arities: Vec::new(),
        });
        self.lambdas.len() - 1
    }

    /// Convert a Clojure form.
    pub(super) fn clj(&mut self, x: LispObj) -> Node {
        if c::is_symbol(x) {
            return self.clj_symbol(x);
        }
        if is_compound(x) {
            return self.clj_compound(x);
        }
        if is_constant(x) {
            return Node::Const(x);
        }
        // A record is a map, but evaluates to itself.
        if !coll::is_kernel_value(x) && coll::core_bool("record?", x) {
            return Node::Const(x);
        }
        let n = if coll::is_seq(x) {
            // An empty seq: ().
            return Node::Const(c::global(G::EmptyList));
        } else if coll::is_vector(x) {
            let v: Vec<Node> = items(x).into_iter().map(|f| self.clj(f)).collect();
            self.vector_node(v)
        } else if coll::is_map(x) {
            let kvs: Vec<LispObj> = items(x).into_iter().flat_map(|e| [coll::nth(e, 0), coll::nth(e, 1)]).collect();
            self.map_node(&kvs)
        } else if coll::is_set(x) {
            let v: Vec<Node> = items(x).into_iter().map(|f| self.clj(f)).collect();
            Self::set_node(v)
        } else {
            return Node::Const(x);
        };
        self.with_form_meta(x, n)
    }

    /// `n`, the value of collection or fn form `x`, with the metadata of
    /// `x` if it has more than type hints.
    fn with_form_meta(&mut self, x: LispObj, n: Node) -> Node {
        let m = coll::meta(x);
        if !has_user_meta(m) {
            return n;
        }
        let m = self.clj(m);
        Node::Call(kernel("with-meta"), vec![n, m])
    }

    /// `(new ty fields...)` with a per-site cache.
    fn new_node(ty: LispObj, fields: Vec<Node>) -> Node {
        let mut v = vec![Node::Const(ty)];
        v.extend(fields);
        v.push(Node::Const(new_cache()));
        Node::Inline(InlineOp::New, v)
    }

    /// `(. ty name args...)`: static method `name` of type `ty`.
    fn static_call(ty: LispObj, name: &str, args: Vec<Node>) -> Node {
        let cache = Node::Const(heap::vector_node(crate::clj::prims::METHOD_CACHE_SIZE));
        let mut v = vec![cache, Node::Const(ty), Node::Const(string_node(name))];
        v.extend(args);
        Node::Call(kernel("call-method"), v)
    }

    /// A vector of the values of `items`, built the way the cljs
    /// compiler emits it.
    fn vector_node(&mut self, items: Vec<Node>) -> Node {
        let n = items.len();
        if n == 0 {
            return Node::Const(c::global(G::EmptyVector));
        }
        if n > 32 {
            return Node::Call(kernel("vector"), items);
        }
        let fields = vec![
            Node::Const(NIL),
            Node::Const(LispObj::fixnum(n as i64)),
            Node::Const(LispObj::fixnum(5)),
            Node::Const(c::global(G::EmptyVectorNode)),
            Node::Inline(InlineOp::Array, items),
            Node::Const(NIL),
        ];
        Self::new_node(c::global(G::VectorType), fields)
    }

    /// A map of the values of `kvs` (k v k v ...): an array map, which
    /// distinct constant keys make directly; a hash map past 8 entries
    /// once cljs.core is loaded. Later keys replace equal earlier ones.
    fn map_node(&mut self, kvs: &[LispObj]) -> Node {
        let n = kvs.len();
        if n == 0 {
            return Node::Const(c::global(G::EmptyArrayMap));
        }
        let phm = c::ns_lookup(c::global(G::CoreNs), string_node("PersistentHashMap"))
            .filter(|&v| c::is_bound(v))
            .map(c::var_value)
            .filter(|&t| c::is_type(t));
        if n > 16
            && let Some(phm) = phm
        {
            let ks = kvs.iter().step_by(2).map(|&k| self.clj(k)).collect();
            let vs = kvs.iter().skip(1).step_by(2).map(|&v| self.clj(v)).collect();
            let arrays = vec![Node::Inline(InlineOp::Array, ks), Node::Inline(InlineOp::Array, vs)];
            return Self::static_call(phm, "fromArrays", arrays);
        }
        let args: Vec<Node> = kvs.iter().map(|&f| self.clj(f)).collect();
        if n > 32 || !distinct_constant_keys(kvs) {
            return Node::Call(kernel("array-map"), args);
        }
        let fields = vec![
            Node::Const(NIL),
            Node::Const(LispObj::fixnum(n as i64 / 2)),
            Node::Inline(InlineOp::Array, args),
            Node::Const(NIL),
        ];
        Self::new_node(c::global(G::ArrayMapType), fields)
    }

    /// A set of the values of `items`: duplicates are an error once
    /// cljs.core is loaded (`createWithCheck`).
    fn set_node(items: Vec<Node>) -> Node {
        if items.is_empty() {
            return Node::Const(c::global(G::EmptyHashSet));
        }
        let ty = c::global(G::HashSetType);
        if items.len() <= 32 && c::prims::has_static(ty, "createWithCheck") {
            return Self::static_call(ty, "createWithCheck", vec![Node::Inline(InlineOp::Array, items)]);
        }
        Node::Call(kernel("hash-set"), items)
    }

    /// `a.b.c` where `a` is a local: `(.-c (.-b a))` (cljs).
    fn local_field_form(&mut self, s: LispObj) -> Option<LispObj> {
        if c::sym_ns(s) != NIL {
            return None;
        }
        let name = c::sym_name(s).string_to_rust();
        let mut parts = name.split('.');
        let first = parts.next()?;
        if first.is_empty() || first.len() == name.len() || parts.clone().any(str::is_empty) {
            return None;
        }
        let local = c::symbol_from(None, first);
        self.lookup_local(local)?;
        Some(parts.fold(local, |acc, p| list_from(&[csym("."), acc, c::symbol_from(None, &format!("-{p}"))])))
    }

    /// The Clojure local named by symbol `s` (unqualified), if any.
    fn lookup_local(&mut self, s: LispObj) -> Option<Local> {
        if !c::is_symbol(s) || c::sym_ns(s) != NIL {
            return None;
        }
        let name = c::sym_name(s);
        let eq = |n| crate::object::lisp_strings_equal(n, name);
        let l = self.scope.iter().rev().find_map(|sc| match *sc {
            Scope::CljVar(n, id) if eq(n) => Some(Local::Var(id)),
            Scope::CljField(n, this, cell, m) if eq(n) => Some(Local::Field(this, cell, m)),
            _ => None,
        })?;
        match l {
            Local::Var(id) | Local::Field(id, ..) => self.reference(id),
        }
        Some(l)
    }

    fn clj_symbol(&mut self, s: LispObj) -> Node {
        if let Some(l) = self.lookup_local(s) {
            return l.node();
        }
        if let Some(form) = static_field_form(s).or_else(|| qualified_member_value(s)) {
            return self.clj(form);
        }
        if let Some(form) = self.local_field_form(s) {
            return self.clj(form);
        }
        match c::resolve(s) {
            Some(v) if is_macro_var(v) => {
                // In a macro namespace a name can be both an inline macro
                // and a clj.boot function referred before it.
                let f = c::find_ns("clj.boot").and_then(|b| c::ns_lookup(b, c::sym_name(s)));
                match f {
                    Some(f) if !is_macro_var(f) => Node::VarRef(f),
                    _ => clj_err("Can't take value of a macro", s),
                }
            }
            Some(v) if kernel_index(v).is_some() => Node::Const(c::var_value(v)),
            Some(v) => Node::VarRef(v),
            None => Node::VarRef(undeclared_var(s)),
        }
    }

    fn clj_args(&mut self, args: &[LispObj]) -> Vec<Node> {
        args.iter().map(|&f| self.clj(f)).collect()
    }

    fn clj_body(&mut self, body: &[LispObj]) -> Node {
        let mut v: Vec<Node> = body.iter().map(|&f| self.clj(f)).collect();
        match v.len() {
            0 => Node::Const(NIL),
            1 => v.pop().unwrap(),
            _ => Node::Do(v),
        }
    }

    fn clj_compound(&mut self, x: LispObj) -> Node {
        let v = items(x);
        let head = v[0];
        let args = &v[1..];
        if c::is_symbol(head) {
            if let Some(form) = interop_sugar(&v, x).or_else(|| qualified_member_call(&v, x)) {
                return self.clj(form);
            }
            if let Some(name) = special_name(head) {
                return self.clj_special(&name, x, &v);
            }
            match self.lookup_local(head) {
                Some(Local::Var(id)) => return Node::Call(Callee::Lex(id), self.clj_args(args)),
                Some(l) => return Node::Call(Callee::Expr(Box::new(l.node())), self.clj_args(args)),
                None => {}
            }
            if let Some(m) = c::resolve_macro(head) {
                let e = expand(m, x, args);
                return self.clj(e);
            }
            return match c::resolve(head) {
                Some(var) => {
                    let args = self.clj_args(args);
                    match kernel_index(var) {
                        Some(idx) => kernel_call(idx, args, x),
                        None => Node::Call(Callee::Expr(Box::new(Node::VarRef(var))), args),
                    }
                }
                None => {
                    let var = undeclared_var(head);
                    let args = self.clj_args(args);
                    Node::Call(Callee::Expr(Box::new(Node::VarRef(var))), args)
                }
            };
        }
        // (:k m) and (:k m d) are (get m :k) and (get m :k d), as
        // Keyword's -invoke, once `get` is defined.
        if c::is_keyword(head) && (1..=2).contains(&args.len()) {
            let get = c::ns_lookup(c::global(G::CoreNs), string_node("get")).filter(|&v| c::is_bound(v));
            if let Some(g) = get {
                return self.keyword_get(head, g, args);
            }
        }
        let f = self.clj(head);
        Node::Call(Callee::Expr(Box::new(f)), self.clj_args(args))
    }

    /// `(:k m)` or `(:k m d)` with an inline search when `m` is a
    /// PersistentArrayMap (keywords are interned: compare with EQ),
    /// else a call of `get` (the var `get`).
    fn keyword_get(&mut self, k: LispObj, get: LispObj, args: &[LispObj]) -> Node {
        let pam = c::global(G::ArrayMapType);
        let arr = coll::PAM_ARR - c::INSTANCE_FIELDS;
        let mut binds = Vec::new();
        let mut simple = |fe: &mut Self, x: LispObj| {
            let n = fe.clj(x);
            if matches!(n, Node::Const(_) | Node::LexRef(_)) {
                return n;
            }
            let v = fe.new_var(NIL);
            binds.push((v, n));
            Node::LexRef(v)
        };
        let m = simple(self, args[0]);
        let d = if args.len() == 2 { simple(self, args[1]) } else { Node::Const(NIL) };
        let copy = |n: &Node| match n {
            Node::LexRef(v) => Node::LexRef(*v),
            Node::Const(x) => Node::Const(*x),
            _ => unreachable!(),
        };
        let mut call_args = vec![copy(&m), Node::Const(k)];
        if args.len() == 2 {
            call_args.push(copy(&d));
        }
        let call = Node::Call(Callee::Expr(Box::new(Node::VarRef(get))), call_args);
        let body = Node::Inline(
            InlineOp::KwGet,
            vec![m, Node::Const(k), d, Node::Const(pam), Node::Const(LispObj::fixnum(arr as i64)), call],
        );
        if binds.is_empty() {
            return body;
        }
        Node::Let(binds, Box::new(body))
    }

    fn clj_special(&mut self, name: &str, x: LispObj, v: &[LispObj]) -> Node {
        let args = &v[1..];
        let a1 = args.first().copied().unwrap_or(NIL);
        match name {
            "quote" => {
                if args.len() != 1 {
                    clj_err("Wrong number of args to quote", x);
                }
                Node::Const(a1)
            }
            "do" => self.clj_body(args),
            "if" => {
                let n = args.len();
                if !(2..=3).contains(&n) {
                    clj_err(if n < 2 { "Too few arguments to if" } else { "Too many arguments to if" }, x);
                }
                Node::If(
                    Box::new(self.clj(a1)),
                    Box::new(self.clj(args[1])),
                    Box::new(self.clj(args.get(2).copied().unwrap_or(NIL))),
                )
            }
            "def" => self.clj_def(x, args),
            "fn*" => self.clj_fn(x, args),
            "let*" => self.clj_let(x, args, false),
            "loop*" => self.clj_let(x, args, true),
            "recur" => self.clj_recur(x, args),
            "letfn*" => self.clj_letfn(x, args),
            "var" => match c::is_symbol(a1).then(|| c::resolve_var(a1)).flatten() {
                Some(v) => Node::Const(v),
                _ => clj_err("Unable to resolve var", a1),
            },
            "set!" => self.clj_set(x, args),
            "." => self.clj_dot(x, args),
            "new" => {
                if args.is_empty() {
                    clj_err("Invalid new form", x);
                }
                let mut v = vec![self.clj(a1)];
                v.extend(self.clj_args(&args[1..]));
                if v.len() > 32 {
                    return Node::Call(kernel("new"), v);
                }
                v.push(Node::Const(new_cache()));
                Node::Inline(InlineOp::New, v)
            }
            "fields*" => self.clj_fields(x, args),
            "throw" => {
                if args.len() != 1 {
                    clj_err("Wrong number of args to throw", x);
                }
                let v = self.clj(a1);
                Node::Call(kernel("throw"), vec![v])
            }
            "try" => self.clj_try(x, args),
            _ => clj_err("Unsupported special form", x),
        }
    }

    fn clj_set(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        if args.len() == 3 {
            // (set! obj -field v)
            let member = args[1];
            if c::is_symbol(member)
                && let Some(f) = c::sym_name(member).string_to_rust().strip_prefix('-')
            {
                let obj = self.clj(args[0]);
                let v = self.clj(args[2]);
                let f = Node::Const(string_node(f));
                return Node::Call(kernel("put-field"), vec![obj, f, v]);
            }
        }
        if args.len() != 2 {
            clj_err("Malformed assignment, expecting (set! target val)", x);
        }
        let (target, val) = (args[0], args[1]);
        if is_compound(target) {
            // (set! (.-field obj) v) / (set! (. obj -field) v)
            let tv = items(target);
            let t = interop_sugar(&tv, target).map_or(tv, items);
            if t.len() == 3 && c::is_sym_named(t[0], ".") && c::is_symbol(t[2]) {
                let name = c::sym_name(t[2]).string_to_rust();
                if let Some(f) = name.strip_prefix('-') {
                    let obj = self.clj(t[1]);
                    let v = self.clj(val);
                    let f = Node::Const(string_node(f));
                    return Node::Call(kernel("put-field"), vec![obj, f, v]);
                }
            }
            clj_err("Invalid assignment target", target);
        }
        match self.lookup_local(target) {
            Some(Local::Field(this, cell, true)) => {
                let v = self.clj(val);
                return Node::UvSet(Box::new(Node::LexRef(this)), cell, Box::new(v));
            }
            Some(_) => clj_err("Cannot assign to non-mutable", target),
            None => {}
        }
        match c::is_symbol(target).then(|| c::resolve(target)).flatten() {
            Some(v) if kernel_index(v).is_some() => clj_err("Cannot set a kernel function", target),
            Some(v) => Node::VarSet(v, Box::new(self.clj(val))),
            _ => clj_err("Invalid assignment target", target),
        }
    }

    /// `(. obj -field)`, `(. obj method args*)`, `(. obj (method args*))`.
    fn clj_dot(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        if args.len() < 2 {
            clj_err("Malformed member expression", x);
        }
        let obj = args[0];
        let mut member = args[1];
        let mut margs = args[2..].to_vec();
        if is_compound(member) {
            let m = items(member);
            member = m[0];
            margs = m[1..].to_vec();
        }
        if !c::is_symbol(member) {
            clj_err("Malformed member expression", x);
        }
        let name = c::sym_name(member).string_to_rust();
        let o = self.clj(obj);
        if let Some(f) = name.strip_prefix('-')
            && margs.is_empty()
        {
            let f = Node::Const(string_node(f));
            // [type field-index]
            let cache = Node::Const(heap::vector_of(&[NIL, LispObj::fixnum(0)]));
            return Node::Inline(InlineOp::GetField, vec![o, f, cache]);
        }
        let cache = Node::Const(heap::vector_node(crate::clj::prims::METHOD_CACHE_SIZE));
        let mut v = vec![cache, o, Node::Const(string_node(&name))];
        v.extend(self.clj_args(&margs));
        Node::Call(kernel("call-method"), v)
    }

    /// `(fields* this [field*] body*)`: the fields of instance `this` (a
    /// local) as locals; `^:mutable` ones can be `set!`.
    fn clj_fields(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        if args.len() < 2 || !coll::is_vector(args[1]) {
            clj_err("fields*: expected a vector of fields", x);
        }
        let this = args[0];
        let this_var = match self.lookup_local(this) {
            Some(Local::Var(id)) => id,
            _ => clj_err("fields*: not a local", this),
        };
        let mark = self.scope.len();
        for (i, f) in items(args[1]).into_iter().enumerate() {
            if !c::is_symbol(f) {
                clj_err("fields*: bad field", f);
            }
            let mutable = truthy(meta_get(c::sym_meta(f), "mutable"));
            self.scope.push(Scope::CljField(c::sym_name(f), this_var, c::INSTANCE_FIELDS + i, mutable));
        }
        let body = self.clj_body(&args[2..]);
        self.scope.truncate(mark);
        body
    }

    fn clj_def(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        let n = args.len();
        let name = args.first().copied().unwrap_or(NIL);
        if !(1..=3).contains(&n) || !c::is_symbol(name) {
            clj_err("Invalid def", x);
        }
        let cur = c::current_ns();
        if c::sym_ns(name) != NIL && c::resolve_ns(cur, c::sym_ns(name)) != Some(cur) {
            clj_err("Can't create defs outside of current ns", name);
        }
        let meta = c::sym_meta(name);
        let flag = |k: &str| truthy(meta_get(meta, k));
        // Macros live in the namespace's macro namespace.
        let ns = if flag("macro") { c::macro_ns_of(cur, true).unwrap() } else { cur };
        let var = c::intern_var(ns, c::sym_name(name));
        if flag("dynamic") {
            c::set_var_flag(var, c::DYNAMIC_FLAG, true);
        }
        set_macro_var(var, flag("macro"));
        // The metadata is kept as read, except that (quote x) is x and
        // calls (such as :test #(...)) are evaluated when the def is.
        let mut kvs = Vec::new();
        let mut evaluated = Vec::new();
        // :file and :line of the top-level form being loaded, as Clojure
        // has them (clojure.repl/source reads them).
        let has = |k: &str| meta_get(meta, k) != NIL;
        let file_line = crate::compiler::load_pos().filter(|_| !has("file") && !has("line"));
        if let Some((file, line)) = &file_line {
            kvs.extend([c::keyword_from(None, "file"), heap::string_node(file), c::keyword_from(None, "line"), LispObj::fixnum(*line)]);
        }
        if meta != NIL {
            for e in items(meta) {
                let (k, x) = (coll::nth(e, 0), coll::nth(e, 1));
                let head = if is_compound(x) { coll::first(x) } else { NIL };
                if c::is_sym_named(head, "quote") {
                    kvs.extend([k, coll::nth(x, 1)]);
                } else if c::is_symbol(head) {
                    kvs.extend([k, NIL]);
                    evaluated.push((k, x));
                } else {
                    kvs.extend([k, x]);
                }
            }
        }
        // (Reading it adds :ns and :name.)
        c::set_var_meta(var, if kvs.is_empty() { NIL } else { coll::array_map_prim(&kvs) });
        c::set_var_flag(var, c::DEF_META_FLAG, true);
        let mut body: Vec<Node> = Vec::new();
        for (k, x) in evaluated {
            let v = self.clj(x);
            body.push(Node::Call(kernel("var-meta-put"), vec![Node::Const(var), Node::Const(k), v]));
        }
        body.push(if n == 1 {
            Node::Const(var)
        } else {
            let v = self.clj(args[n - 1]);
            Node::Call(kernel("def"), vec![Node::Const(var), v])
        });
        if body.len() == 1 { body.pop().unwrap() } else { Node::Do(body) }
    }

    /// Bind Clojure local `sym` to a new variable of the current lambda.
    fn clj_bind(&mut self, sym: LispObj) -> VarId {
        let id = self.new_var(sym);
        self.scope_bind(sym, id);
        id
    }

    /// Bind Clojure local `sym` to variable `id`.
    fn scope_bind(&mut self, sym: LispObj, id: VarId) {
        if !c::is_symbol(sym) || c::sym_ns(sym) != NIL {
            clj_err("Can't let qualified name", sym);
        }
        self.scope.push(Scope::CljVar(c::sym_name(sym), id));
    }

    fn binding_pairs(x: LispObj, v: LispObj) -> Vec<(LispObj, LispObj)> {
        if !coll::is_vector(v) {
            clj_err("Bad binding form, expected vector", x);
        }
        let items = items(v);
        if !items.len().is_multiple_of(2) {
            clj_err("Bad binding form, expected matched symbol expression pairs", x);
        }
        items.chunks(2).map(|p| (p[0], p[1])).collect()
    }

    fn clj_let(&mut self, x: LispObj, args: &[LispObj], is_loop: bool) -> Node {
        let pairs = Self::binding_pairs(x, args.first().copied().unwrap_or(NIL));
        let body = &args[1..];
        let mark = self.scope.len();
        let mut binds = Vec::new();
        let mut header = Vec::new();
        let mut names = Vec::new();
        for (sym, init) in pairs {
            let n = self.clj(init);
            let id = self.clj_bind(sym);
            binds.push((id, n));
            header.push(id);
            names.push(sym);
        }
        let body = if is_loop {
            self.loop_body(&header, &names, body)
        } else {
            self.clj_body(body)
        };
        self.scope.truncate(mark);
        Node::Let(binds, Box::new(body))
    }

    /// `body`, with `names` bound to the variables `header`, as the target
    /// of `recur`, which sets them and jumps back. (A closure made in the
    /// body keeps the values of that iteration: it copies them.)
    fn loop_body(&mut self, header: &[VarId], names: &[LispObj], body: &[LispObj]) -> Node {
        self.loops.push(LoopInfo { vars: header.to_vec() });
        let id = self.loops.len() - 1;
        let saved = self.clj.recur.replace(Recur {
            id,
            lambda: self.cur,
            used: false,
        });
        let saved_across = std::mem::replace(&mut self.clj.across_try, false);
        let mark = self.scope.len();
        for (&h, &sym) in header.iter().zip(names) {
            self.scope_bind(sym, h);
        }
        let b = self.clj_body(body);
        self.scope.truncate(mark);
        let r = std::mem::replace(&mut self.clj.recur, saved).unwrap();
        self.clj.across_try = saved_across;
        if !r.used {
            return b;
        }
        Node::Loop(id, Box::new(b))
    }

    fn clj_recur(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        let (id, lambda) = match &self.clj.recur {
            Some(_) if self.clj.across_try => clj_err("Cannot recur across try", x),
            Some(r) => (r.id, r.lambda),
            None => clj_err("Can't recur here", x),
        };
        if lambda != self.cur {
            clj_err("Can't recur here", x);
        }
        let n = self.loops[id].vars.len();
        if args.len() != n {
            lisp_error(format!("Mismatched argument count to recur, expected: {n} args, got: {}", args.len()));
        }
        self.clj.recur.as_mut().unwrap().used = true;
        let args = self.clj_args(args);
        Node::Recur(id, args)
    }

    fn clj_letfn(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        let pairs = Self::binding_pairs(x, args.first().copied().unwrap_or(NIL));
        let mark = self.scope.len();
        let vars: Vec<VarId> = pairs.iter().map(|&(s, _)| self.clj_bind(s)).collect();
        let mut binds = Vec::new();
        for (&(s, f), &v) in pairs.iter().zip(&vars) {
            let n = self.clj(f);
            if lambda_of(&n).is_none() {
                clj_err("letfn* binding must be a fn", s);
            }
            binds.push((v, n));
        }
        let saved = self.clj.recur.take();
        let body = self.clj_body(&args[1..]);
        self.clj.recur = saved;
        self.scope.truncate(mark);
        Node::LetFn(binds, Box::new(body))
    }

    fn clj_fn(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        let mut rest = args;
        let mut name = None;
        if rest.first().is_some_and(|&n| c::is_symbol(n)) {
            name = Some(rest[0]);
            rest = &rest[1..];
        }
        let arities: Vec<(LispObj, Vec<LispObj>)> = if rest.first().is_some_and(|&p| coll::is_vector(p)) {
            vec![(rest[0], rest[1..].to_vec())]
        } else {
            rest.iter()
                .map(|&a| {
                    let a = if is_compound(a) { items(a) } else { Vec::new() };
                    if a.is_empty() || !coll::is_vector(a[0]) {
                        clj_err("Invalid fn* arity", x);
                    }
                    (a[0], a[1..].to_vec())
                })
                .collect()
        };
        if arities.is_empty() {
            clj_err("Invalid fn*", x);
        }
        let mark = self.scope.len();
        // A name with metadata :display-only (from defn) only names the
        // function; as in Clojure, defn does not bind it in the body.
        // A string :display-only (from deftype's invoke methods) is the
        // name itself, and the first argument is `this`: the code name is
        // [name], so arity errors do not count it.
        let display = name.map_or(NIL, |n| meta_get(c::sym_meta(n), "display-only"));
        let display_only = truthy(display);
        let self_var = name.filter(|_| !display_only).map(|n| self.clj_bind(n));
        // A defn'd function is named ns/name, as in Clojure's errors.
        let lname = match name {
            Some(_) if display.is_string() => heap::vector_of(&[display]),
            Some(n) if display_only => {
                let ns = c::public_ns_name(c::current_ns()).string_to_rust();
                crate::heap::string_node(&format!("{ns}/{}", c::sym_name(n).string_to_rust()))
            }
            _ => name.map_or(NIL, c::sym_name),
        };
        let id = self.clj_lambda(&arities, lname);
        self.scope.truncate(mark);
        let f = match self_var {
            None => Node::Lambda(id),
            Some(v) => Node::LetFn(vec![(v, Node::Lambda(id))], Box::new(Node::LexRef(v))),
        };
        self.with_form_meta(x, f)
    }

    /// Split a parameter vector into fixed parameters and the `&` one.
    fn parse_params(v: LispObj) -> (Vec<LispObj>, Option<LispObj>) {
        let items = items(v);
        let mut fixed = Vec::new();
        let mut rest = None;
        let mut i = 0;
        while i < items.len() {
            let p = items[i];
            if c::is_sym_named(p, "&") {
                if i + 2 != items.len() {
                    clj_err("Invalid parameter list", v);
                }
                rest = Some(items[i + 1]);
                break;
            }
            fixed.push(p);
            i += 1;
        }
        (fixed, rest)
    }

    fn clj_lambda(&mut self, arities: &[(LispObj, Vec<LispObj>)], name: LispObj) -> LambdaId {
        let id = self.push_lambda(name);
        let saved_cur = self.cur;
        self.cur = id;
        let saved_recur = self.clj.recur.take();
        let saved_across = std::mem::replace(&mut self.clj.across_try, false);
        let mut parsed: Vec<_> = arities
            .iter()
            .map(|(p, b)| (Self::parse_params(*p), b.clone()))
            .collect();
        if parsed.iter().filter(|((_, r), _)| r.is_some()).count() > 1 {
            clj_err("Can't have more than 1 variadic overload", NIL);
        }
        let mut seen = Vec::new();
        let variadic = parsed.iter().find(|((_, r), _)| r.is_some()).map(|((f, _), _)| f.len());
        for ((f, r), _) in &parsed {
            if r.is_none() {
                if seen.contains(&f.len()) {
                    clj_err("Can't have 2 overloads with same arity", NIL);
                }
                if variadic.is_some_and(|n| f.len() > n) {
                    clj_err("Can't have fixed arity function with more params than variadic function", NIL);
                }
                seen.push(f.len());
            }
        }
        // The variadic arity takes the counts no fixed one does.
        parsed.sort_by_key(|((_, r), _)| r.is_some());
        for ((fixed, rest), body) in parsed {
            let mark = self.scope.len();
            let req: Vec<VarId> = fixed.iter().map(|&p| self.new_var(p)).collect();
            let rest_var = rest.map(|r| self.new_var(r));
            let mut header = req.clone();
            let mut names = fixed;
            header.extend(rest_var);
            names.extend(rest);
            let body = self.loop_body(&header, &names, &body);
            self.scope.truncate(mark);
            self.lambdas[id].arities.push(Arity { req, rest: rest_var, body });
        }
        self.clj.recur = saved_recur;
        self.clj.across_try = saved_across;
        self.cur = saved_cur;
        id
    }

    /// `(try body* (catch type name handler*)* (finally cleanup*)?)`: a
    /// `Try` whose catch tests the thrown value against each type in
    /// turn and rethrows it if none matches.
    fn clj_try(&mut self, x: LispObj, args: &[LispObj]) -> Node {
        let mut body = Vec::new();
        let mut catches = Vec::new();
        let mut finally = None;
        for &f in args {
            let fv = if is_compound(f) { items(f) } else { Vec::new() };
            let head = fv.first().copied().unwrap_or(NIL);
            if c::is_sym_named(head, "catch") {
                if finally.is_some() || fv.len() < 3 || !c::is_symbol(fv[2]) {
                    clj_err("Invalid catch clause", f);
                }
                catches.push((fv[1], fv[2], fv[3..].to_vec()));
            } else if c::is_sym_named(head, "finally") {
                if finally.is_some() {
                    clj_err("Only one finally clause allowed in try expression", x);
                }
                finally = Some(fv[1..].to_vec());
            } else {
                if !catches.is_empty() || finally.is_some() {
                    clj_err("Only catch or finally clause can follow catch in try expression", x);
                }
                body.push(f);
            }
        }
        let saved_across = std::mem::replace(&mut self.clj.across_try, true);
        let protected = self.clj_body(&body);
        let catch = (!catches.is_empty()).then(|| {
            let cvar = self.new_var(NIL);
            let mut chain = Node::Call(kernel("throw"), vec![Node::LexRef(cvar)]);
            for (ty, e, h) in catches.iter().rev() {
                let mark = self.scope.len();
                let v = self.clj_bind(*e);
                let handler = self.clj_body(h);
                let node = if c::is_keyword(*ty) && c::lisp_str_is(ty.uref(c::KW_NAME), "default") {
                    handler
                } else {
                    let test = list_from(&[kernel_sym("instance?"), *ty, *e]);
                    let test = self.clj(test);
                    Node::If(Box::new(test), Box::new(handler), Box::new(chain))
                };
                self.scope.truncate(mark);
                chain = Node::Let(vec![(v, Node::LexRef(cvar))], Box::new(node));
            }
            (cvar, Box::new(chain))
        });
        let finally = finally.map(|f| Box::new(self.clj_body(&f)));
        self.clj.across_try = saved_across;
        Node::Try { body: Box::new(protected), catch, finally }
    }
}

/// The lambda a `LetFn` init makes.
pub fn lambda_of(n: &Node) -> Option<LambdaId> {
    match n {
        Node::Lambda(id) => Some(*id),
        Node::LetFn(b, body) if b.len() == 1 && matches!(**body, Node::LexRef(v) if v == b[0].0) => lambda_of(&b[0].1),
        _ => None,
    }
}

/// Are the keys of map literal items `kvs` keywords, strings or fixnums,
/// all different?
fn distinct_constant_keys(kvs: &[LispObj]) -> bool {
    let keys: Vec<LispObj> = kvs.iter().step_by(2).copied().collect();
    keys.iter().all(|&k| c::is_keyword(k) || k.is_string() || k.is_fixnum()) && coll::find_duplicate(&keys).is_none()
}

/// `(.method obj args*)` -> `(. obj method args*)`, `(.-field obj)` ->
/// `(. obj -field)`, `(Type. args*)` -> `(new Type args*)`.
fn interop_sugar(x: &[LispObj], form: LispObj) -> Option<LispObj> {
    let head = x[0];
    if !c::is_symbol(head) {
        return None;
    }
    let name = c::sym_name(head).string_to_rust();
    if name.len() > 1 && name.ends_with('.') && !name.starts_with('.') {
        let t = c::symbol(c::sym_ns(head), string_node(&name[..name.len() - 1]));
        return Some(list_of(&[csym("new"), t], &x[1..]));
    }
    if c::sym_ns(head) != NIL {
        return None;
    }
    if name.len() > 1 && name.starts_with('.') && name != ".." {
        if x.len() < 2 {
            clj_err("Malformed member expression, expecting (.member target ...)", form);
        }
        let member = csym(&name[1..]);
        return Some(list_of(&[csym("."), x[1], member], &x[2..]));
    }
    None
}
