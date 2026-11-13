//! Compiler front end: the IR and the state shared by the Clojure front
//! end (`clj.rs`), which converts forms to it.
//!
//! Macros are expanded and variables resolved into an IR first, which
//! records which variables each lambda captures. Clojure locals are
//! immutable, so a closure holds copies of the captured values (a flat
//! environment, no boxes); `LetFn` patches the environments of mutually
//! recursive functions after they are all allocated.

use crate::error::lisp_error;
use crate::object::*;
use crate::runtime::lisp_call;

pub mod clj;

pub type VarId = usize;
pub type LambdaId = usize;
pub type LoopId = usize;

pub struct Var {
    pub name: LispObj,
    pub lambda: LambdaId,
}

/// The target of `recur`: its variables, set before jumping back.
pub struct LoopInfo {
    pub vars: Vec<VarId>,
}

pub struct LambdaInfo {
    pub parent: Option<LambdaId>,
    pub name: LispObj,
    /// Variables of outer lambdas referenced here (or by inner lambdas):
    /// the closure environment, in this order.
    pub free: Vec<VarId>,
    /// The arities, each entered directly for the argument counts it
    /// takes: fixed ones exactly, a variadic one (at most one) any
    /// count not taken by a fixed one from its number of parameters up.
    pub arities: Vec<Arity>,
}

pub struct Arity {
    pub req: Vec<VarId>,
    /// The rest parameter, a list of the arguments past `req`.
    pub rest: Option<VarId>,
    pub body: Node,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InlineOp {
    Eq,
    /// `k/nil?`.
    IsNil,
    /// `k/not`: `true` for nil and `false`.
    Not,
    Add2,
    Sub2,
    NumEq2,
    Lt2,
    Gt2,
    Le2,
    Ge2,
    Mul2,
    /// `k/boolean`: `false` for nil and `false`, else `true`.
    Boolean,
    Aget,
    Alength,
    /// `(.-f x)`: object, field name, per-site cache (a simple-vector
    /// `[type index]` of the last instance type and its field index).
    GetField,
    /// `k/method`, `k/implements`, `k/keyword?`.
    Method,
    Implements,
    KeywordP,
    ProtocolMarker,
    TypeOf,
    /// `k/identical?` (a Clojure boolean).
    Identical,
    /// `(:k m d)`: map, keyword, default, the PersistentArrayMap type,
    /// index of its `arr` field, and the general form (a call of `get`).
    /// Map and default must be simple (variables or constants).
    KwGet,
    /// `(new T args...)`: type, arguments, and a per-site cache (a simple-vector
    /// `[type]` of the last type seen that a plain field fill builds).
    New,
    /// `(k/array x...)`: a new array of the arguments.
    Array,
}

pub enum Callee {
    /// Kernel function (`prims::PRIMS` index).
    Kernel(usize),
    Lex(VarId),
    Expr(Box<Node>),
}

pub enum Node {
    Const(LispObj),
    LexRef(VarId),
    /// The value of a Clojure var (an error if unbound).
    VarRef(LispObj),
    /// Set the value of a var (its innermost binding, else its root).
    VarSet(LispObj, Box<Node>),
    /// `if`: the test is false when nil or `false`.
    If(Box<Node>, Box<Node>, Box<Node>),
    /// Cell `n` of a uvector (no type check): Clojure instance fields.
    UvRef(Box<Node>, usize),
    UvSet(Box<Node>, usize, Box<Node>),
    Do(Vec<Node>),
    /// Bind each variable in turn, then evaluate the body.
    Let(Vec<(VarId, Node)>, Box<Node>),
    /// `letfn*` (and a named `fn`, bound to itself): the variables are in
    /// scope in all the inits, which are functions (`Lambda`s, or a
    /// nested `LetFn` of one function returning it). Each function's
    /// copies of the variables are filled in once all are made.
    LetFn(Vec<(VarId, Node)>, Box<Node>),
    Lambda(LambdaId),
    Call(Callee, Vec<Node>),
    Inline(InlineOp, Vec<Node>),
    /// The body, which `Recur` of the same id (in the same lambda, not
    /// inside a `Try`) restarts after setting the loop's variables.
    Loop(LoopId, Box<Node>),
    Recur(LoopId, Vec<Node>),
    /// `try`: on a throw out of `body`, `catch` (if any) binds the thrown
    /// value and evaluates its handler; `finally` runs however it exits.
    Try {
        body: Box<Node>,
        catch: Option<(VarId, Box<Node>)>,
        finally: Option<Box<Node>>,
    },
}

enum Scope {
    /// Clojure local: name (a string) and variable.
    CljVar(LispObj, VarId),
    /// Clojure instance field as a local: name, variable holding the
    /// instance, cell, mutable.
    CljField(LispObj, VarId, usize, bool),
}

pub struct Frontend {
    pub vars: Vec<Var>,
    pub lambdas: Vec<LambdaInfo>,
    pub loops: Vec<LoopInfo>,
    scope: Vec<Scope>,
    cur: LambdaId,
    clj: clj::State,
}

impl Default for Frontend {
    fn default() -> Self {
        Self::new()
    }
}

impl Frontend {
    pub fn new() -> Frontend {
        Frontend {
            vars: Vec::new(),
            lambdas: Vec::new(),
            loops: Vec::new(),
            scope: Vec::new(),
            cur: 0,
            clj: clj::State::default(),
        }
    }

    fn new_var(&mut self, name: LispObj) -> VarId {
        self.vars.push(Var {
            name,
            lambda: self.cur,
        });
        self.vars.len() - 1
    }

    /// Note a reference to `id` from the current lambda: if it belongs to
    /// an outer lambda it joins the free lists (closure environments)
    /// of the lambdas in between.
    fn reference(&mut self, id: VarId) {
        let owner = self.vars[id].lambda;
        let mut l = self.cur;
        while l != owner {
            if !self.lambdas[l].free.contains(&id) {
                self.lambdas[l].free.push(id);
            }
            l = self.lambdas[l].parent.expect("variable owner not found");
        }
    }
}
