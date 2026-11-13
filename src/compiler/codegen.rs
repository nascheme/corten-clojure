//! Code generator: IR -> x86-64 machine code (via iced-x86), one compiled
//! code object per lambda. See `runtime.rs` for the calling convention.
//!
//! Every value is computed into RAX. Frame layout:
//!   [rbp+16+8i] argument i, [rbp+8] return address, [rbp] saved rbp,
//!   [rbp-8] closure environment (R13 on entry), [rbp-16] argument count,
//!   [rbp-8s] slot s >= 3: locals, temporaries and dynamic frames.
//! The compiler tracks the stack depth (in cells below RBP) statically.
//! Constants are loaded RIP-relative from a literal area after the code.

use std::collections::HashMap;

use iced_x86::code_asm::*;

use super::frontend::*;
use crate::error::lisp_error;
use crate::heap;
use crate::object::*;
use crate::qv_index::*;
use crate::runtime::*;

type R<T = ()> = Result<T, IcedError>;

const NIL_IMM: i32 = NIL.0 as i32;
const FALSE_IMM: i32 = FALSE.0 as i32;
const TRUE_IMM: i32 = TRUE.0 as i32;

fn qvd(i: usize) -> i32 {
    (i * CELL) as i32
}

fn fix(n: i64) -> i32 {
    (n << 3) as i32
}

/// The 32-bit half of `r` (a `mov` to it zero-extends).
fn reg32(r: AsmRegister64) -> Option<AsmRegister32> {
    [(rax, eax), (rcx, ecx), (rdx, edx), (rbx, ebx), (rsi, esi), (rdi, edi), (r8, r8d), (r9, r9d), (r10, r10d), (r11, r11d)]
        .into_iter()
        .find_map(|(r64, r32)| (r64 == r).then_some(r32))
}

/// The value of `n` is always `true` or `false`.
fn returns_boolean(n: &Node) -> bool {
    use InlineOp::*;
    matches!(
        n,
        Node::Inline(
            Eq | IsNil | Not | Boolean | NumEq2 | Lt2 | Gt2 | Le2 | Ge2 | Implements | KeywordP | Identical,
            _
        )
    )
}

/// Where a variable lives.
#[derive(Clone, Copy)]
enum Loc {
    Arg(usize),
    Slot(usize),
    Env(usize),
}

pub static DISASSEMBLE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

struct FnGen<'a> {
    fe: &'a Frontend,
    lambda: LambdaId,
    a: CodeAssembler,
    depth: usize,
    locs: HashMap<VarId, Loc>,
    literals: Vec<(LispObj, CodeLabel)>,
    /// Loops being generated: restart label and stack depth.
    loops: HashMap<LoopId, (CodeLabel, usize)>,
    last_label_at: Option<usize>,
}

/// Compile lambda `id` (and, recursively, its inner lambdas) into a
/// compiled code object.
pub fn compile_lambda(fe: &Frontend, id: LambdaId) -> LispObj {
    let mut g = FnGen {
        fe,
        lambda: id,
        a: CodeAssembler::new(64).unwrap(),
        depth: 0,
        locs: HashMap::new(),
        literals: Vec::new(),
        loops: HashMap::new(),
        last_label_at: None,
    };
    for (i, &v) in fe.lambdas[id].free.iter().enumerate() {
        g.locs.insert(v, Loc::Env(i));
    }
    match g.gen_lambda().and_then(|_| g.finish()) {
        Ok(code) => code,
        Err(e) => lisp_error(format!("Compiler error: {e}")),
    }
}

impl FnGen<'_> {
    // -----------------------------------------------------------------------
    // Small helpers

    fn slot(s: usize) -> AsmMemoryOperand {
        qword_ptr(rbp - (s * CELL) as i32)
    }

    /// Offset of element `i` from a tagged simple-vector pointer.
    const fn sv_elt(i: usize) -> i32 {
        ((ARRAY_SIMPLE_VECTOR_START + i) * CELL) as i32 - UVECTOR_TAG as i32
    }

    fn label(&mut self) -> CodeLabel {
        self.a.create_label()
    }

    /// Define label `l` at the current position (iced allows only one
    /// label per instruction, so pad with a NOP when needed).
    fn here(&mut self, l: &mut CodeLabel) -> R {
        let n = self.a.instructions().len();
        if self.last_label_at == Some(n) {
            self.a.nop()?;
        }
        self.last_label_at = Some(self.a.instructions().len());
        self.a.set_label(l)
    }

    fn literal(&mut self, x: LispObj) -> CodeLabel {
        if let Some(&(_, l)) = self.literals.iter().find(|(o, _)| *o == x) {
            return l;
        }
        let l = self.a.create_label();
        self.literals.push((x, l));
        l
    }

    /// Load constant `x` into `r`.
    fn load_const(&mut self, r: AsmRegister64, x: LispObj) -> R {
        let v = x.raw() as i64;
        if !(x.is_fixnum() || x.is_immediate()) {
            let l = self.literal(x);
            self.a.mov(r, qword_ptr(l))
        } else if let (Ok(u), Some(r32)) = (u32::try_from(v), reg32(r)) {
            self.a.mov(r32, u)
        } else if v == v as i32 as i64 {
            // Sign-extended imm32 (`mov(r, i64)` would use an imm64).
            let r: iced_x86::Register = r.into();
            self.a.add_instruction(iced_x86::Instruction::with2(iced_x86::Code::Mov_rm64_imm32, r, v as i32)?)
        } else {
            let l = self.literal(x);
            self.a.mov(r, qword_ptr(l))
        }
    }

    fn push_rax(&mut self) -> R {
        self.depth += 1;
        self.a.push(rax)
    }

    fn pop_to(&mut self, r: AsmRegister64) -> R {
        self.depth -= 1;
        self.a.pop(r)
    }

    fn drop_cells(&mut self, n: usize) -> R {
        if n > 0 {
            self.depth -= n;
            self.a.add(rsp, (n * CELL) as i32)?;
        }
        Ok(())
    }

    /// Call Rust helper `h` (arguments already in rdi, rsi, rdx, ...).
    fn helper(&mut self, h: Helper) -> R {
        self.a.mov(eax, h as u32)?;
        self.a.call(qword_ptr(r15 + qvd(HELPER_STUB_Index)))
    }

    /// RAX = `true` if the flags say equal (`equal`) or not equal (not
    /// `equal`), else `false`. Clobbers RDX.
    fn bool_from_flags(&mut self, equal: bool) -> R {
        self.a.mov(eax, FALSE_IMM as u32)?;
        self.a.mov(edx, TRUE_IMM as u32)?;
        if equal { self.a.cmove(eax, edx) } else { self.a.cmovne(eax, edx) }
    }

    /// Set ZF if RAX is falsy (nil or false). Clobbers RDX.
    fn test_falsy(&mut self) -> R {
        self.a.mov(rdx, rax)?;
        self.a.or(rdx, FALSY_BIT as i32)?;
        self.a.cmp(rdx, FALSE_IMM)
    }

    // -----------------------------------------------------------------------
    // Variables

    /// Load the variable's value into `r`.
    fn load_loc(&mut self, r: AsmRegister64, v: VarId) -> R {
        match self.locs[&v] {
            Loc::Arg(i) => self.a.mov(r, qword_ptr(rbp + (16 + i * CELL) as i32)),
            Loc::Slot(s) => self.a.mov(r, Self::slot(s)),
            Loc::Env(i) => {
                self.a.mov(r, qword_ptr(rbp - 8))?;
                self.a.mov(
                    r,
                    qword_ptr(r + ((ARRAY_SIMPLE_VECTOR_START + i) * CELL) as i32 - 5),
                )
            }
        }
    }

    fn load_var(&mut self, v: VarId) -> R {
        self.load_loc(rax, v)
    }

    /// Store RAX into a variable of this lambda.
    fn store_var(&mut self, v: VarId) -> R {
        match self.locs[&v] {
            Loc::Arg(i) => self.a.mov(qword_ptr(rbp + (16 + i * CELL) as i32), rax),
            Loc::Slot(s) => self.a.mov(Self::slot(s), rax),
            Loc::Env(_) => unreachable!("closure variables are immutable"),
        }
    }

    /// Bind lexical `v` to RAX in a new slot.
    fn bind_lex_new_slot(&mut self, v: VarId) -> R {
        self.push_rax()?;
        self.locs.insert(v, Loc::Slot(self.depth));
        Ok(())
    }

    /// The value cell of var `v` (load it into `r` first).
    fn var_value(r: AsmRegister64) -> AsmMemoryOperand {
        qword_ptr(r + (crate::clj::VAR_VAL * CELL) as i32 - UVECTOR_TAG as i32)
    }

    /// Push a catch-type frame (catch or unwind-protect) with the tag in
    /// RAX and landing address `landing`.
    fn push_catch_frame(&mut self, kind: i64, landing: CodeLabel) -> R {
        self.a.push(rbp)?;
        self.a.lea(rdx, qword_ptr(landing))?;
        self.a.push(rdx)?;
        self.a.push(rax)?;
        self.a.push(fix(kind))?;
        self.a.push(qword_ptr(r15 + qvd(DYN_FRAME_Index)))?;
        self.a.mov(qword_ptr(r15 + qvd(DYN_FRAME_Index)), rsp)?;
        self.depth += DF_CELLS;
        Ok(())
    }

    /// Unlink and pop the dynamic frame at the top of the stack (keeps
    /// RAX).
    fn pop_dyn_frame(&mut self) -> R {
        self.a.mov(rdx, qword_ptr(rsp))?;
        self.a.mov(qword_ptr(r15 + qvd(DYN_FRAME_Index)), rdx)?;
        self.drop_cells(DF_CELLS)
    }

    fn reset_rsp(&mut self, depth: usize) -> R {
        self.a.lea(rsp, Self::slot(depth))
    }

    // -----------------------------------------------------------------------
    // Lambda

    /// The function's entry: frame, stack check, then a dispatch on the
    /// argument count (RCX) to the arity taking it. A count with
    /// `REST_READY` (from `apply`) enters the variadic arity with the rest
    /// seq as its last argument.
    fn gen_lambda(&mut self) -> R {
        let info = &self.fe.lambdas[self.lambda];
        let mut wrong = self.label();

        self.a.push(rbp)?;
        self.a.mov(rbp, rsp)?;
        self.a.push(r13)?;
        self.a.push(rcx)?;
        self.depth = 2;
        // Deep recursion: an error, not a crash (runtime::check_stack).
        let mut deep = self.label();
        let mut entered = self.label();
        self.a.cmp(rsp, qword_ptr(r15 + qvd(STACK_LIMIT_Index)))?;
        self.a.jb(deep)?;
        self.here(&mut entered)?;

        let mut entries: Vec<CodeLabel> = info.arities.iter().map(|_| self.a.create_label()).collect();
        let mut rest_ready = self.label();
        if info.arities.iter().any(|a| a.rest.is_some()) {
            self.a.cmp(rcx, i32::MAX)?;
            self.a.ja(rest_ready)?;
        }
        for (a, &l) in info.arities.iter().zip(&entries) {
            self.a.cmp(rcx, a.req.len() as i32)?;
            if a.rest.is_some() {
                self.a.jae(l)?;
            } else if info.arities.len() == 1 {
                self.a.jne(wrong)?;
            } else {
                self.a.je(l)?;
            }
        }
        if info.arities.len() > 1 || info.arities[0].rest.is_some() {
            self.a.jmp(wrong)?;
        }
        for (a, l) in info.arities.iter().zip(&mut entries) {
            self.here(l)?;
            self.gen_arity(a, &mut rest_ready)?;
        }

        self.here(&mut deep)?;
        self.helper(Helper::StackOverflow)?;
        self.a.mov(rcx, qword_ptr(rbp - 16))?;
        self.a.jmp(entered)?;

        self.here(&mut wrong)?;
        self.a.mov(rdi, rcx)?;
        self.load_const(rsi, info.name)?;
        self.helper(Helper::WrongArgs)?;
        self.a.ud2()?;
        Ok(())
    }

    /// The body of arity `a` (entered with its argument count), up to the
    /// return. A variadic arity also defines `rest_ready`, where the rest
    /// seq is the argument after the fixed ones.
    fn gen_arity(&mut self, a: &Arity, rest_ready: &mut CodeLabel) -> R {
        self.depth = 2;
        let nreq = a.req.len();
        for (i, &v) in a.req.iter().enumerate() {
            self.locs.insert(v, Loc::Arg(i));
        }
        if let Some(b) = a.rest {
            let mut have = self.label();
            self.a.lea(rdi, qword_ptr(rbp + (16 + nreq * CELL) as i32))?;
            self.a.mov(rsi, rcx)?;
            self.a.sub(rsi, nreq as i32)?;
            self.helper(Helper::MakeRest)?;
            self.a.jmp(have)?;
            self.here(rest_ready)?;
            self.a.mov(rax, qword_ptr(rbp + (16 + nreq * CELL) as i32))?;
            self.here(&mut have)?;
            self.bind_lex_new_slot(b)?;
        }
        self.emit(&a.body)?;
        self.a.mov(rsp, rbp)?;
        self.a.pop(rbp)?;
        self.a.ret()
    }

    /// Assemble and build the code object.
    fn finish(mut self) -> R<LispObj> {
        let lits = std::mem::take(&mut self.literals);
        let mut first = None;
        for (x, mut l) in lits.iter().copied() {
            self.a.set_label(&mut l)?;
            self.a.dq(&[x.raw() as u64])?;
            first.get_or_insert(l);
        }
        let r = self
            .a
            .assemble_options(0, iced_x86::BlockEncoderOptions::RETURN_NEW_INSTRUCTION_OFFSETS)?;
        let lit_off = match first {
            Some(l) => r.label_ip(&l)? as usize,
            None => r.inner.code_buffer.len(),
        };
        let bytes = &r.inner.code_buffer;
        let total = CODE_START + bytes.len();
        let cells = total.div_ceil(CELL);
        let code = heap::alloc_uvector(cells - 1, COMPILED_CODE_TYPE);
        code.uset(
            CODE_LITERALS_OFFSET,
            LispObj::fixnum((CODE_START + lit_off) as i64),
        );
        code.uset(CODE_NUM_LITERALS, LispObj::fixnum(lits.len() as i64));
        code.uset(CODE_NAME, self.fe.lambdas[self.lambda].name);
        code.uset(CODE_INFO, NIL);
        let rest = self.fe.lambdas[self.lambda].arities.iter().find_map(|a| a.rest.map(|_| a.req.len()));
        code.uset(CODE_REST_ARITY, rest.map_or(NIL, |n| LispObj::fixnum(n as i64)));
        unsafe {
            let dst = (code.uvector_ptr() as *mut u8).add(CODE_START);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), dst, bytes.len());
        }
        if DISASSEMBLE.load(std::sync::atomic::Ordering::Relaxed) {
            disassemble(code, lit_off);
        }
        crate::perfmap::code_created(code);
        Ok(code)
    }

    // -----------------------------------------------------------------------
    // Nodes

    /// Generate `n` into RAX.
    fn emit(&mut self, n: &Node) -> R {
        match n {
            Node::Const(x) => {
                self.load_const(rax, *x)
            }
            Node::LexRef(v) => {
                self.load_var(*v)
            }
            Node::VarRef(sym) => {
                let mut ok = self.label();
                self.load_const(rax, *sym)?;
                self.a.mov(rax, Self::var_value(rax))?;
                self.a.cmp(rax, SYMBOL_UNBOUND.raw() as i32)?;
                self.a.jne(ok)?;
                self.load_const(rdi, *sym)?;
                self.helper(Helper::UnboundVariable)?;
                self.here(&mut ok)
            }
            Node::VarSet(sym, val) => {
                self.emit(val)?;
                self.load_const(rdx, *sym)?;
                self.a.mov(Self::var_value(rdx), rax)
            }
            Node::If(c, t, e) => {
                let mut els = self.label();
                let mut end = self.label();
                self.gen_test(c, els)?;
                let d = self.depth;
                self.emit(t)?;
                self.a.jmp(end)?;
                self.here(&mut els)?;
                self.depth = d;
                self.emit(e)?;
                self.here(&mut end)
            }
            Node::UvRef(x, cell) => {
                self.emit(x)?;
                self.a.mov(rax, qword_ptr(rax + (*cell * CELL) as i32 - UVECTOR_TAG as i32))
            }
            Node::UvSet(x, cell, v) => {
                self.emit(x)?;
                self.push_rax()?;
                self.emit(v)?;
                self.pop_to(rdx)?;
                self.a.mov(qword_ptr(rdx + (*cell * CELL) as i32 - UVECTOR_TAG as i32), rax)
            }
            Node::Do(v) => {
                if v.is_empty() {
                    return self.load_const(rax, NIL);
                }
                for f in v {
                    self.emit(f)?;
                }
                Ok(())
            }
            Node::Let(binds, body) => self.gen_let(binds, body),
            Node::LetFn(binds, body) => self.gen_letfn(binds, body),
            Node::Lambda(id) => {
                self.gen_closure(*id)
            }
            Node::Call(callee, args) => self.gen_call(callee, args),
            Node::Inline(op, args) => {
                self.gen_inline(*op, args)
            }
            Node::Loop(id, body) => {
                let mut top = self.label();
                self.here(&mut top)?;
                self.loops.insert(*id, (top, self.depth));
                self.emit(body)
            }
            Node::Recur(id, args) => self.gen_recur(*id, args),
            Node::Try { body, catch, finally } => self.gen_try(body, catch.as_ref(), finally.as_deref()),
        }
    }

    fn gen_let(&mut self, binds: &[(VarId, Node)], body: &Node) -> R {
        let d0 = self.depth;
        for (v, init) in binds {
            self.emit(init)?;
            self.bind_lex_new_slot(*v)?;
        }
        self.emit(body)?;
        let n = self.depth - d0;
        self.drop_cells(n)
    }

    fn gen_letfn(&mut self, binds: &[(VarId, Node)], body: &Node) -> R {
        let d0 = self.depth;
        for (v, _) in binds {
            self.load_const(rax, NIL)?;
            self.bind_lex_new_slot(*v)?;
        }
        for (v, init) in binds {
            self.emit(init)?;
            self.store_var(*v)?;
        }
        // Fill in each function's copies of the variables.
        let vars: Vec<VarId> = binds.iter().map(|b| b.0).collect();
        for (v, init) in binds {
            let id = super::frontend::clj::lambda_of(init).expect("letfn binds functions");
            for (i, fv) in self.fe.lambdas[id].free.iter().enumerate() {
                if vars.contains(fv) {
                    self.load_loc(rdx, *v)?;
                    self.a.mov(rdx, qword_ptr(rdx + (FUNCTION_ENVIRONMENT * CELL) as i32 - UVECTOR_TAG as i32))?;
                    self.load_loc(rax, *fv)?;
                    let off = (ARRAY_SIMPLE_VECTOR_START + i) * CELL;
                    self.a.mov(qword_ptr(rdx + off as i32 - UVECTOR_TAG as i32), rax)?;
                }
            }
        }
        self.emit(body)?;
        let n = self.depth - d0;
        self.drop_cells(n)
    }

    /// Evaluate the new values of the loop's variables, set them, and jump
    /// back to the start of the loop.
    fn gen_recur(&mut self, id: LoopId, args: &[Node]) -> R {
        for x in args {
            self.emit(x)?;
            self.push_rax()?;
        }
        for &v in self.fe.loops[id].vars.iter().rev() {
            self.pop_to(rax)?;
            self.store_var(v)?;
        }
        let (top, depth) = self.loops[&id];
        if self.depth != depth {
            self.reset_rsp(depth)?;
        }
        self.a.jmp(top)
    }

    fn gen_closure(&mut self, id: LambdaId) -> R {
        let code = compile_lambda(self.fe, id);
        let free = &self.fe.lambdas[id].free;
        if free.is_empty() {
            let f = make_function(code, NIL);
            return self.load_const(rax, f);
        }
        let k = free.len();
        for &v in free.clone().iter() {
            self.load_loc(rax, v)?;
            self.push_rax()?;
        }
        self.load_const(rdi, code)?;
        self.a.mov(rsi, rsp)?;
        self.a.mov(edx, k as u32)?;
        self.helper(Helper::MakeClosure)?;
        self.drop_cells(k)
    }

    fn gen_call(&mut self, callee: &Callee, args: &[Node]) -> R {
        if let Callee::Expr(f) = callee
            && let Node::VarRef(sym) = **f
        {
            return self.gen_var_call(sym, args);
        }
        let n = args.len();
        let mut fslot = None;
        if let Callee::Expr(f) = callee {
            self.emit(f)?;
            self.push_rax()?;
            fslot = Some(self.depth);
        }
        if n > 0 {
            self.a.sub(rsp, (n * CELL) as i32)?;
            self.depth += n;
        }
        let d = self.depth;
        for (i, x) in args.iter().enumerate() {
            self.emit(x)?;
            self.a.mov(Self::slot(d - i), rax)?;
        }
        self.a.mov(ecx, n as u32)?;
        match callee {
            Callee::Kernel(idx) => {
                self.a.mov(edi, *idx as u32)?;
                self.a.call(qword_ptr(r15 + qvd(KERNEL_CALL_STUB_Index)))?;
            }
            Callee::Lex(v) => {
                self.load_var(*v)?;
                self.a.call(qword_ptr(r15 + qvd(FUNCALL_STUB_Index)))?;
            }
            Callee::Expr(_) => {
                self.a.mov(rax, Self::slot(fslot.unwrap()))?;
                self.a.call(qword_ptr(r15 + qvd(FUNCALL_STUB_Index)))?;
            }
        }
        self.drop_cells(n + fslot.map_or(0, |_| 1))
    }

    /// A call of the value of global `sym` (a Clojure var): a compiled
    /// function is entered directly, anything else goes through the
    /// funcall stub. The arguments are evaluated before the variable.
    fn gen_var_call(&mut self, sym: LispObj, args: &[Node]) -> R {
        let n = args.len();
        let u = UVECTOR_TAG as i32;
        let mut slow = self.label();
        let mut call = self.label();
        let mut done = self.label();
        self.gen_stack_args(args)?;
        self.load_const(rax, sym)?;
        self.a.mov(rax, Self::var_value(rax))?;
        self.a.mov(edx, eax)?;
        self.a.and(edx, 7)?;
        self.a.cmp(edx, u)?;
        self.a.jne(slow)?;
        self.a.mov(rdx, qword_ptr(rax - u))?;
        self.a.and(edx, 0x1f << 3)?;
        self.a.cmp(edx, (FUNCTION_TYPE << 3) as i32)?;
        self.a.jne(call)?;
        // As the funcall stub.
        self.a.mov(r13, qword_ptr(rax + (FUNCTION_ENVIRONMENT * CELL) as i32 - u))?;
        self.a.mov(rdx, qword_ptr(rax + (FUNCTION_ADDRESS * CELL) as i32 - u))?;
        self.a.add(rdx, CODE_START as i32 - u)?;
        self.a.mov(ecx, n as u32)?;
        self.a.call(rdx)?;
        self.a.jmp(done)?;
        self.here(&mut slow)?;
        self.a.cmp(rax, SYMBOL_UNBOUND.raw() as i32)?;
        self.a.jne(call)?;
        self.load_const(rdi, sym)?;
        self.helper(Helper::UnboundVariable)?;
        self.here(&mut call)?;
        self.a.mov(ecx, n as u32)?;
        self.a.call(qword_ptr(r15 + qvd(FUNCALL_STUB_Index)))?;
        self.here(&mut done)?;
        self.drop_cells(n)
    }

    /// Emit test `c`, jumping to `els` when it is false (nil or
    /// `false`) and falling through otherwise.
    fn gen_test(&mut self, c: &Node, els: CodeLabel) -> R {
        match c {
            Node::Inline(InlineOp::Boolean, a) => self.gen_test(&a[0], els),
            Node::Inline(InlineOp::IsNil, a) => {
                self.emit(&a[0])?;
                self.a.cmp(rax, NIL_IMM)?;
                self.a.jne(els)
            }
            Node::Inline(InlineOp::Eq, a) => {
                self.gen_eq_flags(a)?;
                self.a.jne(els)
            }
            Node::Inline(InlineOp::Not, a) => {
                let mut thn = self.label();
                self.emit(&a[0])?;
                self.a.cmp(rax, NIL_IMM)?;
                self.a.je(thn)?;
                self.a.cmp(rax, FALSE_IMM)?;
                self.a.jne(els)?;
                self.here(&mut thn)
            }
            c => {
                self.emit(c)?;
                // Two compare-and-branch pairs (each fuses into one uop)
                // measure faster than `or` and one compare.
                if !returns_boolean(c) {
                    self.a.cmp(rax, NIL_IMM)?;
                    self.a.je(els)?;
                }
                self.a.cmp(rax, FALSE_IMM)?;
                self.a.je(els)
            }
        }
    }

    /// Set ZF if the two arguments of `k/eq` are the same object.
    fn gen_eq_flags(&mut self, args: &[Node]) -> R {
        let imm = |n: &Node| match n {
            Node::Const(x) if x.is_fixnum() || x.is_immediate() => i32::try_from(x.raw() as i64).ok(),
            _ => None,
        };
        if let Some(k) = imm(&args[1]) {
            self.emit(&args[0])?;
            self.a.cmp(rax, k)
        } else if let Some(k) = imm(&args[0]) {
            self.emit(&args[1])?;
            self.a.cmp(rax, k)
        } else {
            self.gen_two_args(args)?;
            self.a.cmp(rdi, rsi)
        }
    }

    fn gen_two_args(&mut self, args: &[Node]) -> R {
        self.emit(&args[0])?;
        self.push_rax()?;
        self.emit(&args[1])?;
        self.a.mov(rsi, rax)?;
        self.pop_to(rdi)
    }

    fn gen_inline(&mut self, op: InlineOp, args: &[Node]) -> R {
        match op {
            InlineOp::Eq => {
                self.gen_eq_flags(args)?;
                self.bool_from_flags(true)
            }
            InlineOp::IsNil => {
                self.emit(&args[0])?;
                self.a.cmp(rax, NIL_IMM)?;
                self.bool_from_flags(true)
            }
            InlineOp::Not => {
                self.emit(&args[0])?;
                self.test_falsy()?;
                self.bool_from_flags(true)
            }
            InlineOp::Add2
            | InlineOp::Sub2
            | InlineOp::NumEq2
            | InlineOp::Lt2
            | InlineOp::Gt2
            | InlineOp::Le2
            | InlineOp::Ge2 => {
                self.gen_two_args(args)?;
                let h = match op {
                    InlineOp::Add2 => Helper::Add2,
                    InlineOp::Sub2 => Helper::Sub2,
                    InlineOp::NumEq2 => Helper::NumEq2,
                    InlineOp::Lt2 => Helper::Lt2,
                    InlineOp::Gt2 => Helper::Gt2,
                    InlineOp::Le2 => Helper::Le2,
                    _ => Helper::Ge2,
                };
                // Two fixnums (tag 0) are done inline; anything else, or
                // an overflow, goes to the helper.
                let mut slow = self.label();
                let mut done = self.label();
                self.a.mov(rax, rdi)?;
                self.a.or(rax, rsi)?;
                self.a.test(al, 7)?;
                self.a.jnz(slow)?;
                match op {
                    InlineOp::Add2 | InlineOp::Sub2 => {
                        self.a.mov(rax, rdi)?;
                        if op == InlineOp::Add2 {
                            self.a.add(rax, rsi)?;
                        } else {
                            self.a.sub(rax, rsi)?;
                        }
                        self.a.jo(slow)?;
                    }
                    _ => {
                        self.a.mov(eax, FALSE_IMM as u32)?;
                        self.a.mov(edx, TRUE_IMM as u32)?;
                        self.a.cmp(rdi, rsi)?;
                        match op {
                            InlineOp::NumEq2 => self.a.cmove(eax, edx)?,
                            InlineOp::Lt2 => self.a.cmovl(eax, edx)?,
                            InlineOp::Gt2 => self.a.cmovg(eax, edx)?,
                            InlineOp::Le2 => self.a.cmovle(eax, edx)?,
                            _ => self.a.cmovge(eax, edx)?,
                        }
                    }
                }
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                let mut call = self.label();
                self.gen_float_op(op, done, &mut call)?;
                self.here(&mut call)?;
                self.helper(h)?;
                self.here(&mut done)
            }
            InlineOp::Mul2 => {
                self.gen_two_args(args)?;
                let mut slow = self.label();
                let mut done = self.label();
                self.a.mov(rax, rdi)?;
                self.a.or(rax, rsi)?;
                self.a.test(al, 7)?;
                self.a.jnz(slow)?;
                self.a.mov(rax, rdi)?;
                self.a.sar(rax, 3)?;
                self.a.imul_2(rax, rsi)?;
                self.a.jo(slow)?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                let mut call = self.label();
                self.gen_float_op(op, done, &mut call)?;
                self.here(&mut call)?;
                self.helper(Helper::Mul2)?;
                self.here(&mut done)
            }
            InlineOp::Boolean => {
                self.emit(&args[0])?;
                self.test_falsy()?;
                self.bool_from_flags(false)
            }
            InlineOp::Aget => {
                self.gen_two_args(args)?;
                let mut slow = self.label();
                let mut done = self.label();
                self.a.test(sil, 7)?;
                self.a.jnz(slow)?;
                // RDX = data cells of the array (tagged as a simple vector
                // base), RCX = length, both checked; else the helper.
                self.gen_array_storage(&mut slow)?;
                self.a.cmp(rsi, rcx)?;
                self.a.jae(slow)?;
                self.a.mov(rax, qword_ptr(rdx + rsi + (ARRAY_SIMPLE_VECTOR_START * CELL) as i32 - UVECTOR_TAG as i32))?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::Aget)?;
                self.here(&mut done)
            }
            InlineOp::Alength => {
                self.emit(&args[0])?;
                self.a.mov(rdi, rax)?;
                let mut slow = self.label();
                let mut done = self.label();
                self.gen_array_storage(&mut slow)?;
                self.a.mov(rax, rcx)?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::Alength)?;
                self.here(&mut done)
            }
            InlineOp::GetField => {
                let (Node::Const(name), Node::Const(cache)) = (&args[1], &args[2]) else {
                    unreachable!("GetField takes a constant name and cache")
                };
                let (name, cache) = (*name, *cache);
                let u = UVECTOR_TAG as i32;
                let mut slow = self.label();
                let mut done = self.label();
                self.emit(&args[0])?;
                self.a.mov(rdi, rax)?;
                self.gen_check_instance(rdi, edi, slow)?;
                self.a.mov(rdx, qword_ptr(rdi + (crate::clj::INSTANCE_TYPE * CELL) as i32 - u))?;
                self.load_const(rsi, cache)?;
                self.a.cmp(rdx, qword_ptr(rsi + Self::sv_elt(0)))?;
                self.a.jne(slow)?;
                self.a.mov(rcx, qword_ptr(rsi + Self::sv_elt(1)))?;
                self.a.mov(rax, qword_ptr(rdi + rcx + (crate::clj::INSTANCE_FIELDS * CELL) as i32 - u))?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.load_const(rsi, name)?;
                self.load_const(rdx, cache)?;
                self.helper(Helper::GetField)?;
                self.here(&mut done)
            }
            InlineOp::Method => {
                self.gen_two_args(args)?;
                let mut slow = self.label();
                let mut done = self.label();
                self.gen_type_method(slow)?;
                self.a.cmp(rax, NIL_IMM)?;
                self.a.jne(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::Method)?;
                self.here(&mut done)
            }
            InlineOp::Implements => {
                // For an instance, only its own type counts.
                self.gen_two_args(args)?;
                let mut slow = self.label();
                let mut done = self.label();
                self.gen_type_method(slow)?;
                self.a.cmp(rax, NIL_IMM)?;
                self.bool_from_flags(false)?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::Implements)?;
                self.here(&mut done)
            }
            InlineOp::KwGet => {
                let (Node::Const(k), Node::Const(pam), Node::Const(arr)) = (&args[1], &args[3], &args[4])
                else {
                    unreachable!("KwGet takes constant key, type and field")
                };
                let (k, pam, arr) = (*k, *pam, arr.integer() as usize);
                let u = UVECTOR_TAG as i32;
                let mut slow = self.label();
                let mut lp = self.label();
                let mut found = self.label();
                let mut missing = self.label();
                let mut done = self.label();
                let d = self.depth;
                self.emit(&args[0])?;
                self.a.mov(rdi, rax)?;
                self.gen_check_instance(rdi, edi, slow)?;
                self.a.mov(rdx, qword_ptr(rdi + (crate::clj::INSTANCE_TYPE * CELL) as i32 - u))?;
                self.load_const(rax, pam)?;
                self.a.cmp(rdx, rax)?;
                self.a.jne(slow)?;
                self.a.mov(rdi, qword_ptr(rdi + ((crate::clj::INSTANCE_FIELDS + arr) * CELL) as i32 - u))?;
                self.gen_array_storage(&mut slow)?;
                self.load_const(rsi, k)?;
                self.a.xor(eax, eax)?;
                self.here(&mut lp)?;
                self.a.cmp(rax, rcx)?;
                self.a.jae(missing)?;
                let start = (ARRAY_SIMPLE_VECTOR_START * CELL) as i32 - u;
                self.a.cmp(rsi, qword_ptr(rdx + rax + start))?;
                self.a.je(found)?;
                self.a.add(rax, (2 * CELL) as i32)?;
                self.a.jmp(lp)?;
                self.here(&mut found)?;
                self.a.mov(rax, qword_ptr(rdx + rax + start + CELL as i32))?;
                self.a.jmp(done)?;
                self.here(&mut missing)?;
                self.emit(&args[2])?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.emit(&args[5])?;
                self.here(&mut done)?;
                debug_assert_eq!(self.depth, d);
                Ok(())
            }
            InlineOp::New => {
                let n = args.len() - 1;
                let Node::Const(cache) = args[n] else {
                    unreachable!("New takes a constant cache")
                };
                let mut slow = self.label();
                let mut done = self.label();
                let d = self.gen_stack_args(&args[..n])?;
                // Same type as last time: bump-allocate the instance (the
                // type then the fields: `n` cells after the header).
                self.a.mov(rdi, Self::slot(d))?;
                self.load_const(rsi, cache)?;
                self.a.cmp(rdi, qword_ptr(rsi + Self::sv_elt(0)))?;
                self.a.jne(slow)?;
                let units = (n + 2) / 2;
                self.gen_bump_alloc(units * 16, slow)?;
                let header = uvector_header(units, CLJ_INSTANCE_TYPE).raw() as i32;
                self.a.mov(qword_ptr(rax), header)?;
                self.gen_copy_slots(d, n, 1)?;
                if (n + 1) % 2 == 1 {
                    self.a.mov(qword_ptr(rax + ((n + 1) * CELL) as i32), 0)?;
                }
                self.a.add(rax, UVECTOR_TAG as i32)?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.a.lea(rdi, Self::slot(d))?;
                self.a.mov(esi, n as u32)?;
                self.load_const(rdx, cache)?;
                self.helper(Helper::NewInstance)?;
                self.here(&mut done)?;
                self.drop_cells(n)
            }
            InlineOp::Array => {
                // The array header (5 cells) and its simple-vector
                // (length and `n` elements) in one allocation.
                let n = args.len();
                let u = UVECTOR_TAG as i32;
                let mut slow = self.label();
                let mut done = self.label();
                let d = self.gen_stack_args(args)?;
                let vunits = (n + 3) / 2;
                self.gen_bump_alloc((3 + vunits) * 16, slow)?;
                self.a.mov(qword_ptr(rax), uvector_header(3, ARRAY_TYPE).raw() as i32)?;
                self.a.lea(rdx, qword_ptr(rax + 48 + u))?;
                self.a.mov(qword_ptr(rax + (ARRAY_VECTOR * CELL) as i32), rdx)?;
                self.a.mov(qword_ptr(rax + (ARRAY_FILL_POINTER * CELL) as i32), fix(n as i64))?;
                self.a.mov(qword_ptr(rax + (ARRAY_OFFSET * CELL) as i32), fix(0))?;
                self.a.mov(qword_ptr(rax + (ARRAY_DIMENSIONS * CELL) as i32), fix(1))?;
                self.a.mov(qword_ptr(rax + (ARRAY_DIM1_LENGTH * CELL) as i32), fix(n as i64))?;
                self.a.mov(qword_ptr(rax + 48), uvector_header(vunits, SIMPLE_VECTOR_TYPE).raw() as i32)?;
                self.a.mov(qword_ptr(rax + 48 + (ARRAY_SIMPLE_VECTOR_LENGTH * CELL) as i32), fix(n as i64))?;
                self.gen_copy_slots(d, n, 6 + ARRAY_SIMPLE_VECTOR_START)?;
                if n % 2 == 1 {
                    self.a.mov(qword_ptr(rax + 48 + ((n + 2) * CELL) as i32), 0)?;
                }
                self.a.add(rax, u)?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.a.lea(rdi, Self::slot(d))?;
                self.a.mov(esi, n as u32)?;
                self.helper(Helper::Array)?;
                self.here(&mut done)?;
                self.drop_cells(n)
            }
            InlineOp::KeywordP => {
                let mut no = self.label();
                let mut done = self.label();
                self.emit(&args[0])?;
                self.a.mov(rdi, rax)?;
                self.gen_check_instance_of(crate::clj::G::KeywordType, no)?;
                self.a.mov(eax, TRUE_IMM as u32)?;
                self.a.jmp(done)?;
                self.here(&mut no)?;
                self.a.mov(eax, FALSE_IMM as u32)?;
                self.here(&mut done)
            }
            InlineOp::ProtocolMarker => {
                let mut slow = self.label();
                let mut done = self.label();
                self.emit(&args[0])?;
                self.a.mov(rdi, rax)?;
                self.gen_check_instance_of(crate::clj::G::ProtocolType, slow)?;
                let off = (crate::clj::PROTO_MARKER * CELL) as i32 - UVECTOR_TAG as i32;
                self.a.mov(rax, qword_ptr(rdi + off))?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::ProtocolMarker)?;
                self.here(&mut done)
            }
            InlineOp::TypeOf => {
                let mut slow = self.label();
                let mut done = self.label();
                self.emit(&args[0])?;
                self.a.mov(rdi, rax)?;
                self.gen_check_instance(rdi, edi, slow)?;
                let off = (crate::clj::INSTANCE_TYPE * CELL) as i32 - UVECTOR_TAG as i32;
                self.a.mov(rax, qword_ptr(rdi + off))?;
                self.a.jmp(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::TypeOf)?;
                self.here(&mut done)
            }
            InlineOp::Identical => {
                // Only strings and doubles (uvectors) can be identical
                // without being EQ.
                self.gen_two_args(args)?;
                let mut slow = self.label();
                let mut done = self.label();
                self.a.mov(eax, TRUE_IMM as u32)?;
                self.a.cmp(rdi, rsi)?;
                self.a.je(done)?;
                self.a.mov(eax, edi)?;
                self.a.and(eax, 7)?;
                self.a.mov(edx, esi)?;
                self.a.and(edx, 7)?;
                self.a.cmp(eax, edx)?;
                self.a.mov(eax, FALSE_IMM as u32)?;
                self.a.jne(done)?;
                self.a.cmp(edx, UVECTOR_TAG as i32)?;
                self.a.jne(done)?;
                self.here(&mut slow)?;
                self.helper(Helper::Identical)?;
                self.here(&mut done)
            }
        }
    }

    /// XMM `x` = number `r` as a double when it is a fixnum or a double
    /// float, else jump to `slow` (clobbers RAX).
    fn gen_load_double(
        &mut self,
        r: AsmRegister64,
        r32: AsmRegister32,
        rb: AsmRegister8,
        x: AsmRegisterXmm,
        slow: CodeLabel,
    ) -> R {
        let u = UVECTOR_TAG as i32;
        let mut notfix = self.label();
        let mut ok = self.label();
        self.a.test(rb, 7)?;
        self.a.jnz(notfix)?;
        self.a.mov(rax, r)?;
        self.a.sar(rax, 3)?;
        self.a.cvtsi2sd(x, rax)?;
        self.a.jmp(ok)?;
        self.here(&mut notfix)?;
        self.a.mov(eax, r32)?;
        self.a.and(eax, 7)?;
        self.a.cmp(eax, u)?;
        self.a.jne(slow)?;
        self.a.mov(rax, qword_ptr(r - u))?;
        self.a.and(eax, 0x1f << 3)?;
        self.a.cmp(eax, (DOUBLE_FLOAT_TYPE << 3) as i32)?;
        self.a.jne(slow)?;
        self.a.movsd_2(x, qword_ptr(r + (DOUBLE_FLOAT_OFFSET * CELL) as i32 - u))?;
        self.here(&mut ok)
    }

    /// Arithmetic or comparison `op` of RDI and RSI when neither is
    /// anything but a fixnum or a double (and not both fixnums): the
    /// result in RAX, then jump to `done`. Otherwise (or when the heap
    /// block is full) jump to `call`, with RDI and RSI unchanged.
    fn gen_float_op(&mut self, op: InlineOp, done: CodeLabel, call: &mut CodeLabel) -> R {
        let u = UVECTOR_TAG as i32;
        self.a.mov(rax, rdi)?;
        self.a.or(rax, rsi)?;
        self.a.test(al, 7)?;
        self.a.jz(*call)?;
        self.gen_load_double(rdi, edi, dil, xmm0, *call)?;
        self.gen_load_double(rsi, esi, sil, xmm1, *call)?;
        let arith = matches!(op, InlineOp::Add2 | InlineOp::Sub2 | InlineOp::Mul2);
        if arith {
            match op {
                InlineOp::Add2 => self.a.addsd(xmm0, xmm1)?,
                InlineOp::Sub2 => self.a.subsd(xmm0, xmm1)?,
                _ => self.a.mulsd(xmm0, xmm1)?,
            }
            // Bump-allocate the double float (16 bytes) in the current
            // heap block.
            let bytes = (DOUBLE_FLOAT_SIZE + 2) / 2 * 16;
            self.gen_bump_alloc(bytes, *call)?;
            let header = uvector_header(bytes / 16, DOUBLE_FLOAT_TYPE).raw() as i32;
            self.a.mov(qword_ptr(rax), header)?;
            self.a.movsd_2(qword_ptr(rax + (DOUBLE_FLOAT_OFFSET * CELL) as i32), xmm0)?;
            self.a.add(rax, u)?;
            return self.a.jmp(done);
        }
        // Unordered (NaN) compares false: CF, ZF and PF are all set.
        self.a.mov(eax, FALSE_IMM as u32)?;
        self.a.mov(edx, TRUE_IMM as u32)?;
        match op {
            InlineOp::Lt2 | InlineOp::Le2 => self.a.ucomisd(xmm1, xmm0)?,
            _ => self.a.ucomisd(xmm0, xmm1)?,
        }
        match op {
            InlineOp::Lt2 | InlineOp::Gt2 => self.a.cmova(eax, edx)?,
            InlineOp::Le2 | InlineOp::Ge2 => self.a.cmovae(eax, edx)?,
            _ => {
                let mut ne = self.label();
                self.a.jp(ne)?;
                self.a.cmove(eax, edx)?;
                self.here(&mut ne)?;
            }
        }
        self.a.jmp(done)
    }

    /// Evaluate `args` into new stack cells in ascending address order;
    /// returns the depth of the first.
    fn gen_stack_args(&mut self, args: &[Node]) -> Result<usize, IcedError> {
        let n = args.len();
        if n > 0 {
            self.a.sub(rsp, (n * CELL) as i32)?;
            self.depth += n;
        }
        let d = self.depth;
        for (i, x) in args.iter().enumerate() {
            self.emit(x)?;
            self.a.mov(Self::slot(d - i), rax)?;
        }
        Ok(d)
    }

    /// Copy the `n` stack cells from depth `d` (see `gen_stack_args`) to
    /// cells `first..` of the object at RAX (untagged). Clobbers RDX.
    fn gen_copy_slots(&mut self, d: usize, n: usize, first: usize) -> R {
        for i in 0..n {
            self.a.mov(rdx, Self::slot(d - i))?;
            self.a.mov(qword_ptr(rax + ((first + i) * CELL) as i32), rdx)?;
        }
        Ok(())
    }

    /// RAX = `bytes` bump-allocated (untagged, not initialized) in the
    /// current heap block, or jump to `full`. Clobbers RDX.
    fn gen_bump_alloc(&mut self, bytes: usize, full: CodeLabel) -> R {
        self.a.mov(rax, qword_ptr(r15 + qvd(THREAD_HEAP_Index)))?;
        self.a.lea(rdx, qword_ptr(rax + bytes as i32))?;
        self.a.cmp(rdx, qword_ptr(r15 + qvd(THREAD_HEAP_END_Index)))?;
        self.a.ja(full)?;
        self.a.mov(qword_ptr(r15 + qvd(THREAD_HEAP_Index)), rdx)
    }

    /// Jump to `not` unless `r` is a Clojure instance (clobbers EAX).
    fn gen_check_instance(&mut self, r: AsmRegister64, r32: AsmRegister32, not: CodeLabel) -> R {
        let u = UVECTOR_TAG as i32;
        self.a.mov(eax, r32)?;
        self.a.and(eax, 7)?;
        self.a.cmp(eax, u)?;
        self.a.jne(not)?;
        self.a.mov(rax, qword_ptr(r - u))?;
        self.a.and(eax, 0x1f << 3)?;
        self.a.cmp(eax, (CLJ_INSTANCE_TYPE << 3) as i32)?;
        self.a.jne(not)
    }

    /// Jump to `not` unless RDI is an instance of kernel type `g`
    /// (clobbers RAX, RDX).
    fn gen_check_instance_of(&mut self, g: crate::clj::G, not: CodeLabel) -> R {
        let u = UVECTOR_TAG as i32;
        self.gen_check_instance(rdi, edi, not)?;
        self.a.mov(rdx, qword_ptr(rdi + (crate::clj::INSTANCE_TYPE * CELL) as i32 - u))?;
        self.a.mov(rax, qword_ptr(r15 + qvd(CLJ_GLOBALS_Index)))?;
        let off = (ARRAY_SIMPLE_VECTOR_START + g as usize) * CELL;
        self.a.cmp(rdx, qword_ptr(rax + off as i32 - u))?;
        self.a.jne(not)
    }

    /// Method `id` (RDI, a fixnum) of the type of instance RSI into RAX,
    /// which may be NIL; jumps to `slow` for a non-instance or an id past
    /// the end of the type's method vector.
    fn gen_type_method(&mut self, slow: CodeLabel) -> R {
        let u = UVECTOR_TAG as i32;
        let cell = |i: usize| (i * CELL) as i32 - u;
        self.gen_check_instance(rsi, esi, slow)?;
        self.a.mov(rdx, qword_ptr(rsi + cell(crate::clj::INSTANCE_TYPE)))?;
        self.a.mov(rdx, qword_ptr(rdx + cell(crate::clj::TYPE_METHODS)))?;
        self.a.mov(eax, edx)?;
        self.a.and(eax, 7)?;
        self.a.cmp(eax, u)?;
        self.a.jne(slow)?;
        self.a.mov(rax, qword_ptr(rdx - u))?;
        self.a.and(eax, 0x1f << 3)?;
        self.a.cmp(eax, (SIMPLE_VECTOR_TYPE << 3) as i32)?;
        self.a.jne(slow)?;
        self.a.test(dil, 7)?;
        self.a.jnz(slow)?;
        self.a.cmp(rdi, qword_ptr(rdx + cell(ARRAY_SIMPLE_VECTOR_LENGTH)))?;
        self.a.jae(slow)?;
        self.a.mov(rax, qword_ptr(rdx + rdi + cell(ARRAY_SIMPLE_VECTOR_START)))
    }

    /// For a Clojure array in RDI (a simple-vector, or a 1-d adjustable
    /// array with a fill pointer over a simple-vector): RCX = length (a
    /// fixnum), RDX = simple-vector shifted by the array offset so element
    /// i is at the simple-vector position of element 0. Jumps to `slow`
    /// for anything else. Keeps RDI and RSI.
    fn gen_array_storage(&mut self, slow: &mut CodeLabel) -> R {
        let mut adj = self.label();
        let mut got = self.label();
        let u = UVECTOR_TAG as i32;
        let cell = |i: usize| (i * CELL) as i32 - u;
        self.a.mov(eax, edi)?;
        self.a.and(eax, 7)?;
        self.a.cmp(eax, u)?;
        self.a.jne(*slow)?;
        self.a.mov(rax, qword_ptr(rdi - u))?;
        self.a.and(eax, 0x1f << 3)?;
        self.a.cmp(eax, (ARRAY_TYPE << 3) as i32)?;
        self.a.je(adj)?;
        self.a.cmp(eax, (SIMPLE_VECTOR_TYPE << 3) as i32)?;
        self.a.jne(*slow)?;
        self.a.mov(rcx, qword_ptr(rdi + cell(ARRAY_SIMPLE_VECTOR_LENGTH)))?;
        self.a.mov(rdx, rdi)?;
        self.a.jmp(got)?;
        self.here(&mut adj)?;
        self.a.cmp(qword_ptr(rdi + cell(ARRAY_DIMENSIONS)), fix(1))?;
        self.a.jne(*slow)?;
        self.a.mov(rcx, qword_ptr(rdi + cell(ARRAY_FILL_POINTER)))?;
        self.a.test(rcx, rcx)?;
        self.a.js(*slow)?;
        self.a.mov(rdx, qword_ptr(rdi + cell(ARRAY_VECTOR)))?;
        self.a.mov(rax, qword_ptr(rdx - u))?;
        self.a.and(eax, 0x1f << 3)?;
        self.a.cmp(eax, (SIMPLE_VECTOR_TYPE << 3) as i32)?;
        self.a.jne(*slow)?;
        self.a.add(rdx, qword_ptr(rdi + cell(ARRAY_OFFSET)))?;
        self.here(&mut got)
    }

    /// Landing code after a throw to a catch frame: the thrown value.
    fn load_thrown_value(&mut self) -> R {
        self.a.mov(rax, qword_ptr(r15 + qvd(THROWN_VALUE_Index)))
    }

    fn gen_try(&mut self, body: &Node, catch: Option<&(VarId, Box<Node>)>, finally: Option<&Node>) -> R {
        match finally {
            None => self.gen_catch(body, catch),
            Some(f) => self.gen_unwind_protect(|g| g.gen_catch(body, catch), f),
        }
    }

    /// `body`, then if a Clojure throw landed, bind the thrown value to
    /// the catch variable and evaluate the handler.
    fn gen_catch(&mut self, body: &Node, catch: Option<&(VarId, Box<Node>)>) -> R {
        let Some((var, handler)) = catch else { return self.emit(body) };
        let mut landing = self.label();
        let mut end = self.label();
        let d0 = self.depth;
        self.load_const(rax, NIL)?;
        self.push_catch_frame(DF_CATCH, landing)?;
        self.emit(body)?;
        self.pop_dyn_frame()?;
        self.a.jmp(end)?;
        // A throw lands with RSP at the frame, still linked.
        self.here(&mut landing)?;
        self.depth = d0 + DF_CELLS;
        self.load_thrown_value()?;
        self.pop_dyn_frame()?;
        self.bind_lex_new_slot(*var)?;
        self.emit(handler)?;
        self.drop_cells(1)?;
        self.here(&mut end)
    }

    fn gen_unwind_protect(&mut self, protected: impl FnOnce(&mut Self) -> R, cleanup: &Node) -> R {
        let mut landing = self.label();
        let mut end = self.label();
        let d0 = self.depth;
        self.load_const(rax, NIL)?;
        self.push_catch_frame(DF_UNWIND_PROTECT, landing)?;
        protected(self)?;
        self.pop_dyn_frame()?;
        self.push_rax()?;
        self.emit(cleanup)?;
        self.pop_to(rax)?;
        self.a.jmp(end)?;
        // landing from a throw: frame already unlinked, RSP at d0
        self.here(&mut landing)?;
        self.depth = d0;
        // RAX = target frame, RDX = value.
        self.helper(Helper::TakePending)?;
        self.push_rax()?;
        self.a.mov(rax, rdx)?;
        self.push_rax()?;
        self.emit(cleanup)?;
        self.pop_to(rsi)?;
        self.pop_to(rdi)?;
        self.helper(Helper::ContinueThrow)?;
        self.a.ud2()?;
        self.here(&mut end)
    }
}

fn disassemble(code: LispObj, lit_off: usize) {
    use iced_x86::{Decoder, DecoderOptions, Formatter, IntelFormatter};
    let start = unsafe { (code.uvector_ptr() as *const u8).add(CODE_START) };
    let bytes = unsafe { std::slice::from_raw_parts(start, lit_off) };
    let ip = start as u64;
    let mut d = Decoder::with_ip(64, bytes, ip, DecoderOptions::NONE);
    let mut f = IntelFormatter::new();
    let mut out = String::new();
    let name = crate::clj::printer::print_str(code.uref(CODE_NAME));
    eprintln!(";; code object {:#x} ({name})", code.raw());
    for ins in &mut d {
        out.clear();
        f.format(&ins, &mut out);
        eprintln!("  {:6x}  {out}", ins.ip() - ip);
    }
    let n = code.uref(CODE_NUM_LITERALS).integer() as usize;
    for i in 0..n {
        let v = unsafe { (start.add(lit_off) as *const LispObj).add(i).read_unaligned() };
        eprintln!("  lit {i}: {}", crate::clj::printer::print_str(v));
    }
}
