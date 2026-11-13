//! Runtime glue between compiled Lisp code and Rust: call stubs, the
//! dynamic frame chain (catch, unwind-protect, special
//! bindings) and throw.
//!
//! Lisp calling convention (x86-64):
//! - R15 = QV pointer (callee-saved in SysV, so it survives Rust calls).
//! - Arguments are stored on the stack in ascending address order: at the
//!   `call`, `[rsp]` = arg 0, `[rsp+8]` = arg 1, ... The caller pops them.
//! - RCX = argument count (raw) on entry.
//! - RAX = function object on entry (to the funcall stub), the value on
//!   return.
//! - R13 = closure environment on entry to compiled code.
//! - All other registers are caller-saved; RBP is the frame pointer.
//!
//! Dynamic frames live on the Lisp stack and are linked from QV
//! `DYN_FRAME`. Layout (cells from the frame address):
//!   0 previous frame (raw address), 1 kind (fixnum), 2 bound var,
//!   3 landing address (raw) / saved value, 4 saved RBP (raw).
//! A throw restores RSP to the frame address (catch) and jumps to its
//! landing code. Rust frames between a catcher and a throw are discarded
//! without unwinding: Rust code that may call Lisp must not rely on `Drop`.

use std::sync::atomic::{AtomicUsize, Ordering};

use iced_x86::IcedError;
use iced_x86::code_asm::CodeAssembler;

use crate::error::lisp_error;
use crate::heap;
use crate::object::*;
use crate::prims;
use crate::qv::{self, qv, set_qv};
use crate::qv_index::*;

pub const DF_PREV: usize = 0;
pub const DF_KIND: usize = 1;
pub const DF_TAG: usize = 2;
pub const DF_LANDING: usize = 3;
pub const DF_RBP: usize = 4;
pub const DF_CELLS: usize = 5;

pub const DF_CATCH: i64 = 1;
pub const DF_UNWIND_PROTECT: i64 = 2;
pub const DF_SPECIAL: i64 = 3;
/// Garbage collection inhibited (made from Rust by `without_gc`).
pub const DF_GC_INHIBIT: i64 = 4;

/// Compiled code objects (`COMPILED_CODE_TYPE` uvectors).
/// Cells: 1 literal area offset (bytes from the uvector start, fixnum),
/// 2 number of literals (fixnum), 3 name, 4 info, 5 the number of fixed
/// parameters of the variadic arity (fixnum), or NIL. Machine code
/// starts at `CODE_START` (16-byte aligned), which is also the entry
/// point; the literal area (8-byte tagged cells, possibly unaligned)
/// follows the instructions.
pub const CODE_LITERALS_OFFSET: usize = 1;
pub const CODE_NUM_LITERALS: usize = 2;
pub const CODE_NAME: usize = 3;
pub const CODE_INFO: usize = 4;
pub const CODE_REST_ARITY: usize = 5;
pub const CODE_START: usize = 48;

/// Set in the argument count (RCX) of a call of a variadic function
/// whose last argument is the rest seq itself (`apply`): the function
/// enters its variadic arity without building the rest seq. Counts
/// above `i32::MAX` have it.
pub const REST_READY: usize = 1 << 32;

// ---------------------------------------------------------------------------
// Rust helpers called from compiled code through the helper stub.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(usize)]
pub enum Helper {
    MakeRest,
    MakeClosure,
    WrongArgs,
    UnboundVariable,
    TakePending,
    ContinueThrow,
    Add2,
    Sub2,
    NumEq2,
    Lt2,
    Gt2,
    Le2,
    Ge2,
    StackOverflow,
    Mul2,
    Aget,
    Alength,
    GetField,
    Method,
    Implements,
    ProtocolMarker,
    TypeOf,
    Identical,
    NewInstance,
    Array,
}

fn helper_addresses() -> Vec<(Helper, usize)> {
    use Helper::*;
    vec![
        (MakeRest, h_make_rest as *const () as usize),
        (MakeClosure, h_make_closure as *const () as usize),
        (WrongArgs, h_wrong_args as *const () as usize),
        (UnboundVariable, h_unbound_variable as *const () as usize),
        (TakePending, h_take_pending as *const () as usize),
        (ContinueThrow, h_continue_throw as *const () as usize),
        (Add2, h_add2 as *const () as usize),
        (Sub2, h_sub2 as *const () as usize),
        (NumEq2, h_numeq2 as *const () as usize),
        (Lt2, h_lt2 as *const () as usize),
        (Gt2, h_gt2 as *const () as usize),
        (Le2, h_le2 as *const () as usize),
        (Ge2, h_ge2 as *const () as usize),
        (StackOverflow, h_stack_overflow as *const () as usize),
        (Mul2, h_mul2 as *const () as usize),
        (Aget, h_aget as *const () as usize),
        (Alength, h_alength as *const () as usize),
        (GetField, h_get_field as *const () as usize),
        (Method, h_method as *const () as usize),
        (Implements, h_implements as *const () as usize),
        (ProtocolMarker, h_protocol_marker as *const () as usize),
        (TypeOf, h_type_of as *const () as usize),
        (Identical, h_identical as *const () as usize),
        (NewInstance, h_new_instance as *const () as usize),
        (Array, h_array as *const () as usize),
    ]
}

/// Helper names in table order (part of the image kernel id).
pub fn helper_names() -> Vec<String> {
    helper_addresses().iter().map(|(h, _)| format!("{h:?}")).collect()
}

/// The rest seq of the `n` arguments starting at `p` (ascending
/// addresses): an IndexedSeq over an array of them, or nil.
extern "C" fn h_make_rest(p: *const LispObj, n: isize) -> LispObj {
    if n <= 0 {
        return NIL;
    }
    // (The arguments are on the stack: GC pins what they point to.)
    let args = unsafe { std::slice::from_raw_parts(p, n as usize) };
    crate::clj::coll::indexed_seq(args)
}

/// Closure over the `k` values at `p` (pushed in order, so value 0 is at the
/// highest address).
extern "C" fn h_make_closure(code: LispObj, p: *const LispObj, k: usize) -> LispObj {
    let env = heap::vector_node(k);
    for i in 0..k {
        env.svset(i, unsafe { *p.add(k - 1 - i) });
    }
    make_function(code, env)
}

/// A function entered with RSP below the QV stack limit. The limit may
/// be another thread's: then it is updated and the call goes on.
extern "C" fn h_stack_overflow() -> LispObj {
    update_stack_limit();
    NIL
}

extern "C" fn h_wrong_args(nargs: usize, name: LispObj) -> LispObj {
    // [name]: a method whose first argument is `this`.
    let (nargs, name) = if name.is_simple_generic_vector() { (nargs.saturating_sub(1), name.svref(0)) } else { (nargs, name) };
    let n = if name == NIL { "fn".to_string() } else { crate::clj::printer::print_str(name) };
    crate::error::arity_error(nargs, &n)
}

extern "C" fn h_unbound_variable(sym: LispObj) -> LispObj {
    let n = crate::clj::printer::pr_str(sym);
    lisp_error(format!("Var {n} is unbound"))
}

/// Two values returned in RAX and RDX.
#[repr(C)]
pub struct Pair(usize, LispObj);

/// The throw interrupted by an unwind-protect cleanup: target frame
/// (RAX) and value (RDX).
extern "C" fn h_take_pending() -> Pair {
    let p = Pair(qv(THROW_PENDING_Index).raw(), qv(THROWN_VALUE_Index));
    set_qv(THROW_PENDING_Index, LispObj(0));
    set_qv(THROWN_VALUE_Index, NIL);
    p
}

extern "C" fn h_continue_throw(target: usize, value: LispObj) -> LispObj {
    unwind_to(target, value)
}

extern "C" fn h_add2(a: LispObj, b: LispObj) -> LispObj {
    crate::numbers::add(a, b)
}
extern "C" fn h_sub2(a: LispObj, b: LispObj) -> LispObj {
    crate::numbers::sub(a, b)
}
extern "C" fn h_mul2(a: LispObj, b: LispObj) -> LispObj {
    crate::numbers::mul(a, b)
}
extern "C" fn h_aget(a: LispObj, i: LispObj) -> LispObj {
    crate::clj::array::aget(&[a, i])
}
extern "C" fn h_alength(a: LispObj) -> LispObj {
    crate::clj::array::alength(&[a])
}
/// `(.-name x)` missing the call site's cache: look the field up and
/// remember the type and index in `cache` (`[type index]`) for instances.
extern "C" fn h_get_field(x: LispObj, name: LispObj, cache: LispObj) -> LispObj {
    use crate::clj::{self as c, G};
    if c::is_instance(x) && !c::is_type(x) && c::instance_type(x) != c::global(G::ObjectType) {
        if let Some(i) = c::field_index(c::instance_type(x), name) {
            cache.svset(0, c::instance_type(x));
            cache.svset(1, LispObj::fixnum(i as i64));
        }
    }
    crate::clj::prims::get_field(&[x, name])
}
extern "C" fn h_method(id: LispObj, x: LispObj) -> LispObj {
    crate::clj::prims::method(&[id, x])
}
extern "C" fn h_implements(id: LispObj, x: LispObj) -> LispObj {
    crate::clj::prims::implements(&[id, x])
}
extern "C" fn h_protocol_marker(p: LispObj) -> LispObj {
    crate::clj::prims::protocol_marker(&[p])
}
extern "C" fn h_type_of(x: LispObj) -> LispObj {
    crate::clj::type_of(x)
}
/// `(new T args...)` missing the call site's cache: the `n` values at
/// `p` are the type and the arguments. Remember a type whose instances
/// are a plain fill of its fields from the arguments (`[type]`).
extern "C" fn h_new_instance(p: *const LispObj, n: usize, cache: LispObj) -> LispObj {
    let a = unsafe { std::slice::from_raw_parts(p, n) };
    if crate::clj::prims::plain_constructor(a[0], n - 1) {
        cache.svset(0, a[0]);
    }
    crate::clj::prims::new_instance(a)
}
/// `(k/array ...)` of the `n` values at `p` when the heap block is full.
extern "C" fn h_array(p: *const LispObj, n: usize) -> LispObj {
    crate::clj::array::from_slice(unsafe { std::slice::from_raw_parts(p, n) })
}
extern "C" fn h_identical(a: LispObj, b: LispObj) -> LispObj {
    crate::clj::prims::identical_p(&[a, b])
}
extern "C" fn h_numeq2(a: LispObj, b: LispObj) -> LispObj {
    cbool(crate::numbers::partial_compare(a, b).is_some_and(|o| o.is_eq()))
}
extern "C" fn h_lt2(a: LispObj, b: LispObj) -> LispObj {
    cbool(crate::numbers::partial_compare(a, b).is_some_and(|o| o.is_lt()))
}
extern "C" fn h_gt2(a: LispObj, b: LispObj) -> LispObj {
    cbool(crate::numbers::partial_compare(a, b).is_some_and(|o| o.is_gt()))
}
extern "C" fn h_le2(a: LispObj, b: LispObj) -> LispObj {
    cbool(crate::numbers::partial_compare(a, b).is_some_and(|o| o.is_le()))
}
extern "C" fn h_ge2(a: LispObj, b: LispObj) -> LispObj {
    cbool(crate::numbers::partial_compare(a, b).is_some_and(|o| o.is_ge()))
}

// ---------------------------------------------------------------------------
// Errors raised from the stubs

extern "C" fn stub_not_a_function(x: LispObj) -> LispObj {
    let n = crate::clj::printer::error_str(x);
    crate::error::type_error(format!("Not a function: {n}"))
}

/// A function object or a Clojure instance with an invoke method.
pub fn is_funcallable(x: LispObj) -> bool {
    x.is_function() || (crate::clj::is_instance(x) && crate::clj::prims::invoke_function(x).is_some())
}

/// Called by the funcall stub for a Clojure instance: the function to
/// call with the instance prepended to the arguments.
extern "C" fn stub_clj_invoke_function(x: LispObj) -> LispObj {
    match crate::clj::prims::invoke_function(x) {
        Some(f) => f,
        None => stub_not_a_function(x),
    }
}

/// Stack limits of a thread (addresses; 0 when not known yet).
#[derive(Clone, Copy)]
struct StackLimits {
    /// Below it a call signals a stack overflow error.
    soft: usize,
    /// The limit while that error is handled (its handlers run deep).
    hard: usize,
    /// A stack overflow error is being handled: the hard limit applies
    /// until the stack has unwound well above the soft limit.
    handling: bool,
}

thread_local! {
    static STACK: std::cell::Cell<StackLimits> =
        const { std::cell::Cell::new(StackLimits { soft: 0, hard: 0, handling: false }) };
}

/// The stack limits of the current thread, from its stack bounds: a
/// quarter of the stack (at most 1MB) above the bottom is kept for
/// handling the error, a sixteenth (at most 128KB) for what follows.
fn stack_limits() -> StackLimits {
    let mut attr: libc::pthread_attr_t = unsafe { std::mem::zeroed() };
    let mut addr: *mut libc::c_void = std::ptr::null_mut();
    let mut size: libc::size_t = 0;
    let ok = unsafe {
        libc::pthread_getattr_np(libc::pthread_self(), &mut attr) == 0 && {
            let r = libc::pthread_attr_getstack(&attr, &mut addr, &mut size) == 0;
            libc::pthread_attr_destroy(&mut attr);
            r
        }
    };
    if !ok || size == 0 {
        // Unknown: no check.
        return StackLimits { soft: 1, hard: 1, handling: false };
    }
    let bottom = addr as usize;
    StackLimits { soft: bottom + (size / 4).min(1 << 20), hard: bottom + (size / 16).min(1 << 17), handling: false }
}

/// Signal an error if the stack is nearly exhausted (deep recursion),
/// so it can be caught instead of crashing the process. Checked when
/// Lisp code calls a kernel function. The common case (limits known, no
/// overflow being handled, well above the limit) only reads the
/// thread-local limits.
#[inline]
fn check_stack() {
    let marker = 0u8;
    let sp = &marker as *const u8 as usize;
    let st = STACK.with(|c| c.get());
    if st.soft != 0 && !st.handling && sp >= st.soft {
        return;
    }
    update_stack_limit();
}

/// `check_stack` that also stores the running thread's limit in QV,
/// where compiled functions compare RSP with it.
#[inline(never)]
fn update_stack_limit() {
    let marker = 0u8;
    let sp = &marker as *const u8 as usize;
    let mut st = STACK.with(|c| c.get());
    if st.soft == 0 {
        st = stack_limits();
    }
    if st.handling && sp > st.soft + (st.soft - st.hard) {
        st.handling = false;
    }
    let limit = if st.handling { st.hard } else { st.soft };
    let overflow = sp < limit;
    if overflow {
        st.handling = true;
    }
    STACK.with(|c| c.set(st));
    set_qv(STACK_LIMIT_Index, LispObj(if st.handling { st.hard } else { st.soft }));
    if overflow {
        lisp_error("Stack overflow (deep recursion)".to_string());
    }
}

/// Called (via the funcall stub) for kernel functions.
extern "C" fn prim_dispatch(idx: usize, args: *const LispObj, n: usize) -> LispObj {
    check_stack();
    let a = unsafe { std::slice::from_raw_parts(args, n) };
    prims::call_prim(idx, a)
}

// ---------------------------------------------------------------------------
// Stubs (generated at startup into their own executable mapping)

static LISP_CALL_ENTRY: AtomicUsize = AtomicUsize::new(0);
static CATCH_ENTRY: AtomicUsize = AtomicUsize::new(0);
static JUMP_ENTRY: AtomicUsize = AtomicUsize::new(0);

const STUB_BYTES: usize = 4096;

fn qvd(i: usize) -> i32 {
    (i * CELL) as i32
}

fn fix(n: i64) -> i32 {
    (n << 3) as i32
}

fn gen_stubs(base: u64) -> Result<(Vec<u8>, [u64; 6]), IcedError> {
    use iced_x86::code_asm::*;
    let qv_addr = qv::qv_base() as u64;
    let mut a = CodeAssembler::new(64)?;
    let mut funcall = a.create_label();
    let mut helper = a.create_label();
    let mut lcall = a.create_label();
    let mut catch = a.create_label();
    let mut jump = a.create_label();
    let mut kcall = a.create_label();

    // ---- funcall stub: RAX = function, RCX = nargs, args at [rsp+8].
    {
        let mut again = a.create_label();
        let mut notfun = a.create_label();
        let mut fun = a.create_label();
        let mut kfun = a.create_label();
        let mut cljinst = a.create_label();
        a.set_label(&mut funcall)?;
        a.nop()?;
        a.set_label(&mut again)?;
        a.mov(edx, eax)?;
        a.and(edx, 7)?;
        a.cmp(edx, UVECTOR_TAG as i32)?;
        a.jne(notfun)?;
        a.mov(rdx, qword_ptr(rax - 5))?;
        a.shr(edx, 3)?;
        a.and(edx, 0x1f)?;
        a.cmp(edx, FUNCTION_TYPE as i32)?;
        a.je(fun)?;
        a.cmp(edx, KFUNCTION_TYPE as i32)?;
        a.je(kfun)?;
        a.cmp(edx, CLJ_INSTANCE_TYPE as i32)?;
        a.jne(notfun)?;
        a.jmp(cljinst)?;

        a.set_label(&mut fun)?;
        a.mov(r13, qword_ptr(rax + (FUNCTION_ENVIRONMENT * CELL) as i32 - 5))?;
        a.mov(rdx, qword_ptr(rax + (FUNCTION_ADDRESS * CELL) as i32 - 5))?;
        a.add(rdx, CODE_START as i32 - 5)?;
        a.jmp(rdx)?;

        a.set_label(&mut kfun)?;
        a.mov(rdi, qword_ptr(rax + (FUNCTION_ADDRESS * CELL) as i32 - 5))?;
        a.sar(rdi, 3)?;
        // ---- kernel call stub: RDI = PRIMS index, RCX = nargs, args at
        // [rsp+8].
        a.set_label(&mut kcall)?;
        a.push(rbp)?;
        a.mov(rbp, rsp)?;
        a.lea(rsi, qword_ptr(rbp + 16))?;
        a.mov(rdx, rcx)?;
        a.and(rsp, -16)?;
        a.mov(r11, prim_dispatch as *const () as usize as u64)?;
        a.call(r11)?;
        a.mov(rsp, rbp)?;
        a.pop(rbp)?;
        a.ret()?;

        // A Clojure instance: call its invoke method with the instance
        // prepended to the arguments (copied to a new frame).
        {
            let mut lp = a.create_label();
            let mut done = a.create_label();
            a.set_label(&mut cljinst)?;
            a.push(rbp)?;
            a.mov(rbp, rsp)?;
            a.push(rax)?;
            a.push(rcx)?;
            a.mov(rdi, rax)?;
            a.and(rsp, -16)?;
            a.mov(r11, stub_clj_invoke_function as *const () as usize as u64)?;
            a.call(r11)?;
            a.mov(rcx, qword_ptr(rbp - 16))?;
            a.lea(rdx, qword_ptr(rcx * 8 + 8))?;
            a.lea(rsp, qword_ptr(rbp - 16))?;
            a.sub(rsp, rdx)?;
            a.and(rsp, -16)?;
            a.xor(edx, edx)?;
            a.set_label(&mut lp)?;
            a.cmp(rdx, rcx)?;
            a.jae(done)?;
            a.mov(r11, qword_ptr(rbp + rdx * 8 + 16))?;
            a.mov(qword_ptr(rsp + rdx * 8 + 8), r11)?;
            a.inc(rdx)?;
            a.jmp(lp)?;
            a.set_label(&mut done)?;
            a.mov(rdx, qword_ptr(rbp - 8))?;
            a.mov(qword_ptr(rsp), rdx)?;
            a.inc(rcx)?;
            a.call(funcall)?;
            a.mov(rsp, rbp)?;
            a.pop(rbp)?;
            a.ret()?;
        }

        a.set_label(&mut notfun)?;
        a.mov(rdi, rax)?;
        a.and(rsp, -16)?;
        a.mov(r11, stub_not_a_function as *const () as usize as u64)?;
        a.call(r11)?;
        a.ud2()?;
    }

    // ---- helper stub: RAX = helper index, SysV args already in place.
    a.set_label(&mut helper)?;
    a.push(rbp)?;
    a.mov(rbp, rsp)?;
    a.and(rsp, -16)?;
    a.call(qword_ptr(r15 + rax * 8 + qvd(HELPERS_Index)))?;
    a.mov(rsp, rbp)?;
    a.pop(rbp)?;
    a.ret()?;

    // ---- lisp_call entry (SysV): rdi = function, rsi = args, rdx = nargs,
    // rcx = flags for the count (`REST_READY`).
    {
        let mut lp = a.create_label();
        let mut done = a.create_label();
        a.set_label(&mut lcall)?;
        save_callee_saved(&mut a)?;
        a.mov(r15, qv_addr)?;
        a.mov(rax, rdi)?;
        a.lea(r11, qword_ptr(rdx * 8))?;
        a.sub(rsp, r11)?;
        a.and(rsp, -16)?;
        a.xor(r10, r10)?;
        a.set_label(&mut lp)?;
        a.cmp(r10, rdx)?;
        a.jae(done)?;
        a.mov(r11, qword_ptr(rsi + r10 * 8))?;
        a.mov(qword_ptr(rsp + r10 * 8), r11)?;
        a.inc(r10)?;
        a.jmp(lp)?;
        a.set_label(&mut done)?;
        a.or(rcx, rdx)?;
        a.call(qword_ptr(r15 + qvd(FUNCALL_STUB_Index)))?;
        restore_callee_saved(&mut a)?;
    }

    // ---- catch entry (SysV): rdi = NIL, rsi = fn(data), rdx = data.
    // Returns 0 normally, 1 if a throw landed here.
    {
        let mut landing = a.create_label();
        let mut done = a.create_label();
        a.set_label(&mut catch)?;
        save_callee_saved(&mut a)?;
        a.mov(r15, qv_addr)?;
        a.push(rbp)?;
        a.lea(r11, qword_ptr(landing))?;
        a.push(r11)?;
        a.push(rdi)?;
        a.push(fix(DF_CATCH))?;
        a.push(qword_ptr(r15 + qvd(DYN_FRAME_Index)))?;
        a.mov(qword_ptr(r15 + qvd(DYN_FRAME_Index)), rsp)?;
        a.mov(rdi, rdx)?;
        a.call(rsi)?;
        a.mov(rdx, qword_ptr(rsp))?;
        a.mov(qword_ptr(r15 + qvd(DYN_FRAME_Index)), rdx)?;
        a.xor(eax, eax)?;
        a.jmp(done)?;
        a.set_label(&mut landing)?;
        a.mov(rdx, qword_ptr(rsp))?;
        a.mov(qword_ptr(r15 + qvd(DYN_FRAME_Index)), rdx)?;
        a.mov(eax, 1)?;
        a.set_label(&mut done)?;
        restore_callee_saved(&mut a)?;
    }

    // ---- jump (SysV): rdi = new rsp, rsi = new rbp, rdx = target.
    a.set_label(&mut jump)?;
    a.mov(rsp, rdi)?;
    a.mov(rbp, rsi)?;
    a.mov(r15, qv_addr)?;
    a.jmp(rdx)?;

    let r = a.assemble_options(base, iced_x86::BlockEncoderOptions::RETURN_NEW_INSTRUCTION_OFFSETS)?;
    let addrs = [
        r.label_ip(&funcall)?,
        r.label_ip(&helper)?,
        r.label_ip(&lcall)?,
        r.label_ip(&catch)?,
        r.label_ip(&jump)?,
        r.label_ip(&kcall)?,
    ];
    Ok((r.inner.code_buffer, addrs))
}

/// push rbp; mov rbp,rsp; push rbx, r12-r15. RSP ends 16-byte aligned
/// (rbp - 40, rbp aligned) so 5-cell frames keep calls aligned.
fn save_callee_saved(a: &mut CodeAssembler) -> Result<(), IcedError> {
    use iced_x86::code_asm::*;
    a.push(rbp)?;
    a.mov(rbp, rsp)?;
    a.push(rbx)?;
    a.push(r12)?;
    a.push(r13)?;
    a.push(r14)?;
    a.push(r15)?;
    Ok(())
}

fn restore_callee_saved(a: &mut CodeAssembler) -> Result<(), IcedError> {
    use iced_x86::code_asm::*;
    a.lea(rsp, qword_ptr(rbp - 40))?;
    a.pop(r15)?;
    a.pop(r14)?;
    a.pop(r13)?;
    a.pop(r12)?;
    a.pop(rbx)?;
    a.pop(rbp)?;
    a.ret()?;
    Ok(())
}

/// Generate the stubs and fill the QV slots that compiled code uses.
/// Called at startup (and after loading an image).
pub(crate) fn init_runtime() {
    let mem = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            STUB_BYTES,
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        )
    };
    assert!(mem != libc::MAP_FAILED, "cannot map stub area");
    let base = mem as u64;
    let (code, addrs) = gen_stubs(base).expect("stub generation");
    assert!(code.len() <= STUB_BYTES);
    unsafe { std::ptr::copy_nonoverlapping(code.as_ptr(), mem as *mut u8, code.len()) };
    set_qv(FUNCALL_STUB_Index, LispObj(addrs[0] as usize));
    set_qv(HELPER_STUB_Index, LispObj(addrs[1] as usize));
    LISP_CALL_ENTRY.store(addrs[2] as usize, Ordering::Relaxed);
    CATCH_ENTRY.store(addrs[3] as usize, Ordering::Relaxed);
    JUMP_ENTRY.store(addrs[4] as usize, Ordering::Relaxed);
    set_qv(KERNEL_CALL_STUB_Index, LispObj(addrs[5] as usize));
    let hs = helper_addresses();
    assert!(hs.len() <= MAX_HELPERS);
    for (i, (h, addr)) in hs.into_iter().enumerate() {
        assert_eq!(h as usize, i, "helper table out of order");
        set_qv(HELPERS_Index + i, LispObj(addr));
    }
    set_qv(DYN_FRAME_Index, LispObj(0));
    set_qv(THROW_PENDING_Index, LispObj(0));
    set_qv(THROWN_VALUE_Index, NIL);
    set_qv(STACK_LIMIT_Index, LispObj(0));
    update_stack_limit();
}

// ---------------------------------------------------------------------------
// Function objects and calls

/// A function object for compiled code object `code` closed over `env`.
pub fn make_function(code: LispObj, env: LispObj) -> LispObj {
    let f = heap::alloc_uvector(FUNCTION_SIZE, FUNCTION_TYPE);
    f.uset(FUNCTION_ENVIRONMENT, env);
    f.uset(FUNCTION_ADDRESS, code);
    f
}

/// Call a function from Rust.
pub fn lisp_call(f: LispObj, args: &[LispObj]) -> LispObj {
    let entry = LISP_CALL_ENTRY.load(Ordering::Relaxed);
    let fp: extern "C" fn(LispObj, *const LispObj, usize, usize) -> LispObj =
        unsafe { std::mem::transmute(entry) };
    fp(f, args.as_ptr(), args.len(), 0)
}

/// Call compiled variadic function `f` with its fixed arguments and, last,
/// the rest seq itself (see `REST_READY`).
pub fn lisp_call_rest_ready(f: LispObj, args: &[LispObj]) -> LispObj {
    let entry = LISP_CALL_ENTRY.load(Ordering::Relaxed);
    let fp: extern "C" fn(LispObj, *const LispObj, usize, usize) -> LispObj =
        unsafe { std::mem::transmute(entry) };
    fp(f, args.as_ptr(), args.len(), REST_READY)
}

struct CatchData<F, R> {
    f: Option<F>,
    r: Option<R>,
}

extern "C" fn catch_tramp<F: FnOnce() -> R, R>(d: *mut CatchData<F, R>) {
    let d = unsafe { &mut *d };
    let f = d.f.take().unwrap();
    d.r = Some(f());
}

/// A throw landed in `rust_catch`; the thrown value is QV `THROWN_VALUE`.
#[derive(Debug, Clone, Copy)]
pub struct Thrown;

/// Run `f` with a catcher established. Returns `Err(Thrown)` if a throw
/// landed there.
pub fn rust_catch<R>(f: impl FnOnce() -> R) -> Result<R, Thrown> {
    fn run<F: FnOnce() -> R, R>(f: F) -> Result<R, Thrown> {
        let mut d = CatchData::<F, R> { f: Some(f), r: None };
        let entry = CATCH_ENTRY.load(Ordering::Relaxed);
        let fp: extern "C" fn(LispObj, usize, *mut CatchData<F, R>) -> usize =
            unsafe { std::mem::transmute(entry) };
        let flag = fp(NIL, catch_tramp::<F, R> as *const () as usize, &mut d);
        if flag == 0 {
            Ok(d.r.take().unwrap())
        } else {
            Err(Thrown)
        }
    }
    run(f)
}

// ---------------------------------------------------------------------------
// Dynamic frames and throw

#[inline]
fn df(f: usize, i: usize) -> LispObj {
    unsafe { *((f + i * CELL) as *const LispObj) }
}

fn dyn_head() -> usize {
    qv(DYN_FRAME_Index).raw()
}

fn set_dyn_head(f: usize) {
    set_qv(DYN_FRAME_Index, LispObj(f));
}

/// The special binding frames, innermost first: (frame address, bound
/// symbol or var).
pub fn special_frames() -> Vec<(usize, LispObj)> {
    let mut out = Vec::new();
    let mut f = dyn_head();
    while f != 0 {
        if df(f, DF_KIND).integer() == DF_SPECIAL {
            out.push((f, df(f, DF_TAG)));
        }
        f = df(f, DF_PREV).raw();
    }
    out
}

/// The symbol or var a special frame binds.
pub fn frame_tag(f: usize) -> LispObj {
    df(f, DF_TAG)
}

/// The value a special frame restores on exit (the outer value).
pub fn frame_saved_value(f: usize) -> LispObj {
    df(f, DF_LANDING)
}

pub fn set_frame_saved_value(f: usize, v: LispObj) {
    unsafe { *((f + DF_LANDING * CELL) as *mut LispObj) = v }
}

/// The innermost catch frame, or 0.
fn find_catcher() -> usize {
    let mut f = dyn_head();
    while f != 0 && df(f, DF_KIND).integer() != DF_CATCH {
        f = df(f, DF_PREV).raw();
    }
    f
}

/// Is a catch frame established?
pub fn has_catcher() -> bool {
    find_catcher() != 0
}

/// Throw `value` to the innermost catch frame (there must be one).
pub fn throw_to(value: LispObj) -> ! {
    let target = find_catcher();
    assert!(target != 0, "throw with no catcher");
    unwind_to(target, value)
}

/// Unwind the dynamic frames down to catch frame `target`, running
/// unwind-protect cleanups, then land there.
fn unwind_to(target: usize, value: LispObj) -> ! {
    loop {
        let f = dyn_head();
        if f == 0 {
            eprintln!("internal error: throw target vanished");
            std::process::abort();
        }
        if f == target {
            set_qv(THROWN_VALUE_Index, value);
            jump(f, df(f, DF_RBP).raw(), df(f, DF_LANDING).raw());
        }
        let prev = df(f, DF_PREV).raw();
        match df(f, DF_KIND).integer() {
            DF_SPECIAL => {
                crate::clj::set_binding_value(df(f, DF_TAG), df(f, DF_LANDING));
                set_dyn_head(prev);
            }
            DF_CATCH => set_dyn_head(prev),
            DF_GC_INHIBIT => {
                heap::inhibit_gc(-1);
                set_dyn_head(prev);
            }
            DF_UNWIND_PROTECT => {
                set_dyn_head(prev);
                set_qv(THROW_PENDING_Index, LispObj(target));
                set_qv(THROWN_VALUE_Index, value);
                jump(f + DF_CELLS * CELL, df(f, DF_RBP).raw(), df(f, DF_LANDING).raw());
            }
            k => {
                eprintln!("internal error: bad dynamic frame kind {k}");
                std::process::abort();
            }
        }
    }
}

fn jump(rsp: usize, rbp: usize, target: usize) -> ! {
    let entry = JUMP_ENTRY.load(Ordering::Relaxed);
    let fp: extern "C" fn(usize, usize, usize) -> ! = unsafe { std::mem::transmute(entry) };
    fp(rsp, rbp, target)
}

/// Run `f` with garbage collection inhibited. A dynamic frame records the
/// inhibition so that a throw out of `f` (which skips Rust frames without
/// unwinding) re-enables collection.
pub fn without_gc<R>(f: impl FnOnce() -> R) -> R {
    let mut frame = [LispObj(0); DF_CELLS];
    frame[DF_PREV] = LispObj(dyn_head());
    frame[DF_KIND] = LispObj::fixnum(DF_GC_INHIBIT);
    let addr = std::hint::black_box(frame.as_mut_ptr()) as usize;
    set_dyn_head(addr);
    heap::inhibit_gc(1);
    let r = f();
    heap::inhibit_gc(-1);
    debug_assert_eq!(dyn_head(), addr);
    set_dyn_head(df(addr, DF_PREV).raw());
    std::hint::black_box(&mut frame);
    r
}

/// Run `f` with special `sym` bound to its current value, so the value is
/// restored however `f` exits (also by a throw past this frame).
pub fn rebinding<R>(sym: LispObj, f: impl FnOnce() -> R) -> R {
    let mut frame = [LispObj(0); DF_CELLS];
    frame[DF_PREV] = LispObj(dyn_head());
    frame[DF_KIND] = LispObj::fixnum(DF_SPECIAL);
    frame[DF_TAG] = sym;
    frame[DF_LANDING] = crate::clj::binding_value(sym);
    let addr = std::hint::black_box(frame.as_mut_ptr()) as usize;
    set_dyn_head(addr);
    let r = f();
    debug_assert_eq!(dyn_head(), addr);
    crate::clj::set_binding_value(sym, df(addr, DF_LANDING));
    set_dyn_head(df(addr, DF_PREV).raw());
    std::hint::black_box(&mut frame);
    r
}

/// `(k/progv vars vals thunk)`: `vars` and `vals` are seqs. Binds each
/// var in a special frame on this function's (Rust) stack, as
/// `without_gc` does, so a throw out of the body restores the values;
/// extra vars become unbound.
pub fn progv_prim(a: &[LispObj]) -> LispObj {
    let vars = crate::heap::vector_of(&crate::clj::coll::items(a[0]));
    let vals = crate::heap::vector_of(&crate::clj::coll::items(a[1]));
    progv_bind(vars, vals, 0, a[2])
}

fn progv_bind(vars: LispObj, vals: LispObj, i: usize, thunk: LispObj) -> LispObj {
    if i == vars.vector_length().integer() as usize {
        return prims::funcall(thunk, &[]);
    }
    let sym = vars.svref(i);
    if !crate::clj::is_var(sym) {
        lisp_error("progv: not a var");
    }
    let val = if i < vals.vector_length().integer() as usize { vals.svref(i) } else { SYMBOL_UNBOUND };
    let mut frame = [LispObj(0); DF_CELLS];
    frame[DF_PREV] = LispObj(dyn_head());
    frame[DF_KIND] = LispObj::fixnum(DF_SPECIAL);
    frame[DF_TAG] = sym;
    frame[DF_LANDING] = crate::clj::binding_value(sym);
    let addr = std::hint::black_box(frame.as_mut_ptr()) as usize;
    set_dyn_head(addr);
    crate::clj::set_binding_value(sym, val);
    let r = progv_bind(vars, vals, i + 1, thunk);
    debug_assert_eq!(dyn_head(), addr);
    crate::clj::set_binding_value(sym, df(addr, DF_LANDING));
    set_dyn_head(df(addr, DF_PREV).raw());
    std::hint::black_box(&mut frame);
    r
}
