//! QV slot indices. QV is the kernel's globals vector, at the start of
//! the heap region; compiled code addresses it through R15. Slots hold
//! raw (untagged) values unless noted; tagged slots are GC roots.
#![allow(non_upper_case_globals)]

/// Allocation pointer of the current block.
pub const THREAD_HEAP_Index: usize = 0;
/// End of the current allocation block.
pub const THREAD_HEAP_END_Index: usize = 1;
/// The value being thrown, while a throw lands or runs an
/// unwind-protect cleanup (tagged).
pub const THROWN_VALUE_Index: usize = 2;
/// Head of the dynamic frame chain (catch, unwind-protect, special binding).
pub const DYN_FRAME_Index: usize = 3;
/// Address of the generic funcall stub.
pub const FUNCALL_STUB_Index: usize = 4;
/// Address of the stub calling a Rust helper whose index is in RAX.
pub const HELPER_STUB_Index: usize = 5;
/// Target frame (raw address) of the throw whose unwind-protect
/// cleanup runs.
pub const THROW_PENDING_Index: usize = 6;
/// Simple-vector of Clojure kernel globals (`clj::G`), tagged.
pub const CLJ_GLOBALS_Index: usize = 8;
/// Lowest stack address compiled code may run at: below it a function
/// signals a stack overflow error (`runtime::check_stack`).
pub const STACK_LIMIT_Index: usize = 9;
/// Number of collections so far, counting those in the processes that
/// built the current image (fixnum).
pub const GC_COUNT_Index: usize = 10;

/// Address of the stub calling kernel function RDI (`prims::PRIMS`
/// index), skipping the funcall stub's type dispatch.
pub const KERNEL_CALL_STUB_Index: usize = 11;

/// Table of Rust helper addresses called from compiled code.
pub const HELPERS_Index: usize = 12;
pub const MAX_HELPERS: usize = 112;

pub const QV_MAX: usize = HELPERS_Index + MAX_HELPERS;
