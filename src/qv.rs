//! The QV globals vector, at the start of the heap region; compiled code
//! addresses it through R15. Only one Lisp thread is supported.

use crate::object::LispObj;
use crate::qv_index::*;
use std::sync::atomic::{AtomicPtr, Ordering};

static QV: AtomicPtr<LispObj> = AtomicPtr::new(std::ptr::null_mut());

pub(crate) fn set_qv_base(p: *mut LispObj) {
    QV.store(p, Ordering::Relaxed);
}

#[inline]
pub fn qv_base() -> *mut LispObj {
    QV.load(Ordering::Relaxed)
}

#[inline]
pub fn qv(i: usize) -> LispObj {
    debug_assert!(i < QV_MAX);
    unsafe { *qv_base().add(i) }
}

#[inline]
pub fn set_qv(i: usize, v: LispObj) {
    debug_assert!(i < QV_MAX);
    unsafe { *qv_base().add(i) = v }
}
