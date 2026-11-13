//! Core kernel primitives: functions, predicates, numbers and output.

use super::funcall;
use crate::numbers;
use crate::object::*;

// ---------------------------------------------------------------------------
// Functions

pub fn funcall_prim(a: &[LispObj]) -> LispObj {
    funcall(a[0], &a[1..])
}

// ---------------------------------------------------------------------------
// Predicates

macro_rules! pred {
    ($name:ident, $test:expr) => {
        pub fn $name(a: &[LispObj]) -> LispObj {
            let f: fn(LispObj) -> bool = $test;
            cbool(f(a[0]))
        }
    };
}

pred!(null_prim, |x| x == NIL);
pred!(stringp, |x| x.is_string());
pred!(integerp, crate::numbers::is_integer);
pred!(characterp, |x| x.is_character());
pred!(functionp, crate::runtime::is_funcallable);

pub fn eq_prim(a: &[LispObj]) -> LispObj {
    cbool(a[0] == a[1])
}

// ---------------------------------------------------------------------------
// Numbers

pub fn plus(a: &[LispObj]) -> LispObj {
    let mut r = LispObj::fixnum(0);
    for &x in a {
        r = numbers::add(r, x);
    }
    r
}

pub fn times(a: &[LispObj]) -> LispObj {
    let mut r = LispObj::fixnum(1);
    for &x in a {
        r = numbers::mul(r, x);
    }
    r
}

pub fn minus(a: &[LispObj]) -> LispObj {
    if a.len() == 1 {
        return numbers::sub(LispObj::fixnum(0), a[0]);
    }
    a[1..].iter().fold(a[0], |r, &x| numbers::sub(r, x))
}

pub fn divide(a: &[LispObj]) -> LispObj {
    if a.len() == 1 {
        return numbers::div(LispObj::fixnum(1), a[0]);
    }
    a[1..].iter().fold(a[0], |r, &x| numbers::div(r, x))
}

fn compare_chain(a: &[LispObj], ok: fn(std::cmp::Ordering) -> bool) -> LispObj {
    for &x in a {
        numbers::check_number(x);
    }
    cbool(a.windows(2).all(|w| numbers::partial_compare(w[0], w[1]).is_some_and(ok)))
}

pub fn num_eq(a: &[LispObj]) -> LispObj {
    compare_chain(a, |o| o.is_eq())
}
pub fn num_lt(a: &[LispObj]) -> LispObj {
    compare_chain(a, |o| o.is_lt())
}
pub fn num_le(a: &[LispObj]) -> LispObj {
    compare_chain(a, |o| o.is_le())
}
pub fn num_gt(a: &[LispObj]) -> LispObj {
    compare_chain(a, |o| o.is_gt())
}
pub fn num_ge(a: &[LispObj]) -> LispObj {
    compare_chain(a, |o| o.is_ge())
}

/// `(k/truncate a b)`: the quotient of `a` by `b` rounded toward zero.
pub fn truncate(a: &[LispObj]) -> LispObj {
    let d = a.get(1).copied().unwrap_or(LispObj::fixnum(1));
    numbers::int_divide(a[0], d, numbers::Rounding::Trunc).0
}

pub fn mod_prim(a: &[LispObj]) -> LispObj {
    numbers::int_divide(a[0], a[1], numbers::Rounding::Floor).1
}

// ---------------------------------------------------------------------------
// Misc

/// `(GET-INTERNAL-REAL-TIME)`: monotonic microseconds.
pub fn get_internal_real_time(_a: &[LispObj]) -> LispObj {
    let mut ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) };
    LispObj::fixnum(ts.tv_sec * 1_000_000 + ts.tv_nsec / 1000)
}
