//! Kernel primitives on characters, strings, files and numbers, and the
//! argument checks shared by primitives.

use crate::numbers::{self};
use crate::object::*;

fn err_obj(msg: &str, x: LispObj) -> ! {
    crate::error::type_error(format!("{msg}: {}", crate::clj::printer::pr_str(x)))
}

pub fn check_fixnum(x: LispObj) -> i64 {
    if !x.is_fixnum() {
        err_obj("Not an integer", x);
    }
    x.integer()
}

pub fn check_index(x: LispObj) -> usize {
    let n = check_fixnum(x);
    if n < 0 {
        err_obj("Not a valid index", x);
    }
    n as usize
}

pub fn check_character(x: LispObj) -> u32 {
    if !x.is_character() {
        err_obj("Not a character", x);
    }
    x.char_code()
}

pub fn check_uvector(x: LispObj) {
    if !x.is_uvector() {
        err_obj("Not a uvector", x);
    }
}

pub fn check_string(x: LispObj) {
    if !x.is_string() {
        err_obj("Not a string", x);
    }
}

/// Contents of a string (simple or adjustable) as UTF-16 units.
pub fn string_units(s: LispObj) -> &'static [LispChar] {
    let len = s.vector_length().integer() as usize;
    let len = if s.is_adjustable_array() && has_fill_pointer(s) {
        s.uref(ARRAY_FILL_POINTER).integer() as usize
    } else {
        len
    };
    unsafe { std::slice::from_raw_parts(s.char_array_start(), len) }
}

pub fn has_fill_pointer(a: LispObj) -> bool {
    a.is_adjustable_array() && a.uref(ARRAY_FILL_POINTER) != NIL
}

// ---------------------------------------------------------------------------
// Characters

pub fn char_int(a: &[LispObj]) -> LispObj {
    LispObj::fixnum(check_character(a[0]) as i64)
}

pub fn int_char(a: &[LispObj]) -> LispObj {
    // Characters are UTF-16 units.
    LispObj::character((check_fixnum(a[0]) as u32) & 0xffff)
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

pred!(numberp, numbers::is_number);

// ---------------------------------------------------------------------------
// Strings

/// `STRING=` (two strings, case sensitive).
pub fn string_equal(a: &[LispObj]) -> LispObj {
    check_string(a[0]);
    check_string(a[1]);
    cbool(string_units(a[0]) == string_units(a[1]))
}

// ---------------------------------------------------------------------------
// Numbers

pub fn float(a: &[LispObj]) -> LispObj {
    let double = matches!(a.get(1), Some(p) if p.is_double_float());
    numbers::to_float(a[0], double)
}

// ---------------------------------------------------------------------------
// Misc

/// (GC &optional level): collect garbage now. There are no generations,
/// so the level is ignored.
pub fn gc(_a: &[LispObj]) -> LispObj {
    crate::gc::collect();
    NIL
}

fn simple_vector_index(v: LispObj, index: LispObj) -> usize {
    check_uvector(v);
    let i = check_index(index);
    if i >= v.vector_length().integer() as usize {
        err_obj("Index out of range", index);
    }
    i
}

pub fn svref(a: &[LispObj]) -> LispObj {
    if !a[0].is_uvector() || a[0].uvector_type() != SIMPLE_VECTOR_TYPE {
        err_obj("Not a simple vector", a[0]);
    }
    let i = simple_vector_index(a[0], a[1]);
    a[0].svref(i)
}
