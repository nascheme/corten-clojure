//! Clojure number primitives (see `crate::numbers`).

use super::{FALSE, TRUE};
use crate::heap::{double_float_node, single_float_node};
use crate::numbers::{self as n, Num, Overflow};
use crate::object::{LispObj, NIL};

fn cbool(b: bool) -> LispObj {
    if b { TRUE } else { FALSE }
}

fn int_arg(x: LispObj) -> i64 {
    if x.is_fixnum() { x.integer() } else { crate::error::type_error("Not a fixnum") }
}

/// `(k/arith op overflow a b)`: op 0 +, 1 -, 2 *; overflow 0 error,
/// 1 promote to BigInt (`+'`), 2 wrap (`unchecked-add`).
pub fn arith(a: &[LispObj]) -> LispObj {
    let ov = match int_arg(a[1]) {
        0 => Overflow::Error,
        1 => Overflow::Promote,
        _ => Overflow::Wrap,
    };
    n::arith_op(int_arg(a[0]), a[2], a[3], ov)
}

/// `(k/lbit op a b)`: bit operations on Longs (`numbers::bit_op`).
pub fn lbit(a: &[LispObj]) -> LispObj {
    n::bit_op(int_arg(a[0]), a[1], a[2])
}

/// `(k/lbit-not x)`.
pub fn lbit_not(a: &[LispObj]) -> LispObj {
    n::bit_not(a[0])
}

/// `(k/num-equiv a b)`: Clojure `=` on numbers.
pub fn num_equiv(a: &[LispObj]) -> LispObj {
    cbool(n::equiv(a[0], a[1]))
}

/// `(k/hash-number x)`: Clojure `hash` of a number.
pub fn hash_number(a: &[LispObj]) -> LispObj {
    LispObj::fixnum(n::hash(a[0]) as i64)
}

/// `(k/num-kind x)`: 0 not a number, 1 Long, 2 BigInt, 3 Ratio,
/// 4 double, 5 single float.
pub fn num_kind(a: &[LispObj]) -> LispObj {
    let x = a[0];
    LispObj::fixnum(if x.is_fixnum() || n::is_long(x) {
        1
    } else if n::is_bigint(x) {
        2
    } else if n::is_ratio(x) {
        3
    } else if x.is_double_float() {
        4
    } else if x.is_single_float() {
        5
    } else {
        0
    })
}

fn check_num(x: LispObj) -> Num {
    if !n::is_number(x) {
        crate::error::type_error(format!("Not a number: {}", super::printer::pr_str(x)));
    }
    n::num(x)
}

/// Long of `x` in `lo..=hi`, truncating floats and ratios (as Clojure's
/// RT.intCast etc.: a float must be in the range before truncation, NaN
/// gives 0).
fn ranged(x: LispObj, lo: i64, hi: i64, what: &str) -> LispObj {
    check_num(x);
    let bad = || -> ! { crate::error::argument_error(format!("Value out of range for {what}: {}", super::printer::pr_str(x))) };
    if x.is_float() {
        let f = n::to_f64(x);
        if f < lo as f64 || f > hi as f64 {
            bad();
        }
        return n::long_obj(if f.is_nan() { 0 } else { f as i64 });
    }
    let v = n::integer_value(n::to_long(x));
    if v < lo || v > hi {
        bad();
    }
    n::long_obj(v)
}

/// `(k/num-cast kind x)`: 0 `long`, 1 `int`, 2 `short`, 3 `byte`,
/// 4 `unchecked-int` (wraps to 32 bits), 5 `unchecked-long` (wraps to
/// 64 bits), 6 `bigint`, 7 `rationalize`, 8 `numerator`,
/// 9 `denominator`, 10 `double`, 11 `float`, 12 `num`. Kinds 0 to 5
/// also take a character, giving its code.
pub fn num_cast(a: &[LispObj]) -> LispObj {
    let mut x = a[1];
    // As in Clojure's RT casts, the integer casts take characters.
    if x.is_character() && int_arg(a[0]) <= 5 {
        x = n::long_obj(x.char_code() as i64);
    }
    match int_arg(a[0]) {
        0 => {
            check_num(x);
            if x.is_float() && n::to_f64(x).is_nan() {
                crate::error::argument_error("Value out of range for long: NaN");
            }
            n::to_long(x)
        }
        1 => ranged(x, i32::MIN as i64, i32::MAX as i64, "int"),
        2 => ranged(x, i16::MIN as i64, i16::MAX as i64, "short"),
        3 => ranged(x, i8::MIN as i64, i8::MAX as i64, "byte"),
        4 | 5 => {
            let v = match check_num(x) {
                Num::Int(i) => i,
                Num::Big(b) => {
                    let (_, digits) = b.to_u64_digits();
                    let m = digits.first().copied().unwrap_or(0) as i64;
                    if b.sign() == num_bigint::Sign::Minus { m.wrapping_neg() } else { m }
                }
                num => {
                    let f = n::as_f64(&num);
                    if f.is_nan() { 0 } else { f as i64 }
                }
            };
            n::long_obj(if int_arg(a[0]) == 4 { v as i32 as i64 } else { v })
        }
        6 => {
            if x.is_string() {
                let s = x.string_to_rust();
                let (neg, d) = match s.strip_prefix('-') {
                    Some(d) => (true, d),
                    None => (false, s.strip_prefix('+').unwrap_or(&s)),
                };
                let ok = !d.is_empty() && d.bytes().all(|c| c.is_ascii_digit());
                return match ok.then(|| n::parse_integer(d, 10, neg, true)).flatten() {
                    Some(v) => v,
                    None => crate::error::argument_error(format!("Invalid BigInt: {s}")),
                };
            }
            check_num(x);
            n::to_bigint(x)
        }
        7 => {
            check_num(x);
            n::rationalize(x)
        }
        8 => n::ratio_part(x, false),
        9 => n::ratio_part(x, true),
        10 => {
            check_num(x);
            if x.is_double_float() { x } else { double_float_node(n::to_f64(x)) }
        }
        11 => {
            check_num(x);
            let f = n::to_f64(x);
            if f.abs() > f32::MAX as f64 {
                crate::error::argument_error(format!("Value out of range for float: {}", super::printer::pr_str(x)));
            }
            single_float_node(f as f32)
        }
        _ => {
            if x != NIL {
                check_num(x);
            }
            x
        }
    }
}

/// `(k/integer-string x radix)`: digits of integer `x` in `radix`
/// (lower case, `-` sign).
pub fn integer_string(a: &[LispObj]) -> LispObj {
    if !n::is_integer(a[0]) {
        crate::error::type_error(format!("Not an integer: {}", super::printer::pr_str(a[0])));
    }
    let radix = int_arg(a[1]);
    if !(2..=36).contains(&radix) {
        crate::error::argument_error(format!("Bad radix: {radix}"));
    }
    crate::heap::string_node(&n::integer_string(a[0], radix as u32))
}

/// `(k/parse-integer s radix)`: the integer (Long if it fits) of
/// string `s` (optional sign, digits in `radix`), else an error.
pub fn parse_integer(a: &[LispObj]) -> LispObj {
    crate::prims::kernel::check_string(a[0]);
    let s = a[0].string_to_rust();
    let radix = int_arg(a[1]);
    let (neg, d) = match s.strip_prefix('-') {
        Some(d) => (true, d),
        None => (false, s.strip_prefix('+').unwrap_or(&s)),
    };
    let ok = (2..=36).contains(&radix) && !d.is_empty() && d.chars().all(|c| c.is_digit(radix as u32));
    match ok.then(|| n::parse_integer(d, radix as u32, neg, false)).flatten() {
        Some(v) => v,
        None => crate::error::argument_error(format!("number format error: {s}")),
    }
}
