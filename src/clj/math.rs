//! `clojure.math` (and the matching `Math/` statics): `java.lang.Math`
//! semantics on doubles and longs.

use crate::error::{arithmetic_error, argument_error};
use crate::heap::double_float_node;
use crate::numbers::{integer_value, long_obj, to_f64};
use crate::object::LispObj;

unsafe extern "C" {
    fn remainder(x: f64, y: f64) -> f64;
    fn scalbn(x: f64, n: i32) -> f64;
}

fn op_name(x: LispObj) -> String {
    if !x.is_string() {
        argument_error("Math: bad operation");
    }
    x.string_to_rust()
}

/// `Math.getExponent`: the unbiased exponent (1024 for NaN and the
/// infinities, -1023 for zero and subnormals).
fn get_exponent(d: f64) -> i64 {
    ((d.to_bits() >> 52) & 0x7ff) as i64 - 1023
}

/// `Math.ulp`.
fn ulp(d: f64) -> f64 {
    if d.is_nan() {
        return d;
    }
    if d.is_infinite() {
        return f64::INFINITY;
    }
    let e = get_exponent(d);
    if e == -1023 {
        return f64::from_bits(1);
    }
    let e = e - 52;
    if e >= -1022 {
        f64::from_bits(((e + 1023) as u64) << 52)
    } else {
        f64::from_bits(1u64 << (e + 1074))
    }
}

/// `Math.signum`: zeros and NaN are returned as is.
fn signum(d: f64) -> f64 {
    if d == 0.0 || d.is_nan() { d } else { d.signum() }
}

/// `Math.nextAfter(start, direction)`.
fn next_after(start: f64, direction: f64) -> f64 {
    if start.is_nan() || direction.is_nan() {
        f64::NAN
    } else if start == direction {
        direction
    } else if start < direction {
        start.next_up()
    } else {
        start.next_down()
    }
}

/// `Math.pow`: C's `pow` except that a NaN exponent or 1.0 to an
/// infinite power is NaN.
fn pow(x: f64, y: f64) -> f64 {
    if y.is_nan() || (x.abs() == 1.0 && y.is_infinite()) {
        return f64::NAN;
    }
    x.powf(y)
}

/// `Math.round`: the nearest long, ties toward positive infinity,
/// saturating; NaN is 0.
fn round(d: f64) -> i64 {
    if d.is_nan() {
        return 0;
    }
    let f = d.floor();
    let r = if d - f >= 0.5 { f + 1.0 } else { f };
    r as i64
}

/// `(k/math1 op x)`: a function of one double, as a double.
pub fn math1(a: &[LispObj]) -> LispObj {
    let op = op_name(a[0]);
    let d = to_f64(a[1]);
    let r = match op.as_str() {
        "sin" => d.sin(),
        "cos" => d.cos(),
        "tan" => d.tan(),
        "asin" => d.asin(),
        "acos" => d.acos(),
        "atan" => d.atan(),
        "to-radians" => d * (std::f64::consts::PI / 180.0),
        "to-degrees" => d * (180.0 / std::f64::consts::PI),
        "exp" => d.exp(),
        "log" => d.ln(),
        "log10" => d.log10(),
        "sqrt" => d.sqrt(),
        "cbrt" => d.cbrt(),
        "ceil" => d.ceil(),
        "floor" => d.floor(),
        "rint" => d.round_ties_even(),
        "ulp" => ulp(d),
        "signum" => signum(d),
        "sinh" => d.sinh(),
        "cosh" => d.cosh(),
        "tanh" => d.tanh(),
        "expm1" => d.exp_m1(),
        "log1p" => d.ln_1p(),
        "next-up" => d.next_up(),
        "next-down" => d.next_down(),
        "round" => return long_obj(round(d)),
        "get-exponent" => return LispObj::fixnum(get_exponent(d)),
        _ => argument_error(format!("No Math function {op}")),
    };
    double_float_node(r)
}

/// `(k/math2 op x y)`: a function of two doubles, as a double.
pub fn math2(a: &[LispObj]) -> LispObj {
    let op = op_name(a[0]);
    let x = to_f64(a[1]);
    if op == "scalb" {
        let n = integer_value(a[2]).clamp(i32::MIN as i64, i32::MAX as i64) as i32;
        return double_float_node(unsafe { scalbn(x, n) });
    }
    let y = to_f64(a[2]);
    let r = match op.as_str() {
        "IEEE-remainder" => unsafe { remainder(x, y) },
        "atan2" => x.atan2(y),
        "pow" => pow(x, y),
        "hypot" => x.hypot(y),
        "copy-sign" => x.copysign(y),
        "next-after" => next_after(x, y),
        _ => argument_error(format!("No Math function {op}")),
    };
    double_float_node(r)
}

/// `(k/math-exact op a [b])`: long arithmetic that throws on overflow
/// (`add-exact` etc.), and `floor-div`/`floor-mod`.
pub fn math_exact(a: &[LispObj]) -> LispObj {
    let op = op_name(a[0]);
    let x = integer_value(a[1]);
    let y = if a.len() == 3 { integer_value(a[2]) } else { 0 };
    let r = match op.as_str() {
        "add-exact" => x.checked_add(y),
        "subtract-exact" => x.checked_sub(y),
        "multiply-exact" => x.checked_mul(y),
        "increment-exact" => x.checked_add(1),
        "decrement-exact" => x.checked_sub(1),
        "negate-exact" => x.checked_neg(),
        "floor-div" | "floor-mod" => {
            if y == 0 {
                arithmetic_error("Divide by zero");
            }
            let q = x.wrapping_div(y);
            let q = if (x.wrapping_rem(y) != 0) && ((x < 0) != (y < 0)) { q - 1 } else { q };
            Some(if op == "floor-div" { q } else { x.wrapping_sub(q.wrapping_mul(y)) })
        }
        _ => argument_error(format!("No Math function {op}")),
    };
    match r {
        Some(v) => long_obj(v),
        None => arithmetic_error("long overflow"),
    }
}

/// `(k/double-compare x y)`: `Double.compare`, a total order with
/// -0.0 below 0.0 and NaN above everything (and equal to itself).
pub fn double_compare(a: &[LispObj]) -> LispObj {
    let canon = |d: f64| if d.is_nan() { f64::NAN } else { d };
    let (x, y) = (canon(to_f64(a[0])), canon(to_f64(a[1])));
    LispObj::fixnum(x.total_cmp(&y) as i64)
}
