//! Clojure numbers: Long (fixnums, and 64-bit values outside the fixnum
//! range boxed in a bignum uvector), BigInt, Ratio (of integers) and
//! floats (double, and single for `float`). Arithmetic follows Clojure's
//! `clojure.lang.Numbers`: Long operations throw on overflow (the `_p`
//! versions promote to BigInt), BigInt and Ratio are contagious, any
//! float gives a double.
//!
//! Bignum uvector (`BIGNUM_TYPE`, raw cells): cell 1 = `nlimbs << 2 |
//! LONG_FLAG | NEG_FLAG`, cells 2.. = magnitude limbs, least significant
//! first. A boxed Long has `LONG_FLAG` and one limb.

use std::cmp::Ordering;

use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

use crate::error::lisp_error;
use crate::heap::{alloc_uvector, double_float_node};
use crate::object::*;

pub enum Num {
    /// A Long (fixnum or boxed).
    Int(i64),
    /// A BigInt (whatever its magnitude).
    Big(BigInt),
    /// A normalized ratio: denominator > 1.
    Ratio(BigInt, BigInt),
    Single(f32),
    Double(f64),
}

pub const RATIO_NUMERATOR: usize = 1;
pub const RATIO_DENOMINATOR: usize = 2;

const NEG_FLAG: usize = 1;
const LONG_FLAG: usize = 2;

fn bignum_info(x: LispObj) -> usize {
    x.uref(1).raw()
}

fn bignum_limbs(x: LispObj) -> &'static [u64] {
    let n = bignum_info(x) >> 2;
    unsafe { std::slice::from_raw_parts(x.uvector_ptr().add(2) as *const u64, n) }
}

fn alloc_bignum(neg: bool, long: bool, limbs: &[u64]) -> LispObj {
    let v = alloc_uvector(1 + limbs.len().max(1), BIGNUM_TYPE);
    let info = limbs.len() << 2 | if neg { NEG_FLAG } else { 0 } | if long { LONG_FLAG } else { 0 };
    v.uset(1, LispObj(info));
    for (i, &l) in limbs.iter().enumerate() {
        v.uset(2 + i, LispObj(l as usize));
    }
    v
}

/// A Long: a fixnum if it fits, else boxed.
pub fn long_obj(n: i64) -> LispObj {
    match LispObj::fixnum_checked(n) {
        Some(v) => v,
        None => alloc_bignum(n < 0, true, &[n.unsigned_abs()]),
    }
}

/// A BigInt.
pub fn bigint_obj(n: &BigInt) -> LispObj {
    let (sign, limbs) = n.to_u64_digits();
    alloc_bignum(sign == Sign::Minus, false, &limbs)
}

/// An integer: a Long if it fits, else a BigInt.
pub fn integer_obj(n: &BigInt) -> LispObj {
    match n.to_i64() {
        Some(v) => long_obj(v),
        None => bigint_obj(n),
    }
}

pub fn is_bignum(x: LispObj) -> bool {
    x.is_uvector_of(BIGNUM_TYPE)
}

/// A Long (fixnum or boxed).
pub fn is_long(x: LispObj) -> bool {
    x.is_fixnum() || (is_bignum(x) && bignum_info(x) & LONG_FLAG != 0)
}

pub fn is_bigint(x: LispObj) -> bool {
    is_bignum(x) && bignum_info(x) & LONG_FLAG == 0
}

pub fn is_integer(x: LispObj) -> bool {
    x.is_fixnum() || is_bignum(x)
}

pub fn is_ratio(x: LispObj) -> bool {
    x.is_uvector_of(RATIO_TYPE)
}

pub fn is_number(x: LispObj) -> bool {
    x.is_fixnum() || x.is_float() || is_bignum(x) || is_ratio(x)
}

fn bignum_value(x: LispObj) -> BigInt {
    let sign = if bignum_info(x) & NEG_FLAG != 0 { Sign::Minus } else { Sign::Plus };
    BigInt::from_biguint(sign, num_bigint::BigUint::new(u32_digits(bignum_limbs(x))))
}

fn u32_digits(limbs: &[u64]) -> Vec<u32> {
    limbs.iter().flat_map(|&l| [l as u32, (l >> 32) as u32]).collect()
}

/// The integer value of an integer object.
fn integer_value_big(x: LispObj) -> BigInt {
    if x.is_fixnum() { BigInt::from(x.integer()) } else { bignum_value(x) }
}

pub fn num(x: LispObj) -> Num {
    if x.is_fixnum() {
        Num::Int(x.integer())
    } else if x.is_double_float() {
        Num::Double(x.double_float())
    } else if is_bignum(x) {
        if bignum_info(x) & LONG_FLAG != 0 {
            let m = bignum_limbs(x)[0];
            Num::Int(if bignum_info(x) & NEG_FLAG != 0 { (m as i64).wrapping_neg() } else { m as i64 })
        } else {
            Num::Big(bignum_value(x))
        }
    } else if is_ratio(x) {
        Num::Ratio(integer_value_big(x.uref(RATIO_NUMERATOR)), integer_value_big(x.uref(RATIO_DENOMINATOR)))
    } else if x.is_single_float() {
        Num::Single(x.single_float())
    } else {
        crate::error::type_error(format!("Not a number: {}", crate::clj::printer::pr_str(x)))
    }
}

pub fn check_number(x: LispObj) {
    if !is_number(x) {
        num(x);
    }
}

/// A Long's value (an error for anything else).
pub fn integer_value(x: LispObj) -> i64 {
    if x.is_fixnum() {
        return x.integer();
    }
    match is_long(x).then(|| num(x)) {
        Some(Num::Int(i)) => i,
        _ => crate::error::type_error(format!("Not a long: {}", crate::clj::printer::pr_str(x))),
    }
}

fn overflow() -> ! {
    crate::error::arithmetic_error("integer overflow")
}

pub fn divide_by_zero() -> ! {
    crate::error::arithmetic_error("Divide by zero")
}

/// Normalized rational n/d as a BigInt or a Ratio (Clojure's
/// `Numbers.divide(BigInteger, BigInteger)`).
pub fn make_ratio(n: BigInt, d: BigInt) -> LispObj {
    if d.is_zero() {
        divide_by_zero();
    }
    let g = n.gcd(&d);
    let (mut n, mut d) = (n / &g, d / &g);
    if d.is_negative() {
        n = -n;
        d = -d;
    }
    if d.is_one() {
        return bigint_obj(&n);
    }
    let r = alloc_uvector(RATIO_DENOMINATOR, RATIO_TYPE);
    r.uset(RATIO_NUMERATOR, LispObj::fixnum(0));
    r.uset(RATIO_DENOMINATOR, LispObj::fixnum(0));
    let no = integer_obj(&n);
    r.uset(RATIO_NUMERATOR, no);
    let dobj = integer_obj(&d);
    r.uset(RATIO_DENOMINATOR, dobj);
    r
}

pub fn as_f64(n: &Num) -> f64 {
    match n {
        Num::Int(i) => *i as f64,
        Num::Big(b) => big_to_f64(b),
        Num::Ratio(a, b) => ratio_to_f64(a, b),
        Num::Single(f) => *f as f64,
        Num::Double(f) => *f,
    }
}

fn big_to_f64(b: &BigInt) -> f64 {
    b.to_f64().unwrap_or(if b.is_negative() { f64::NEG_INFINITY } else { f64::INFINITY })
}

fn ratio_to_f64(a: &BigInt, b: &BigInt) -> f64 {
    match (a.to_f64(), b.to_f64()) {
        (Some(x), Some(y)) if x.is_finite() && y.is_finite() && x.abs() < 1e300 && y.abs() < 1e300 => x / y,
        _ => {
            // Scale to keep 64 significant bits.
            let shift = a.bits() as i64 - b.bits() as i64 - 64;
            let q = if shift > 0 { a / (b << shift as usize) } else { (a << (-shift) as usize) / b };
            big_to_f64(&q) * 2f64.powi(shift as i32)
        }
    }
}

pub fn is_float_num(n: &Num) -> bool {
    matches!(n, Num::Single(_) | Num::Double(_))
}

fn to_big(n: &Num) -> BigInt {
    match n {
        Num::Int(i) => BigInt::from(*i),
        Num::Big(b) => b.clone(),
        _ => unreachable!(),
    }
}

/// Numerator and denominator of an exact number.
fn to_rat(n: &Num) -> (BigInt, BigInt) {
    match n {
        Num::Ratio(a, b) => (a.clone(), b.clone()),
        _ => (to_big(n), BigInt::one()),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
}

/// Binary +, -, * with Long overflow `overflow`: error, promote to
/// BigInt, or wrap (unchecked).
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Overflow {
    Error,
    Promote,
    Wrap,
}

fn arith(a: LispObj, b: LispObj, op: Op, ov: Overflow) -> LispObj {
    let (x, y) = (num(a), num(b));
    match (&x, &y) {
        (Num::Int(i), Num::Int(j)) => {
            let r = match op {
                Op::Add => i.checked_add(*j),
                Op::Sub => i.checked_sub(*j),
                Op::Mul => i.checked_mul(*j),
            };
            match (r, ov) {
                (Some(v), _) => long_obj(v),
                (None, Overflow::Error) => overflow(),
                (None, Overflow::Wrap) => long_obj(match op {
                    Op::Add => i.wrapping_add(*j),
                    Op::Sub => i.wrapping_sub(*j),
                    Op::Mul => i.wrapping_mul(*j),
                }),
                (None, Overflow::Promote) => {
                    let (p, q) = (BigInt::from(*i), BigInt::from(*j));
                    bigint_obj(&match op {
                        Op::Add => p + q,
                        Op::Sub => p - q,
                        Op::Mul => p * q,
                    })
                }
            }
        }
        _ if is_float_num(&x) || is_float_num(&y) => {
            let (p, q) = (as_f64(&x), as_f64(&y));
            double_float_node(match op {
                Op::Add => p + q,
                Op::Sub => p - q,
                Op::Mul => p * q,
            })
        }
        (Num::Ratio(..), _) | (_, Num::Ratio(..)) => {
            let ((p, q), (r, s)) = (to_rat(&x), to_rat(&y));
            match op {
                Op::Add => make_ratio(p * &s + r * &q, q * s),
                Op::Sub => make_ratio(p * &s - r * &q, q * s),
                Op::Mul => make_ratio(p * r, q * s),
            }
        }
        _ => {
            let (p, q) = (to_big(&x), to_big(&y));
            bigint_obj(&match op {
                Op::Add => p + q,
                Op::Sub => p - q,
                Op::Mul => p * q,
            })
        }
    }
}

pub fn add(a: LispObj, b: LispObj) -> LispObj {
    if a.is_fixnum() && b.is_fixnum() {
        return long_obj(a.integer() + b.integer());
    }
    arith(a, b, Op::Add, Overflow::Error)
}

pub fn sub(a: LispObj, b: LispObj) -> LispObj {
    if a.is_fixnum() && b.is_fixnum() {
        return long_obj(a.integer() - b.integer());
    }
    arith(a, b, Op::Sub, Overflow::Error)
}

pub fn mul(a: LispObj, b: LispObj) -> LispObj {
    arith(a, b, Op::Mul, Overflow::Error)
}

/// `op` 0 +, 1 -, 2 * with overflow handling `ov`.
pub fn arith_op(op: i64, a: LispObj, b: LispObj, ov: Overflow) -> LispObj {
    let op = match op {
        0 => Op::Add,
        1 => Op::Sub,
        _ => Op::Mul,
    };
    arith(a, b, op, ov)
}

/// Clojure `/`: an exact Long quotient of Longs is a Long; other exact
/// quotients a BigInt or Ratio; division by exact zero is an error;
/// floats divide as doubles (IEEE).
pub fn div(a: LispObj, b: LispObj) -> LispObj {
    let (x, y) = (num(a), num(b));
    if is_float_num(&x) || is_float_num(&y) {
        return double_float_node(as_f64(&x) / as_f64(&y));
    }
    if let (Num::Int(i), Num::Int(j)) = (&x, &y) {
        if *j == 0 {
            divide_by_zero();
        }
        if i % j == 0
            && let Some(q) = i.checked_div(*j)
        {
            return long_obj(q);
        }
    }
    let ((p, q), (r, s)) = (to_rat(&x), to_rat(&y));
    if r.is_zero() {
        divide_by_zero();
    }
    make_ratio(p * s, q * r)
}

pub fn compare(a: LispObj, b: LispObj) -> Ordering {
    partial_compare(a, b).unwrap_or(Ordering::Less)
}

/// Numeric comparison; `None` when either number is a NaN (so `<`, `==`
/// etc. are all false). Floats compare as doubles.
pub fn partial_compare(a: LispObj, b: LispObj) -> Option<Ordering> {
    if a.is_fixnum() && b.is_fixnum() {
        return Some(a.integer().cmp(&b.integer()));
    }
    let (x, y) = (num(a), num(b));
    match (&x, &y) {
        (Num::Int(i), Num::Int(j)) => Some(i.cmp(j)),
        _ if is_float_num(&x) || is_float_num(&y) => as_f64(&x).partial_cmp(&as_f64(&y)),
        _ => {
            let ((p, q), (r, s)) = (to_rat(&x), to_rat(&y));
            Some((p * s).cmp(&(r * q)))
        }
    }
}

#[derive(PartialEq, Eq)]
enum Category {
    Integer,
    Ratio,
    Float,
}

fn category(n: &Num) -> Category {
    match n {
        Num::Int(_) | Num::Big(_) => Category::Integer,
        Num::Ratio(..) => Category::Ratio,
        _ => Category::Float,
    }
}

/// Clojure `=` on two numbers: the same category (integer, ratio,
/// float) and `==`.
pub fn equiv(a: LispObj, b: LispObj) -> bool {
    if a.is_fixnum() && b.is_fixnum() {
        return a == b;
    }
    if !is_number(a) || !is_number(b) {
        return false;
    }
    category(&num(a)) == category(&num(b)) && partial_compare(a, b) == Some(Ordering::Equal)
}

fn hash_long(n: i64) -> i32 {
    // Murmur3 hashLong, as Clojure's Numbers.hasheq for Longs.
    if n == 0 {
        return 0;
    }
    let mix_k1 = |k: u32| k.wrapping_mul(0xcc9e2d51).rotate_left(15).wrapping_mul(0x1b873593);
    let mix_h1 = |h: u32, k: u32| (h ^ k).rotate_left(13).wrapping_mul(5).wrapping_add(0xe6546b64);
    let low = n as u32;
    let high = (n as u64 >> 32) as u32;
    let mut h = mix_h1(0, mix_k1(low));
    h = mix_h1(h, mix_k1(high));
    h ^= 8;
    h ^= h >> 16;
    h = h.wrapping_mul(0x85ebca6b);
    h ^= h >> 13;
    h = h.wrapping_mul(0xc2b2ae35);
    h ^= h >> 16;
    h as i32
}

fn hash_big(b: &BigInt) -> i32 {
    match b.to_i64() {
        Some(v) => hash_long(v),
        None => b.to_signed_bytes_le().chunks(4).fold(0i32, |h, c| {
            let mut w = [0u8; 4];
            w[..c.len()].copy_from_slice(c);
            h.wrapping_mul(31).wrapping_add(i32::from_le_bytes(w))
        }),
    }
}

/// Hash consistent with `equiv`.
pub fn hash(x: LispObj) -> i32 {
    match num(x) {
        Num::Int(i) => hash_long(i),
        Num::Big(b) => hash_big(&b),
        Num::Ratio(a, b) => hash_big(&a) ^ hash_big(&b),
        n => {
            let f = as_f64(&n);
            let f = if f == 0.0 { 0.0 } else { f };
            let bits = f.to_bits();
            ((bits >> 32) as u32 ^ bits as u32) as i32
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Rounding {
    Trunc,
    Floor,
    Ceiling,
    Round,
}

fn round_f(q: f64, mode: Rounding) -> f64 {
    match mode {
        Rounding::Trunc => q.trunc(),
        Rounding::Floor => q.floor(),
        Rounding::Ceiling => q.ceil(),
        Rounding::Round => q.round_ties_even(),
    }
}

fn round_big(n: BigInt, d: BigInt, mode: Rounding) -> BigInt {
    let (n, d) = if d.is_negative() { (-n, -d) } else { (n, d) };
    let (fl, rem) = n.div_mod_floor(&d);
    match mode {
        Rounding::Floor => fl,
        Rounding::Ceiling => fl + i32::from(!rem.is_zero()),
        Rounding::Trunc => fl + i32::from(!rem.is_zero() && n.is_negative()),
        Rounding::Round => {
            let twice: BigInt = rem * 2;
            if twice > d || (twice == d && fl.is_odd()) { fl + 1 } else { fl }
        }
    }
}

/// Integer division with the given rounding. Returns (quotient,
/// remainder). Exact operands give an integer quotient (a Long if it
/// fits, a BigInt if either operand is one); floats give double ones.
pub fn int_divide(a: LispObj, b: LispObj, mode: Rounding) -> (LispObj, LispObj) {
    let (x, y) = (num(a), num(b));
    if is_float_num(&x) || is_float_num(&y) {
        let (xf, yf) = (as_f64(&x), as_f64(&y));
        let q = round_f(xf / yf, mode);
        return (double_float_node(q), double_float_node(xf - q * yf));
    }
    if let (Num::Int(i), Num::Int(j)) = (&x, &y) {
        let (i, j) = (*i, *j);
        if j == 0 {
            divide_by_zero();
        }
        if let (Some(mut q), Some(mut r)) = (i.checked_div(j), i.checked_rem(j)) {
            if r != 0 {
                let adjust = match mode {
                    Rounding::Trunc => 0,
                    Rounding::Floor => -i64::from((r < 0) != (j < 0)),
                    Rounding::Ceiling => i64::from((r < 0) == (j < 0)),
                    Rounding::Round => {
                        let twice = (r.unsigned_abs() as u128) * 2;
                        let aj = j.unsigned_abs() as u128;
                        let sign = if (i < 0) != (j < 0) { -1 } else { 1 };
                        if twice > aj || (twice == aj && q % 2 != 0) { sign } else { 0 }
                    }
                };
                q += adjust;
                r = i - q * j;
            }
            return (long_obj(q), long_obj(r));
        }
        // MIN / -1
        overflow();
    }
    // Any BigInt or Ratio operand gives a BigInt quotient.
    let big = !matches!((&x, &y), (Num::Int(_), Num::Int(_)));
    let ((p, q), (r, s)) = (to_rat(&x), to_rat(&y));
    if r.is_zero() {
        divide_by_zero();
    }
    let quot = round_big(p * &s, q * r, mode);
    let qo = if big { bigint_obj(&quot) } else { integer_obj(&quot) };
    // x - q*y
    let rem = arith(a, mul_exact(qo, b), Op::Sub, Overflow::Promote);
    (qo, rem)
}

fn mul_exact(a: LispObj, b: LispObj) -> LispObj {
    arith(a, b, Op::Mul, Overflow::Promote)
}

/// Convert to a double, or a single float if `double` is false.
pub fn to_float(x: LispObj, double: bool) -> LispObj {
    let n = num(x);
    match (&n, double) {
        (Num::Double(_), true) | (Num::Single(_), false) => x,
        (_, true) => double_float_node(as_f64(&n)),
        (_, false) => crate::heap::single_float_node(as_f64(&n) as f32),
    }
}

/// The value of an integer object as a BigInt.
pub fn to_bigint_value(x: LispObj) -> Option<BigInt> {
    match num(x) {
        Num::Int(i) => Some(BigInt::from(i)),
        Num::Big(b) => Some(b),
        _ => None,
    }
}

/// Decimal digits of an integer object (no `N`).
pub fn integer_string(x: LispObj, radix: u32) -> String {
    match num(x) {
        Num::Int(i) if radix == 10 => i.to_string(),
        n => to_big(&n).to_str_radix(radix),
    }
}

/// Clojure string form of an exact number (`str`): BigInts without `N`.
pub fn exact_string(x: LispObj) -> Option<String> {
    match num(x) {
        Num::Int(i) => Some(i.to_string()),
        Num::Big(b) => Some(b.to_string()),
        Num::Ratio(a, b) => Some(format!("{a}/{b}")),
        _ => None,
    }
}

/// Parse an integer in `radix` (digits only) as a Long if it fits, else
/// a BigInt; `big` forces a BigInt.
pub fn parse_integer(digits: &str, radix: u32, neg: bool, big: bool) -> Option<LispObj> {
    let mut n = BigInt::parse_bytes(digits.as_bytes(), radix)?;
    if neg {
        n = -n;
    }
    Some(if big { bigint_obj(&n) } else { integer_obj(&n) })
}

/// Parse a ratio literal's parts (decimal digits).
pub fn parse_ratio(n: &str, d: &str, neg: bool) -> Option<LispObj> {
    let mut n = BigInt::parse_bytes(n.as_bytes(), 10)?;
    let d = BigInt::parse_bytes(d.as_bytes(), 10)?;
    if neg {
        n = -n;
    }
    if d.is_zero() {
        return None;
    }
    // A ratio literal with denominator dividing the numerator reads as
    // an integer, as in Clojure's reader (a Long if it fits).
    let g = n.gcd(&d);
    if (&d / &g).is_one() {
        return Some(integer_obj(&(n / g)));
    }
    Some(make_ratio(n, d))
}

/// BigInt of a number (Clojure `bigint`): floats truncate via their
/// shortest decimal form (as `BigDecimal.valueOf`), ratios truncate.
pub fn to_bigint(x: LispObj) -> LispObj {
    let n = num(x);
    let b = match &n {
        Num::Int(_) | Num::Big(_) => to_big(&n),
        Num::Ratio(a, b) => a / b,
        _ => {
            let f = as_f64(&n);
            if !f.is_finite() {
                crate::error::type_error(format!("Cannot convert {} to a BigInt", crate::clj::printer::pr_str(x)));
            }
            float_to_bigint(f)
        }
    };
    bigint_obj(&b)
}

fn float_to_bigint(f: f64) -> BigInt {
    // "d.ddddde[-]x" shortest round-trip digits.
    let s = format!("{:e}", f.abs());
    let (mant, exp) = s.split_once('e').unwrap();
    let exp: i64 = exp.parse().unwrap();
    let (ip, fp) = mant.split_once('.').unwrap_or((mant, ""));
    let digits = format!("{ip}{fp}");
    let point = 1 + exp; // digits before the decimal point
    let r = if point <= 0 {
        BigInt::zero()
    } else if point as usize >= digits.len() {
        let mut d = digits.clone();
        d.extend(std::iter::repeat_n('0', point as usize - digits.len()));
        BigInt::parse_bytes(d.as_bytes(), 10).unwrap()
    } else {
        BigInt::parse_bytes(digits[..point as usize].as_bytes(), 10).unwrap()
    };
    if f < 0.0 { -r } else { r }
}

/// Long of a number (Clojure `long`): truncates floats and ratios; an
/// error when out of range.
pub fn to_long(x: LispObj) -> LispObj {
    let n = num(x);
    let v = match &n {
        Num::Int(_) => return x,
        Num::Big(b) => b.to_i64(),
        Num::Ratio(a, b) => (a / b).to_i64(),
        _ => {
            let f = as_f64(&n);
            if f.is_nan() {
                Some(0)
            } else if (-9.223372036854776e18..9.223372036854776e18).contains(&f) {
                Some(f.trunc() as i64)
            } else {
                None
            }
        }
    };
    match v {
        Some(v) => long_obj(v),
        None => crate::error::argument_error(format!("Value out of range for long: {}", crate::clj::printer::pr_str(x))),
    }
}

/// Numerator or denominator of a ratio.
pub fn ratio_part(x: LispObj, denominator: bool) -> LispObj {
    if !is_ratio(x) {
        crate::error::type_error(format!("Not a ratio: {}", crate::clj::printer::pr_str(x)));
    }
    x.uref(if denominator { RATIO_DENOMINATOR } else { RATIO_NUMERATOR })
}

/// Exact rational of a number (Clojure `rationalize`): floats via their
/// shortest decimal form.
pub fn rationalize(x: LispObj) -> LispObj {
    let n = num(x);
    if !is_float_num(&n) {
        return x;
    }
    let f = as_f64(&n);
    if !f.is_finite() {
        crate::error::argument_error(format!("Cannot rationalize {}", crate::clj::printer::pr_str(x)));
    }
    let s = format!("{:e}", f);
    let (mant, exp) = s.split_once('e').unwrap();
    let exp: i64 = exp.parse().unwrap();
    let (ip, fp) = mant.split_once('.').unwrap_or((mant, ""));
    let digits = BigInt::parse_bytes(format!("{ip}{fp}").as_bytes(), 10).unwrap();
    let scale = exp - fp.len() as i64;
    let ten = BigInt::from(10);
    if scale >= 0 {
        integer_obj(&(digits * num_traits::pow(ten, scale as usize)))
    } else {
        let r = make_ratio(digits, num_traits::pow(ten, (-scale) as usize));
        // Whole values are Longs when they fit (BigDecimal.toBigInteger
        // normalized).
        if is_bigint(r) { integer_obj(&to_bigint_value(r).unwrap()) } else { r }
    }
}

/// Bitwise operations on Longs: op 0 and, 1 or, 2 xor, 3 and-not, 4 shl,
/// 5 shr, 6 unsigned shr (shift counts mod 64), 7 popcount of `a`.
pub fn bit_op(op: i64, a: LispObj, b: LispObj) -> LispObj {
    let (x, y) = (bit_arg(a), bit_arg(b));
    long_obj(match op {
        0 => x & y,
        1 => x | y,
        2 => x ^ y,
        3 => x & !y,
        4 => x.wrapping_shl(y as u32 & 63),
        5 => x >> (y as u32 & 63),
        6 => ((x as u64) >> (y as u32 & 63)) as i64,
        7 => x.count_ones() as i64,
        _ => lisp_error("Bad bit operation"),
    })
}

fn bit_arg(x: LispObj) -> i64 {
    if x.is_fixnum() {
        return x.integer();
    }
    if is_long(x) {
        return integer_value(x);
    }
    crate::error::argument_error(format!("bit operation not supported for: {}", crate::clj::printer::pr_str(x)))
}

pub fn bit_not(a: LispObj) -> LispObj {
    long_obj(!bit_arg(a))
}

/// A number's value as a double.
pub fn to_f64(x: LispObj) -> f64 {
    as_f64(&num(x))
}
