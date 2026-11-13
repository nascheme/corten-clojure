//! `format`: `java.util.Formatter`'s syntax and output for the general,
//! character, integral and floating-point conversions
//! (`%[index$][flags][width][.precision]conversion`), without dates
//! (`%t`) or `%h`/`%a`.

use super::FALSE;
use crate::error::{argument_error, error_of_kind, ErrorKind};
use crate::numbers::{is_bigint, is_long, to_bigint_value};
use crate::object::{LispObj, NIL};
use crate::prims::funcall;
use num_bigint::BigInt;
use num_traits::Signed;

#[derive(Clone)]
struct Spec {
    index: Option<usize>,
    relative: bool,
    left: bool,
    alternate: bool,
    plus: bool,
    space: bool,
    zero: bool,
    group: bool,
    paren: bool,
    width: Option<usize>,
    precision: Option<usize>,
    conv: char,
    text: String,
}

fn bad_format(msg: String) -> ! {
    argument_error(msg)
}

/// Parse the specifier after a `%` at `chars[i]`; returns it and the
/// index after it.
fn parse_spec(chars: &[char], start: usize) -> (Spec, usize) {
    let mut i = start;
    let num = |i: &mut usize| -> Option<usize> {
        let s = *i;
        while *i < chars.len() && chars[*i].is_ascii_digit() {
            *i += 1;
        }
        (s < *i).then(|| chars[s..*i].iter().collect::<String>().parse().unwrap_or(usize::MAX))
    };
    let mut spec = Spec {
        index: None,
        relative: false,
        left: false,
        alternate: false,
        plus: false,
        space: false,
        zero: false,
        group: false,
        paren: false,
        width: None,
        precision: None,
        conv: ' ',
        text: String::new(),
    };
    // An argument index is digits followed by `$`.
    let save = i;
    if let Some(n) = num(&mut i) {
        if i < chars.len() && chars[i] == '$' {
            spec.index = Some(n);
            i += 1;
        } else {
            i = save;
        }
    }
    while i < chars.len() {
        match chars[i] {
            '-' => spec.left = true,
            '#' => spec.alternate = true,
            '+' => spec.plus = true,
            ' ' => spec.space = true,
            '0' => spec.zero = true,
            ',' => spec.group = true,
            '(' => spec.paren = true,
            '<' => spec.relative = true,
            _ => break,
        }
        i += 1;
    }
    spec.width = num(&mut i);
    if i < chars.len() && chars[i] == '.' {
        i += 1;
        spec.precision = num(&mut i);
        if spec.precision.is_none() {
            bad_format(format!("Illegal format precision in %{}", chars[start..i].iter().collect::<String>()));
        }
    }
    if i >= chars.len() {
        bad_format(format!("Format specifier '%{}' is incomplete", chars[start..].iter().collect::<String>()));
    }
    spec.conv = chars[i];
    i += 1;
    spec.text = chars[start - 1..i].iter().collect();
    (spec, i)
}

fn pad(spec: &Spec, s: String) -> String {
    let n = s.chars().count();
    match spec.width {
        Some(w) if w > n => {
            let fill = " ".repeat(w - n);
            if spec.left { s + &fill } else { fill + &s }
        }
        _ => s,
    }
}

fn upper(spec: &Spec, s: String) -> String {
    if spec.conv.is_ascii_uppercase() { s.to_uppercase() } else { s }
}

fn group_digits(digits: &str) -> String {
    let n = digits.len();
    let mut out = String::with_capacity(n + n / 3);
    for (i, c) in digits.chars().enumerate() {
        if i > 0 && (n - i) % 3 == 0 {
            out.push(',');
        }
        out.push(c);
    }
    out
}

/// Sign, grouping, zero padding and width for a number whose magnitude
/// is `mag` (digits, maybe a fraction or exponent; its leading digits
/// are grouped with `,`).
fn finish_number(spec: &Spec, negative: bool, mag: String, prefix: &str) -> String {
    let mut mag = mag;
    if spec.group {
        let end = mag.find(|c: char| !c.is_ascii_digit()).unwrap_or(mag.len());
        mag = group_digits(&mag[..end]) + &mag[end..];
    }
    let (lead, trail) = if negative {
        if spec.paren { ("(", ")") } else { ("-", "") }
    } else if spec.plus {
        ("+", "")
    } else if spec.space {
        (" ", "")
    } else {
        ("", "")
    };
    let body_len = lead.len() + prefix.len() + mag.chars().count() + trail.len();
    if spec.zero
        && let Some(w) = spec.width
        && w > body_len
    {
        return format!("{lead}{prefix}{}{mag}{trail}", "0".repeat(w - body_len));
    }
    pad(spec, format!("{lead}{prefix}{mag}{trail}"))
}

fn conversion_error(spec: &Spec, x: LispObj) -> ! {
    bad_format(format!("{} != {}", spec.conv, super::printer::pr_str(super::type_of(x))))
}

fn integer_arg(spec: &Spec, x: LispObj) -> BigInt {
    if is_long(x) || is_bigint(x) {
        to_bigint_value(x).unwrap()
    } else {
        conversion_error(spec, x)
    }
}

fn format_integer(spec: &Spec, x: LispObj) -> String {
    let v = integer_arg(spec, x);
    match spec.conv {
        'd' => finish_number(spec, v.is_negative(), v.abs().to_string(), ""),
        'o' | 'x' | 'X' => {
            let radix = if spec.conv == 'o' { 8 } else { 16 };
            // A negative Long is its two's complement; a BigInt keeps its sign.
            let (negative, digits) = if v.is_negative() && is_long(x) {
                let u = i64::try_from(&v).unwrap() as u64;
                (false, if radix == 8 { format!("{u:o}") } else { format!("{u:x}") })
            } else {
                (v.is_negative(), v.abs().to_str_radix(radix))
            };
            let prefix = match (spec.alternate, radix) {
                (false, _) => "",
                (true, 8) => "0",
                (true, _) => "0x",
            };
            upper(spec, finish_number(spec, negative, digits, prefix))
        }
        _ => unreachable!(),
    }
}

/// A finite positive double's shortest round-trip decimal digits and
/// the exponent of the first: value = 0.d1d2... * 10^(exp + 1).
fn shortest_digits(d: f64) -> (Vec<u8>, i32) {
    let s = format!("{d:e}");
    let (m, e) = s.split_once('e').unwrap();
    let exp: i32 = e.parse().unwrap();
    let digits: Vec<u8> = m.bytes().filter(|c| c.is_ascii_digit()).map(|c| c - b'0').collect();
    (digits, exp)
}

/// Round `digits` (the first at 10^exp) to their first `keep`, half
/// up as Java does; returns the new digits and exponent.
fn round_digits(mut digits: Vec<u8>, mut exp: i32, keep: i64) -> (Vec<u8>, i32) {
    if keep < 0 {
        return (vec![0], exp);
    }
    let keep = keep as usize;
    if digits.len() > keep {
        let up = digits[keep] >= 5;
        digits.truncate(keep);
        if up {
            let mut i = digits.len();
            loop {
                if i == 0 {
                    digits.insert(0, 1);
                    exp += 1;
                    break;
                }
                i -= 1;
                if digits[i] == 9 {
                    digits[i] = 0;
                } else {
                    digits[i] += 1;
                    break;
                }
            }
        }
    }
    if digits.is_empty() {
        digits.push(0);
    }
    (digits, exp)
}

/// `%f` of a finite non-negative double: fixed point with `prec` digits.
fn fixed(d: f64, prec: usize) -> String {
    if d == 0.0 {
        return if prec == 0 { "0".into() } else { format!("0.{}", "0".repeat(prec)) };
    }
    let (digits, exp) = shortest_digits(d);
    let (digits, exp) = round_digits(digits, exp, exp as i64 + 1 + prec as i64);
    let digit = |pos: i32| -> char {
        let i = exp - pos;
        if i >= 0 && (i as usize) < digits.len() { (b'0' + digits[i as usize]) as char } else { '0' }
    };
    let mut s = String::new();
    let top = exp.max(0);
    for p in (0..=top).rev() {
        s.push(digit(p));
    }
    if prec > 0 {
        s.push('.');
        for p in 1..=prec as i32 {
            s.push(digit(-p));
        }
    }
    s
}

/// `%e` of a finite non-negative double.
fn scientific(d: f64, prec: usize) -> String {
    let (digits, exp) = if d == 0.0 { (vec![0], 0) } else { shortest_digits(d) };
    let (digits, exp) = round_digits(digits, exp, prec as i64 + 1);
    let mut s = String::new();
    s.push((b'0' + digits[0]) as char);
    if prec > 0 {
        s.push('.');
        for i in 1..=prec {
            s.push((b'0' + digits.get(i).copied().unwrap_or(0)) as char);
        }
    }
    s.push('e');
    s.push(if exp < 0 { '-' } else { '+' });
    s.push_str(&format!("{:02}", exp.abs()));
    s
}

fn format_float(spec: &Spec, x: LispObj) -> String {
    if !x.is_float() {
        conversion_error(spec, x);
    }
    let d = crate::numbers::to_f64(x);
    if d.is_nan() || d.is_infinite() {
        let s = if d.is_nan() { "NaN".to_string() } else { "Infinity".to_string() };
        let neg = d.is_infinite() && d < 0.0;
        let spec = Spec { zero: false, group: false, ..spec.clone() };
        return upper(&spec, finish_number(&spec, neg, s, ""));
    }
    let neg = d.is_sign_negative();
    let a = d.abs();
    let prec = spec.precision.unwrap_or(6);
    let mag = match spec.conv {
        'f' => fixed(a, prec),
        'e' | 'E' => scientific(a, prec),
        _ => {
            // %g: scientific unless 10^-4 <= rounded value < 10^precision.
            let p = if prec == 0 { 1 } else { prec };
            let (digits, exp) = if a == 0.0 { (vec![0], 0) } else { shortest_digits(a) };
            let (_, rexp) = round_digits(digits, exp, p as i64);
            if a != 0.0 && (rexp < -4 || rexp >= p as i32) {
                scientific(a, p - 1)
            } else {
                fixed(a, (p as i32 - 1 - if a == 0.0 { 0 } else { rexp }).max(0) as usize)
            }
        }
    };
    upper(spec, finish_number(spec, neg, mag, ""))
}

/// `(k/format fmt args str)`: `args` an array, `str` the function
/// converting a value for `%s`.
pub fn format(a: &[LispObj]) -> LispObj {
    if !a[0].is_string() {
        crate::error::type_error(format!("format: not a string: {}", super::printer::pr_str(a[0])));
    }
    let fmt: Vec<char> = a[0].string_to_rust().chars().collect();
    let args = a[1];
    let to_str = a[2];
    let nargs = super::array::length(args);
    let mut out = String::new();
    let mut next = 0usize;
    let mut last: Option<usize> = None;
    let mut i = 0;
    while i < fmt.len() {
        let c = fmt[i];
        i += 1;
        if c != '%' {
            out.push(c);
            continue;
        }
        let (spec, j) = parse_spec(&fmt, i);
        i = j;
        match spec.conv {
            '%' => {
                out.push_str(&pad(&spec, "%".into()));
                continue;
            }
            'n' => {
                out.push('\n');
                continue;
            }
            _ => {}
        }
        let k = if spec.relative {
            last.unwrap_or_else(|| bad_format(format!("Missing format argument for '{}'", spec.text)))
        } else if let Some(n) = spec.index {
            if n == 0 {
                bad_format(format!("Illegal format argument index = 0 in '{}'", spec.text));
            }
            n - 1
        } else {
            next += 1;
            next - 1
        };
        if k >= nargs {
            bad_format(format!("Format specifier '{}'", spec.text));
        }
        last = Some(k);
        let x = super::array::get(args, k);
        let s = match spec.conv {
            's' | 'S' => {
                let mut s = if x == NIL { "null".to_string() } else { funcall(to_str, &[x]).string_to_rust() };
                if let Some(p) = spec.precision {
                    s = s.chars().take(p).collect();
                }
                upper(&spec, pad(&spec, s))
            }
            'b' | 'B' => {
                let b = !(x == NIL || x == FALSE);
                let mut s = b.to_string();
                if let Some(p) = spec.precision {
                    s.truncate(p);
                }
                upper(&spec, pad(&spec, s))
            }
            'c' | 'C' => {
                let ch = if x == NIL {
                    "null".to_string()
                } else if x.is_character() {
                    char::from_u32(x.char_code()).unwrap_or('\u{fffd}').to_string()
                } else if is_long(x) {
                    let v = integer_arg(&spec, x);
                    match u32::try_from(&v).ok().and_then(char::from_u32) {
                        Some(c) => c.to_string(),
                        None => bad_format(format!("Code point = {v} is not valid")),
                    }
                } else {
                    conversion_error(&spec, x)
                };
                upper(&spec, pad(&spec, ch))
            }
            'd' | 'o' | 'x' | 'X' => {
                if x == NIL { pad(&spec, "null".into()) } else { format_integer(&spec, x) }
            }
            'e' | 'E' | 'f' | 'g' | 'G' => {
                if x == NIL { pad(&spec, "null".into()) } else { format_float(&spec, x) }
            }
            c => error_of_kind(
                ErrorKind::Unsupported,
                format!("Unsupported format conversion '{c}' in '{}'", spec.text),
            ),
        };
        out.push_str(&s);
    }
    crate::heap::string_node(&out)
}
