// Native `JS.BigInt` operations. Semantics follow the JavaScript FFI:
// arbitrary-precision integers with two's-complement bitwise operators,
// sign-aware shifts, `BigInt.asIntN/asUintN` wrapping and the same fallible
// string/number conversions.
use std::rc::Rc;

pub use num_bigint_dig::BigInt;
use num_traits::{Pow, Signed};

fn boxed(value: BigInt) -> crate::UnknownType {
    crate::Value::Class(Rc::new(Rc::new(value)))
}

fn just(
    just: &purust_core::Func1<crate::UnknownType, Rc<Purs_Data_Maybe::Maybe>>,
    value: BigInt,
) -> Rc<Purs_Data_Maybe::Maybe> {
    just(boxed(value))
}

fn parse_decimal(text: &str) -> Option<BigInt> {
    BigInt::parse_bytes(text.as_bytes(), 10)
}

/// `BigInt(string)` accepts trimmed decimal literals with an optional sign,
/// and unsigned `0x`/`0o`/`0b` prefixed integers. Exponent notation, fraction
/// digits and prefixed signs throw in JavaScript, so they fail here too.
fn parse_js_bigint(text: &str) -> Option<BigInt> {
    let text = text.trim_matches(|c: char| {
        matches!(
            c,
            '\t'..='\r'
                | ' '
                | '\u{a0}'
                | '\u{1680}'
                | '\u{2000}'..='\u{200a}'
                | '\u{2028}'
                | '\u{2029}'
                | '\u{202f}'
                | '\u{205f}'
                | '\u{3000}'
                | '\u{feff}'
        )
    });
    if text.is_empty() {
        return Some(BigInt::from(0));
    }
    if let Some(digits) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        return BigInt::parse_bytes(digits.as_bytes(), 16);
    }
    if let Some(digits) = text.strip_prefix("0o").or_else(|| text.strip_prefix("0O")) {
        return BigInt::parse_bytes(digits.as_bytes(), 8);
    }
    if let Some(digits) = text.strip_prefix("0b").or_else(|| text.strip_prefix("0B")) {
        return BigInt::parse_bytes(digits.as_bytes(), 2);
    }
    let (negative, digits) = match text.as_bytes()[0] {
        b'-' => (true, &text[1..]),
        b'+' => (false, &text[1..]),
        _ => (false, text),
    };
    if digits.is_empty() || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    let value = BigInt::parse_bytes(digits.as_bytes(), 10)?;
    Some(if negative { -value } else { value })
}

pub fn JS_BigInt_fromStringImpl(
    just_fn: purust_core::Func1<crate::UnknownType, Rc<Purs_Data_Maybe::Maybe>>,
    nothing: Rc<Purs_Data_Maybe::Maybe>,
    text: String,
) -> Rc<Purs_Data_Maybe::Maybe> {
    let text = purust_core::purust_string_to_utf8_lossy(&text);
    match parse_js_bigint(&text) {
        Some(value) => just(&just_fn, value),
        None => nothing,
    }
}

pub fn JS_BigInt_fromStringAsImpl(
    just_fn: purust_core::Func1<crate::UnknownType, Rc<Purs_Data_Maybe::Maybe>>,
    nothing: Rc<Purs_Data_Maybe::Maybe>,
    radix: i64,
    text: String,
) -> Rc<Purs_Data_Maybe::Maybe> {
    let parse = || -> Option<BigInt> {
        let text = purust_core::purust_string_to_utf8_lossy(&text);
        let text = text.trim();
        if !(2..=36).contains(&radix) || text.is_empty() {
            return None;
        }
        let (digits, negative) = match text.as_bytes()[0] {
            b'-' => (&text[1..], true),
            b'+' => (&text[1..], false),
            _ => (text, false),
        };
        if digits.is_empty() {
            return None;
        }
        let value = BigInt::parse_bytes(digits.as_bytes(), radix as u32)?;
        Some(if negative { -value } else { value })
    };
    match parse() {
        Some(value) => just(&just_fn, value),
        None => nothing,
    }
}

pub fn JS_BigInt_fromNumberImpl(
    just_fn: purust_core::Func1<crate::UnknownType, Rc<Purs_Data_Maybe::Maybe>>,
    nothing: Rc<Purs_Data_Maybe::Maybe>,
    number: f64,
) -> Rc<Purs_Data_Maybe::Maybe> {
    if !number.is_finite() || number.fract() != 0.0 {
        return nothing;
    }
    match format!("{number:.0}").parse::<BigInt>() {
        Ok(value) => just(&just_fn, value),
        Err(_) => nothing,
    }
}

pub fn JS_BigInt_fromInt(value: i64) -> Rc<BigInt> {
    Rc::new(BigInt::from(value))
}

pub fn JS_BigInt_fromTypeLevelInt(text: String) -> Rc<BigInt> {
    let text = purust_core::purust_string_to_utf8_lossy(&text);
    Rc::new(
        parse_decimal(text.trim()).unwrap_or_else(|| panic!("JS.BigInt.fromTypeLevelInt: {text}")),
    )
}

pub fn JS_BigInt_toNumber(value: Rc<BigInt>) -> f64 {
    match value.to_string().parse::<f64>() {
        Ok(number) => number,
        Err(_) => f64::NAN,
    }
}

pub fn JS_BigInt_biZero() -> Rc<BigInt> {
    Rc::new(BigInt::from(0))
}

pub fn JS_BigInt_biOne() -> Rc<BigInt> {
    Rc::new(BigInt::from(1))
}

pub fn JS_BigInt_biAdd(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(left.as_ref() + right.as_ref())
}

pub fn JS_BigInt_biSub(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(left.as_ref() - right.as_ref())
}

pub fn JS_BigInt_biMul(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(left.as_ref() * right.as_ref())
}

/// JavaScript `%` for BigInt: the result takes the sign of the divisor.
fn js_remainder(left: &BigInt, right: &BigInt) -> BigInt {
    if right == &BigInt::from(0) {
        return BigInt::from(0);
    }
    let divisor = right.abs();
    let remainder = left % &divisor;
    let remainder = if remainder < BigInt::from(0) {
        remainder + &divisor
    } else {
        remainder
    };
    remainder % &divisor
}

pub fn JS_BigInt_biMod(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(js_remainder(&left, &right))
}

pub fn JS_BigInt_biDiv(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    if right == Rc::new(BigInt::from(0)) {
        return Rc::new(BigInt::from(0));
    }
    let remainder = js_remainder(&left, &right);
    Rc::new((left.as_ref() - &remainder) / right.as_ref())
}

/// `EuclideanRing.degree` for BigInt: the absolute value, saturated to Int.
pub fn JS_BigInt_biDegree(value: Rc<BigInt>) -> i64 {
    let magnitude = if value.as_ref() < &BigInt::from(0) {
        -value.as_ref()
    } else {
        value.as_ref().clone()
    };
    magnitude.to_string().parse::<i64>().unwrap_or(i64::MAX)
}

pub fn JS_BigInt_pow(base: Rc<BigInt>, exponent: Rc<BigInt>) -> Rc<BigInt> {
    if exponent < Rc::new(BigInt::from(0)) {
        return Rc::new(BigInt::from(0));
    }
    let exponent = exponent.to_string().parse::<u32>().unwrap_or(u32::MAX);
    Rc::new(base.pow(exponent))
}

pub fn JS_BigInt_not(value: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(-value.as_ref() - BigInt::from(1))
}

pub fn JS_BigInt_or(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(left.as_ref() | right.as_ref())
}

pub fn JS_BigInt_xor(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(left.as_ref() ^ right.as_ref())
}

pub fn JS_BigInt_and(left: Rc<BigInt>, right: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(left.as_ref() & right.as_ref())
}

/// BigInt shifts accept negative counts and then shift the other way.
fn shift_count(value: &BigInt) -> i64 {
    value.to_string().parse::<i64>().unwrap_or(0)
}

/// Negative counts are handled by the caller; huge counts cannot be allocated.
fn shift_amount(count: i64) -> usize {
    usize::try_from(count).expect("JS.BigInt: shift count out of range")
}

pub fn JS_BigInt_shl(value: Rc<BigInt>, count: Rc<BigInt>) -> Rc<BigInt> {
    let count = shift_count(&count);
    if count >= 0 {
        Rc::new(value.as_ref() << shift_amount(count))
    } else {
        Rc::new(value.as_ref() >> shift_amount(-count))
    }
}

pub fn JS_BigInt_shr(value: Rc<BigInt>, count: Rc<BigInt>) -> Rc<BigInt> {
    let count = shift_count(&count);
    if count >= 0 {
        Rc::new(value.as_ref() >> shift_amount(count))
    } else {
        Rc::new(value.as_ref() << shift_amount(-count))
    }
}

pub fn JS_BigInt_biEquals(left: Rc<BigInt>, right: Rc<BigInt>) -> bool {
    left.as_ref() == right.as_ref()
}

pub fn JS_BigInt_biCompare(left: Rc<BigInt>, right: Rc<BigInt>) -> i64 {
    match left.as_ref().cmp(right.as_ref()) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

pub fn JS_BigInt_toString(value: Rc<BigInt>) -> String {
    value.to_string()
}

pub fn JS_BigInt_toStringAs(radix: i64, value: Rc<BigInt>) -> String {
    if !(2..=36).contains(&radix) {
        panic!("JS.BigInt.toStringAs: radix {radix} is out of range");
    }
    value.to_str_radix(radix as u32)
}

/// `BigInt.asUintN`: reduce to the unsigned lower `bits` bits.
fn as_uint_n(bits: i64, value: &BigInt) -> BigInt {
    if bits <= 0 {
        return BigInt::from(0);
    }
    let modulus = BigInt::from(1) << shift_amount(bits);
    ((value % &modulus) + &modulus) % &modulus
}

pub fn JS_BigInt_asIntN(bits: i64, value: Rc<BigInt>) -> Rc<BigInt> {
    if bits <= 0 {
        return Rc::new(BigInt::from(0));
    }
    let modulus = BigInt::from(1) << shift_amount(bits);
    let unsigned = as_uint_n(bits, &value);
    let half = BigInt::from(1) << shift_amount(bits - 1);
    Rc::new(if unsigned >= half {
        unsigned - modulus
    } else {
        unsigned
    })
}

pub fn JS_BigInt_asUintN(bits: i64, value: Rc<BigInt>) -> Rc<BigInt> {
    Rc::new(as_uint_n(bits, &value))
}
