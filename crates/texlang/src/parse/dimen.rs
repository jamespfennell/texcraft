use core::{Scaled, ScaledUnit};

use super::keyword::parse_keyword;
use crate::prelude as txl;
use crate::token::Value;
use crate::traits::*;
use crate::*;

impl<S: TexlangState> Parsable<S> for Scaled {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let negative = super::integer::parse_optional_signs(input)?.is_some();
        let first_token = input.next(DimenEndOfInputError {})?;
        let (integer_part, has_fractional_part) = match first_token.value() {
            Value::CommandRef(command_ref) => {
                // TeX.2021.449
                use super::integer::InternalNumber;
                match super::integer::parse_internal_number(input, first_token, command_ref)? {
                    InternalNumber::Integer(i) => (i, false),
                    InternalNumber::Dimen(d) => {
                        if negative {
                            return Ok(-d);
                        }
                        return Ok(d);
                    }
                }
            }
            Value::Other(',' | '.') => (0, true),
            _ => {
                input.back(first_token);
                let (_, i, radix) = super::integer::parse_integer(input)?;
                // We scan for a fractional part if the integer was an decimal constant
                // and the next token is a period or comma.
                let has_fractional_part = match radix {
                    Some(10) => match input.next_or()? {
                        Some(next) => match next.value() {
                            Value::Other(',' | '.') => true,
                            _ => {
                                input.back(next);
                                false
                            }
                        },
                        None => false,
                    },
                    _ => false,
                };
                (i, has_fractional_part)
            }
        };
        let fractional_part = if has_fractional_part {
            scan_decimal_fraction(input)?
        } else {
            Scaled::ZERO
        };

        let (negative, integer_part) = if integer_part < 0 {
            // This can only happen if the integer was parsed from an internal integer.
            // In this case the fractional part is always 0.
            (!negative, -integer_part)
        } else {
            (negative, integer_part)
        };
        let s = match scan_and_apply_units(input, integer_part, fractional_part)? {
            ScanAndApplyResult::Scaled(s) => s,
            ScanAndApplyResult::Overflow(neg) => {
                input.vm().error(
                    parse::Error::new(
                        "a dimension in the range (-2^14pt,2^14pt)",
                        Some(first_token),
                        "",
                    )
                    .with_got_override("a dimension that's too large"),
                )?;
                if neg {
                    -core::Scaled::MAX_DIMEN
                } else {
                    core::Scaled::MAX_DIMEN
                }
            }
        };
        if negative {
            Ok(-s)
        } else {
            Ok(s)
        }
    }
}

enum ScanAndApplyResult {
    // A finite result.
    Scaled(Scaled),
    // An overflowed result.
    // The boolean specified is true if it was negative overflow.
    Overflow(bool),
}

/// TeX.2021.453, except infinite units are not handled.
///
/// Returns `Ok(None)` if overflow occurs.
fn scan_and_apply_units<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    integer_part: i32,
    fractional_part: Scaled,
) -> txl::Result<ScanAndApplyResult> {
    // todo: scan spaces and non-call tokens
    // First try to scan from an internal number.
    // TeX.2021.455
    if let Some(next) = input.next_or()? {
        let v_or = match next.value() {
            Value::CommandRef(command_ref) => {
                use super::integer::InternalNumber;
                Some(
                    match super::integer::parse_internal_number(input, next, command_ref)? {
                        // TeX silently interprets an integer as sp points in this position
                        InternalNumber::Integer(i) => Scaled(i),
                        InternalNumber::Dimen(scaled) => scaled,
                    },
                )
            }
            _ => {
                input.back(next);
                if parse_keyword(input, "em")? {
                    super::OptionalSpace::parse(input)?;
                    Some(input.state().em_width())
                } else if parse_keyword(input, "ex")? {
                    super::OptionalSpace::parse(input)?;
                    Some(input.state().ex_height())
                } else {
                    None
                }
            }
        };
        if let Some(v) = v_or {
            let Ok(adjusted_fractional_part) = v.xn_over_d(fractional_part.0, 0o200000) else {
                return Ok(ScanAndApplyResult::Overflow(v < Scaled::ZERO));
            };
            return Ok(
                match v.nx_plus_y(integer_part, adjusted_fractional_part.0) {
                    Ok(s) => ScanAndApplyResult::Scaled(s),
                    Err(_) => ScanAndApplyResult::Overflow(v < Scaled::ZERO),
                },
            );
        }
    }

    // Potentially adjust for the magnification ratio.
    if parse_keyword(input, "true")? {
        // TeX.2021.457
        // TODO
    }

    // Finally try to scan unit constants.
    // TeX.2021.458
    let scaled_unit = <core::ScaledUnit as Parsable<S>>::parse(input)?;
    super::OptionalSpace::parse(input)?;
    let (integer_part, fractional_part) = match scaled_unit {
        // For sp units, the fractional part is silently dropped.
        ScaledUnit::ScaledPoint => {
            let s = Scaled(integer_part);
            return Ok(if s > Scaled::MAX_DIMEN {
                ScanAndApplyResult::Overflow(false)
            } else {
                ScanAndApplyResult::Scaled(s)
            });
        }
        ScaledUnit::Point => (integer_part, fractional_part),
        _ => {
            let (n, d) = scaled_unit.conversion_fraction();
            let Ok((i, remainder)) = xn_over_d(integer_part, n, d) else {
                return Ok(ScanAndApplyResult::Overflow(false));
            };
            let f = fractional_part
                .nx_plus_y(n, Scaled::from_integer(remainder).expect("remainder<d<=7200<2^13, so a valid scaled number"))
                .expect("fractional_part<2^16, remainder<2^16*d, so nx_plus_y<2^16(n+d). Each (n,d) makes this <2^30")
                / d;
            (i + f.integer_part(), f.fractional_part())
        }
    };
    let Ok(integer_part) = Scaled::from_integer(integer_part) else {
        return Ok(ScanAndApplyResult::Overflow(false));
    };
    Ok(ScanAndApplyResult::Scaled(integer_part + fractional_part))
}

fn xn_over_d(x: i32, n: i32, d: i32) -> Result<(i32, i32), core::OverflowError> {
    let (a, b) = Scaled(x).xn_over_d(n, d)?;
    Ok((a.0, b.0))
}

impl<S: TexlangState> Parsable<S> for core::ScaledUnit {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        use core::ScaledUnit;
        for (keyword, unit) in [
            ("pt", ScaledUnit::Point),
            ("in", ScaledUnit::Inch),
            ("pc", ScaledUnit::Pica),
            ("cm", ScaledUnit::Centimeter),
            ("mm", ScaledUnit::Millimeter),
            ("bp", ScaledUnit::BigPoint),
            ("dd", ScaledUnit::DidotPoint),
            ("cc", ScaledUnit::Cicero),
            ("sp", ScaledUnit::ScaledPoint),
        ] {
            if parse_keyword(input, keyword)? {
                return Ok(unit);
            }
        }
        input.vm().error(error::TODO)?;
        Ok(core::ScaledUnit::Point)
    }
}

/// TeX.2021.452
fn scan_decimal_fraction<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
) -> txl::Result<Scaled> {
    // We only get up to 17 digits, because further digits won't affect the result given
    // that the smallest scaled number is 2^(-16). This is very nice because it means
    // we don't need to allocate a vector to store the digits.
    let mut digits = [0_u8; 17];
    let mut i = 0_usize;
    while let Some(token) = input.next_or()? {
        let d: u8 = match token.value() {
            Value::Other('0') => 0,
            Value::Other('1') => 1,
            Value::Other('2') => 2,
            Value::Other('3') => 3,
            Value::Other('4') => 4,
            Value::Other('5') => 5,
            Value::Other('6') => 6,
            Value::Other('7') => 7,
            Value::Other('8') => 8,
            Value::Other('9') => 9,
            Value::Space(_) => {
                break;
            }
            _ => {
                input.back(token);
                break;
            }
        };
        if let Some(digit) = digits.get_mut(i) {
            *digit = d;
            i += 1;
        }
    }
    Ok(Scaled::from_decimal_fraction(&digits[0..i]))
}

#[derive(Debug)]
struct DimenEndOfInputError;

impl error::EndOfInputError for DimenEndOfInputError {
    fn doing(&self) -> String {
        "parsing a number".into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::testing::*;

    #[derive(Default)]
    struct State;

    impl TexlangState for State {}

    parse_success_tests![
        (zero_pt, "0pt", Scaled::ZERO),
        (one_pt, "1pt", Scaled::ONE),
        (one_pt_negative, "-1pt", -Scaled::ONE),
        (two_pt, "2pt", Scaled::TWO),
        (empty_point, ".pt", Scaled::ZERO), // TeX.2021.452
        (fraction_1, "0.5pt", Scaled::from_decimal_fraction(&[5])),
        (fraction_2, "-0.5pt", -Scaled::from_decimal_fraction(&[5])),
        (
            fraction_3,
            "1.5pt",
            Scaled::ONE + Scaled::from_decimal_fraction(&[5])
        ),
        (
            fraction_4,
            "-1.5pt",
            -Scaled::ONE - Scaled::from_decimal_fraction(&[5])
        ),
        (units_in_1, "1in", (Scaled::ONE * 7227) / 100),
        (units_in_2, "1 in", (Scaled::ONE * 7227) / 100),
        (units_pc, "1pc", Scaled::ONE * 12),
        (units_cm, "1cm", (Scaled::ONE * 7227) / 254),
        (units_mm, "1mm", (Scaled::ONE * 7227) / 2540),
        (units_bp, "1bp", (Scaled::ONE * 7227) / 7200),
        (units_dd, "1dd", (Scaled::ONE * 1238) / 1157),
        (units_cc, "1cc", (Scaled::ONE * 14856) / 1157),
        (units_sp_1, "1sp", Scaled(1)),
        (units_sp_2, "1.999999sp", Scaled(1)),
        (nearly_overflow_pt, "16383.99998pt", Scaled::MAX_DIMEN,),
        // (2^30-1) * d / (n * 2^16)
        // (2^30-1) * d / (n * 2^16)
        // (nearly_overflow_in, "226.705406in", Scaled::MAX_DIMEN ),
        (nearly_overflow_sp_1, "1073741823sp", Scaled::MAX_DIMEN,),
        (
            nearly_overflow_sp_2,
            "1073741823.99999999sp",
            Scaled::MAX_DIMEN,
        )
    ];

    parse_failure_tests!(
        Scaled,
        State,
        (invalid_unit, "1xy", Scaled::ONE),
        (overflow_pt, "16384pt", Scaled::MAX_DIMEN),
        (overflow_pt_neg, "-16384pt", -Scaled::MAX_DIMEN),
        (overflow_in_1, "300in", Scaled::MAX_DIMEN),
        (overflow_in_2, "300000000in", Scaled::MAX_DIMEN),
        (overflow_in_3, "-300in", -Scaled::MAX_DIMEN),
        (overflow_in_4, "-300000000in", -Scaled::MAX_DIMEN),
        (overflow_sp, "1073741824sp", Scaled::MAX_DIMEN),
        (overflow_sp_neg, "-1073741824sp", -Scaled::MAX_DIMEN),
    );
}
