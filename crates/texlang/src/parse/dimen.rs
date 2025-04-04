use core::Scaled;

use super::keyword::parse_keyword;
use crate::prelude as txl;
use crate::token::Value;
use crate::traits::*;
use crate::*;

impl Parsable for Scaled {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        scan_dimen(input, None)
    }
}

// TeX.2021.448 scan_dimen with:
//
// - shortcut=false
// - the inf parameter is replaced by an optional pointer to a glue order
pub(crate) fn scan_dimen<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    glue_order: Option<&mut core::GlueOrder>,
) -> txl::Result<core::Scaled> {
    let negative = match super::integer::parse_optional_signs(input)? {
        None => 1,
        Some(_) => -1,
    };
    let first_token = input.next_or_err(DimenEndOfInputError {})?;
    let (negative, integer_part, fractional_part) = match first_token.value() {
        Value::CommandRef(command_ref) => {
            // TeX.2021.449
            use super::integer::InternalNumber;
            match super::integer::parse_internal_number(input, first_token, command_ref)? {
                InternalNumber::Integer(i) => (negative * i.signum(), i.abs(), Scaled::ZERO),
                InternalNumber::Dimen(d) => {
                    return Ok(d * negative);
                }
                InternalNumber::Glue(g) => {
                    return Ok(g.width * negative);
                }
            }
        }
        _ => {
            let (i, f) = scan_constant_dimen(input, first_token)?;
            (negative, i, f)
        }
    };
    Ok(scan_and_apply_units(
        input,
        first_token,
        integer_part,
        fractional_part,
        glue_order,
    )? * negative)
}

/// Part of TeX.2021.448
pub(crate) fn scan_constant_dimen<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    first_token: token::Token,
) -> txl::Result<(i32, core::Scaled)> {
    Ok(match first_token.value() {
        Value::Other(',' | '.') => (0, scan_decimal_fraction(input)?),
        _ => {
            input.back(first_token);
            let (_, i, radix) = super::integer::parse_integer(input)?;
            // We scan for a fractional part if the integer was an decimal constant
            // and the next token is a period or comma.
            let fractional_part = if radix == Some(10) {
                match input.next()? {
                    Some(next) => match next.value() {
                        Value::Other(',' | '.') => scan_decimal_fraction(input)?,
                        _ => {
                            input.back(next);
                            Scaled::ZERO
                        }
                    },
                    None => Scaled::ZERO,
                }
            } else {
                Scaled::ZERO
            };
            (i, fractional_part)
        }
    })
}

/// TeX.2021.453
pub(crate) fn scan_and_apply_units<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    first_token: token::Token,
    integer_part: i32,
    fractional_part: Scaled,
    glue_order: Option<&mut core::GlueOrder>,
) -> txl::Result<core::Scaled> {
    // todo: scan spaces and non-call tokens

    // First try to scan for infinite units, if this is the stretch
    // or shrink of a glue.
    if let Some(glue_order) = glue_order {
        if parse_keyword(input, "fil")? {
            *glue_order = core::GlueOrder::Fil;
            while let Some(token) = input.next()? {
                match token.value() {
                    token::Value::Letter('l' | 'L') => match glue_order.next() {
                        None => {
                            input.error(fillll_error(token))?;
                        }
                        Some(o) => *glue_order = o,
                    },
                    _ => {
                        input.back(token);
                        break;
                    }
                }
            }
            super::OptionalSpace::parse(input)?;
            return match Scaled::from_integer(integer_part) {
                Ok(integer_part) => Ok(integer_part + fractional_part),
                Err(_) => handle_overflow(input, first_token, false),
            };
        }
    }
    // Then try to scan from an internal number.
    // TeX.2021.455
    if let Some(next) = input.next()? {
        let v_or = match next.value() {
            Value::CommandRef(command_ref) => {
                use super::integer::InternalNumber;
                Some(
                    match super::integer::parse_internal_number(input, next, command_ref)? {
                        // TeX silently interprets an integer as sp points in this position
                        InternalNumber::Integer(i) => Scaled(i),
                        InternalNumber::Dimen(scaled) => scaled,
                        InternalNumber::Glue(glue) => glue.width,
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
            let adjusted_fractional_part = v
                .xn_over_d(fractional_part.0, Scaled::ONE.0)
                .expect("n<d=Scaled::ONE, so overflow can't occur");
            return match v.nx_plus_y(integer_part, adjusted_fractional_part.0) {
                Ok(s) => Ok(s),
                Err(_) => handle_overflow(input, first_token, v < Scaled::ZERO),
            };
        }
    }

    // Potentially adjust for the magnification ratio.
    if parse_keyword(input, "true")? {
        // TeX.2021.457
        // TODO
    }

    // Finally try to scan unit constants.
    let scaled_unit = <core::ScaledUnit as Parsable>::parse(input)?;
    super::OptionalSpace::parse(input)?;
    match Scaled::new(integer_part, fractional_part, scaled_unit) {
        Ok(s) => Ok(s),
        Err(_) => handle_overflow(input, first_token, false),
    }
}

fn handle_overflow<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    first_token: token::Token,
    neg: bool,
) -> txl::Result<core::Scaled> {
    input.error(
        parse::Error::new(
            "a dimension in the range (-2^14pt,2^14pt)",
            Some(first_token),
            "",
        )
        .with_got_override("a dimension that's too large"),
    )?;
    Ok(if neg {
        -core::Scaled::MAX_DIMEN
    } else {
        core::Scaled::MAX_DIMEN
    })
}

fn fillll_error(token: token::Token) -> parse::Error {
    parse::Error::new("an infinite glue stretch or shrink order", Some(token), "")
        .with_got_override("too many l characters")
}

impl Parsable for core::ScaledUnit {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
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
        input.error(error::TODO())?;
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
    while let Some(token) = input.next()? {
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
    Ok(Scaled::from_decimal_digits(&digits[0..i]))
}

#[derive(Debug)]
struct DimenEndOfInputError;

impl error::EndOfInputError for DimenEndOfInputError {
    fn doing(&self) -> String {
        "parsing a dimension".into()
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
        (fraction_1, "0.5pt", Scaled::from_decimal_digits(&[5])),
        (fraction_2, "-0.5pt", -Scaled::from_decimal_digits(&[5])),
        (
            fraction_3,
            "1.5pt",
            Scaled::ONE + Scaled::from_decimal_digits(&[5])
        ),
        (
            fraction_4,
            "-1.5pt",
            -Scaled::ONE - Scaled::from_decimal_digits(&[5])
        ),
        (units_in_1, "1in", (Scaled::ONE * 7227) / 100),
        (units_in_2, "1 in", (Scaled::ONE * 7227) / 100),
        (units_in_3, "0.075in", Scaled(355207)),
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
