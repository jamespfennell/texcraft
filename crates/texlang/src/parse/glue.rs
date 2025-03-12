use core::{Glue, Scaled};

use super::keyword::parse_keyword;
use crate::prelude as txl;
use crate::token::Value;
use crate::traits::*;
use crate::*;

impl Parsable for Glue {
    // TeX.2021.461
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let negative = match super::integer::parse_optional_signs(input)? {
            None => 1,
            Some(_) => -1,
        };
        let first_token = input.next_or_err(GlueEndOfInputError {})?;
        let width = match first_token.value() {
            Value::CommandRef(command_ref) => {
                use super::integer::InternalNumber;
                match super::integer::parse_internal_number(input, first_token, command_ref)? {
                    InternalNumber::Integer(i) => {
                        super::dimen::scan_and_apply_units(
                            input,
                            first_token,
                            i.abs(),
                            Scaled::ZERO,
                            None,
                        )? * negative
                            * i.signum()
                    }
                    InternalNumber::Dimen(d) => d * negative,
                    InternalNumber::Glue(g) => {
                        return Ok(g * negative);
                    }
                }
            }
            _ => {
                input.back(first_token);
                core::Scaled::parse(input)? * negative
            }
        };

        let mut g = Glue {
            width,
            ..Default::default()
        };
        if parse_keyword(input, "plus")? {
            g.stretch = super::dimen::scan_dimen(input, Some(&mut g.stretch_order))?
        }
        if parse_keyword(input, "minus")? {
            g.shrink = super::dimen::scan_dimen(input, Some(&mut g.shrink_order))?
        };
        Ok(g)
    }
}

#[derive(Debug)]
struct GlueEndOfInputError;

impl error::EndOfInputError for GlueEndOfInputError {
    fn doing(&self) -> String {
        "parsing a glue".into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::testing::*;
    use core::GlueOrder;

    #[derive(Default)]
    struct State;

    impl TexlangState for State {}

    parse_success_tests![
        (
            width_1,
            "0pt",
            Glue {
                ..Default::default()
            }
        ),
        (
            width_2,
            "1pt",
            Glue {
                width: Scaled::ONE,
                ..Default::default()
            }
        ),
        (
            width_3,
            "-1pt",
            Glue {
                width: -Scaled::ONE,
                ..Default::default()
            }
        ),
        (
            stretch_1,
            "1pt plus 1pt",
            Glue {
                width: Scaled::ONE,
                stretch: Scaled::ONE,
                ..Default::default()
            }
        ),
        (
            stretch_fil,
            "1pt plus 1fil",
            Glue {
                width: Scaled::ONE,
                stretch: Scaled::ONE,
                stretch_order: GlueOrder::Fil,
                ..Default::default()
            }
        ),
        (
            stretch_fill,
            "1pt plus 1fill",
            Glue {
                width: Scaled::ONE,
                stretch: Scaled::ONE,
                stretch_order: GlueOrder::Fill,
                ..Default::default()
            }
        ),
        (
            stretch_filll,
            "1pt plus 1filll",
            Glue {
                width: Scaled::ONE,
                stretch: Scaled::ONE,
                stretch_order: GlueOrder::Filll,
                ..Default::default()
            }
        ),
    ];

    parse_failure_tests!(
        Glue,
        State,
        (
            stretch_overflow_1,
            "1pt plus 30000000fil",
            Glue {
                width: Scaled::ONE,
                stretch: Scaled::MAX_DIMEN,
                stretch_order: GlueOrder::Fil,
                ..Default::default()
            }
        ),
        (
            stretch_overflow_2,
            "1pt plus -30000000fil",
            Glue {
                width: Scaled::ONE,
                stretch: -Scaled::MAX_DIMEN,
                stretch_order: GlueOrder::Fil,
                ..Default::default()
            }
        ),
        (
            stretch_fillll,
            "1pt plus 2fillll",
            Glue {
                width: Scaled::ONE,
                stretch: Scaled::ONE * 2,
                stretch_order: GlueOrder::Filll,
                ..Default::default()
            }
        ),
    );
}
