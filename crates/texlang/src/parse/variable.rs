use crate::prelude as txl;
use crate::traits::*;
use crate::*;

/// Parses an arithmetic variable (integer, glue, dimension, etc).
pub struct ArithmeticVariable<S>(pub variable::Variable<S>);

impl<S: TexlangState> Parsable<S> for ArithmeticVariable<S> {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        match input.next()? {
            None => Err(input
                .vm()
                .fatal_error(parse::Error::new("a variable", None, ""))),
            Some(token) => match token.value() {
                token::Value::CommandRef(command_ref) => {
                    match input.commands_map().get_command(&command_ref) {
                        None => Err(input.vm().fatal_error(
                            parse::Error::new("a variable", Some(token), "")
                                .with_got_override("got an undefined control sequence")
                                .with_annotation_override("undefined control sequence"),
                        )),
                        Some(command::Command::Variable(cmd)) => {
                            if !cmd.is_arithmetic() {
                                Err(input.vm().fatal_error(
                                    parse::Error::new("an arithmetic variable", Some(token), "")
                                        .with_got_override("got a non-arithmetic variable"),
                                ))
                            } else {
                                Ok(ArithmeticVariable(cmd.clone().resolve(token, input)?))
                            }
                        }
                        Some(cmd) => Err(input.vm().fatal_error(
                            parse::Error::new("a variable", Some(token), "")
                                .with_got_override("got a non-variable command")
                                .with_annotation_override(format![
                                    "control sequence referencing {cmd}"
                                ]),
                        )),
                    }
                }
                _ => Err(input.vm().fatal_error(
                    parse::Error::new("a variable", Some(token), "")
                        .with_got_override("got a character token"),
                )),
            },
        }
    }
}

/// When parsed, this type consumes an optional equals from the token stream.
pub struct OptionalEquals;

impl<S: TexlangState> Parsable<S> for OptionalEquals {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        parse_optional_equals(input)?;
        Ok(OptionalEquals {})
    }
}

/// When parsed, this type consumes an optional equals from the token stream without performing expansion.
pub struct OptionalEqualsUnexpanded;

impl<S: TexlangState> Parsable<S> for OptionalEqualsUnexpanded {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        parse_optional_equals(input.unexpanded())?;
        Ok(OptionalEqualsUnexpanded {})
    }
}

// Corresponds to the `scan_optional_equals` procedure in Knuth's TeX (405)
fn parse_optional_equals<S: TexlangState, I: TokenStream<S = S>>(input: &mut I) -> txl::Result<()> {
    while let Some(found_equals) = get_optional_element![
        input,
        token::Value::Other('=') => true,
        token::Value::Space(_) => false,
    ] {
        if found_equals {
            break;
        }
    }
    // TODO: this is not correct: this function should not scan spaces after the equals.
    // A separate routine corresponding to Knuth TeX 404 needs to be added and used instead
    // at the right call sites.
    while get_optional_element![
        input,
        token::Value::Space(_) => (),
    ]
    .is_some()
    {}
    Ok(())
}
