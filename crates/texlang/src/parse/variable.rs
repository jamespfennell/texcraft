use crate::traits::*;
use crate::*;

impl<S: TexlangState> Parsable<S> for variable::Variable<S> {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        match input.next()? {
            None => Err(parse::Error::new(input.vm(), "a variable", None, "").into()),
            Some(token) => match token.value() {
                token::Value::ControlSequence(name) => {
                    match input.commands_map().get_command(&name) {
                        None => Err(parse::Error::new(input.vm(), "a variable", Some(token), "")
                            .with_got_override("got an undefined control sequence")
                            .with_annotation_override("undefined control sequence")
                            .into()),
                        Some(command::Command::Variable(cmd)) => {
                            Ok(cmd.clone().resolve(token, input)?)
                        }
                        Some(cmd) => {
                            Err(parse::Error::new(input.vm(), "a variable", Some(token), "")
                                .with_got_override("got a non-variable command")
                                .with_annotation_override(format![
                                    "control sequence referencing {cmd}"
                                ])
                                .into())
                        }
                    }
                }
                _ => Err(parse::Error::new(input.vm(), "a variable", Some(token), "")
                    .with_got_override("got a character token")
                    .into()),
            },
        }
    }
}

/// When parsed, this type consumes an optional equals from the token stream.
pub struct OptionalEquals;

impl<S: TexlangState> Parsable<S> for OptionalEquals {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        parse_optional_equals(input)?;
        Ok(OptionalEquals {})
    }
}

/// When parsed, this type consumes an optional equals from the token stream without performing expansion.
pub struct OptionalEqualsUnexpanded;

impl<S: TexlangState> Parsable<S> for OptionalEqualsUnexpanded {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        parse_optional_equals(input.unexpanded())?;
        Ok(OptionalEqualsUnexpanded {})
    }
}

fn parse_optional_equals<S: TexlangState, I: TokenStream<S = S>>(
    input: &mut I,
) -> Result<(), Box<error::Error>> {
    while let Some(found_equals) = get_optional_element![
        input,
        token::Value::Other('=') => false,
        token::Value::Space(_) => true,
    ] {
        if found_equals {
            break;
        }
    }
    while get_optional_element![
        input,
        token::Value::Space(_) => (),
    ]
    .is_some()
    {}
    Ok(())
}