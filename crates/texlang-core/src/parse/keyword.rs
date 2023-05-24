use crate::error;
use crate::token;
use crate::traits::*;
use crate::vm;

/// When parsed, this type consumes an optional `by` keyword from the input stream.
pub struct OptionalBy;

impl<S: TexlangState> Parsable<S> for OptionalBy {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        let next_is_b = get_optional_element![
            input,
            token::Value::Letter('b') => (),
            token::Value::Letter('B') => (),
        ];
        if let Some(()) = next_is_b {
            get_element![
                input,
                optional_by_error,
                token::Value::Letter('y') => OptionalBy{},
                token::Value::Letter('Y') => OptionalBy{},
            ]
        } else {
            Ok(OptionalBy {})
        }
    }
}

fn optional_by_error(token: Option<token::Token>) -> anyhow::Error {
    match token {
        None => {
            error::EndOfInputError::new("unexpected end of input while parsing the `by` keyword")
                .add_note("expected the letter `y` or `Y`")
                .cast()
        }
        Some(token) => {
            error::TokenError::new(token, "unexpected token while parsing the `by` keyword")
                .add_note("expected the letter `y` or `Y`")
                .cast()
        }
    }
}
