use crate::error;
use crate::token;
use crate::traits::*;
use crate::vm;

/// When parsed, this type consumes an optional `by` keyword from the input stream.
pub struct OptionalBy;

impl<S: TexlangState> Parsable<S> for OptionalBy {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        let next_is_b = get_optional_element![
            input,
            token::Value::Letter('b') => (),
            token::Value::Letter('B') => (),
        ];
        if let Some(()) = next_is_b {
            get_required_element![
                input,
                "the second of letter of the `by` keyword",
                "the `by` keyword consists of a b or B letter token, then a y or Y letter token",
                token::Value::Letter('y') => OptionalBy{},
                token::Value::Letter('Y') => OptionalBy{},
            ]
        } else {
            Ok(OptionalBy {})
        }
    }
}
