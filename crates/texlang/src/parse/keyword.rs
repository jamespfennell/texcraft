use crate::prelude as txl;
use crate::token;
use crate::traits::*;
use crate::vm;

/// When parsed, this type consumes an optional `by` keyword from the input stream.
pub struct OptionalBy;

impl<S: TexlangState> Parsable<S> for OptionalBy {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let Some(first) = input.peek()?.copied() else {
            return Ok(OptionalBy {});
        };
        match first.value() {
            token::Value::Letter('b') | token::Value::Letter('B') => (),
            _ => return Ok(OptionalBy),
        };
        input.consume()?;

        let Some(second) = input.peek()?.copied() else {
            return Ok(OptionalBy {});
        };
        match second.value() {
            token::Value::Letter('y') | token::Value::Letter('Y') => {
                input.consume()?;
            }
            _ => {
                input.expansions_mut().push(first);
            }
        };
        Ok(OptionalBy)
    }
}

/// When parsed, this type consumes a required `to` keyword from the input stream.
pub struct To;

impl<S: TexlangState> Parsable<S> for To {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        // TODO: this should follow the impl of Or
        // scan_keyword
        get_required_element![
            input,
                "the first of letter of the `to` keyword",
                "the `to` keyword consists of a t or T letter token, then a o or O letter token",
            token::Value::Letter('t') => (),
            token::Value::Letter('T') => (),
        ];
        get_required_element![
            input,
            "the second of letter of the `to` keyword",
            "the `to` keyword consists of a t or T letter token, then a o or O letter token",
            token::Value::Letter('o') => OptionalBy{},
            token::Value::Letter('O') => OptionalBy{},
        ];
        Ok(To {})
    }
}
