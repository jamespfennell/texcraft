use crate::tex::prelude::*;
use crate::tex::token::catcode::CatCode;

fn optional_by_error(token: Option<Token>) -> anyhow::Error {
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

pub fn parse_optional_by<S>(stream: &mut ExpandedInput<S>) -> anyhow::Result<()> {
    let next_is_b = get_optional_element![
        stream,
        Character('b', CatCode::Letter) => (),
        Character('B', CatCode::Letter) => (),
    ];
    if let Some(()) = next_is_b {
        get_element![
            stream,
            optional_by_error,
            Character('y', CatCode::Letter) => (),
            Character('Y', CatCode::Letter) => (),
        ]
    } else {
        Ok(())
    }
}
