use crate::prelude::*;

pub fn parse_optional_by<S, I: AsMut<runtime::ExpansionInput<S>>>(
    stream: &mut I,
) -> anyhow::Result<()> {
    let stream = stream.as_mut();
    let next_is_b = get_optional_element![
        stream,
        Value::Letter('b') => (),
        Value::Letter('B') => (),
    ];
    if let Some(()) = next_is_b {
        get_element![
            stream,
            optional_by_error,
            Value::Letter('y') => (),
            Value::Letter('Y') => (),
        ]
    } else {
        Ok(())
    }
}

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
