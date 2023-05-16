use crate::error;
use crate::token;
use crate::vm;
use crate::vm::TokenStream;

pub fn parse_optional_by<S, I: AsMut<vm::ExpansionInput<S>>>(stream: &mut I) -> anyhow::Result<()> {
    let stream = stream.as_mut();
    let next_is_b = get_optional_element![
        stream,
        token::Value::Letter('b') => (),
        token::Value::Letter('B') => (),
    ];
    if let Some(()) = next_is_b {
        get_element![
            stream,
            optional_by_error,
            token::Value::Letter('y') => (),
            token::Value::Letter('Y') => (),
        ]
    } else {
        Ok(())
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
