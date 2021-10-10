//! The Texcraft primitive, which returns the word Texcraft as 8 letter tokens.

use crate::tex::prelude::*;

pub fn texcraft_primitive_fn<S>(
    _: Token,
    _: &mut command::ExpandedInput<S>,
) -> anyhow::Result<Vec<Token>> {
    Ok(vec![
        Token::new_character('T', CatCode::Letter),
        Token::new_character('e', CatCode::Letter),
        Token::new_character('x', CatCode::Letter),
        Token::new_character('c', CatCode::Letter),
        Token::new_character('r', CatCode::Letter),
        Token::new_character('a', CatCode::Letter),
        Token::new_character('f', CatCode::Letter),
        Token::new_character('t', CatCode::Letter),
    ])
}

pub fn get_texcraft<S>() -> command::ExpansionPrimitive<S> {
    command::ExpansionPrimitive {
        call_fn: texcraft_primitive_fn,
        docs: "",
        id: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tex::driver;
    use crate::tex::state::Base;
    use crate::tex::token::catcode;

    struct State;
    fn new_state() -> State {
        State {}
    }

    fn setup_expansion_test(s: &mut Base<State>) {
        s.set_command("texcraft", get_texcraft());
    }

    expansion_test![texcraft, r"\texcraft", r"Texcraft"];
}
