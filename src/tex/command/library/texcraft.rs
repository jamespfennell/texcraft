//! The Texcraft primitive, which returns the word Texcraft as 8 letter tokens.

use crate::tex::prelude::*;

pub fn texcraft_primitive_fn<S>(
    _: Token,
    _: &mut command::ExpansionInput<S>,
) -> anyhow::Result<stream::VecStream> {
    Ok(stream::VecStream::new(vec![
        Token::new_letter('T'),
        Token::new_letter('e'),
        Token::new_letter('x'),
        Token::new_letter('c'),
        Token::new_letter('r'),
        Token::new_letter('a'),
        Token::new_letter('f'),
        Token::new_letter('t'),
    ]))
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
    use crate::tex::input;
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
