//! The Texcraft primitive, which returns the word Texcraft as 8 letter tokens.

use crate::tex::prelude::*;

/// Get the `\texcraft` expansion primtive.
pub fn get_texcraft<S>() -> command::ExpansionFn<S> {
    texcraft_primitive_fn
}

pub fn texcraft_primitive_fn<S>(
    _: Token,
    _: &mut command::ExpandedInput<S>,
) -> anyhow::Result<Vec<Token>> {
    Ok(vec![
        Token::new_letter('T'),
        Token::new_letter('e'),
        Token::new_letter('x'),
        Token::new_letter('c'),
        Token::new_letter('r'),
        Token::new_letter('a'),
        Token::new_letter('f'),
        Token::new_letter('t'),
    ])
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
