//! The Texcraft primitive, which returns the word Texcraft as eight separate letter tokens
//!
//! This primitive is essentially equivalent to `\def\Texcraft{Texcraft}`.
//! It was implemented to be a simple example of a custom expansion primitive.

use texlang_core::*;

/// Get the `\texcraft` expansion primtive.
pub fn get_texcraft<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(texcraft_primitive_fn)
}

pub fn texcraft_primitive_fn<S>(
    t: token::Token,
    _: &mut vm::ExpansionInput<S>,
) -> command::Result<Vec<token::Token>> {
    Ok(vec![
        token::Token::new_letter('T', t.trace_key()),
        token::Token::new_letter('e', t.trace_key()),
        token::Token::new_letter('x', t.trace_key()),
        token::Token::new_letter('c', t.trace_key()),
        token::Token::new_letter('r', t.trace_key()),
        token::Token::new_letter('a', t.trace_key()),
        token::Token::new_letter('f', t.trace_key()),
        token::Token::new_letter('t', t.trace_key()),
    ])
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::testing::*;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([("texcraft", get_texcraft())])
    }

    test_suite![expansion_equality_tests((
        texcraft,
        r"\texcraft",
        r"Texcraft"
    ),),];
}
