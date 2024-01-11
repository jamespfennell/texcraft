//! The Texcraft primitive, which returns the word Texcraft as eight separate letter tokens
//!
//! This primitive is essentially equivalent to `\def\Texcraft{Texcraft}`.
//! It was implemented to serve as a simple example of a custom expansion primitive.

use texlang::*;

/// Get the `\texcraft` expansion primitive.
pub fn get_texcraft<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(texcraft_primitive_fn)
}

pub fn texcraft_primitive_fn<S>(
    token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> command::Result<()> {
    let expansions = input.expansions_mut();
    for c in "Texcraft".chars().rev() {
        expansions.push(token::Token::new_letter(c, token.trace_key()))
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use texlang_testing::State;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([("texcraft", get_texcraft())])
    }

    texlang_testing::test_suite![expansion_equality_tests((
        texcraft,
        r"\texcraft",
        r"Texcraft"
    ),),];
}
