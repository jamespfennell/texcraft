//! The Texcraft primitive, which returns the word Texcraft as 8 letter tokens.

use texlang_core::prelude::*;

/// Get the `\texcraft` expansion primtive.
pub fn get_texcraft<S>() -> command::ExpansionFn<S> {
    texcraft_primitive_fn
}

pub fn texcraft_primitive_fn<S>(
    t: Token,
    _: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    Ok(vec![
        Token::new_letter('T', t.trace_key()),
        Token::new_letter('e', t.trace_key()),
        Token::new_letter('x', t.trace_key()),
        Token::new_letter('c', t.trace_key()),
        Token::new_letter('r', t.trace_key()),
        Token::new_letter('a', t.trace_key()),
        Token::new_letter('f', t.trace_key()),
        Token::new_letter('t', t.trace_key()),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::*;

    fn setup_expansion_test(s: &mut runtime::Env<State>) {
        s.set_command("texcraft", get_texcraft());
    }

    expansion_test![texcraft, r"\texcraft", r"Texcraft"];
}
