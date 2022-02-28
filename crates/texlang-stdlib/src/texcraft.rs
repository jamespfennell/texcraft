//! The Texcraft primitive, which returns the word Texcraft as 8 letter tokens.

use texlang_core::prelude::*;

/// Get the `\texcraft` expansion primtive.
pub fn get_texcraft<S>() -> command::ExpansionFn<S> {
    texcraft_primitive_fn
}

pub fn texcraft_primitive_fn<S>(
    t: Token,
    _: &mut runtime::ExpandedInput<S>,
) -> anyhow::Result<Vec<Token>> {
    Ok(vec![
        Token::new_letter('T', t.traceback_id()),
        Token::new_letter('e', t.traceback_id()),
        Token::new_letter('x', t.traceback_id()),
        Token::new_letter('c', t.traceback_id()),
        Token::new_letter('r', t.traceback_id()),
        Token::new_letter('a', t.traceback_id()),
        Token::new_letter('f', t.traceback_id()),
        Token::new_letter('t', t.traceback_id()),
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
