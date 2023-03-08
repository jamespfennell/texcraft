//! `\let` assignments

use crate::prefix;
use texlang_core::parse;
use texlang_core::prelude::*;

pub const LET_DOC: &str = "Assign a command or character to a control sequence";

/// Get the `\let` command.
pub fn get_let<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(let_primitive_fn)
        .with_id(let_id())
        .with_doc(LET_DOC)
}

struct Let;

pub fn let_id() -> std::any::TypeId {
    std::any::TypeId::of::<Let>()
}

fn let_primitive_fn<S: HasComponent<prefix::Component>>(
    let_token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let scope = input.state_mut().component_mut().read_and_reset_global();
    let alias = parse::parse_command_target("\\let assignment", let_token, input.unexpanded())?;
    parse::parse_optional_equals(input.unexpanded())?;
    match input.unexpanded().next()? {
        None => Err(error::EndOfInputError::new(
            "unexpected end of input while reading the right hand side of a \\let assignment",
        )
        .cast()),
        Some(token) => match token.value() {
            ControlSequence(control_sequence) => {
                match input.base_mut().commands_map.alias_control_sequence(
                    alias,
                    control_sequence,
                    scope,
                ) {
                    Ok(()) => Ok(()),
                    Err(_) => Err(error::new_undefined_cs_error(token, input.vm())),
                }
            }
            _ => {
                input
                    .base_mut()
                    .commands_map
                    .alias_token(alias, token, scope);
                Ok(())
            }
        },
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::def;
    use crate::testutil::*;

    fn setup_expansion_test() -> HashMap<&'static str, command::Command<State>> {
        HashMap::from([
            ("def", def::get_def()),
            ("global", prefix::get_global()),
            ("let", get_let()),
        ])
    }

    expansion_test![let_for_macro, r"\def\A{abc}\let\B\A\B", "abc"];
    expansion_test![local, r"\def\A{a}\def\B{b}\let\C=\A{\let\C=\B \C}\C", "ba"];
    expansion_test![
        global,
        r"\def\A{a}\def\B{b}\let\C=\A{\global\let\C=\B \C}\C",
        "bb"
    ];
    expansion_test![let_for_macro_equals, r"\def\A{abc}\let\B=\A\B", "abc"];
    expansion_failure_test!(let_unknown_cs_name, r"\let \B=\A");
}
