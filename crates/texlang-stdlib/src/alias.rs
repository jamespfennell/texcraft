//! `\let` aliasing command

use crate::prefix;
use texlang_core::traits::*;
use texlang_core::*;

pub const LET_DOC: &str = "Assign a command or character to a control sequence";

/// Get the `\let` command.
pub fn get_let<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(let_primitive_fn)
        .with_tag(let_tag())
        .with_doc(LET_DOC)
}

static LET_TAG: command::StaticTag = command::StaticTag::new();

pub fn let_tag() -> command::Tag {
    LET_TAG.get()
}

fn let_primitive_fn<S: HasComponent<prefix::Component>>(
    let_token: token::Token,
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
            token::Value::ControlSequence(control_sequence) => {
                match input.commands_map_mut().alias_control_sequence(
                    alias,
                    control_sequence,
                    scope,
                ) {
                    Ok(()) => Ok(()),
                    Err(_) => Err(error::new_undefined_command_error(token, input.vm())),
                }
            }
            _ => {
                input.commands_map_mut().alias_token(alias, token, scope);
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
    use crate::testing::*;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", def::get_def()),
            ("global", prefix::get_global()),
            ("let", get_let()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (let_for_macro, r"\def\A{abc}\let\B\A\B", "abc"),
            (local, r"\def\A{a}\def\B{b}\let\C=\A{\let\C=\B \C}\C", "ba"),
            (
                global,
                r"\def\A{a}\def\B{b}\let\C=\A{\global\let\C=\B \C}\C",
                "bb"
            ),
            (let_for_macro_equals, r"\def\A{abc}\let\B=\A\B", "abc"),
        ),
        failure_tests((let_unknown_cs_name, r"\let \B=\A")),
    ];
}
