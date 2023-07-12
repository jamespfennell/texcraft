//! `\let` aliasing command

use crate::prefix;
use texlang::parse::{Command, OptionalEqualsUnexpanded};
use texlang::traits::*;
use texlang::*;

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
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<error::Error>> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let Command::ControlSequence(alias) = Command::parse(input)?;
    OptionalEqualsUnexpanded::parse(input)?;
    match input.unexpanded().next()? {
        None => Err(error::SimpleEndOfInputError::new(
            input.vm(),
            "unexpected end of input while reading the right hand side of a \\let assignment",
        )
        .into()),
        Some(token) => match token.value() {
            token::Value::ControlSequence(control_sequence) => {
                match input.commands_map_mut().alias_control_sequence(
                    alias,
                    control_sequence,
                    scope,
                ) {
                    Ok(()) => Ok(()),
                    Err(_) => Err(error::UndefinedCommandError::new(input.vm(), token).into()),
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
    use crate::the;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", def::get_def()),
            ("global", prefix::get_global()),
            ("integer", State::get_integer()),
            ("let", get_let()),
            ("the", the::get_the()),
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
            (let_character, r"\let\A=B\A", "B"),
        ),
        serde_tests(
            (
                alias_execution_primitive,
                r"\let\defNew=\def",
                r"\defNew\A{abc}\A"
            ),
            (
                alias_expansion_primitive,
                r"\let\theNew=\the",
                r"\integer=3\theNew\integer"
            ),
            (
                alias_variable_singleton,
                r"\let\integerNew=\integer",
                r"\integerNew=3\the\integer"
            ),
            (serde_macro, r"\def\A{Hello World}\let\B=\A ", r"\A \B",),
            (serde_character, r"\let\A=B ", r"\A",),
        ),
        failure_tests((let_unknown_cs_name, r"\let \B=\A")),
    ];
}
