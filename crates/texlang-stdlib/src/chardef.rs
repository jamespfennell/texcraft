//! The `\chardef` primitive.
use texlang::traits::*;
use texlang::*;

/// Get the `\chardef` command.
pub fn get_chardef<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(chardef_primitive_fn)
}

fn chardef_primitive_fn<S: TexlangState>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<error::Error>> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let (target, _, c) = <(token::CommandRef, parse::OptionalEquals, char)>::parse(input)?;
    input
        .commands_map_mut()
        .insert(target, command::Command::Character(c), scope);
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{testing::*, the};
    use std::collections::HashMap;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("chardef", get_chardef()),
            ("the", the::get_the()),
            ("i", TestingComponent::get_integer()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (basic_case, r"\chardef\Hello = `\+ \Hello", "+"),
            (
                basic_case_with_the,
                r"\chardef\Hello = 123 \the\Hello",
                "123"
            ),
            (
                parsable_as_number,
                r"\chardef\Hello = 13 \i=\Hello x\the\i",
                "x13"
            ),
            (
                parsable_as_number_negative,
                r"\chardef\Hello = 13 \i=-\Hello x\the\i",
                "x-13"
            ),
        ),
        serde_tests((basic_case, r"\chardef\Hello = `\+ ", r"\Hello"),),
    ];
}
