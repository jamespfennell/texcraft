//! The `\mathchardef` primitive.
use texlang::traits::*;
use texlang::*;

/// Get the `\mathchardef` command.
pub fn get_mathchardef<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(mathchardef_primitive_fn)
}

fn mathchardef_primitive_fn<S: TexlangState>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<error::Error>> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let (target, _, c) =
        <(token::CommandRef, parse::OptionalEquals, types::MathCode)>::parse(input)?;
    input
        .commands_map_mut()
        .insert(target, command::Command::MathCharacter(c), scope);
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::the;
    use std::collections::HashMap;
    use texlang_testing::*;

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("mathchardef", get_mathchardef()),
            ("the", the::get_the()),
            ("i", TestingComponent::get_integer()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (
                basic_case,
                r"\mathchardef\Hello = `\+ \Hello",
                "MathCode(43)"
            ),
            (
                basic_case_with_the,
                r"\mathchardef\Hello = 123 \the\Hello",
                "123"
            ),
            (
                parsable_as_number,
                r"\mathchardef\Hello = 13 \i=\Hello x\the\i",
                "x13"
            ),
            (
                parsable_as_number_negative,
                r"\mathchardef\Hello = 13 \i=-\Hello x\the\i",
                "x-13"
            ),
        ),
        serde_tests((basic_case, r"\mathchardef\Hello = `\+ ", r"\Hello"),),
    ];
}
