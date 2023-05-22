//! Register variables (`\count`, `\countdef`)

use texcraft_stdext::collections::groupingmap;
use texlang_core::traits::*;
use texlang_core::*;

pub const COUNT_DOC: &str = "Get or set an integer register";
pub const COUNTDEF_DOC: &str = "Bind an integer register to a control sequence";

struct Registers<T: Copy + Default, const N: usize> {
    values: [T; N],
}

impl<T: Copy + Default, const N: usize> Registers<T, N> {
    fn new() -> Registers<T, N> {
        Registers {
            values: [Default::default(); N],
        }
    }

    fn read(&self, index: usize) -> &T {
        self.values.get(index).unwrap()
    }

    fn write(&mut self, index: usize) -> &mut T {
        self.values.get_mut(index).unwrap()
    }
}

/// A component holding the values of all registers.
pub struct Component<const N: usize> {
    int_registers: Registers<i32, N>,
}

impl<const N: usize> Component<N> {
    /// Creates a new registers component.
    pub fn new() -> Component<N> {
        Component {
            int_registers: Registers::new(),
        }
    }
}

impl<const N: usize> Default for Component<N> {
    fn default() -> Self {
        Component::new()
    }
}

/// Get the `\count` command.
pub fn get_count<S: HasComponent<Component<N>>, const N: usize>() -> command::BuiltIn<S> {
    variable::Command::new_array(
        int_register_ref_fn,
        int_register_mut_ref_fn,
        variable::IndexResolver::Dynamic(count_fn),
    )
    .into()
}

fn count_fn<S: HasComponent<Component<N>>, const N: usize>(
    count_token: token::Token,
    input: &mut vm::ExpandedStream<S>,
) -> anyhow::Result<variable::Index> {
    let index: usize = parse::parse_number(input)?;
    if index >= N {
        return Err(integer_register_too_large_error(count_token, index, N));
    }
    Ok(index.into())
}

/// Get the `\countdef` command.
pub fn get_countdef<S: HasComponent<Component<N>>, const N: usize>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(countdef_fn)
}

fn countdef_fn<S: HasComponent<Component<N>>, const N: usize>(
    countdef_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let cs_name = parse::parse_command_target("countdef", countdef_token, input.unexpanded())?;
    parse::parse_optional_equals(input)?;
    let index: usize = parse::parse_number(input)?;
    if index >= N {
        return Err(integer_register_too_large_error(countdef_token, index, N));
    }
    // TODO: I suspect \countdef should honor \global, but haven't checked pdfTeX.
    input.commands_map_mut().insert_variable_command(
        cs_name,
        variable::Command::new_array(
            int_register_ref_fn,
            int_register_mut_ref_fn,
            variable::IndexResolver::Static(index.into()),
        ),
        groupingmap::Scope::Local,
    );
    Ok(())
}

fn int_register_ref_fn<S: HasComponent<Component<N>>, const N: usize>(
    state: &S,
    index: variable::Index,
) -> &i32 {
    state.component().int_registers.read(index.0)
}

fn int_register_mut_ref_fn<S: HasComponent<Component<N>>, const N: usize>(
    state: &mut S,
    index: variable::Index,
) -> &mut i32 {
    state.component_mut().int_registers.write(index.0)
}

fn integer_register_too_large_error(
    token: token::Token,
    index: usize,
    num: usize,
) -> anyhow::Error {
    error::TokenError::new(
        token,
        format![
            "Register number {index} passed to {token} is too large; there are only {num} integer registers"
        ],
    )
    .cast()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::script;
    use crate::testing::*;
    use crate::the;
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        registers: Component<256>,
        exec: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![
        State,
        (Component<256>, registers),
        (script::Component, exec),
    ];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("count", get_count()),
            ("countdef", get_countdef()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (write_and_read_register, r"\count 23 4 \the\count 23", r"4"),
            (
                write_and_read_register_eq,
                r"\count 23 = 4 \the\count 23",
                r"4"
            ),
            (countdef_base_case, r"\countdef\A 23\A 4 \the\A", r"4"),
            (countdef_base_case_eq, r"\countdef\A = 23\A 4 \the\A", r"4"),
            (
                countdef_with_count,
                r"\countdef\A 23\A 4\count 1 0 \the\A",
                r"4"
            ),
            (
                countdef_with_same_count,
                r"\countdef\A 23\A 4\count 23 5 \the\A",
                r"5"
            ),
        ),
        failure_tests(
            (write_register_index_too_big, r"\count 260 = 4"),
            (write_register_negative_index, r"\count -1 = 4"),
            (countdef_register_index_too_big, r"\countdef\A 260 \A= 4"),
        ),
    ];
}
