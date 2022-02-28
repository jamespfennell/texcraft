//! TeX registers and commands to access them

use texlang_core::parse;
use texlang_core::prelude::*;
use texlang_core::runtime::HasComponent;
use texlang_core::variable::{TypedVariable, Variable};

pub const COUNT_DOC: &str = "Get or set an integer register";
pub const COUNTDEF_DOC: &str = "Bind an integer register to a control sequence";

struct Registers<T: Copy + Default, const N: usize> {
    values: [T; N],
    fallback: T,
}

impl<T: Copy + Default, const N: usize> Registers<T, N> {
    fn new() -> Registers<T, N> {
        Registers {
            values: [Default::default(); N],
            fallback: Default::default(),
        }
    }

    fn read(&self, addr: usize) -> &T {
        match self.values.get(addr) {
            None => &self.fallback,
            Some(value) => value,
        }
    }

    fn write(&mut self, addr: usize) -> &mut T {
        match self.values.get_mut(addr) {
            None => &mut self.fallback,
            Some(r) => r,
        }
    }

    fn num(&self) -> usize {
        self.values.len()
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
pub fn get_count<S: HasComponent<Component<N>>, const N: usize>() -> command::VariableFn<S> {
    count_fn
}

fn count_fn<S: HasComponent<Component<N>>, const N: usize>(
    count_token: Token,
    input: &mut runtime::ExpandedInput<S>,
    _: usize,
) -> anyhow::Result<Variable<S>> {
    let addr: usize = parse::parse_number(input)?;
    if addr >= N {
        return Err(integer_register_too_large_error(
            count_token,
            addr,
            input.state().component().int_registers.num(),
        ));
    }
    Ok(Variable::Int(TypedVariable::new(
        int_register_ref_fn,
        int_register_mut_ref_fn,
        addr,
    )))
}

/// Get the `\countdef` command.
pub fn get_countdef<S: HasComponent<Component<N>>, const N: usize>() -> command::ExecutionFn<S> {
    countdef_fn
}

fn countdef_fn<S: HasComponent<Component<N>>, const N: usize>(
    countdef_token: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let cs_name =
        parse::parse_command_target("countdef", countdef_token, input.unexpanded_stream())?;
    parse::parse_optional_equals(input.regular())?;
    let addr: usize = parse::parse_number(input.regular())?;
    if addr >= input.state().component().int_registers.num() {
        return Err(integer_register_too_large_error(
            countdef_token,
            addr,
            input.state().component().int_registers.num(),
        ));
    }
    let new_cmd = command::VariableCommand::new(singleton_fn, addr);
    input.base_mut().commands_map.insert(cs_name, new_cmd);
    Ok(())
}

fn singleton_fn<S: HasComponent<Component<N>>, const N: usize>(
    _: Token,
    _: &mut runtime::ExpandedInput<S>,
    addr: usize,
) -> anyhow::Result<Variable<S>> {
    Ok(Variable::Int(TypedVariable::new(
        int_register_ref_fn,
        int_register_mut_ref_fn,
        addr,
    )))
}

fn int_register_ref_fn<S: HasComponent<Component<N>>, const N: usize>(
    state: &S,
    addr: usize,
) -> &i32 {
    state.component().int_registers.read(addr)
}

fn int_register_mut_ref_fn<S: HasComponent<Component<N>>, const N: usize>(
    state: &mut S,
    addr: usize,
) -> &mut i32 {
    state.component_mut().int_registers.write(addr)
}

fn integer_register_too_large_error(token: Token, addr: usize, num: usize) -> anyhow::Error {
    error::TokenError::new(
        token,
        format![
            "Register number {} passed to {} is too large; there are only {} integer registers",
            addr, token, num,
        ],
    )
    .cast()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execwhitespace;
    use crate::testutil::*;
    use crate::the;
    use texlang_core::runtime::implement_has_component;

    #[derive(Default)]
    struct State {
        registers: Component<256>,
        exec: execwhitespace::Component,
    }

    implement_has_component![
        State,
        (Component<256>, registers),
        (execwhitespace::Component, exec),
    ];

    fn setup_expansion_test(s: &mut runtime::Env<State>) {
        s.set_command("the", the::get_the());
        s.set_command("count", get_count());
        s.set_command("countdef", get_countdef());
    }

    expansion_test![write_and_read_register, r"\count 23 4 \the\count 23", r"4"];
    expansion_test![
        write_and_read_register_eq,
        r"\count 23 = 4 \the\count 23",
        r"4"
    ];
    expansion_failure_test![write_register_index_too_big, r"\count 260 = 4"];
    expansion_failure_test![write_register_negative_index, r"\count -1 = 4"];

    expansion_test![countdef_base_case, r"\countdef\A 23\A 4 \the\A", r"4"];
    expansion_test![countdef_base_case_eq, r"\countdef\A = 23\A 4 \the\A", r"4"];
    expansion_test![
        countdef_with_count,
        r"\countdef\A 23\A 4\count 1 0 \the\A",
        r"4"
    ];
    expansion_test![
        countdef_with_same_count,
        r"\countdef\A 23\A 4\count 23 5 \the\A",
        r"5"
    ];
    expansion_failure_test![countdef_register_index_too_big, r"\countdef\A 260 \A= 4"];
}
