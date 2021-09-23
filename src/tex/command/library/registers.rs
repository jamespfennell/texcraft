//! TeX registers and commands to access them

use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::variable;
use crate::tex::variable::{TypedVariable, Variable};

const COUNT_DOC: &str = "Get or set an integer register";
const COUNTDEF_DOC: &str = "Bind an integer register to a control sequence";

/// Trait for a state that has a registers component.
pub trait HasRegisters {
    /// Get a reference to the registers component.
    fn registers(&self) -> &Component;

    /// Get a mutable reference to the registers component.
    fn registers_mut(&mut self) -> &mut Component;
}

macro_rules! implement_has_registers {
    ( $type: ident, $field: ident ) => {
        impl registers::HasRegisters for $type {
            fn registers(&self) -> &registers::Component {
                &self.$field
            }
            fn registers_mut(&mut self) -> &mut registers::Component {
                &mut self.$field
            }
        }
    };
}

struct Registers<T: Copy + Default> {
    values: Vec<T>,
    fallback: T,
}

impl<T: Copy + Default> Registers<T> {
    fn new(num_registers: usize) -> Registers<T> {
        Registers {
            values: vec![Default::default(); num_registers],
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
pub struct Component {
    int_registers: Registers<i32>,
}

impl Component {
    /// Creates a new registers component.
    pub fn new(num_int_registers: usize) -> Component {
        Component {
            int_registers: Registers::new(num_int_registers),
        }
    }
}

fn read_int_register_fn<S: HasRegisters>(state: &S, addr: usize) -> &i32 {
    &state.registers().int_registers.read(addr)
}

fn write_int_register_fn<S: HasRegisters>(state: &mut S, addr: usize) -> &mut i32 {
    state.registers_mut().int_registers.write(addr)
}

fn count_fn<S: HasRegisters>(
    count_token: &Token,
    input: &mut ExpansionInput<S>,
    _: usize,
) -> anyhow::Result<Variable<S>> {
    let addr: usize = parse::parse_number(input)?;
    if addr >= input.state().registers().int_registers.num() {
        return Err(integer_register_too_large_error(
            count_token.clone(),
            addr,
            input.state().registers().int_registers.num(),
        ));
    }
    Ok(Variable::Int(TypedVariable::new(
        read_int_register_fn,
        write_int_register_fn,
        addr,
    )))
}

/// Get the `\count` command.
pub fn get_count<S: HasRegisters>() -> variable::Command<S> {
    variable::Command::Dynamic(count_fn, 0, COUNT_DOC)
}

fn countdef_fn<S: HasRegisters>(
    countdef_token: Token,
    input: &mut command::ExecutionInput<S>,
) -> anyhow::Result<()> {
    // TODO: don't clone the token!
    let cs_name = parse::parse_command_target(
        "countdef",
        countdef_token.clone(),
        input.unexpanded_stream(),
    )?;
    parse::parse_optional_equals(&mut input.regular())?;
    let addr: usize = parse::parse_number(&mut input.regular())?;
    if addr >= input.state().registers().int_registers.num() {
        return Err(integer_register_too_large_error(
            countdef_token,
            addr,
            input.state().registers().int_registers.num(),
        ));
    }
    let new_cmd = Variable::Int(TypedVariable::new(
        read_int_register_fn,
        write_int_register_fn,
        addr,
    ));
    input.base_mut().set_command(cs_name, new_cmd);
    Ok(())
}

/// Get the `\countdef` command.
pub fn get_countdef<S: HasRegisters>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: countdef_fn,
        docs: COUNTDEF_DOC,
        id: None,
    }
}

fn integer_register_too_large_error(token: Token, addr: usize, num: usize) -> anyhow::Error {
    error::TokenError::new(
        token.clone(),
        format![
            "Register number {} passed to {} is too large; there are only {} integer registers",
            addr, token, num,
        ],
    )
    .cast()
}

#[cfg(test)]
mod tests {
    use crate::tex::command::library::registers;
    use crate::tex::command::library::the;
    use crate::tex::driver;
    use crate::tex::input;
    use crate::tex::state::Base;
    use crate::tex::token::catcode;

    struct State {
        registers: registers::Component,
    }

    fn new_state() -> State {
        State {
            registers: registers::Component::new(256),
        }
    }

    implement_has_registers![State, registers];

    fn setup_expansion_test(s: &mut Base<State>) {
        s.set_command("the", the::get_the());
        s.set_command("count", registers::get_count());
        s.set_command("countdef", registers::get_countdef());
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
