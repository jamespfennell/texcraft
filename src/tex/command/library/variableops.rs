//! Operations on variables (add, multiply, divide)

use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::variable::Variable;

const ADVANCE_DOC: &str = "Add an integer to a variable";
const MULTIPLY_DOC: &str = "Multiple a variable by an integer";
const DIVIDE_DOC: &str = "Divide a variable by an integer";

macro_rules! create_arithmetic_primitive {
    ($prim_fn: ident, $arithmetic_op: ident) => {
        fn $prim_fn<S>(
            token: Token,
            input: &mut ExecutionInput<S>,
        ) -> anyhow::Result<()> {
            let variable = parse::parse_variable(&mut input.regular())?;
            parse::parse_optional_by(&mut input.regular())?;
            let n: i32 = parse::parse_number(&mut input.regular())?;
            match variable {
                Variable::Int(variable) => {
                    $arithmetic_op(token,  variable.get_mut(input.state_mut()), n)
                }
                Variable::CatCode(_) => Err(error::TokenError::new(
                    token,
                    "arithmetic commands cannot be applied to variables of type X",
                )
                .add_note(
                    "airthmetic commands (\\advance, \\multiply, \\divide) can be applied to integer, dimension, glue and muglue variables",
                )
                .cast()),
            }
        }
    };
}

fn add(_: Token, lhs: &mut i32, rhs: i32) -> anyhow::Result<()> {
    // Note: TeX explicitely permits overflow in arithmetic operations
    *lhs = lhs.wrapping_add(rhs);
    Ok(())
}

fn multiply(_: Token, lhs: &mut i32, rhs: i32) -> anyhow::Result<()> {
    *lhs = lhs.wrapping_mul(rhs);
    Ok(())
}

fn divide(token: Token, lhs: &mut i32, rhs: i32) -> anyhow::Result<()> {
    if rhs == 0 {
        return Err(error::TokenError::new(token, "division by zero").cast());
    }
    *lhs = lhs.wrapping_div(rhs);
    Ok(())
}

create_arithmetic_primitive![advance_fn, add];
create_arithmetic_primitive![multiply_fn, multiply];
create_arithmetic_primitive![divide_fn, divide];

/// Get the `\advance` command.
pub fn get_advance<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: advance_fn,
        docs: ADVANCE_DOC,
        id: None,
    }
}

/// Get the `\multiply` command.
pub fn get_multiply<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: multiply_fn,
        docs: MULTIPLY_DOC,
        id: None,
    }
}

/// Get the `\divide` command.
pub fn get_divide<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: divide_fn,
        docs: DIVIDE_DOC,
        id: None,
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::tex::command::library::catcodecmd;
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
            registers: registers::Component::new(),
        }
    }

    implement_has_registers![State, registers];

    fn setup_expansion_test(s: &mut Base<State>) {
        s.set_command("the", the::get_the());
        s.set_command("advance", get_advance());
        s.set_command("multiply", get_multiply());
        s.set_command("divide", get_divide());
        s.set_command("count", registers::get_count());
        s.set_command("catcode", catcodecmd::get_catcode());
    }

    macro_rules! arithmetic_test {
        ($name: ident, $op: expr, $lhs: expr, $rhs: expr, $expected: expr) => {
            expansion_test![
                $name,
                format![r"\count 1 {} {} \count 1 {} \the\count 1", $lhs, $op, $rhs],
                $expected
            ];
        };
    }

    arithmetic_test![advance_base_case, r"\advance", "1", "2", "3"];
    arithmetic_test![advance_base_case_with_by, r"\advance", "1", "by 2", "3"];
    arithmetic_test![advance_negative_summand, r"\advance", "10", "-2", "8"];
    arithmetic_test![
        advance_overflow_case,
        r"\advance",
        "2147483647",
        "1",
        "-2147483648"
    ];
    expansion_failure_test![
        advance_incorrect_keyword_1,
        r"\count 1 1\advance\count 1 fy 2 \the \count 1"
    ];
    expansion_failure_test![
        advance_incorrect_keyword_2,
        r"\count 1 1\advance\count 1 be 2 \the \count 1"
    ];
    expansion_failure_test![advance_catcode_not_supported, r"\advance\catcode 100 by 2"];

    arithmetic_test![multiply_base_case, r"\multiply", "5", "4", "20"];
    arithmetic_test![multiply_base_case_with_by, r"\multiply", "5", "by 4", "20"];
    arithmetic_test![multiply_pos_neg, r"\multiply", "-5", "4", "-20"];
    arithmetic_test![multiply_neg_pos, r"\multiply", "5", "-4", "-20"];
    arithmetic_test![multiply_neg_neg, r"\multiply", "-5", "-4", "20"];
    arithmetic_test![
        multiply_overflow,
        r"\multiply",
        "100000",
        "100000",
        "1410065408"
    ];

    arithmetic_test![divide_base_case, r"\divide", "9", "4", "2"];
    arithmetic_test![divide_with_by, r"\divide", "9", "by 4", "2"];
    arithmetic_test![divide_pos_neg, r"\divide", "-9", "4", "-2"];
    arithmetic_test![divide_neg_pos, r"\divide", "9", "-4", "-2"];
    arithmetic_test![divide_neg_neg, r"\divide", "-9", "-4", "2"];
    arithmetic_test![divide_exact, r"\divide", "100", "10", "10"];

    expansion_failure_test![divide_by_zero, r"\divide\count 1 by 0"];
}