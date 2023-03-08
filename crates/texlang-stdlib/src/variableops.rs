//! Operations on variables (add, multiply, divide)

use crate::prefix;
use texlang_core::parse;
use texlang_core::prelude::*;
use texlang_core::variable;

pub const ADVANCE_DOC: &str = "Add an integer to a variable";
pub const ADVANCECHK_DOC: &str = "Add an integer to a variable and error on overflow";
pub const MULTIPLY_DOC: &str = "Multiply a variable by an integer";
pub const MULTIPLYCHK_DOC: &str = "Multiply a variable by an integer and error on overflow";
pub const DIVIDE_DOC: &str = "Divide a variable by an integer";

/// Get the `\advance` command.
pub fn get_advance<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(advance_fn).with_id(get_variable_op_id())
}

/// Get the `\advancechk` command.
pub fn get_advancechk<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(advancechk_fn).with_id(get_variable_op_id())
}

/// Get the `\multiply` command.
pub fn get_multiply<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(multiply_fn).with_id(get_variable_op_id())
}

/// Get the `\multiplychk` command.
pub fn get_multiplychk<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(multiplychk_fn).with_id(get_variable_op_id())
}

/// Get the `\divide` command.
pub fn get_divide<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(divide_fn).with_id(get_variable_op_id())
}
struct VariableOp;

pub fn get_variable_op_id() -> std::any::TypeId {
    std::any::TypeId::of::<VariableOp>()
}

macro_rules! create_arithmetic_primitive {
    ($prim_fn: ident, $arithmetic_op: ident) => {
        fn $prim_fn<S: HasComponent<prefix::Component>>(
            token: Token,
            input: &mut vm::ExecutionInput<S>,
        ) -> anyhow::Result<()> {
            let scope = input.state_mut().component_mut().read_and_reset_global();
            let variable = parse::parse_variable(input)?;
            parse::parse_optional_by(input)?;
            match variable {
                variable::Variable::Int(variable) => {
                    let n: i32 = parse::parse_number(input)?;
                    let i = variable.value_mut(input, scope);
                    *i = $arithmetic_op(token, *i, n)?;
                    Ok(())
                }
                variable::Variable::CatCode(_) => invalid_variable_error(token),
            }
        }
    };
}

fn invalid_variable_error(token: Token) -> anyhow::Result<()> {
    Err(error::TokenError::new(
        token,
        "arithmetic commands cannot be applied to variables of type X",
    )
    .add_note(
        "airthmetic commands (\\advance, \\multiply, \\divide) can be applied to integer, dimension, glue and muglue variables",
    )
    .cast())
}

#[inline]
fn add(_: Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    // Note: TeX explicitely permits overflow in \advance
    Ok(lhs.wrapping_add(rhs))
}

fn checked_add(token: Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    match lhs.checked_add(rhs) {
        Some(result) => Ok(result),
        None => Err(
            error::TokenError::new(token, "overflow in checked addition")
                .add_note(format!["left hand side evaluated to {lhs}"])
                .add_note(format!["right hand side evaluated {rhs}"])
                .add_note(format![
                    "overflowed result would be {}",
                    lhs.wrapping_add(rhs)
                ])
                .add_note("use \\advance instead of \\advancechk to permit overflowing")
                .cast(),
        ),
    }
}

#[inline]
fn multiply(_: Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    // Note: TeX explicitely permits overflow in \multiply
    Ok(lhs.wrapping_mul(rhs))
}

fn checked_multiply(token: Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    match lhs.checked_mul(rhs) {
        Some(result) => Ok(result),
        None => Err(
            error::TokenError::new(token, "overflow in checked multiplication")
                .add_note(format!["left hand side evaluated to {lhs}"])
                .add_note(format!["right hand side evaluated {rhs}"])
                .add_note(format![
                    "overflowed result would be {}",
                    lhs.wrapping_mul(rhs)
                ])
                .add_note("use \\multiply instead of \\multiplychk to permit overflowing")
                .cast(),
        ),
    }
}

#[inline]
fn divide(token: Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    if rhs == 0 {
        return Err(error::TokenError::new(token, "division by zero").cast());
    }
    Ok(lhs.wrapping_div(rhs))
}

create_arithmetic_primitive![advance_fn, add];
create_arithmetic_primitive![advancechk_fn, checked_add];
create_arithmetic_primitive![multiply_fn, multiply];
create_arithmetic_primitive![multiplychk_fn, checked_multiply];
create_arithmetic_primitive![divide_fn, divide];

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::catcodecmd;
    use crate::registers;
    use crate::script;
    use crate::testutil::*;
    use crate::the;
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        registers: registers::Component<256>,
        exec: script::Component,
        prefix: prefix::Component,
    }

    implement_has_component![
        State,
        (registers::Component<256>, registers),
        (script::Component, exec),
        (prefix::Component, prefix),
    ];

    fn setup_expansion_test() -> HashMap<&'static str, command::Command<State>> {
        HashMap::from([
            ("advance", get_advance()),
            ("advancechk", get_advancechk()),
            ("multiply", get_multiply()),
            ("multiplychk", get_multiplychk()),
            ("divide", get_divide()),
            //
            ("catcode", catcodecmd::get_catcode()),
            ("count", registers::get_count()),
            ("global", prefix::get_global()),
            ("the", the::get_the()),
        ])
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
    arithmetic_test![advancechk_base_case, r"\advancechk", "1", "2", "3"];
    arithmetic_test![advancechk_negative_summand, r"\advancechk", "10", "-2", "8"];
    expansion_failure_test![
        advancechk_overflow,
        r"\count 1 2147483647 \advancechk\count 1 by 1"
    ];
    expansion_test![
        advance_x_by_x,
        r"\count 1 200 \advance \count 1 by \count 1 a\the\count 1",
        r"a400"
    ];
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
    arithmetic_test![multiplychk_base_case, r"\multiplychk", "5", "4", "20"];
    expansion_failure_test![
        multiplychk_overflow,
        r"\count 1 100000 \multiplychk\count 1 by 100000"
    ];

    arithmetic_test![divide_base_case, r"\divide", "9", "4", "2"];
    arithmetic_test![divide_with_by, r"\divide", "9", "by 4", "2"];
    arithmetic_test![divide_pos_neg, r"\divide", "-9", "4", "-2"];
    arithmetic_test![divide_neg_pos, r"\divide", "9", "-4", "-2"];
    arithmetic_test![divide_neg_neg, r"\divide", "-9", "-4", "2"];
    arithmetic_test![divide_exact, r"\divide", "100", "10", "10"];
    expansion_test![
        local_advance,
        r"\count 1 5{\advance\count 1 8}\the\count 1",
        "5"
    ];
    expansion_test![
        global_advance,
        r"\count 1 5{\global\advance\count 1 8}\the\count 1",
        "13"
    ];

    expansion_failure_test![divide_by_zero, r"\divide\count 1 by 0"];
}
