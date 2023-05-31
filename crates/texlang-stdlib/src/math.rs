//! Operations on variables (add, multiply, divide)

use crate::prefix;
use texlang_core::parse::OptionalBy;
use texlang_core::traits::*;
use texlang_core::*;

pub const ADVANCE_DOC: &str = "Add an integer to a variable";
pub const ADVANCECHK_DOC: &str = "Add an integer to a variable and error on overflow";
pub const MULTIPLY_DOC: &str = "Multiply a variable by an integer";
pub const MULTIPLYCHK_DOC: &str = "Multiply a variable by an integer and error on overflow";
pub const DIVIDE_DOC: &str = "Divide a variable by an integer";

/// Get the `\advance` command.
pub fn get_advance<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(advance_fn).with_tag(get_variable_op_tag())
}

/// Get the `\advancechk` command.
pub fn get_advancechk<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(advancechk_fn).with_tag(get_variable_op_tag())
}

/// Get the `\multiply` command.
pub fn get_multiply<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(multiply_fn).with_tag(get_variable_op_tag())
}

/// Get the `\multiplychk` command.
pub fn get_multiplychk<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(multiplychk_fn).with_tag(get_variable_op_tag())
}

/// Get the `\divide` command.
pub fn get_divide<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(divide_fn).with_tag(get_variable_op_tag())
}

static VARIABLE_OP_TAG: command::StaticTag = command::StaticTag::new();

pub fn get_variable_op_tag() -> command::Tag {
    VARIABLE_OP_TAG.get()
}

macro_rules! create_arithmetic_primitive {
    ($prim_fn: ident, $arithmetic_op: ident) => {
        fn $prim_fn<S: HasComponent<prefix::Component>>(
            token: token::Token,
            input: &mut vm::ExecutionInput<S>,
        ) -> anyhow::Result<()> {
            let scope = input.state_mut().component_mut().read_and_reset_global();
            let variable = variable::Variable::parse(input)?;
            OptionalBy::parse(input)?;
            match variable {
                variable::Variable::Int(variable) => {
                    let n = i32::parse(input)?;
                    let i = variable.value_mut(input, scope);
                    *i = $arithmetic_op(token, *i, n)?;
                    Ok(())
                }
                variable::Variable::CatCode(_) => invalid_variable_error(token),
            }
        }
    };
}

fn invalid_variable_error(token: token::Token) -> anyhow::Result<()> {
    Err(error::TokenError::new(
        token,
        "arithmetic commands cannot be applied to variables of type X",
    )
    .add_note(
        "arithmetic commands (\\advance, \\multiply, \\divide) can be applied to integer, dimension, glue and muglue variables",
    )
    .cast())
}

#[inline]
fn add(_: token::Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    // Note: TeX explicitely permits overflow in \advance
    Ok(lhs.wrapping_add(rhs))
}

fn checked_add(token: token::Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
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
fn multiply(_: token::Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
    // Note: TeX explicitely permits overflow in \multiply
    Ok(lhs.wrapping_mul(rhs))
}

fn checked_multiply(token: token::Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
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
fn divide(token: token::Token, lhs: i32, rhs: i32) -> anyhow::Result<i32> {
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
    use crate::catcode;
    use crate::registers;
    use crate::script;
    use crate::testing::*;
    use crate::the;
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        catcode: catcode::Component,
        prefix: prefix::Component,
        registers: registers::Component<i32, 256>,
        script: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![
        State,
        (catcode::Component, catcode),
        (prefix::Component, prefix),
        (registers::Component<i32, 256>, registers),
        (script::Component, script),
    ];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("advance", get_advance()),
            ("advancechk", get_advancechk()),
            ("multiply", get_multiply()),
            ("multiplychk", get_multiplychk()),
            ("divide", get_divide()),
            //
            ("catcode", catcode::get_catcode()),
            ("count", registers::get_count()),
            ("global", prefix::get_global()),
            ("the", the::get_the()),
        ])
    }

    macro_rules! arithmetic_tests {
        ( $( ($name: ident, $op: expr, $lhs: expr, $rhs: expr, $expected: expr) ),* $(,)? ) => {
            test_suite![
                expansion_equality_tests(
                    $(
                        (
                            $name,
                            format![r"\count 1 {} {} \count 1 {} \the\count 1", $lhs, $op, $rhs],
                            $expected
                        ),
                    )*
                )
            ];
        };
    }

    arithmetic_tests![
        (advance_base_case, r"\advance", "1", "2", "3"),
        (advance_base_case_with_by, r"\advance", "1", "by 2", "3"),
        (advance_negative_summand, r"\advance", "10", "-2", "8"),
        (
            advance_overflow_case,
            r"\advance",
            "2147483647",
            "1",
            "-2147483648"
        ),
        (multiply_base_case, r"\multiply", "5", "4", "20"),
        (multiply_base_case_with_by, r"\multiply", "5", "by 4", "20"),
        (multiply_pos_neg, r"\multiply", "-5", "4", "-20"),
        (multiply_neg_pos, r"\multiply", "5", "-4", "-20"),
        (multiply_neg_neg, r"\multiply", "-5", "-4", "20"),
        (
            multiply_overflow,
            r"\multiply",
            "100000",
            "100000",
            "1410065408"
        ),
        (multiplychk_base_case, r"\multiplychk", "5", "4", "20"),
        (divide_base_case, r"\divide", "9", "4", "2"),
        (divide_with_by, r"\divide", "9", "by 4", "2"),
        (divide_pos_neg, r"\divide", "-9", "4", "-2"),
        (divide_neg_pos, r"\divide", "9", "-4", "-2"),
        (divide_neg_neg, r"\divide", "-9", "-4", "2"),
        (divide_exact, r"\divide", "100", "10", "10"),
        (advancechk_base_case, r"\advancechk", "1", "2", "3"),
        (advancechk_negative_summand, r"\advancechk", "10", "-2", "8")
    ];

    test_suite![
        expansion_equality_tests(
            (
                advance_x_by_x,
                r"\count 1 200 \advance \count 1 by \count 1 a\the\count 1",
                r"a400"
            ),
            (
                global_advance,
                r"\count 1 5{\global\advance\count 1 8}\the\count 1",
                "13"
            ),
            (
                local_advance,
                r"\count 1 5{\advance\count 1 8}\the\count 1",
                "5"
            ),
        ),
        failure_tests(
            (
                advance_incorrect_keyword_1,
                r"\count 1 1\advance\count 1 fy 2 \the \count 1"
            ),
            (
                advance_incorrect_keyword_2,
                r"\count 1 1\advance\count 1 be 2 \the \count 1"
            ),
            (advance_catcode_not_supported, r"\advance\catcode 100 by 2"),
            (
                advancechk_overflow,
                r"\count 1 2147483647 \advancechk\count 1 by 1"
            ),
            (
                multiplychk_overflow,
                r"\count 1 100000 \multiplychk\count 1 by 100000"
            ),
            (divide_by_zero, r"\divide\count 1 by 0"),
        ),
    ];
}
