//! Operations on variables (add, multiply, divide)

use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

/// Get the `\advance` command.
pub fn get_advance<S: TexlangState>() -> command::BuiltIn<S> {
    get_command::<S, AddOp>()
}

/// Get the `\advanceChecked` command.
pub fn get_advance_checked<S: TexlangState>() -> command::BuiltIn<S> {
    get_command::<S, AddCheckedOp>()
}

/// Get the `\multiply` command.
pub fn get_multiply<S: TexlangState>() -> command::BuiltIn<S> {
    get_command::<S, MultiplyOp>()
}

/// Get the `\multiplyWrapped` command.
pub fn get_multiply_wrapped<S: TexlangState>() -> command::BuiltIn<S> {
    get_command::<S, MultiplyWrappedOp>()
}

/// Get the `\divide` command.
pub fn get_divide<S: TexlangState>() -> command::BuiltIn<S> {
    get_command::<S, DivideOp>()
}

fn get_command<S: TexlangState, O: Op>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(math_primitive_fn::<S, O>)
        .with_tag(variable_op_tag())
        .with_doc(O::DOC)
}

static VARIABLE_OP_TAG: command::StaticTag = command::StaticTag::new();

pub fn variable_op_tag() -> command::Tag {
    VARIABLE_OP_TAG.get()
}

trait Op {
    const DOC: &'static str = "";
    type Error: error::TexError;
    fn perform(lhs: i32, rhs: i32) -> Result<i32, Self::Error>;
}

struct AddOp;

impl Op for AddOp {
    const DOC: &'static str = "Add an integer to a variable";
    type Error = OverflowError;
    fn perform(lhs: i32, rhs: i32) -> Result<i32, Self::Error> {
        // Note: TeX silently overflows in \advance
        Ok(lhs.wrapping_add(rhs))
    }
}

struct AddCheckedOp;

impl Op for AddCheckedOp {
    const DOC: &'static str = "Add an integer to a variable and error on overflow";
    type Error = OverflowError;
    fn perform(lhs: i32, rhs: i32) -> Result<i32, Self::Error> {
        match lhs.checked_add(rhs) {
            Some(result) => Ok(result),
            None => Err(OverflowError {
                op_name: "addition",
                lhs,
                rhs,
                wrapped_result: lhs.wrapping_add(rhs),
            }),
        }
    }
}

struct MultiplyOp;

impl Op for MultiplyOp {
    const DOC: &'static str = "Multiply a variable by an integer";
    type Error = OverflowError;
    fn perform(lhs: i32, rhs: i32) -> Result<i32, Self::Error> {
        match lhs.checked_mul(rhs) {
            Some(result) => Ok(result),
            None => Err(OverflowError {
                op_name: "multiplication",
                lhs,
                rhs,
                wrapped_result: lhs.wrapping_mul(rhs),
            }),
        }
    }
}

struct MultiplyWrappedOp;

impl Op for MultiplyWrappedOp {
    const DOC: &'static str = "Multiply a variable by an integer and wrap on overflow";
    type Error = OverflowError;
    fn perform(lhs: i32, rhs: i32) -> Result<i32, Self::Error> {
        Ok(lhs.wrapping_mul(rhs))
    }
}

struct DivideOp;

impl Op for DivideOp {
    const DOC: &'static str = "Divide a variable by an integer";
    type Error = DivisionByZeroError;
    fn perform(lhs: i32, rhs: i32) -> Result<i32, Self::Error> {
        if rhs == 0 {
            return Err(DivisionByZeroError { numerator: lhs });
        }
        Ok(lhs.wrapping_div(rhs))
    }
}

#[derive(Debug)]
struct OverflowError {
    op_name: &'static str,
    lhs: i32,
    rhs: i32,
    wrapped_result: i32,
}

impl error::TexError for OverflowError {
    fn kind(&self) -> error::Kind {
        error::Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        format!["overflow in checked {}", self.op_name]
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![
            format!["left hand side evaluated to {}", self.lhs].into(),
            format!["right hand side evaluated to {}", self.rhs].into(),
            format!["wrapped result would be {}", self.wrapped_result].into(),
        ]
    }
}

#[derive(Debug)]
struct DivisionByZeroError {
    numerator: i32,
}

impl error::TexError for DivisionByZeroError {
    fn kind(&self) -> error::Kind {
        error::Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        "division by zero".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![format!["numerator evaluated to {}", self.numerator].into()]
    }
}

fn math_primitive_fn<S: TexlangState, O: Op>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let Some(variable) = texlang::parse::ArithmeticVariable::parse(input)?.0 else {
        return Ok(());
    };
    OptionalBy::parse(input)?;
    match variable {
        variable::Variable::Int(variable) => {
            let lhs = *variable.get(input.state());
            let rhs = i32::parse(input)?;
            let result = match O::perform(lhs, rhs) {
                Ok(result) => result,
                Err(err) => {
                    return input.vm().error(err);
                }
            };
            variable.set(input, scope, result);
            Ok(())
        }
        variable::Variable::CatCode(_)
        | variable::Variable::TokenList(_)
        | variable::Variable::MathCode(_)
        | variable::Variable::Font(_) => {
            unreachable!("only arithmetic commands are considered");
        }
    }
}

/// When parsed, this type consumes an optional `by` keyword from the input stream.
struct OptionalBy;

impl<S: TexlangState> Parsable<S> for OptionalBy {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        texlang::parse::parse_keyword(input, "by")?;
        Ok(OptionalBy)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::codes;
    use crate::prefix;
    use crate::registers;
    use crate::the;
    use texlang::types::CatCode;
    use texlang::vm::implement_has_component;
    use texlang_testing::*;

    #[derive(Default)]
    struct State {
        catcode: codes::Component<CatCode>,
        prefix: prefix::Component,
        registers: registers::Component<i32, 256>,
        testing: TestingComponent,
    }

    impl TexlangState for State {
        fn variable_assignment_scope_hook(
            state: &mut Self,
        ) -> texcraft_stdext::collections::groupingmap::Scope {
            prefix::variable_assignment_scope_hook(state)
        }
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedError,
        ) -> Result<(), Box<dyn error::TexError>> {
            texlang_testing::TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
    }
    impl the::TheCompatible for State {}

    implement_has_component![State{
        catcode: codes::Component<CatCode>,
        prefix: prefix::Component,
        registers: registers::Component<i32, 256>,
        testing: TestingComponent,
    }];

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("advance", get_advance()),
            ("advanceChecked", get_advance_checked()),
            ("multiply", get_multiply()),
            ("multiplyWrapped", get_multiply_wrapped()),
            ("divide", get_divide()),
            //
            ("catcode", codes::get_catcode()),
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
                ),
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
            multiply_wrapping_overflow,
            r"\multiplyWrapped",
            "100000",
            "100000",
            "1410065408"
        ),
        (
            multiply_wrapping_base_case,
            r"\multiplyWrapped",
            "5",
            "4",
            "20"
        ),
        (divide_base_case, r"\divide", "9", "4", "2"),
        (divide_with_by, r"\divide", "9", "by 4", "2"),
        (divide_pos_neg, r"\divide", "-9", "4", "-2"),
        (divide_neg_pos, r"\divide", "9", "-4", "-2"),
        (divide_neg_neg, r"\divide", "-9", "-4", "2"),
        (divide_exact, r"\divide", "100", "10", "10"),
        (advance_checked_base_case, r"\advanceChecked", "1", "2", "3"),
        (
            advance_checked_negative_summand,
            r"\advanceChecked",
            "10",
            "-2",
            "8"
        )
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
        recoverable_failure_tests(
            (
                advance_incorrect_keyword_1,
                r"\count 1 1\advance\count 1 fy 2 \the \count 1",
                "fy 2 1",
            ),
            (
                advance_incorrect_keyword_2,
                r"\count 1 1\advance\count 1 be 2 \the \count 1",
                "be 2 1",
            ),
            (
                advance_catcode_not_supported,
                r"\advance\catcode 100 by 2",
                "100 by 2",
            ),
            (
                advance_checked_overflow,
                r"\count 1 2147483647 \advanceChecked\count 1 by 1",
                "",
            ),
            (
                multiply_overflow,
                r"\count 1 100000 \multiply\count 1 by 100000 \the \count 1",
                "100000"
            ),
            (
                divide_by_zero,
                r"\count 1 20 \divide\count 1 by 0 \the\count 1",
                "20"
            ),
        ),
    ];
}
