//! Commands for tracing TeX execution

use texlang_core::command;
use texlang_core::variable;
use texlang_core::vm::BaseState;

/// Get the `\tracingmacros` command.
pub fn get_tracingmacros<S>() -> command::Command<S> {
    command::Command::new_variable(
        |_, _, _| -> anyhow::Result<variable::Variable<S>> {
            Ok(variable::Variable::BaseInt(variable::TypedVariable::new(
                |base: &BaseState<S>, _: u32| -> &i32 { &base.tracing_macros },
                |base: &mut BaseState<S>, _: u32| -> &mut i32 { &mut base.tracing_macros },
                0,
            )))
        },
        0,
    )
}
