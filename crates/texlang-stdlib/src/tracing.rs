//! Commands for tracing TeX execution

use texlang_core::command;
use texlang_core::runtime::BaseState;
use texlang_core::variable;

/// Get the `\tracingmacros` command.
pub fn get_tracingmacros<S>() -> command::VariableFn<S> {
    |_, _, _| -> anyhow::Result<variable::Variable<S>> {
        Ok(variable::Variable::BaseInt(variable::TypedVariable::new(
            |base: &BaseState<S>, _: usize| -> &i32 { &base.tracing_macros },
            |base: &mut BaseState<S>, _: usize| -> &mut i32 { &mut base.tracing_macros },
            0,
        )))
    }
}
