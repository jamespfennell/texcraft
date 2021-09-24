//! Commands for tracing TeX execution

use crate::tex::state::Base;
use crate::tex::variable;

/// Get the `\tracingmacros` command.
pub fn get_tracingmacros<S>() -> variable::Variable<S> {
    variable::Variable::BaseInt(variable::TypedVariable::new(
        |base: &Base<S>, _: usize| -> &i32 { &base.tracing_macros },
        |base: &mut Base<S>, _: usize| -> &mut i32 { &mut base.tracing_macros },
        0,
    ))
}
