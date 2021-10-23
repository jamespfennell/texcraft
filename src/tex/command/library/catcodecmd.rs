use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::token::catcode::CatCode;
use crate::tex::variable::TypedVariable;
use crate::tex::variable::Variable;

use std::convert::TryFrom;

pub const CATCODE_DOC: &str = "Get or set a catcode register";

/// Get the `\catcode` command.
pub fn get_catcode<S>() -> command::VariableFn<S> {
    catcode_fn
}

fn catcode_fn<S>(
    _catcode_token: Token,
    input: &mut command::ExpandedInput<S>,
    _: usize,
) -> anyhow::Result<Variable<S>> {
    let addr: u32 = parse::parse_number(input)?;
    Ok(Variable::CatCode(TypedVariable::new(
        |state: &Base<S>, addr: usize| -> &CatCode {
            let addr = u32::try_from(addr).unwrap();
            state.cat_codes().get(&addr).unwrap_or(&CatCode::Other)
        },
        |state: &mut Base<S>, addr: usize| -> &mut CatCode {
            let addr = u32::try_from(addr).unwrap();
            state
                .cat_codes_mut()
                .entry(addr)
                .or_insert_with(|| CatCode::Other)
        },
        addr as usize,
    )))
}

#[cfg(test)]
mod tests {
    use crate::tex::command::library::catcodecmd;
    use crate::tex::command::library::the;
    use crate::tex::driver;
    use crate::tex::state::Base;
    use crate::tex::token::catcode;

    struct State;
    fn new_state() -> State {
        State {}
    }

    fn setup_expansion_test(s: &mut Base<State>) {
        s.set_command("the", the::get_the());
        s.set_command("catcode", catcodecmd::get_catcode());
    }

    expansion_test![catcode_base_case, r"\catcode 48 11 \the\catcode 48", r"11"];
    expansion_test![catcode_default, r"\the\catcode 48", r"12"];
    expansion_failure_test![catcode_value_too_large, r"\catcode 48 16"];
    expansion_failure_test![catcode_value_is_negative_large, r"\catcode 48 -1"];
}
