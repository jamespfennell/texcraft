use texlang_core::parse;
use texlang_core::prelude::*;
use texlang_core::variable::{TypedVariable, Variable};

pub const CATCODE_DOC: &str = "Get or set a catcode register";

/// Get the `\catcode` command.
pub fn get_catcode<S>() -> command::Command<S> {
    command::Command::new_variable(catcode_fn, 0)
}

fn catcode_fn<S>(
    _catcode_token: Token,
    input: &mut runtime::ExpansionInput<S>,
    _: u32,
) -> anyhow::Result<Variable<S>> {
    let addr: u32 = parse::parse_number(input)?;
    Ok(Variable::CatCode(TypedVariable::new(
        |state: &runtime::BaseState<S>, addr: u32| -> &CatCode {
            let addr = char::from_u32(addr).unwrap();
            state.cat_code_map.get(&addr)
        },
        |state: &mut runtime::BaseState<S>, addr: u32| -> &mut CatCode {
            let addr = char::from_u32(addr).unwrap();
            state.cat_code_map.get_mut(&addr)
        },
        addr,
    )))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::catcodecmd;
    use crate::testutil::*;
    use crate::the;
    use texlang_core::prelude::*;

    fn setup_expansion_test() -> HashMap<&'static str, command::Command<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("catcode", catcodecmd::get_catcode()),
        ])
    }

    expansion_test![catcode_base_case, r"\catcode 48 11 \the\catcode 48", r"11"];
    expansion_test![
        grouping,
        r"{\catcode 48 11 \the\catcode 48}\the\catcode 48",
        r"1112"
    ];
    expansion_test![catcode_default, r"\the\catcode 48", r"12"];
    expansion_failure_test![catcode_value_too_large, r"\catcode 48 16"];
    expansion_failure_test![catcode_value_is_negative_large, r"\catcode 48 -1"];
}
