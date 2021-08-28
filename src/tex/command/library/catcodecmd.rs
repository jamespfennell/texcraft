use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::token::catcode::RawCatCode;
use crate::tex::variable;
use crate::tex::variable::TypedVariable;
use crate::tex::variable::Variable;
use std::collections::HashMap;
use std::convert::TryFrom;

const CATCODE_DOC: &str = "Get or set a catcode register";

/// A component holding the current category code values.
pub struct Component {
    // TODO: maybe keying off of u32 is the wrong choice, because these may not be valid unicode characters.
    cat_codes: HashMap<u32, RawCatCode>,
    fallback: RawCatCode,
}

impl Component {
    pub fn new(initial_cat_codes: HashMap<u32, RawCatCode>) -> Component {
        Component {
            cat_codes: initial_cat_codes,
            fallback: RawCatCode::default(),
        }
    }

    pub fn cat_codes_map(&self) -> &HashMap<u32, RawCatCode> {
        &self.cat_codes
    }
}

fn read_catcode_register_fn<S>(state: &Base<S>, addr: usize) -> &RawCatCode {
    let addr = u32::try_from(addr).unwrap();
    state
        .cat_codes
        .cat_codes
        .get(&addr)
        .unwrap_or(&state.cat_codes.fallback)
}

fn write_catcode_register_fn<S>(state: &mut Base<S>, addr: usize) -> &mut RawCatCode {
    let addr = u32::try_from(addr).unwrap();
    state
        .cat_codes
        .cat_codes
        .entry(addr)
        .or_insert(RawCatCode::default())
}

fn catcode_fn<S>(
    _catcode_token: &Token,
    input: &mut command::ExpansionInput<S>,
) -> anyhow::Result<Variable<S>> {
    let addr: u32 = parse::parse_number(input)?;
    Ok(Variable::CatCode(TypedVariable::new(
        read_catcode_register_fn,
        write_catcode_register_fn,
        addr as usize,
    )))
}

/// Get the `\catcode` command.
pub fn get_catcode<S>() -> variable::Command<S> {
    variable::Command::Dynamic(catcode_fn, CATCODE_DOC)
}

#[cfg(test)]
mod tests {
    use crate::tex::command::library::catcodecmd;
    use crate::tex::command::library::the;
    use crate::tex::driver;
    use crate::tex::input;
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
