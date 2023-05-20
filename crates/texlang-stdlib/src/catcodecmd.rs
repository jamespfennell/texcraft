//! The `\catcode` primitive

use texlang_core::token::catcode;
use texlang_core::*;

pub const CATCODE_DOC: &str = "Get or set a catcode register";

/// Get the `\catcode` command.
pub fn get_catcode<S>() -> command::BuiltIn<S> {
    variable::Command::new_base(
        |state: &vm::BaseState<S>, addr: variable::Address| -> &catcode::CatCode {
            let addr = char::from_u32(addr.0.try_into().unwrap()).unwrap();
            state.cat_code_map.get(&addr)
        },
        |state: &mut vm::BaseState<S>, addr: variable::Address| -> &mut catcode::CatCode {
            let addr = char::from_u32(addr.0.try_into().unwrap()).unwrap();
            state.cat_code_map.get_mut(&addr)
        },
        variable::AddressSpec::Dynamic(
            |token: token::Token,
             input: &mut vm::ExpandedStream<S>|
             -> anyhow::Result<variable::Address> {
                let address: u32 = parse::parse_number(input)?;
                match char::from_u32(address) {
                    None => Err(error::TokenError::new(
                        token,
                        format![
                            "Argument {address} passed to {token} is not a valid UTF-8 codepoint"
                        ],
                    )
                    .cast()),
                    Some(_) => Ok((address as usize).into()),
                }
            },
        ),
    )
    .into()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::testing::*;
    use crate::the;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([("the", the::get_the()), ("catcode", get_catcode())])
    }

    test_suite![
        expansion_equality_tests(
            (catcode_base_case, r"\catcode 48 11 \the\catcode 48", r"11"),
            (
                grouping,
                r"{\catcode 48 11 \the\catcode 48}\the\catcode 48",
                r"1112"
            ),
            (catcode_default, r"\the\catcode 48", r"12"),
        ),
        failure_tests(
            (catcode_value_too_large, r"\catcode 48 16"),
            (catcode_value_is_negative_large, r"\catcode 48 -1"),
            (invalid_utf_8_number, r"\catcode 55296 12"),
        ),
    ];
}
