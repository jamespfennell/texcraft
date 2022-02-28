//! Utilities for writing unit tests

use crate::execwhitespace;
use anyhow::Result;
use texlang_core::runtime::implement_has_component;
use texlang_core::runtime::Env;
use texlang_core::runtime::HasComponent;
use texlang_core::token;
use texlang_core::token::catcode;

#[derive(Default)]
pub struct State {
    exec: execwhitespace::Component,
}

implement_has_component![State, execwhitespace::Component, exec];

#[macro_export]
macro_rules! expansion_test {
    ($name: ident, $lhs: expr, $rhs: expr) => {
        expansion_test![$name, $lhs, $rhs, setup_expansion_test];
    };
    ($name: ident, $lhs: expr, $rhs: expr, $setup_fn: ident) => {
        #[test]
        fn $name() {
            use ::texlang_core::token::Value::ControlSequence;

            let (output_1, env_1) = crate::testutil::run($setup_fn, $lhs.to_string());
            let (output_2, env_2) = crate::testutil::run($setup_fn, $rhs.to_string());

            let output_1 = output_1.unwrap();
            let output_2 = output_2.unwrap();

            let equal = match output_1.len() == output_2.len() {
                false => false,
                true => {
                    let mut result = true;
                    for (token_1, token_2) in output_1.iter().zip(output_2.iter()) {
                        let token_equal = match (&token_1.value(), &token_2.value()) {
                            (ControlSequence(cs_name_1), ControlSequence(cs_name_2)) => {
                                let name_1 = env_1.cs_name_interner().resolve(cs_name_1).unwrap();
                                let name_2 = env_2.cs_name_interner().resolve(cs_name_2).unwrap();
                                name_1 == name_2
                            }
                            _ => token_1 == token_2,
                        };
                        if !token_equal {
                            result = false;
                            break;
                        }
                    }
                    result
                }
            };

            if !equal {
                println!("Expansion output is different:");
                println!("------[lhs]------");
                println!(
                    "{}",
                    ::texlang_core::token::write_tokens(&output_1, &env_1.cs_name_interner())
                );
                println!("------[rhs]------");
                println!(
                    "{}",
                    ::texlang_core::token::write_tokens(&output_2, &env_2.cs_name_interner())
                );
                println!("-----------------");
                panic!("Expansion test failed");
            }
        }
    };
}

pub use expansion_test;

#[macro_export]
macro_rules! expansion_failure_test {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let (result, env) = crate::testutil::run(setup_expansion_test, $input.to_string());
            if let Ok(output) = result {
                println!("Expansion succeeded:");
                println!(
                    "{}",
                    ::texlang_core::token::write_tokens(&output, &env.cs_name_interner())
                );
                panic!("Expansion failure test did not pass: expansion successful");
            }
        }
    };
}

pub use expansion_failure_test;

pub fn run<S: Default + HasComponent<execwhitespace::Component>>(
    setup_fn: fn(&mut Env<S>),
    source: String,
) -> (Result<Vec<token::Token>>, Env<S>) {
    let mut env_1 = Env::<S>::new(
        catcode::CatCodeMap::new_with_tex_defaults(),
        Default::default(),
    );
    setup_fn(&mut env_1);
    env_1.push_source(source);
    let mut execution_input_1 = ::texlang_core::runtime::ExecutionInput::new(env_1);
    let output_1 = execwhitespace::exec(&mut execution_input_1, false);
    (output_1, execution_input_1.take_env())
}
