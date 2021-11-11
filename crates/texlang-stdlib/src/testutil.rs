//! Utilities for writing unit tests

use crate::execwhitespace;

#[derive(Default)]
pub struct TestUtilState<S> {
    pub inner: S,
    exec_component: execwhitespace::Component,
}

impl<S> execwhitespace::HasExec for TestUtilState<S> {
    fn exec(&self) -> &execwhitespace::Component {
        &self.exec_component
    }

    fn exec_mut(&mut self) -> &mut execwhitespace::Component {
        &mut self.exec_component
    }
}

#[macro_export]
macro_rules! expansion_test {
    ($name: ident, $lhs: expr, $rhs: expr) => {
        expansion_test![$name, $lhs, $rhs, setup_expansion_test];
    };
    ($name: ident, $lhs: expr, $rhs: expr, $setup_fn: ident) => {
        #[test]
        fn $name() {
            use crate::execwhitespace::exec;
            use texlang_core::token;
            let mut state_1 = runtime::Env::<TestUtilState<State>>::new(
                CatCodeMap::new_with_tex_defaults(),
                Default::default(),
            );
            $setup_fn(&mut state_1);
            state_1.push_source($lhs.to_string());
            let mut execution_input_1 = runtime::ExecutionInput::new(state_1);
            let output_1 = exec(&mut execution_input_1, false).unwrap();
            let state_1 = execution_input_1.take_env();

            let mut state_2 = runtime::Env::<TestUtilState<State>>::new(
                CatCodeMap::new_with_tex_defaults(),
                Default::default(),
            );
            $setup_fn(&mut state_2);
            state_2.push_source($lhs.to_string());
            let mut execution_input_2 = runtime::ExecutionInput::new(state_2);
            let output_2 = exec(&mut execution_input_2, false).unwrap();
            let state_2 = execution_input_2.take_env();

            let equal = match output_1.len() == output_2.len() {
                false => false,
                true => {
                    let mut result = true;
                    for (token_1, token_2) in output_1.iter().zip(output_2.iter()) {
                        let token_equal = match (&token_1.value(), &token_2.value()) {
                            (ControlSequence(cs_name_1), ControlSequence(cs_name_2)) => {
                                let name_1 = state_1.cs_name_interner().resolve(cs_name_1).unwrap();
                                let name_2 = state_2.cs_name_interner().resolve(cs_name_2).unwrap();
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
                    token::write_tokens(&output_1, &state_1.cs_name_interner())
                );
                println!("------[rhs]------");
                println!(
                    "{}",
                    token::write_tokens(&output_2, &state_2.cs_name_interner())
                );
                println!("-----------------");
                panic!("Expansion test failed");
            }
        }
    };
}

#[macro_export]
macro_rules! expansion_failure_test {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            use crate::execwhitespace::exec;
            use texlang_core::token;
            let mut state = runtime::Env::<TestUtilState<State>>::new(
                CatCodeMap::new_with_tex_defaults(),
                Default::default(),
            );
            setup_expansion_test(&mut state);
            state.push_source($input.to_string());
            let mut execution_input = runtime::ExecutionInput::new(state);
            let result = exec(&mut execution_input, false);
            let state = execution_input.take_env();
            if let Ok(output) = result {
                println!("Expansion succeeded:");
                println!(
                    "{}",
                    token::write_tokens(&output, &state.cs_name_interner())
                );
                panic!("Expansion failure test did not pass: expansion successful");
            }
        }
    };
}

/*
use crate::token::stream;

pub fn length(stream: &mut dyn stream::Stream) -> u64 {
    let mut result = 0;
    while stream.next().unwrap() != None {
        result += 1;
    }
    result
}

 */
