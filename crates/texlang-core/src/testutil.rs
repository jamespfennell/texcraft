//! Utilities for writing unit tests

#[macro_export]
macro_rules! expansion_test {
    ($name: ident, $lhs: expr, $rhs: expr) => {
        expansion_test![$name, $lhs, $rhs, setup_expansion_test];
    };
    ($name: ident, $lhs: expr, $rhs: expr, $setup_fn: ident) => {
        #[test]
        fn $name() {
            let mut state_1 = Base::<State>::new(CatCodeMap::new_with_tex_defaults(), new_state());
            $setup_fn(&mut state_1);
            let mut execution_input_1 = driver::ExecutionInput::new_with_str(state_1, $lhs);
            let output_1 = driver::exec(&mut execution_input_1, false).unwrap();
            let state_1 = execution_input_1.take_base();

            let mut state_2 = Base::<State>::new(CatCodeMap::new_with_tex_defaults(), new_state());
            $setup_fn(&mut state_2);
            let mut execution_input_2 = driver::ExecutionInput::new_with_str(state_2, $rhs);
            let output_2 = driver::exec(&mut execution_input_2, false).unwrap();
            let state_2 = execution_input_2.take_base();

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
            let mut state = Base::<State>::new(CatCodeMap::new_with_tex_defaults(), new_state());
            setup_expansion_test(&mut state);
            let mut execution_input = driver::ExecutionInput::new_with_str(state, $input);
            let result = driver::exec(&mut execution_input, false);
            let state = execution_input.take_base();
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

use crate::token::stream;

pub fn length(stream: &mut dyn stream::Stream) -> u64 {
    let mut result = 0;
    while stream.next().unwrap() != None {
        result += 1;
    }
    result
}
