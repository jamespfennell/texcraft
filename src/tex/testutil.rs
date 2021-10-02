//! Utilities for writing unit tests

macro_rules! expansion_test {
    ($name: ident, $lhs: expr, $rhs: expr) => {
        expansion_test![$name, $lhs, $rhs, setup_expansion_test];
    };
    ($name: ident, $lhs: expr, $rhs: expr, $setup_fn: ident) => {
        #[test]
        fn $name() {
            let mut state_1 = Base::<State>::new(catcode::tex_defaults(), new_state());
            $setup_fn(&mut state_1);
            let mut input_1 = input::Unit::new();
            input_1.push_new_str($lhs);
            let output_1 = driver::exec(&mut state_1, &mut input_1, false).unwrap();

            let mut state_2 = Base::<State>::new(catcode::tex_defaults(), new_state());
            $setup_fn(&mut state_2);
            let mut input_2 = input::Unit::new();
            input_2.push_new_str($rhs);
            let output_2 = driver::exec(&mut state_2, &mut input_2, false).unwrap();

            let equal = match output_1.len() == output_2.len() {
                false => false,
                true => {
                    let mut result = true;
                    for (token_1, token_2) in output_1.iter().zip(output_2.iter()) {
                        use crate::tex::token::Value::Character;
                        use crate::tex::token::Value::ControlSequence;
                        let token_equal = match (&token_1.value(), &token_2.value()) {
                            (Character(..), Character(..)) => token_1.value() == token_2.value(),
                            (ControlSequence(c_1, cs_name_1), ControlSequence(c_2, cs_name_2)) => {
                                let name_1 = state_1.cs_names.resolve(cs_name_1);
                                let name_2 = state_2.cs_names.resolve(cs_name_2);
                                c_1 == c_2 && name_1 == name_2
                            }
                            _ => false,
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
                    crate::tex::token::write_tokens(&output_1, &state_1.cs_names)
                );
                println!("------[rhs]------");
                println!(
                    "{}",
                    crate::tex::token::write_tokens(&output_2, &state_2.cs_names)
                );
                println!("-----------------");
                panic!("Expansion test failed");
            }
        }
    };
}

macro_rules! expansion_failure_test {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            let mut state = Base::<State>::new(catcode::tex_defaults(), new_state());
            setup_expansion_test(&mut state);
            let mut input = input::Unit::new();
            input.push_new_str($input);
            let result = driver::exec(&mut state, &mut input, false);
            if let Ok(output) = result {
                println!("Expansion succeeded:");
                println!(
                    "{}",
                    crate::tex::token::write_tokens(&output, &state.cs_names)
                );
                panic!("Expansion failure test did not pass: expansion successful");
            }
        }
    };
}

use crate::tex::token::catcode;
use crate::tex::token::lexer;
use crate::tex::token::stream;
use crate::tex::token::CsNameInterner;
use std::collections::HashMap;
use std::io;

pub fn tokenize(input: &str) -> stream::VecStream {
    let map = catcode::tex_defaults();
    tokenize_with_map(input, &map)
}

pub fn tokenize_with_map(
    input: &str,
    cat_codes: &HashMap<u32, catcode::RawCatCode>,
) -> stream::VecStream {
    let mut interner = CsNameInterner::new();
    let f = Box::new(io::Cursor::new(input.to_string()));
    let mut lexer = lexer::Lexer::new(f, &mut interner);
    let mut result = Vec::new();
    while let Some(t) = lexer.next(cat_codes, &mut &mut interner).unwrap() {
        result.push(t);
    }
    stream::VecStream::new(result)
}

pub fn length(stream: &mut dyn stream::Stream) -> u64 {
    let mut result = 0;
    while stream.next().unwrap() != None {
        result += 1;
    }
    result
}
