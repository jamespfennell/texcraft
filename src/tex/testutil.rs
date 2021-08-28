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
            let output_1 = driver::expand(&mut state_1, &mut input_1).unwrap();

            let mut state_2 = Base::<State>::new(catcode::tex_defaults(), new_state());
            $setup_fn(&mut state_2);
            let mut input_2 = input::Unit::new();
            input_2.push_new_str($rhs);
            let output_2 = driver::expand(&mut state_2, &mut input_2).unwrap();

            if output_1 != output_2 {
                println!("Expansion output is different:");
                println!("------[lhs]------");
                println!("{}", crate::tex::token::write_tokens(&output_1, false));
                println!("------[rhs]------");
                println!("{}", crate::tex::token::write_tokens(&output_2, false));
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
            let result = driver::expand(&mut state, &mut input);
            if let Ok(output) = result {
                println!("Expansion succeeded:");
                println!("{}", crate::tex::token::write_tokens(&output, false));
                panic!("Expansion failure test did not pass: expansion successful");
            }
        }
    };
}

use crate::tex::token::catcode;
use crate::tex::token::lexer;
use crate::tex::token::stream;
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
    let f = Box::new(io::Cursor::new(input.to_string()));
    let mut lexer = lexer::Lexer::new(f);
    let mut result = Vec::new();
    while let Some(t) = lexer.next(cat_codes).unwrap() {
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
