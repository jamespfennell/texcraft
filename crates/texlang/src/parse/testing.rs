use crate::traits::*;
use crate::vm;
use std::collections::HashMap;
use std::fmt::Debug;

pub fn run_parse_success_test<S: TexlangState + Default, T: Parsable<S> + Debug + Eq>(
    source: &str,
    want: T,
) {
    let mut vm = vm::VM::<S>::new(HashMap::new());
    vm.push_source("".to_string(), source.to_string()).unwrap();
    let input = vm::ExecutionInput::new(&mut vm);
    let got = T::parse(input).unwrap();
    assert_eq!(got, want);
}

pub fn run_parse_failure_test<S: TexlangState + Default, T: Parsable<S> + Debug>(source: &str) {
    let mut vm = vm::VM::<S>::new(HashMap::new());
    vm.push_source("".to_string(), source.to_string()).unwrap();
    let input = vm::ExecutionInput::new(&mut vm);
    let result = T::parse(input);
    if let Ok(value) = result {
        panic![
            "Successfully parsed a value '{value:?}' of type '{}' from invalid input '{source}'",
            std::any::type_name::<T>()
        ];
    }
}

macro_rules! parse_success_tests {
    ($( ($name: ident, $input: expr, $expected: expr $(,)? ) ),+ $(,)? ) => {
        $(
        #[test]
        fn $name() {
            let source = $input;
            let want = $expected;
            run_parse_success_test::<(), _>(&source, want);
        }
        )+
    };
}

pub(crate) use parse_success_tests;

macro_rules! parse_failure_tests {
    ( $parsable_type: ty, $state: ty, $( ($name: ident, $input: expr), )+) => {
        $(
        #[test]
        fn $name() {
            let input = $input;
            run_parse_failure_test::<$state, $parsable_type>(&input);
        }
        )+
    };
}

pub(crate) use parse_failure_tests;
