use crate::traits::*;
use crate::types;
use crate::vm;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;

pub(crate) fn run_parse_success_test<S: TexlangState + Default, T: Parsable<S> + Debug + Eq>(
    source: &str,
    want: T,
) {
    let mut vm = vm::VM::<S>::new_with_built_in_commands(HashMap::new());
    vm.push_source("".to_string(), source.to_string()).unwrap();
    let input = vm::ExecutionInput::new(&mut vm);
    let got = T::parse(input).unwrap();
    assert_eq!(got, want);
}

pub(crate) fn run_parse_failure_test<S: TexlangState + Default, T: Parsable<S> + Debug>(
    source: &str,
) {
    let mut vm = vm::VM::<S>::new_with_built_in_commands(HashMap::new());
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

pub(crate) fn run_parse_failure_recovery_test<
    S: TexlangState + Default,
    T: Parsable<WrappedState<S>> + Debug + Eq,
>(
    source: &str,
    want: T,
) {
    let mut vm = vm::VM::<WrappedState<S>>::new_with_built_in_commands(HashMap::new());
    vm.state.recover_from_errors = true;
    vm.push_source("".to_string(), source.to_string()).unwrap();
    let input = vm::ExecutionInput::new(&mut vm);
    let got = T::parse(input).unwrap();
    assert_eq!(got, want);
    let got_num_errors = *vm.state.num_errors.borrow();
    assert_eq!(
        got_num_errors, 1,
        "expected 1 TeX error, found {} TeX errors",
        got_num_errors
    );
}

#[derive(Default)]
pub(crate) struct WrappedState<S> {
    inner: S,
    recover_from_errors: bool,
    num_errors: RefCell<usize>,
}

impl<S: TexlangState> TexlangState for WrappedState<S> {
    fn cat_code(&self, c: char) -> types::CatCode {
        self.inner.cat_code(c)
    }
    fn end_line_char(&self) -> Option<char> {
        self.inner.end_line_char()
    }
    fn recoverable_error_hook(
        &self,
        recoverable_error: crate::error::TracedError,
    ) -> Result<(), Box<dyn crate::error::TexError>> {
        let mut num_errors = self.num_errors.borrow_mut();
        *num_errors = *num_errors + 1;
        if self.recover_from_errors {
            Ok(())
        } else {
            Err(recoverable_error.error)
        }
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
    ( $parsable_type: ty, $state: ty, $( ($name: ident, $input: expr $( , $want: expr )? ), )+) => {
        $(
        mod $name {
            use super::*;
            #[test]
            fn recovery_disabled() {
                let input = $input;
                run_parse_failure_test::<$state, $parsable_type>(&input);
            }
            #[test]
            fn recovery_enabled() {
                let source = $input;
                #[allow(unused_mut,unused_assignments)]
                let mut want: $parsable_type = Default::default();
                $(
                    want = $want;
                )?
                run_parse_failure_recovery_test::<$state, $parsable_type>(&source, want);
            }
        }
        )+
    };
}

pub(crate) use parse_failure_tests;
