//! Utilities for writing unit tests
//!
//! This module contains utilities (types, helper functions and a Rust macro)
//!     that make it easier to write unit tests for Texlang primitives.
//! It's based on the philosophy that high-equality extensive unit tests
//!     will be written if and only if writing them is easy.
//!
//! In general the main tool used in this module is the [test_suite] Rust macro,
//!     which generates a suite of unit tests for a set of primitives.

use std::collections::HashMap;

use crate::prefix;
use crate::script;
use anyhow::Result;
use texlang_core::command;
use texlang_core::token;
use texlang_core::traits::*;
use texlang_core::variable;
use texlang_core::vm;
use texlang_core::vm::implement_has_component;
use texlang_core::vm::VM;

/// Simple state type for use in unit tests.
///
/// If the primitives under test don't require custom components or
/// other pieces in the state, it is easier to use this type rather than defining a custom one.
#[derive(Default)]
pub struct State {
    prefix: prefix::Component,
    script: script::Component,
    integer: i32,
}

impl TexlangState for State {}

implement_has_component![
    State,
    (prefix::Component, prefix),
    (script::Component, script),
];

impl State {
    pub fn get_integer() -> command::BuiltIn<State> {
        variable::Command::new_singleton(
            |state: &State, _: variable::Index| -> &i32 { &state.integer },
            |state: &mut State, _: variable::Index| -> &mut i32 { &mut state.integer },
        )
        .into()
    }
}

/// Option passed to a test runner.
pub enum TestOption<'a, S> {
    /// The initial commands are the result of invoking the provided static function.
    ///
    /// Overrides previous `InitialCommands` or `InitialCommandsDyn` options.
    InitialCommands(fn() -> HashMap<&'static str, command::BuiltIn<S>>),

    /// The initial commands are the result of invoking the provided closure.
    ///
    /// Overrides previous `InitialCommands` or `InitialCommandsDyn` options.
    InitialCommandsDyn(Box<dyn Fn() -> HashMap<&'static str, command::BuiltIn<S>> + 'a>),

    /// The provided static function is invoked after the VM is created and before execution starts.
    /// This can be used to provide more custom VM initialization.
    ///
    /// Overrides previous `CustomVMInitialization` or `CustomVMInitializationDyn` options.
    CustomVMInitialization(fn(&mut VM<S>)),

    /// The provided closure is invoked after the VM is created and before execution starts.
    /// This can be used to provide more custom VM initialization.
    ///
    /// Overrides previous `CustomVMInitialization` or `CustomVMInitializationDyn` options.
    #[allow(clippy::type_complexity)]
    CustomVMInitializationDyn(Box<dyn Fn(&mut VM<S>) + 'a>),
}

/// Run an expansion equality test.
///
/// The test passes if the two provided input strings expand to the same tokens.
pub fn run_expansion_equality_test<S>(lhs: &str, rhs: &str, options: &[TestOption<S>])
where
    S: Default + HasComponent<script::Component>,
{
    use ::texlang_core::token::Value::ControlSequence;

    let (output_1, vm_1) = crate::testing::execute_source_code(lhs, options);
    let (output_2, vm_2) = crate::testing::execute_source_code(rhs, options);

    let output_1 = output_1.unwrap();
    let output_2 = output_2.unwrap();

    println!("{output_1:?}");
    let equal = match output_1.len() == output_2.len() {
        false => {
            println!(
                "output lengths do not match: {} != {}",
                output_1.len(),
                output_2.len()
            );
            false
        }
        true => {
            let mut equal = true;
            for (token_1, token_2) in output_1.iter().zip(output_2.iter()) {
                let token_equal = match (&token_1.value(), &token_2.value()) {
                    (ControlSequence(cs_name_1), ControlSequence(cs_name_2)) => {
                        let name_1 = vm_1.cs_name_interner().resolve(*cs_name_1).unwrap();
                        let name_2 = vm_2.cs_name_interner().resolve(*cs_name_2).unwrap();
                        name_1 == name_2
                    }
                    _ => token_1 == token_2,
                };
                if !token_equal {
                    equal = false;
                    break;
                }
            }
            equal
        }
    };

    if !equal {
        println!("Expansion output is different:");
        println!("------[lhs]------");
        println!(
            "'{}'",
            ::texlang_core::token::write_tokens(&output_1, vm_1.cs_name_interner())
        );
        println!("------[rhs]------");
        println!(
            "'{}'",
            ::texlang_core::token::write_tokens(&output_2, vm_2.cs_name_interner())
        );
        println!("-----------------");
        panic!("Expansion test failed");
    }
}

/// Run a failure test.
///
/// The test passes if execution of the provided input fails.
pub fn run_failure_test<S>(input: &str, options: &[TestOption<S>])
where
    S: Default + HasComponent<script::Component>,
{
    let (result, vm) = execute_source_code(input, options);
    if let Ok(output) = result {
        println!("Expansion succeeded:");
        println!(
            "{}",
            ::texlang_core::token::write_tokens(&output, vm.cs_name_interner())
        );
        panic!("Expansion failure test did not pass: expansion successful");
    }
}

/// Execute source code in a VM with the provided options.
pub fn execute_source_code<S>(
    source: &str,
    options: &[TestOption<S>],
) -> (Result<Vec<token::Token>>, VM<S>)
where
    S: Default + HasComponent<script::Component>,
{
    let mut initial_commands: &dyn Fn() -> HashMap<&'static str, command::BuiltIn<S>> =
        &HashMap::new;
    let mut custom_vm_initialization: &dyn Fn(&mut VM<S>) = &|_| {};
    for option in options {
        match option {
            TestOption::InitialCommands(f) => initial_commands = f,
            TestOption::InitialCommandsDyn(f) => initial_commands = f,
            TestOption::CustomVMInitialization(f) => custom_vm_initialization = f,
            TestOption::CustomVMInitializationDyn(f) => custom_vm_initialization = f,
        }
    }

    let mut vm = VM::<S>::new((initial_commands)(), Default::default());
    (custom_vm_initialization)(&mut vm);
    vm.push_source("testing.tex".to_string(), source.to_string())
        .unwrap();
    script::set_allow_undefined_command(&mut vm.custom_state, true);
    let output = script::run(&mut vm);
    (output, vm)
}

/// In-memory filesystem for use in unit tests.
///
/// This type mocks out the file system operations in the VM.
/// It provides an in-memory system to which "files" can be added before the test runs.
/// It is designed to help test primitives that interact with the filesystem.
///
/// Given a VM, the file system can be set as follows:
/// ```
/// # type State = ();
/// # use texlang_core::vm;
/// # use texlang_stdlib::testing::*;
/// let mut vm = vm::VM::<State>::new(
///     Default::default(),  // initial commands
///     Default::default(),  // initial state
/// );
/// let mock_file_system_ops: InMemoryFileSystem = Default::default();
/// vm.file_system_ops = Box::new(mock_file_system_ops);
/// ```
///
/// When using the test runners in this module or the [test_suite] macro,
///     assign the file system ops using the
///     [TestOption::CustomVMInitializationDyn] option:
/// ```
/// # type State = ();
/// # use texlang_core::vm;
/// # use texlang_stdlib::testing::*;
/// let options = TestOption::CustomVMInitializationDyn(Box::new(|vm: &mut vm::VM<State>|{
///     let mock_file_system_ops: InMemoryFileSystem = Default::default();
///     vm.file_system_ops = Box::new(mock_file_system_ops);
/// }));
/// ```
#[derive(Default)]
pub struct InMemoryFileSystem {
    files: HashMap<std::path::PathBuf, String>,
}

impl vm::FileSystemOps for InMemoryFileSystem {
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
        match self.files.get(path) {
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "not found",
            )),
            Some(content) => Ok(content.clone()),
        }
    }
}

impl InMemoryFileSystem {
    /// Add a file to the in-memory file system.
    pub fn add_file(&mut self, path: std::path::PathBuf, content: &str) {
        self.files.insert(path, content.to_string());
    }
}

/// Macro to generate a suite of unit tests
///
/// The general use of this macros looks like this:
/// ```
/// # use texlang_stdlib::testing::*;
/// test_suite![
///     state(State),
///     options(TestOptions::InitialCommands(initial_commands)),
///     expansion_equality_tests(
///         (case_1, "lhs_1", "rhs_1"),
///         (case_2, "lhs_2", "rhs_2"),
///     ),
///     failure_tests(
///         (case_3, "input_3"),
///         (case_4, "input_4"),
///     ),
/// ];
/// ```
///
/// The arguments to the macro are:
///
/// - `state(State)`: defines which Rust type to use as the VM state in the tests.
///     This can be omitted, in which case it defaults to the type name `State` in the current scope.
///
/// - `options(option_1, option_2, ..., option_n)`: options to pass to the test runner.
///     This is a list of values of type [TestOption].
///     The options can be omitted, in which case they default to `options(TestOptions::InitialCommands(initial_commands))`.
///     In this case `initial_commands` is a static function that returns a list of built-in primitives
///     to initialize the VM with.
///
/// - `expansion_equality_tests(cases...)`: a list of expansion equality test cases.
///     Each case is of the form (case name, left hand side, right hand side).
///     The data here is fed into the [run_expansion_equality_test] test runner.
///     
/// - `failure_tests(cases...)`: a list of failure test cases.
///     Each case is of the form (case name, input).
///     The data here is fed into the [run_failure_test] test runner.
///
/// Only one `state()` argument may be provided, and if provided it must be in the first position.
/// Only one `options()` argument may be provided, and if provided it must be in the first position
///     or after the `state()` argument.
/// Zero or more of the other arguments may be provided, and in any order.
#[macro_export]
macro_rules! test_suite {
    ( state($state: ty), options $options: tt, expansion_equality_tests ( $( ($name: ident, $lhs: expr, $rhs: expr $(,)? ) ),* $(,)? ) $(,)? ) => (
        $(
            #[test]
            fn $name() {
                let lhs = $lhs;
                let rhs = $rhs;
                let options = vec! $options;
                $crate::testing::run_expansion_equality_test::<$state>(&lhs, &rhs, &options);
            }
        )*
    );
    ( state($state: ty), options $options: tt, expansion_equality_tests $test_body: tt $(,)? ) => (
        compile_error!("Invalid test cases for expansion_equality_tests: must be a list of tuples (name, lhs, rhs)");
    );
    ( state($state: ty), options $options: tt, failure_tests ( $( ($name: ident, $input: expr $(,)? ) ),* $(,)? ) $(,)? ) => (
        $(
            #[test]
            fn $name() {
                let input = $input;
                let options = vec! $options;
                $crate::testing::run_failure_test::<$state>(&input, &options);
            }
        )*
    );
    ( state($state: ty), options $options: tt, $test_kind: ident $test_cases: tt $(,)? ) => (
        compile_error!("Invalid keyword: test_suite! only accepts the following keywords: `state, `options`, `expansion_equality_tests`, `failure_tests`");
    );
    ( state($state: ty), options $options: tt, $( $test_kind: ident $test_cases: tt ),+ $(,)? ) => (
        $(
            test_suite![state($state), options $options, $test_kind $test_cases,];
        )+
    );
    ( options $options: tt, $( $test_kind: ident $test_cases: tt ),+ $(,)? ) => (
        test_suite![state(State), options $options, $( $test_kind $test_cases, )+ ];
    );
    ( $( $test_kind: ident $test_cases: tt ),+ $(,)? ) => (
        test_suite![options (TestOption::InitialCommands(initial_commands)), $( $test_kind $test_cases, )+ ];
    );
}

pub use test_suite;
