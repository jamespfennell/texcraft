/*!
Texlang unit testing library

This is a crate for writing unit tests for code that uses Texlang.
It is used extensively in the Texlang standard library,
    so the unit tests there are good examples of what this crate can do.
This crate is designed to be used outside of the Texlang project,
    and work for any Rust code that uses Texlang.

## Basic setup

As is common in all Texlang code,
    each unit test built with this library works with a specific user-defined Texlang state type.
This state type is provided by the unit test writer.
In addition to implementing the [`TexlangState`] trait, this state must also:

1. Include the [`TestingComponent`] type as a component.
    I.e., the state must implement the [`HasComponent<TestingComponent>`](texlang::traits::HasComponent<TestingComponent>) trait.

1. Configure the `recoverable_error_hook` method on the [`TexlangState`]
    trait to invoke [`TestingComponent::recoverable_error_hook`].

1. Implement [`Default`].

If the unit test doesn't require anything else from the state,
    the [`State`] type defined in this library can simply be used.
This type satisfies all the conditions above.

## Test types

The crate offers a few different types of tests.

### Expansion equality tests

Run using [`run_expansion_equality_test`].

These tests verify that two different TeX snippets expand to the same output.
For example, an output equality test can verify that
```tex
\def\HelloWorld{Hola Mundo}\HelloWorld - \HelloWorld
```
and
```tex
Hola Mundo - Hola Mundo
```
produce the same output.

In this example the second input is just a constant, which is usually how these tests are used.
We generally use these tests to verify some non-trivial TeX expression
    (like a `\def` macro definition)
    evaluates to a specific constant output.

These tests do _not_ verify that the state of the VM is the same in both cases.
In fact the state is usually different; for example, in the first snippet above the
    macro `\HelloWorld` will be defined in the state but won't be in the second snippet.

### Failure tests

Run using [`run_failure_test`].

These tests verify that a specific TeX snippet fails to execute.

### Serde tests

Run using [`run_serde_test`].

These tests verify that the Texlang state being used can be
    successfully serialized and deserialized in the middle of executing a TeX snippet.

A serde test accepts two TeX snippets, `A` and `B`.
It first runs the VM for the concatenated snippet `AB` and saves the result.
It then initializes a new VM and performs the following steps:

- Runs the snippet `A`
- Serializes and deserialized the VM, using a specified format.
- Runs the snipped `B` in the deserialized VM.

The test verifies that the result from this is the same as the result from the VM for the concatenated snippet `AB`.

## The test suite macro

All of the test types can be run using the run functions described above (e.g., [`run_failure_test`]).
However the preferred way to write a suite of unit tests is to use the [`test_suite`] macro.
This macro removes a bunch of boilerplate and makes it easy to add new test cases.

See the macro's documentation for instructions on using it.
Also the Texlang standard library uses this macro extensively.
*/

use std::collections::HashMap;

use texlang::traits::*;
use texlang::vm::implement_has_component;
use texlang::vm::VM;
use texlang::*;

/// Texlang component that every unit testing state needs to have.
#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TestingComponent {
    allow_undefined_command: bool,
    recover_from_errors: bool,
    num_recovered_errors: std::cell::RefCell<usize>,
    tokens: Vec<token::Token>,
    integer: i32,
}

impl TestingComponent {
    fn take_tokens(&mut self) -> Vec<token::Token> {
        let mut result = Vec::new();
        std::mem::swap(&mut result, &mut self.tokens);
        result
    }
    /// Recoverable error hook for the testing component.
    ///
    /// States used in unit testing must be configured to use this hook.
    pub fn recoverable_error_hook<S: HasComponent<Self>>(
        vm: &VM<S>,
        recoverable_error: Box<error::Error>,
    ) -> Result<(), Box<error::Error>> {
        let component = vm.state.component();
        if component.recover_from_errors {
            let mut num_recovered_errors = component.num_recovered_errors.borrow_mut();
            *num_recovered_errors += 1;
            Ok(())
        } else {
            Err(recoverable_error)
        }
    }
    /// Returns an integer variable command that references an integer stored in the testing component.
    ///
    /// If you're writing a unit test that needs an integer variable it's easiest to use this
    ///     rather than building your own variable.
    pub fn get_integer<S: HasComponent<TestingComponent>>() -> command::BuiltIn<S> {
        variable::Command::new_singleton(
            |state: &S, _: variable::Index| -> &i32 { &state.component().integer },
            |state: &mut S, _: variable::Index| -> &mut i32 { &mut state.component_mut().integer },
        )
        .into()
    }
}

/// Simple state type for simple unit tests.
///
/// If the primitives under test don't require custom components or
/// other pieces in the state, it is easier to use this type rather than defining a custom one.
#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct State {
    testing: TestingComponent,
}

impl TexlangState for State {
    fn recoverable_error_hook(
        vm: &VM<Self>,
        recoverable_error: Box<error::Error>,
    ) -> Result<(), Box<error::Error>> {
        TestingComponent::recoverable_error_hook(vm, recoverable_error)
    }
}

implement_has_component![State {
    testing: TestingComponent,
}];

/// Option passed to a test runner.
pub enum TestOption<'a, S> {
    /// The built-in commands are the result of invoking the provided static function.
    ///
    /// Overrides previous `InitialCommands` or `InitialCommandsDyn` options.
    BuiltInCommands(fn() -> HashMap<&'static str, command::BuiltIn<S>>),

    /// The built-in commands are the result of invoking the provided closure.
    ///
    /// Overrides previous `InitialCommands` or `InitialCommandsDyn` options.
    BuiltInCommandsDyn(Box<dyn Fn() -> HashMap<&'static str, command::BuiltIn<S>> + 'a>),

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

    /// Whether undefined commands raise an error.
    ///
    /// Overrides previous `AllowUndefinedCommands` options.
    AllowUndefinedCommands(bool),

    /// Whether to recover from errors.
    ///
    /// Overrides previous `RecoverFromErrors` options.
    RecoverFromErrors(bool),
}

/// Run an expansion equality test.
///
/// The test passes if the two provided input strings expand to the same tokens.
pub fn run_expansion_equality_test<S>(
    lhs: &str,
    rhs: &str,
    expect_recoverable_errors: bool,
    options: &[TestOption<S>],
) where
    S: Default + HasComponent<TestingComponent>,
{
    let options = ResolvedOptions::new(options);

    let mut vm_1 = initialize_vm(&options);
    let (output_1, _) = execute_source_code(&mut vm_1, lhs, &options)
        .map_err(|err| {
            println!("{err}");
            err
        })
        .unwrap();

    let mut vm_2 = initialize_vm(&options);
    let (output_2, _) = execute_source_code(&mut vm_2, rhs, &options)
        .map_err(|err| {
            println!("{err}");
            err
        })
        .unwrap();
    compare_output(output_1, &vm_1, output_2, &vm_2);

    let num_recovered_errors = *vm_1.state.component().num_recovered_errors.borrow();
    match (expect_recoverable_errors, num_recovered_errors) {
        (true, 0) => {
            panic!("expected recoverable errors but didn't have any");
        }
        (true, _) | (false, 0) => (),
        (false, i) => {
            panic!("did not expect recoverable errors but had {i} recoverable errors",);
        }
    }
}

fn compare_output<S>(
    mut output_1: Vec<token::Token>,
    vm_1: &vm::VM<S>,
    mut output_2: Vec<token::Token>,
    vm_2: &vm::VM<S>,
) {
    let trim_space = |v: &mut Vec<token::Token>| {
        let last = match v.last() {
            None => return,
            Some(last) => last,
        };
        if last.cat_code() == Some(types::CatCode::Space) {
            v.pop();
        }
    };
    trim_space(&mut output_1);
    trim_space(&mut output_2);

    println!("{output_1:?}");
    println!("{output_2:?}");
    use ::texlang::token::CommandRef::ControlSequence;
    use ::texlang::token::Value::CommandRef;
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
                    (
                        CommandRef(ControlSequence(cs_name_1)),
                        CommandRef(ControlSequence(cs_name_2)),
                    ) => {
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
            ::texlang::token::write_tokens(&output_1, vm_1.cs_name_interner())
        );
        println!("------[rhs]------");
        println!(
            "'{}'",
            ::texlang::token::write_tokens(&output_2, vm_2.cs_name_interner())
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
    S: Default + HasComponent<TestingComponent>,
{
    let options = ResolvedOptions::new(options);

    let mut vm = initialize_vm(&options);
    let result = execute_source_code(&mut vm, input, &options);
    if let Ok((output, _)) = result {
        println!("Expansion succeeded:");
        println!(
            "{}",
            ::texlang::token::write_tokens(&output, vm.cs_name_interner())
        );
        panic!("Expansion failure test did not pass: expansion successful");
    }
}

/// Format to use in a serde test.
pub enum SerdeFormat {
    Json,
    MessagePack,
    BinCode,
}

#[cfg(not(feature = "serde"))]
/// Run a serialization/deserialization test
pub fn run_serde_test<S>(
    input_1: &str,
    input_2: &str,
    options: &[TestOption<S>],
    format: SerdeFormat,
) {
}

#[cfg(feature = "serde")]
/// Run a serialization/deserialization test
pub fn run_serde_test<S>(
    input_1: &str,
    input_2: &str,
    options: &[TestOption<S>],
    format: SerdeFormat,
) where
    S: Default + HasComponent<TestingComponent> + serde::Serialize + serde::de::DeserializeOwned,
{
    let options = ResolvedOptions::new(options);

    let mut vm_1 = initialize_vm(&options);
    let (mut output_1_1, _) = execute_source_code(&mut vm_1, input_1, &options).unwrap();

    let mut vm_1 = match format {
        SerdeFormat::Json => {
            let serialized = serde_json::to_string_pretty(&vm_1).unwrap();
            println!("Serialized VM: {serialized}");
            let mut deserializer = serde_json::Deserializer::from_str(&serialized);
            vm::VM::deserialize_with_built_in_commands(
                &mut deserializer,
                (options.built_in_commands)(),
            )
            .unwrap()
        }
        SerdeFormat::MessagePack => {
            let serialized = rmp_serde::to_vec(&vm_1).unwrap();
            let mut deserializer = rmp_serde::decode::Deserializer::from_read_ref(&serialized);
            vm::VM::deserialize_with_built_in_commands(
                &mut deserializer,
                (options.built_in_commands)(),
            )
            .unwrap()
        }
        SerdeFormat::BinCode => {
            let serialized =
                bincode::serde::encode_to_vec(&vm_1, bincode::config::standard()).unwrap();
            let deserialized: Box<vm::serde::DeserializedVM<S>> =
                bincode::serde::decode_from_slice(&serialized, bincode::config::standard())
                    .unwrap()
                    .0;
            vm::serde::finish_deserialization(deserialized, (options.built_in_commands)())
        }
    };

    let (mut output_1_2, _) = execute_source_code(&mut vm_1, input_2, &options).unwrap();
    output_1_1.append(&mut output_1_2);

    let mut vm_2 = initialize_vm(&options);
    let combined_input = format!["{input_1}{input_2}"];
    let (output_2, _) = execute_source_code(&mut vm_2, &combined_input, &options).unwrap();

    compare_output(output_1_1, &vm_1, output_2, &vm_2)
}

struct ResolvedOptions<'a, S> {
    built_in_commands: &'a dyn Fn() -> HashMap<&'static str, command::BuiltIn<S>>,
    custom_vm_initialization: &'a dyn Fn(&mut VM<S>),
    allow_undefined_commands: bool,
    recover_from_errors: bool,
}

impl<'a, S> ResolvedOptions<'a, S> {
    pub fn new(options: &'a [TestOption<S>]) -> Self {
        let mut resolved = Self {
            built_in_commands: &HashMap::new,
            custom_vm_initialization: &|_| {},
            allow_undefined_commands: false,
            recover_from_errors: false,
        };
        for option in options {
            match option {
                TestOption::BuiltInCommands(f) => resolved.built_in_commands = f,
                TestOption::BuiltInCommandsDyn(f) => resolved.built_in_commands = f,
                TestOption::CustomVMInitialization(f) => resolved.custom_vm_initialization = f,
                TestOption::CustomVMInitializationDyn(f) => resolved.custom_vm_initialization = f,
                TestOption::AllowUndefinedCommands(b) => resolved.allow_undefined_commands = *b,
                TestOption::RecoverFromErrors(b) => resolved.recover_from_errors = *b,
            }
        }
        resolved
    }
}

fn initialize_vm<S: Default>(options: &ResolvedOptions<S>) -> Box<vm::VM<S>> {
    let mut vm = Box::new(VM::<S>::new_with_built_in_commands((options
        .built_in_commands)(
    )));
    (options.custom_vm_initialization)(&mut vm);
    vm
}

/// Execute source code in a VM with the provided options.
fn execute_source_code<S>(
    vm: &mut vm::VM<S>,
    source: &str,
    options: &ResolvedOptions<S>,
) -> Result<(Vec<token::Token>, usize), Box<error::Error>>
where
    S: Default + HasComponent<TestingComponent>,
{
    vm.push_source("testing.tex", source).unwrap();
    {
        let component = vm.state.component_mut();
        component.allow_undefined_command = options.allow_undefined_commands;
        component.recover_from_errors = options.recover_from_errors;
        *component.num_recovered_errors.borrow_mut() = 0;
    }
    vm.run::<Handlers>()?;
    Ok({
        let component = vm.state.component_mut();
        let tokens = component.take_tokens();
        let num_recovered_errors = *component.num_recovered_errors.borrow();
        (tokens, num_recovered_errors)
    })
}

struct Handlers;

impl<S: HasComponent<TestingComponent>> vm::Handlers<S> for Handlers {
    fn character_handler(
        input: &mut vm::ExecutionInput<S>,
        token: token::Token,
        _: char,
    ) -> command::Result<()> {
        input.state_mut().component_mut().tokens.push(token);
        Ok(())
    }

    fn math_character_handler(
        input: &mut vm::ExecutionInput<S>,
        token: token::Token,
        math_character: types::MathCode,
    ) -> Result<(), Box<error::Error>> {
        let s = format!("{math_character:?}");
        for c in s.chars() {
            let token = if c.is_ascii_alphabetic() {
                token::Token::new_letter(c, token.trace_key())
            } else {
                token::Token::new_other(c, token.trace_key())
            };
            input.state_mut().component_mut().tokens.push(token);
        }
        Ok(())
    }

    fn undefined_command_handler(
        input: &mut vm::ExecutionInput<S>,
        token: token::Token,
    ) -> command::Result<()> {
        if input.state().component().allow_undefined_command {
            input.state_mut().component_mut().tokens.push(token);
            Ok(())
        } else {
            Err(error::UndefinedCommandError::new(input.vm(), token).into())
        }
    }

    fn unexpanded_expansion_command(
        input: &mut vm::ExecutionInput<S>,
        token: token::Token,
    ) -> command::Result<()> {
        input.state_mut().component_mut().tokens.push(token);
        Ok(())
    }
}

/// Macro to generate a suite of unit tests
///
/// The general use of this macros looks like this:
/// ```
/// # use texlang_testing::*;
/// test_suite![
///     state(State),
///     options(TestOptions::InitialCommands(built_in_commands)),
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
///     The options can be omitted, in which case they default to `options(TestOptions::InitialCommands(built_in_commands))`.
///     In this case `built_in_commands` is a static function that returns a list of built-in primitives
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
                texlang_testing::run_expansion_equality_test::<$state>(&lhs, &rhs, false, &options);
            }
        )*
    );
    ( state($state: ty), options $options: tt, expansion_equality_tests $test_body: tt $(,)? ) => (
        compile_error!("Invalid test cases for expansion_equality_tests: must be a list of tuples (name, lhs, rhs)");
    );
    ( state($state: ty), options $options: tt, serde_tests ( $( ($name: ident, $lhs: expr, $rhs: expr $(,)? ) ),* $(,)? ) $(,)? ) => (
        $(
            mod $name {
                use super::*;
                #[cfg_attr(not(feature = "serde"), ignore)]
                #[test]
                fn json() {
                    let lhs = $lhs;
                    let rhs = $rhs;
                    let options = vec! $options;
                    texlang_testing::run_serde_test::<$state>(&lhs, &rhs, &options, texlang_testing::SerdeFormat::Json);
                }
                #[cfg_attr(not(feature = "serde"), ignore)]
                #[test]
                fn message_pack() {
                    let lhs = $lhs;
                    let rhs = $rhs;
                    let options = vec! $options;
                    texlang_testing::run_serde_test::<$state>(&lhs, &rhs, &options, texlang_testing::SerdeFormat::MessagePack);
                }
                #[cfg_attr(not(feature = "serde"), ignore)]
                #[test]
                fn bincode() {
                    let lhs = $lhs;
                    let rhs = $rhs;
                    let options = vec! $options;
                    texlang_testing::run_serde_test::<$state>(&lhs, &rhs, &options, texlang_testing::SerdeFormat::BinCode);
                }
            }
        )*
    );
    // TODO: rename unrecoverable_error_test
    ( state($state: ty), options $options: tt, failure_tests ( $( ($name: ident, $input: expr $(,)? ) ),* $(,)? ) $(,)? ) => (
        $(
            #[test]
            fn $name() {
                let input = $input;
                let options = vec! $options;
                texlang_testing::run_failure_test::<$state>(&input, &options);
            }
        )*
    );
    ( state($state: ty), options $options: tt, recoverable_failure_tests ( $( ($name: ident, $lhs: expr, $rhs: expr $(,)? ) ),* $(,)? ) $(,)? ) => (
        $(
            mod $name {
                use super::*;
                #[test]
                fn error_recovery_enabled() {
                    let lhs = $lhs;
                    let rhs = $rhs;
                    let mut options = vec! $options;
                    options.push(TestOption::RecoverFromErrors(true));
                    // TODO: verify a recoverable error was thrown?
                    texlang_testing::run_expansion_equality_test::<$state>(&lhs, &rhs, true, &options);
                }
                #[test]
                fn error_recovery_disabled() {
                    let input = $lhs;
                    let mut options = vec! $options;
                    options.push(TestOption::RecoverFromErrors(false));
                    texlang_testing::run_failure_test::<$state>(&input, &options);
                }
            }
        )*
    );
    ( state($state: ty), options $options: tt, $test_kind: ident $test_cases: tt $(,)? ) => (
        compile_error!("Invalid keyword: test_suite! only accepts the following keywords: `state, `options`, `expansion_equality_tests`, `failure_tests`, `serde_tests`");
    );
    ( state($state: ty), options $options: tt, $( $test_kind: ident $test_cases: tt ),+ $(,)? ) => (
        $(
            texlang_testing::test_suite![state($state), options $options, $test_kind $test_cases,];
        )+
    );
    ( options $options: tt, $( $test_kind: ident $test_cases: tt ),+ $(,)? ) => (
        texlang_testing::test_suite![state(State), options $options, $( $test_kind $test_cases, )+ ];
    );
    ( $( $test_kind: ident $test_cases: tt ),+ $(,)? ) => (
        texlang_testing::test_suite![options (texlang_testing::TestOption::BuiltInCommands(built_in_commands)), $( $test_kind $test_cases, )+ ];
    );
}
