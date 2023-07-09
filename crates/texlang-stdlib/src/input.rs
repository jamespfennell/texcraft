//! Primitives for input and output of files
//!

use std::path;
use texlang::parse::{FileLocation, OptionalEquals};
use texlang::token::lexer;
use texlang::traits::*;
use texlang::*;

use crate::conditional::{self, Condition};

/// Get the `\input` expansion primitive.
pub fn get_input<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(input_fn)
}

fn input_fn<S: TexlangState>(
    input_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<command::Error>> {
    let file_location = FileLocation::parse(input)?;
    let (file_path, source_code) = read_file(input_token, input.vm(), file_location, ".tex")?;
    input.push_source(input_token, file_path.into(), source_code)?;
    Ok(Vec::new())
}

/// Get the `\endinput` expansion primitive.
pub fn get_endinput<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(endinput_fn)
}

fn endinput_fn<S: TexlangState>(
    _: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<command::Error>> {
    input.end_current_file();
    Ok(Vec::new())
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component<const N: usize> {
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "serialize_files",
            deserialize_with = "deserialize_files"
        )
    )]
    files: [Option<Box<lexer::Lexer>>; N],
}

impl<const N: usize> Default for Component<N> {
    fn default() -> Self {
        // We construct the array as a vector first because the element type
        // is not clonable and so we can't use the standard array constructor.
        // But note that this isn't inefficient, because the array will
        // simply use the vector's buffer.
        let v: Vec<Option<Box<lexer::Lexer>>> = (0..N).into_iter().map(|_| None).collect();
        Self {
            files: v.try_into().unwrap(),
        }
    }
}

#[cfg(feature = "serde")]
fn serialize_files<S>(input: &[Option<Box<lexer::Lexer>>], serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::Serialize;
    input.serialize(serializer)
}

#[cfg(feature = "serde")]
fn deserialize_files<'de, const N: usize, D>(
    deserializer: D,
) -> Result<[Option<Box<lexer::Lexer>>; N], D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::Deserialize;
    let vec = Vec::<Option<Box<lexer::Lexer>>>::deserialize(deserializer)?;
    // TODO: return an error instead of panicing
    // TODO: can we write a generic helper for serding arrays?
    Ok(vec.try_into().unwrap())
}

/// Get the `\openin` execution primitive.
pub fn get_openin<const N: usize, S: HasComponent<Component<N>>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(openin_fn)
}

fn openin_fn<const N: usize, S: HasComponent<Component<N>>>(
    openin_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<command::Error>> {
    let (u, _, file_location) = <(usize, OptionalEquals, FileLocation)>::parse(input)?;
    let lexer_or = match read_file(openin_token, input.vm(), file_location, ".tex") {
        // If the file fails to open TeX does not error out.
        // Instead, TeX users are expected to test the result with \ifeof.
        Err(_) => None,
        Ok((file_path, source_code)) => {
            let trace_key_range = input.tracer_mut().register_source_code(
                Some(openin_token),
                file_path.into(),
                &source_code,
            );
            Some(Box::new(lexer::Lexer::new(source_code, trace_key_range)))
        }
    };
    match input.state_mut().component_mut().files.get_mut(u) {
        None => todo!(), // TODO: we should just have an Uint<N> parsable type
        Some(file) => {
            *file = lexer_or;
        }
    };
    Ok(())
}

/// Get the `\closein` execution primitive.
pub fn get_closein<const N: usize, S: HasComponent<Component<N>>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(closein_fn)
}

fn closein_fn<const N: usize, S: HasComponent<Component<N>>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<command::Error>> {
    let u = usize::parse(input)?;
    match input.state_mut().component_mut().files.get_mut(u) {
        None => todo!(),
        Some(file) => {
            *file = None;
        }
    };
    Ok(())
}

/// Get the `\ifeof` conditional expansion primitive.
pub fn get_ifeof<const N: usize, S>() -> command::BuiltIn<S>
where
    S: HasComponent<Component<N>> + HasComponent<conditional::Component>,
{
    IsEof::build_if_command()
}

struct IsEof<const N: usize>;

impl<const N: usize, S> conditional::Condition<S> for IsEof<N>
where
    S: HasComponent<Component<N>> + HasComponent<conditional::Component>,
{
    fn evaluate(input: &mut vm::ExpansionInput<S>) -> Result<bool, Box<error::Error>> {
        let u = usize::parse(input)?;
        match HasComponent::<Component<N>>::component(input.state())
            .files
            .get(u)
        {
            None => todo!(),
            Some(file_or) => Ok(file_or.is_none()),
        }
    }
}

fn read_file<S>(
    t: token::Token,
    vm: &vm::VM<S>,
    file_location: parse::FileLocation,
    default_extension: &str,
) -> command::Result<(String, String)> {
    // TODO: return a Path instead of String
    let raw_file_path = format![
        "{}{}",
        &file_location.path,
        match &file_location.extension {
            None => default_extension,
            Some(ext) => ext,
        }
    ];
    let file_path = match path::Path::new(&raw_file_path).is_absolute() {
        true => path::Path::new(&raw_file_path).to_path_buf(),
        false => match file_location.area {
            None => match &vm.working_directory {
                None => {
                    panic!("TODO: handle this error");
                }
                Some(working_directory) => working_directory.join(&raw_file_path),
            },
            Some(_area) => {
                panic!("TODO: handle this error");
            }
        },
    };

    match vm.file_system.read_to_string(&file_path) {
        Ok(s) => Ok((raw_file_path, s)),
        Err(_err) => Err(error::SimpleTokenError::new(
            vm,
            t,
            format!("could not read from {:?}", &file_path),
        )
        .into()), // .add_note(format!("underlying filesystem error: {err}")) TODO
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{def, prefix, script, testing::*};
    use std::collections::HashMap;

    #[derive(Default)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct State {
        conditional: conditional::Component,
        input: Component<16>,
        prefix: prefix::Component,
        script: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![
        State,
        (conditional::Component, conditional),
        (Component<16>, input),
        (prefix::Component, prefix),
        (script::Component, script),
    ];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", def::get_def()),
            ("else", conditional::get_else()),
            ("endinput", get_endinput()),
            ("fi", conditional::get_fi()),
            ("ifeof", get_ifeof()),
            ("openin", get_openin()),
            ("closein", get_closein()),
            ("input", get_input()),
        ])
    }

    fn custom_vm_initialization(vm: &mut vm::VM<State>) {
        let cwd = vm.working_directory.clone().unwrap();

        let mut file_system: InMemoryFileSystem = Default::default();

        let mut path_1 = cwd.to_path_buf();
        path_1.push("file1.tex");
        file_system.add_file(path_1, "content1\n");

        let mut path_2 = cwd.to_path_buf();
        path_2.push("file2.tex");
        file_system.add_file(path_2, "content2%\n");

        let mut path_3 = cwd.to_path_buf();
        path_3.push("file3.tex");
        file_system.add_file(path_3, r"\input nested/file4");

        let mut path_4 = cwd.to_path_buf();
        path_4.push("nested");
        path_4.push("file4.tex");
        file_system.add_file(path_4, "content4");

        vm.file_system = Box::new(file_system);
    }

    test_suite!(
        options(
            TestOption::InitialCommands(initial_commands),
            TestOption::CustomVMInitialization(custom_vm_initialization),
        ),
        expansion_equality_tests(
            (basic_case, r"\input file1 hello", "content1 hello"),
            (input_together, r"\input file2 hello", r"content2hello"),
            (basic_case_with_ext, r"\input file1.tex", r"content1 "),
            (nested, r"\input file3", r"content4"),
        ),
    );

    fn end_input_vm_initialization(vm: &mut vm::VM<State>) {
        let cwd = vm.working_directory.clone().unwrap();

        let mut file_system: InMemoryFileSystem = Default::default();

        let mut path_1 = cwd.to_path_buf();
        path_1.push("file1.tex");
        file_system.add_file(
            path_1,
            "Hello\\def\\Macro{Hola\\endinput Mundo}\\Macro World\n",
        );

        vm.file_system = Box::new(file_system);
    }

    test_suite!(
        options(
            TestOption::InitialCommands(initial_commands),
            TestOption::CustomVMInitialization(end_input_vm_initialization),
        ),
        expansion_equality_tests(
            (end_input_simple, r"Hello\endinput World", "Hello",),
            (
                end_input_in_second_file,
                r"Before\input file1 After",
                "BeforeHelloHolaMundoAfter"
            ),
        ),
    );

    fn read_vm_initialization(vm: &mut vm::VM<State>) {
        let cwd = vm.working_directory.clone().unwrap();

        let mut file_system: InMemoryFileSystem = Default::default();

        let mut path_1 = cwd.to_path_buf();
        path_1.push("file1.tex");
        file_system.add_file(path_1, "Hello World");

        vm.file_system = Box::new(file_system);
    }

    test_suite!(
        options(
            TestOption::InitialCommands(initial_commands),
            TestOption::CustomVMInitialization(read_vm_initialization),
        ),
        expansion_equality_tests(
            (
                ifeof_nothing_open,
                r"\ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                ifeof_non_existant_file,
                r"\openin 0 doesnotexist \ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                ifeof_file_exists,
                r"\openin 0 file1 \ifeof 0 Closed\else Open\fi",
                "Open",
            ),
            (
                ifeof_non_existant_file_2,
                r"\openin 0 file1 \openin 0 doesnotexist \ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                ifeof_file_closed,
                r"\openin 0 file1 \closein 0 \ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
        ),
        serde_tests((
            iseof_file_exists_serde,
            r"\openin 0 file1 ",
            r"\ifeof 0 Closed\else Open\fi"
        ),),
    );
}
