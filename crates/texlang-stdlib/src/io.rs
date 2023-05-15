//! Primitives for input and output of files
//!

use std::path;
use texlang_core::parse;
use texlang_core::prelude::*;

/// Get the `\input` expansion primitive.
pub fn get_input<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(input_fn)
}

fn input_fn<S>(
    input_token: Token,
    input: &mut vm::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    let file_location = parse::parse_file_location(input)?;
    let (file_path, source_code) = read_file(input_token, input.vm(), file_location, ".tex")?;
    input.push_source(input_token, file_path, source_code)?;
    Ok(Vec::new())
}

fn read_file<S>(
    t: Token,
    vm: &vm::VM<S>,
    file_location: parse::FileLocation,
    default_extension: &str,
) -> anyhow::Result<(String, String)> {
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
            None => match vm.working_directory() {
                None => {
                    return Err(anyhow::anyhow!(
                      format![  "cannot read from relative path {raw_file_path} because no working directory is set"]
                    ));
                }
                Some(working_directory) => working_directory.join(&raw_file_path),
            },
            Some(area) => {
                return Err(anyhow::anyhow!(format![
                    "cannot read from area {area} as areas are not implemented"
                ]));
            }
        },
    };

    match vm.file_system_ops.read_to_string(&file_path) {
        Ok(s) => Ok((raw_file_path, s)),
        Err(err) => Err(
            error::TokenError::new(t, format!("could not read from {:?}", &file_path))
                .add_note(format!("underlying filesystem error: {err}"))
                .cast(),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::*;
    use std::collections::HashMap;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([("input", get_input())])
    }

    fn custom_vm_initialization(vm: &mut vm::VM<State>) {
        let cwd = vm.working_directory().unwrap();

        let mut file_system_ops: InMemoryFileSystem = Default::default();

        let mut path_1 = cwd.to_path_buf();
        path_1.push("file1.tex");
        file_system_ops.add_file(path_1, "content1\n");

        let mut path_2 = cwd.to_path_buf();
        path_2.push("file2.tex");
        file_system_ops.add_file(path_2, "content2%\n");

        let mut path_3 = cwd.to_path_buf();
        path_3.push("file3.tex");
        file_system_ops.add_file(path_3, r"\input nested/file4");

        let mut path_4 = cwd.to_path_buf();
        path_4.push("nested");
        path_4.push("file4.tex");
        file_system_ops.add_file(path_4, "content4");

        vm.file_system_ops = Box::new(file_system_ops);
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
}
