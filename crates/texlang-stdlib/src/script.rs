//! Support for running TeX scripts.
//!
//! This module enables using TeX as a scripting language.
//! TeX files are processed using the usual TeX semantics, but instead
//! of typesetting the result and outputting it to PDF (say), the output is returned as a list of tokens.
//! These can be easily converted to a string using [texlang_core::token::write_tokens].

use texlang_core::prelude::*;

#[derive(Default)]
pub struct Component {
    exec_output: Vec<Token>,
    num_trailing_newlines: usize,
}

/// Get the `\newline` command.
///
/// This adds a newline to the output.
pub fn get_newline<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newline_primitive_fn)
}

fn newline_primitive_fn<S: HasComponent<Component>>(
    t: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let c = input.state_mut().component_mut();
    let newline_token = Token::new_space('\n', t.trace_key());
    c.exec_output.push(newline_token);
    c.num_trailing_newlines += 1;
    Ok(())
}

/// Get the `\par` command.
///
/// The `\par` command adds two newlines to the output.
/// Consecutive `\par` commands are treated as one.
pub fn get_par<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(par_primitive_fn)
}

fn par_primitive_fn<S: HasComponent<Component>>(
    t: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let c = input.state_mut().component_mut();
    if c.exec_output.is_empty() {
        return Ok(());
    }
    let par_token = Token::new_space('\n', t.trace_key());
    match c.num_trailing_newlines {
        0 => {
            c.exec_output.push(par_token);
            c.exec_output.push(par_token);
            c.num_trailing_newlines += 2;
        }
        1 => {
            c.exec_output.push(par_token);
            c.num_trailing_newlines += 1;
        }
        _ => {}
    }
    Ok(())
}

/// Run the Texlang interpreter for the provided VM and return the result as list of tokens.
pub fn run<S: HasComponent<Component>>(
    vm: &mut vm::VM<S>,
    err_for_undefined_cs: bool,
) -> anyhow::Result<Vec<Token>> {
    let undefined_cs_handler = match err_for_undefined_cs {
        true => vm::default_undefined_cs_handler,
        false => handle_character,
    };
    vm::run(vm, handle_character, undefined_cs_handler)?;
    let mut result = Vec::new();
    std::mem::swap(
        &mut result,
        &mut vm.custom_state.component_mut().exec_output,
    );
    Ok(result)
}

fn handle_character<S: HasComponent<Component>>(
    mut token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let c = input.state_mut().component_mut();
    if let Some('\n') = token.char() {
        token = Token::new_space(' ', token.trace_key());
    }
    c.exec_output.push(token);
    c.num_trailing_newlines = 0;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::def;
    use crate::testing::*;
    use texlang_core::token;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("par", get_par()),
            ("def", def::get_def()),
            ("newline", get_newline()),
        ])
    }

    macro_rules! script_tests {
        ( $( ($name: ident, $input: expr, $want: expr) ),* $(,)? ) => {
            $(
            #[test]
            fn $name() {
                let options = vec![TestOption::InitialCommands(initial_commands)];
                let input = $input;
                let (got_tokens, vm) = execute_source_code(&input, &options);
                let got = token::write_tokens(&got_tokens.unwrap(), vm.cs_name_interner());
                let want = $want.to_string();

                if got != want {
                    println!("Output is different:");
                    println!("------[got]-------");
                    println!("{}", got);
                    println!("------[want]------");
                    println!("{}", want);
                    println!("-----------------");
                    panic!("write_tokens test failed");
                }
            }
            )*
        };
    }

    script_tests![
        (char_newline_1, "H\nW", "H W"),
        (newline_1, "H\\newline W", "H\nW"),
        (newline_2, "H\\newline \\newline W", "H\n\nW"),
        (newline_3, "H\\newline \\newline \\newline W", "H\n\n\nW"),
        (par_1, "H\n\n\nW", "H\n\nW"),
        (par_2, "H\n\n\n\n\nW", "H\n\nW"),
    ];
}
