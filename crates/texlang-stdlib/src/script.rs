//! Support for running TeX scripts.
//!
//! This module enables using TeX as a scripting language.
//! TeX files are processed using the usual TeX semantics, but instead
//! of typesetting the result and outputting it to PDF (say), the output is returned as a list of tokens.
//! These can be easily converted to a string using [texlang::token::write_tokens].

use texlang::traits::*;
use texlang::*;

#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    tokens: Vec<token::Token>,
    #[cfg_attr(feature = "serde", serde(skip))]
    writer: Option<token::Writer<Box<dyn std::io::Write>>>,
    num_trailing_newlines: usize,
    allow_undefined_command: bool,
}

impl Component {
    fn push_token(&mut self, token: token::Token, interner: &token::CsNameInterner) {
        self.tokens.push(token);
        if let Some(writer) = &mut self.writer {
            writer.write(interner, token).unwrap()
        }
    }
}

// TODO: this should just be an argument to the run function
pub fn set_allow_undefined_command<S: HasComponent<Component>>(state: &mut S, value: bool) {
    state.component_mut().allow_undefined_command = value;
}

/// Get the `\newline` command.
///
/// This adds a newline to the output.
pub fn get_newline<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newline_primitive_fn)
}

fn newline_primitive_fn<S: HasComponent<Component>>(
    t: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    let vm::Parts {
        state,
        cs_name_interner,
        ..
    } = input.vm_parts();
    let mut c = state.component_mut();
    let newline_token = token::Token::new_space('\n', t.trace_key());
    c.push_token(newline_token, cs_name_interner);
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
    t: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    let vm::Parts {
        state,
        cs_name_interner,
        ..
    } = input.vm_parts();
    let mut c = state.component_mut();
    let par_token = token::Token::new_space('\n', t.trace_key());
    match c.num_trailing_newlines {
        0 => {
            c.push_token(par_token, cs_name_interner);
            c.push_token(par_token, cs_name_interner);
            c.num_trailing_newlines += 2;
        }
        1 => {
            c.push_token(par_token, cs_name_interner);
            c.num_trailing_newlines += 1;
        }
        _ => {}
    }
    Ok(())
}

/// Run the Texlang interpreter for the provided VM and return the result as list of tokens.
pub fn run<S: HasComponent<Component>>(
    vm: &mut vm::VM<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    vm.run::<Handlers>()?;
    let mut result = Vec::new();
    std::mem::swap(&mut result, &mut vm.state.component_mut().tokens);
    Ok(result)
}

/// Run the Texlang interpreter for the provided VM and write the tokens as strings to the provided writer.
pub fn run_and_write<S: HasComponent<Component>>(
    vm: &mut vm::VM<S>,
    io_writer: Box<dyn std::io::Write>,
) -> Result<(), Box<error::Error>> {
    vm.state.component_mut().writer = Some(token::Writer::new(io_writer));
    vm.run::<Handlers>()
}

struct Handlers;

impl<S: HasComponent<Component>> vm::Handlers<S> for Handlers {
    fn character_handler(
        mut token: token::Token,
        input: &mut vm::ExecutionInput<S>,
    ) -> command::Result<()> {
        let vm::Parts {
            state,
            cs_name_interner,
            ..
        } = input.vm_parts();
        let mut c = state.component_mut();
        if let Some('\n') = token.char() {
            token = token::Token::new_space(' ', token.trace_key());
        }
        c.push_token(token, cs_name_interner);
        c.num_trailing_newlines = 0;
        Ok(())
    }

    fn undefined_command_handler(
        token: token::Token,
        input: &mut vm::ExecutionInput<S>,
    ) -> command::Result<()> {
        if input.state().component().allow_undefined_command {
            Handlers::character_handler(token, input)
        } else {
            Err(error::UndefinedCommandError::new(input.vm(), token).into())
        }
    }

    fn unexpanded_expansion_command(
        token: token::Token,
        input: &mut vm::ExecutionInput<S>,
    ) -> command::Result<()> {
        Handlers::character_handler(token, input)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::def;
    use crate::testing::*;
    use texlang::token;

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
                let options = ResolvedOptions::new(&options);
                let mut vm = initialize_vm(&options);
                let got_tokens = execute_source_code(&mut vm, input, &options);
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
