//! Support for running TeX REPLs

use super::script;
use std::sync::Arc;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;
use texlang_common as common;

pub struct RunOptions<'a> {
    pub prompt: &'a str,
    pub help: &'a str,
}

#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    help: String,
    quit_requested: bool,
}

#[cfg(feature = "repl")]
pub mod run {
    use super::*;
    use linefeed::{Interface, ReadResult};

    pub fn run<S: HasComponent<script::Component> + HasComponent<Component>>(
        vm: &mut vm::VM<S>,
        opts: RunOptions,
    ) {
        let c = HasComponent::<Component>::component_mut(&mut vm.state);
        c.help = opts.help.into();
        c.quit_requested = false;
        let reader = Interface::new("").unwrap();

        reader.set_prompt(opts.prompt).unwrap();

        let mut names: Vec<String> = vm.get_commands_as_map_slow().into_keys().collect();
        names.sort();
        let mut num_names = names.len();
        let a = Arc::new(ControlSequenceCompleter { names });
        reader.set_completer(a);
        script::set_io_writer(vm, std::io::stdout());
        while let ReadResult::Input(input) = reader.read_line().unwrap() {
            reader.add_history(input.clone());

            vm.clear_sources();
            vm.push_source("".to_string(), input).unwrap();
            match script::run(vm) {
                Ok(()) => (),
                Err(err) => {
                    if HasComponent::<Component>::component(&vm.state).quit_requested {
                        return;
                    }
                    println!("{err}");
                    continue;
                }
            };
            // TODO: better new line handling in the REPL
            println!();
            if vm.commands_map.len() != num_names {
                let mut names: Vec<String> = vm.get_commands_as_map_slow().into_keys().collect();
                names.sort();
                num_names = names.len();
                let a = Arc::new(ControlSequenceCompleter { names });
                reader.set_completer(a);
            }
        }
    }

    struct ControlSequenceCompleter {
        names: Vec<String>,
    }

    impl<Term: linefeed::Terminal> linefeed::Completer<Term> for ControlSequenceCompleter {
        fn complete(
            &self,
            word: &str,
            prompter: &linefeed::Prompter<Term>,
            start: usize,
            _end: usize,
        ) -> Option<Vec<linefeed::Completion>> {
            if prompter.buffer()[..start].ends_with('\\') {
                return None;
            }
            let mut completions = Vec::new();
            for name in &self.names {
                if name.starts_with(word) {
                    completions.push(linefeed::Completion {
                        completion: name.clone(),
                        display: None,
                        suffix: linefeed::Suffix::Default,
                    });
                }
            }
            Some(completions)
        }
    }
}

/// Get the `\exit` command.
///
/// This exits the REPL.
pub fn get_exit<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |t: token::Token, input: &mut vm::ExecutionInput<S>| -> txl::Result<()> {
            HasComponent::<Component>::component_mut(input.state_mut()).quit_requested = true;
            // todo: mechanism to exit without erroring
            // something like input.shutdown_vm()
            Err(input.vm().fatal_error(error::SimpleTokenError::new(
                t,
                "quitting Texcraft REPL. This error should never be seen!",
            )))
        },
    )
}

/// Get the `\help` command.
///
/// This prints help text for the REPL.
pub fn get_help<S: HasComponent<Component> + common::HasLogging>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |token: token::Token, input: &mut vm::ExecutionInput<S>| -> txl::Result<()> {
            let help = HasComponent::<Component>::component(input.state())
                .help
                .clone();
            match writeln![input.state().terminal_out().borrow_mut(), "{help}"] {
                Ok(_) => Ok(()),
                Err(err) => Err(input.vm().fatal_error(error::SimpleTokenError::new(
                    token,
                    format!["failed to write help text: {err}"],
                ))),
            }
        },
    )
}

/// Get the `\doc` command.
///
/// This prints the documentation for a TeX command.
pub fn get_doc<S: TexlangState + common::HasLogging>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |token: token::Token, input: &mut vm::ExecutionInput<S>| -> txl::Result<()> {
            let Some(cmd_ref) = Option::<token::CommandRef>::parse(input)? else {
                return Ok(());
            };
            let name = cmd_ref.to_string(input.vm().cs_name_interner());
            let doc = match input.commands_map().get_command_slow(&cmd_ref) {
                None => format!["Unknown command {name}"],
                Some(cmd) => match cmd.doc() {
                    None => format!["No documentation available for the {name} command"],
                    Some(doc) => format!["{name}  {doc}"],
                },
            };
            match writeln![input.state().terminal_out().borrow_mut(), "{doc}"] {
                Ok(_) => Ok(()),
                Err(err) => Err(input.vm().fatal_error(error::SimpleTokenError::new(
                    token,
                    format!["failed to write doc text: {err}"],
                ))),
            }
        },
    )
}
