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

/// Start a REPL session using the provided VM.
#[cfg(feature = "repl")]
pub fn start<S: HasComponent<script::Component> + HasComponent<Component>>(
    vm: &mut vm::VM<S>,
    opts: RunOptions,
) {
    use linefeed::{Interface, ReadResult};

    let c = HasComponent::<Component>::component_mut(&mut vm.state);
    c.help = opts.help.into();
    c.quit_requested = false;

    let reader = Interface::new("").unwrap();
    reader.set_prompt(opts.prompt).unwrap();
    script::set_io_writer(vm, std::io::stdout());

    let mut num_commands: Option<usize> = None;
    loop {
        // We detect new commands (via \def say) by seeing if the number of commands
        // have changed. This is incorrect though; in the following case, the
        // commands will not be updated:
        //
        // TeX> {
        // TeX> \def \Apple{A}
        // TeX> } \def \Orange{B}
        //
        // Instead we need to iterate over all commands in the map and
        // check the diff.
        if Some(vm.commands_map.len()) != num_commands {
            let mut names: Vec<String> = vm
                .get_commands_as_map_slow()
                .into_keys()
                .map(|s| format!["{s}"])
                .collect();
            names.sort();
            num_commands = Some(names.len());
            let a = Arc::new(ControlSequenceCompleter { names });
            reader.set_completer(a);
        }

        let ReadResult::Input(input) = reader.read_line().unwrap() else {
            break;
        };
        reader.add_history(input.clone());

        vm.clear_sources();
        vm.push_source("".to_string(), input).unwrap();
        match script::run(vm) {
            Ok(()) => (),
            Err(err) => {
                println!("{err}");
                continue;
            }
        };
        if HasComponent::<Component>::component(&vm.state).quit_requested {
            return;
        }
        // TODO: better new line handling in the REPL
        println!();
    }
}

struct ControlSequenceCompleter {
    names: Vec<String>,
}

#[cfg(feature = "repl")]
impl<Term: linefeed::Terminal> linefeed::Completer<Term> for ControlSequenceCompleter {
    fn complete(
        &self,
        word: &str,
        prompter: &linefeed::Prompter<Term>,
        start: usize,
        _end: usize,
    ) -> Option<Vec<linefeed::Completion>> {
        if !prompter.buffer()[..start].ends_with('\\') {
            return None;
        }
        let mut completions = Vec::new();
        for name in &self.names {
            if name.starts_with(word) {
                completions.push(linefeed::Completion {
                    completion: name.to_string(),
                    display: Some(format!["\\{name}"]),
                    suffix: linefeed::Suffix::Default,
                });
            }
        }
        Some(completions)
    }
}

/// Get the `\exit` command.
///
/// This exits the REPL.
pub fn get_exit<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: token::Token, input: &mut vm::ExecutionInput<S>| -> txl::Result<()> {
            HasComponent::<Component>::component_mut(input.state_mut()).quit_requested = true;
            Err(input.shutdown())
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
                Err(err) => Err(input.fatal_error(error::SimpleTokenError::new(
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
                Err(err) => Err(input.fatal_error(error::SimpleTokenError::new(
                    token,
                    format!["failed to write doc text: {err}"],
                ))),
            }
        },
    )
}
