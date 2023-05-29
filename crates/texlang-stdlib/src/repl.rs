//! Support for running TeX REPLs

use super::script;
use anyhow::anyhow;
use linefeed::{Interface, ReadResult};
use std::sync::Arc;
use texlang_core::parse::CommandTarget;
use texlang_core::traits::*;
use texlang_core::*;

pub struct RunOptions<'a> {
    pub prompt: &'a str,
    pub help: &'a str,
}

#[derive(Default)]
pub struct Component {
    help: String,
    quit_requested: bool,
}

pub fn run<S: HasComponent<script::Component> + HasComponent<Component>>(
    vm: &mut vm::VM<S>,
    opts: RunOptions,
) {
    let mut c = HasComponent::<Component>::component_mut(&mut vm.custom_state);
    c.help = opts.help.into();
    c.quit_requested = false;
    let reader = Interface::new("").unwrap();

    reader.set_prompt(opts.prompt).unwrap();

    let mut names: Vec<String> = vm.get_commands_as_map_slow().into_keys().collect();
    names.sort();
    let mut num_names = names.len();
    let a = Arc::new(ControlSequenceCompleter { names });
    reader.set_completer(a);

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        reader.add_history(input.clone());

        vm.clear_sources();
        vm.push_source("".to_string(), input).unwrap();
        let tokens = match script::run(vm) {
            Ok(s) => s,
            Err(err) => {
                if HasComponent::<Component>::component(&vm.custom_state).quit_requested {
                    return;
                }
                println!("{err}");
                continue;
            }
        };
        let pretty = token::write_tokens(&tokens, vm.cs_name_interner());
        if !pretty.trim().is_empty() {
            println!("{pretty}\n");
        }

        if vm.commands_map.len() != num_names {
            let mut names: Vec<String> = vm.get_commands_as_map_slow().into_keys().collect();
            names.sort();
            num_names = names.len();
            let a = Arc::new(ControlSequenceCompleter { names });
            reader.set_completer(a);
        }
    }
}

/// Get the `\exit` command.
///
/// This exits the REPL.
pub fn get_exit<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: token::Token, input: &mut vm::ExecutionInput<S>| -> anyhow::Result<()> {
            HasComponent::<Component>::component_mut(input.state_mut()).quit_requested = true;
            Err(anyhow!(""))
        },
    )
}

/// Get the `\help` command.
///
/// This prints help text for the REPL.
pub fn get_help<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: token::Token, input: &mut vm::ExecutionInput<S>| -> anyhow::Result<()> {
            let help = HasComponent::<Component>::component(input.state())
                .help
                .clone();
            writeln![input.vm().std_err.borrow_mut(), "{help}"]?;
            Ok(())
        },
    )
}

/// Get the `\doc` command.
///
/// This prints the documentation for a TeX command.
pub fn get_doc<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: token::Token, input: &mut vm::ExecutionInput<S>| -> anyhow::Result<()> {
            let CommandTarget::ControlSequence(cs_name) = CommandTarget::parse(input)?;
            let cs_name_s = input.vm().cs_name_interner().resolve(cs_name).unwrap();
            let doc = match input.commands_map().get_command_slow(&cs_name) {
                None => format!["Unknown command \\{cs_name_s}"],
                Some(cmd) => match cmd.doc() {
                    None => format!["No documentation available for the \\{cs_name_s} command"],
                    Some(doc) => format!["\\{cs_name_s}  {doc}"],
                },
            };
            writeln![input.vm().std_err.borrow_mut(), "{doc}"]?;
            Ok(())
        },
    )
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
        if prompter.buffer()[..start].chars().rev().next() != Some('\\') {
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
