//! Support for running TeX REPLs

use super::script;
use linefeed::{Interface, ReadResult};
use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;
use texlang_core::traits::*;
use texlang_core::*;

pub struct RunOptions<'a> {
    pub prompt: &'a str,
    pub help: &'a str,
}

pub fn run<S: HasComponent<script::Component>>(vm: &mut vm::VM<S>, opts: RunOptions) {
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
                if let Some(signal) = anyhow::Error::downcast_ref::<Signal>(&err) {
                    match signal {
                        Signal::Exit => {
                            return;
                        }
                        Signal::Help => {
                            println!("{}\n", opts.help.trim());
                            continue;
                        }
                        Signal::Doc(doc) => {
                            println!("{doc}\n");
                            continue;
                        }
                    }
                }
                println!("{err}");
                continue;
            }
        };
        let pretty = token::write_tokens(&tokens, vm.cs_name_interner());
        if !pretty.trim().is_empty() {
            println!("{pretty}\n");
        }

        if vm.base_state.commands_map.len() != num_names {
            let mut names: Vec<String> = vm.get_commands_as_map_slow().into_keys().collect();
            names.sort();
            num_names = names.len();
            let a = Arc::new(ControlSequenceCompleter { names });
            reader.set_completer(a);
        }
    }
}

#[derive(Debug)]
enum Signal {
    Exit,
    Help,
    Doc(String),
}

impl Display for Signal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl std::error::Error for Signal {}

/// Get the `\exit` command.
///
/// This exits the REPL.
pub fn get_exit<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(
        |_: token::Token, _: &mut vm::ExpansionInput<S>| -> anyhow::Result<Vec<token::Token>> {
            Err(Signal::Exit.into())
        },
    )
}

/// Get the `\help` command.
///
/// This prints help text for the REPL.
pub fn get_help<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(
        |_: token::Token, _: &mut vm::ExpansionInput<S>| -> anyhow::Result<Vec<token::Token>> {
            Err(Signal::Help.into())
        },
    )
}

/// Get the `\doc` command.
///
/// This prints the documentation for a TeX command.
pub fn get_doc<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(
        |token: token::Token,
         input: &mut vm::ExpansionInput<S>|
         -> anyhow::Result<Vec<token::Token>> {
            let target = texlang_core::parse::parse_command_target("", token, input.unexpanded())?;
            let cs_name_s = input.vm().cs_name_interner().resolve(target).unwrap();
            let doc = match input.base().commands_map.get_command_slow(&target) {
                None => format!["Unknown command \\{cs_name_s}"],
                Some(cmd) => match cmd.doc() {
                    None => format!["No documentation available for the \\{cs_name_s} command"],
                    Some(doc) => format!["\\{cs_name_s}  {doc}"],
                },
            };
            Err(Signal::Doc(doc).into())
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
