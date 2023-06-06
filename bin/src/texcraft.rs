use clap::Parser;
use colored::Colorize;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use texlang_core::*;
use texlang_stdlib::repl;
use texlang_stdlib::script;
use texlang_stdlib::StdLibState;

/// This is a program that demonstrates some of the features of the
///   Texcraft project.
/// See the subcommands for things it can do.
///
/// Website: `<https://texcraft.dev>`.
#[derive(Parser)]
#[clap(version)]
struct Cli {
    #[clap(subcommand)]
    sub_command: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Doc(Doc),
    /// Start an interactive TeX terminal
    Repl,
    Run(Run),
}

/// Print documentation for a TeX command
#[derive(Parser)]
struct Doc {
    /// Name of the control sequence
    name: Option<String>,
}

/// Run a TeX file as a script
#[derive(Parser)]
struct Run {
    /// Path to the TeX file to run
    file_path: PathBuf,
}

fn main() {
    let args: Cli = Cli::parse();
    let result = match args.sub_command {
        SubCommand::Doc(d) => doc(d.name),
        SubCommand::Repl => {
            repl();
            Ok(())
        }
        SubCommand::Run(run_args) => {
            let result = run(run_args.file_path);
            if let Err(err) = result {
                println!["{err}"];
                std::process::exit(1);
            }
            Ok(())
        }
    };
    if let Err(err) = result {
        println!["{err}"];
        std::process::exit(1);
    }
}

fn run(mut path: PathBuf) -> Result<(), Box<error::Error>> {
    if path.extension().is_none() {
        path.set_extension("tex");
    }
    let source_code = match fs::read_to_string(&path) {
        Ok(source_code) => source_code,
        Err(err) => {
            println!["Failed to open file {:?}: {err}", &path];
            std::process::exit(1);
        }
    };
    let mut vm = new_vm();
    // The only error possible is input stack size exceeded, which can't be hit.
    // TODO: the external VM method shouldn't error
    let _ = vm.push_source(path.to_string_lossy().to_string(), source_code);
    script::run_and_write(&mut vm, Box::new(std::io::stdout()))?;
    Ok(())
}

fn repl() {
    println!("{}\n", REPL_START.trim());
    let mut vm = new_repl_vm();
    repl::run::run(
        &mut vm,
        repl::RunOptions {
            prompt: "tex> ",
            help: REPL_HELP,
        },
    )
}

const REPL_START: &str = r"
Texcraft interactive TeX terminal
* Type \exit or \quit or \q or ctrl+d to exit. 
* Type \help for help and tips.
";

const REPL_HELP: &str = r"
Texcraft interactive TeX terminal

This is a REPL for running TeX. No better way to get started than defining a macro:

tex> \def\hello{Hello, World}

and then running it:

tex> \hello
Hello, World

TeX supports variables (see the \count and \countdef commands), math (\advance, \multiple, \divide),
and conditional control flow (\if, \else, \fi). You can input external TeX scripts using \input.

Tips

* The terminal includes tab autocompletion for TeX commands. Type \<tab><tab> to see all commands.
* Type \doc \command to read the documentation for a TeX command.

Website: https://texcraft.dev
";

fn doc(cs_name: Option<String>) -> Result<(), String> {
    let vm = new_vm();

    match cs_name {
        None => {
            let mut cs_names = Vec::new();
            let commands = vm.get_commands_as_map_slow();
            for cs_name in commands.keys() {
                cs_names.push(cs_name);
            }
            cs_names.sort();
            let mut last_prefix = None;
            for (i, cs_name) in cs_names.into_iter().enumerate() {
                let new_last_prefix = cs_name.chars().next();
                if last_prefix != new_last_prefix {
                    last_prefix = new_last_prefix;
                    if i != 0 {
                        println!();
                    }
                }
                let cs_name_s = match vm.cs_name_interner().get(cs_name) {
                    None => continue,
                    Some(s) => s,
                };
                let doc = match vm.commands_map.get_command_slow(&cs_name_s) {
                    None => "",
                    Some(cmd) => cmd.doc().unwrap_or(""),
                };
                let first_line = doc.split('\n').next().unwrap_or("");
                println!["\\{}  {}", cs_name.bold(), first_line];
            }
            Ok(())
        }
        Some(name) => {
            let cs_name = match vm.cs_name_interner().get(&name) {
                None => return Err(format!("Unknown command \\{name}")),
                Some(s) => s,
            };
            let cmd = match vm.commands_map.get_command_slow(&cs_name) {
                None => {
                    return Err(format!("Unknown command \\{name}"));
                }
                Some(cmd) => cmd,
            };
            println!["\\{}  {}", name.bold(), cmd.doc().unwrap_or("")];
            Ok(())
        }
    }
}

fn new_vm() -> vm::VM<StdLibState> {
    vm::VM::<StdLibState>::new(initial_built_ins(), Default::default())
}

fn new_repl_vm() -> vm::VM<StdLibState> {
    let mut m = initial_built_ins();
    m.insert("doc", repl::get_doc());
    m.insert("help", repl::get_help());
    m.insert("exit", repl::get_exit());
    m.insert("quit", repl::get_exit());
    m.insert("q", repl::get_exit());
    vm::VM::<StdLibState>::new(m, Default::default())
}

fn initial_built_ins() -> HashMap<&'static str, command::BuiltIn<StdLibState>> {
    let mut m = StdLibState::all_initial_built_ins();
    m.insert("par", script::get_par());
    m.insert("newline", script::get_newline());
    m.insert(
        "\\",
        command::Command::Character(token::Value::Other('\\')).into(),
    );
    m
}
