use clap::Parser;
use colored::Colorize;
use std::fs;
use std::path::PathBuf;
use texlang_core::prelude::*;
use texlang_core::token;
use texlang_stdlib::repl;
use texlang_stdlib::script;
use texlang_stdlib::StdLibState;

/// This is a "portfolio binary" that demonstrates some of the features of the
///   Texcraft project.
/// See the subcommands for things it can do.
///
/// Website: https://texcraft.dev.
#[derive(Parser)]
#[clap(version)]
struct Cli {
    #[clap(subcommand)]
    subcmd: SubCommand,
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
    let result = match args.subcmd {
        SubCommand::Doc(d) => doc(d.name),
        SubCommand::Repl => {
            repl();
            Ok(())
        }
        SubCommand::Run(run_args) => run(run_args.file_path),
    };
    if let Err(err) = result {
        println!["{}", err];
        std::process::exit(1);
    }
}

fn run(mut path: PathBuf) -> Result<(), anyhow::Error> {
    if path.extension().is_none() {
        path.set_extension("tex");
    }
    let source_code = match fs::read_to_string(&path) {
        Ok(source_code) => source_code,
        Err(err) => {
            return Err(anyhow::anyhow!["Failed to open file {:?}: {}", &path, err]);
        }
    };
    let mut env = new_env();
    // The only error possible is input stack size exceeded, which can't be hit.
    let _ = env.push_source(path.to_string_lossy().to_string(), source_code);
    let tokens = script::run(&mut env, true)?;
    let pretty = token::write_tokens(&tokens, env.cs_name_interner());
    println!("{}", pretty);
    Ok(())
}

fn repl() {
    println!("{}\n", REPL_START.trim());
    let mut env = new_env();
    env.set_command("doc", repl::get_doc());
    env.set_command("help", repl::get_help());
    env.set_command("exit", repl::get_exit());
    env.set_command("quit", repl::get_exit());
    env.set_command("q", repl::get_exit());
    repl::run(
        &mut env,
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

fn doc(cs_name: Option<String>) -> Result<(), anyhow::Error> {
    let env = new_env();

    match cs_name {
        None => {
            let mut cs_names = Vec::new();
            let commands = env.get_commands_as_map();
            for cs_name in commands.keys() {
                cs_names.push(cs_name);
            }
            cs_names.sort();
            let mut last_prefix = None;
            for cs_name in cs_names.into_iter() {
                let _cmd = commands.get(cs_name);
                let new_last_prefix = cs_name.chars().next();
                if last_prefix != new_last_prefix {
                    last_prefix = new_last_prefix;
                }
                let doc = "todo".to_string();
                let first_line = doc.split('\n').next().unwrap_or("");
                println!["\\{}  {}", cs_name.bold(), first_line];
            }
            Ok(())
        }
        Some(cs_name) => {
            let cs_name_s = match env.cs_name_interner().get(cs_name.clone()) {
                None => {
                    return Err(anyhow::anyhow!("Unknown command \\{}", cs_name));
                }
                Some(s) => s,
            };
            let doc = match env.base_state.commands_map.get_doc(&cs_name_s) {
                None => {
                    return Err(anyhow::anyhow!("Unknown command \\{}", cs_name));
                }
                Some(d) => d,
            };
            println!["\\{}  {}", cs_name.bold(), doc];
            Ok(())
        }
    }
}

fn new_env() -> runtime::Env<StdLibState> {
    let mut s = StdLibState::new();
    s.set_command("par", script::get_par());
    s.set_command("newline", script::get_newline());
    s.set_command("\\", command::Command::Character(Value::Other('\\')));
    s
}
