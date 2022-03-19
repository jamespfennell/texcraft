use clap::Parser;
use colored::Colorize;
use std::fs;
use texlang_core::prelude::*;
use texlang_core::token;
use texlang_stdlib::script;
use texlang_stdlib::StdLibState;

/// Texcraft
#[derive(Parser)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Doc(Doc),
    Exec(Exec),
}

/// Get docs on a TeX command
#[derive(Parser)]
struct Doc {
    /// Name of the control sequence
    name: Option<String>,
}

/// Execute a TeX file as a script
#[derive(Parser)]
struct Exec {
    /// Path to the TeX file to execute
    file_path: String,
}

fn main() {
    let opts: Opts = Opts::parse();
    match opts.subcmd {
        SubCommand::Exec(e) => {
            if let Err(err) = exec(&e.file_path) {
                print!("{}", err);
                std::process::exit(1);
            }
        }
        SubCommand::Doc(d) => {
            doc(d.name).unwrap();
        }
    }
}

fn exec(file_name: &str) -> Result<(), anyhow::Error> {
    let source_code = fs::read_to_string(file_name)?;
    let mut env = init_state();
    env.push_source(file_name.to_string(), source_code)?;
    let tokens = script::run(&mut env, true)?;
    let pretty = token::write_tokens(&tokens, env.cs_name_interner());
    println!("{}", pretty);
    Ok(())
}

fn doc(cs_name: Option<String>) -> Result<(), anyhow::Error> {
    let env = init_state();

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
            return Ok(());
        }
        Some(cs_name) => {
            let cs_name_s = match env.cs_name_interner().get(cs_name.clone()) {
                None => {
                    print!("undefined command");
                    std::process::exit(1);
                }
                Some(s) => s,
            };
            let doc = match env.base_state.commands_map.get_doc(&cs_name_s) {
                None => {
                    print!("undefined command");
                    std::process::exit(1);
                }
                Some(d) => d,
            };
            println!["\\{}  {}", cs_name.bold(), doc];
        }
    }
    Ok(())
}

fn init_state() -> runtime::Env<StdLibState> {
    let mut s = StdLibState::new();
    s.set_command("par", script::get_par());
    s.set_command("newline", script::get_newline());
    s.set_command("\\", command::Command::Character(Value::Other('\\')));
    s
}
