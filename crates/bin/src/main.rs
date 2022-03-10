use clap::Parser;
use std::fs;
use texlang_core::prelude::*;
use texlang_core::token;
use texlang_stdlib::execwhitespace;
use texlang_stdlib::StdLibState;

/// Texcraft
#[derive(Parser)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Exec(Exec),
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
                print!("Error: {}", err);
                std::process::exit(1);
            }
        }
    }
}

fn exec(file_name: &str) -> Result<(), anyhow::Error> {
    let source_code = fs::read_to_string(file_name)?;
    let mut env = init_state();
    env.push_source(source_code)?;
    let tokens = execwhitespace::exec(&mut env, true)?;
    let pretty = token::write_tokens(&tokens, env.cs_name_interner());
    print!("{}", pretty);
    Ok(())
}

/*
fn docs(cs_name: &str, optional_file_name: Option<&String>) -> Result<(), anyhow::Error> {
    let s = match optional_file_name {
        None => init_state(),
        Some(file_name) => {
            let mut execution_input = driver::ExecutionInput::new_with_source(
                init_state(),
                Box::new(io::BufReader::new(fs::File::open(file_name)?)),
            );
            driver::exec(&mut execution_input, true)?;
            execution_input.take_base()
        }
    };

    if cs_name == "list" {
        let mut cs_names = Vec::new();
        let commands = s.get_commands_as_map();
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

    /* todo: reenable
    match s.get_command(&token::CsName::from(cs_name)) {
        None => {
            println!("Unknown command \\{}", cs_name);
            process::exit(1);
        }
        Some(command) => {
            println!("{}", command.doc());
        }
    }
     */
    Ok(())
}*/

fn init_state() -> runtime::Env<StdLibState> {
    let mut s = StdLibState::new();
    s.set_command("par", execwhitespace::get_par());
    s.set_command("newline", execwhitespace::get_newline());
    s.set_command("\\", command::Command::Character(Token::new_other('\\', 0)));
    s
}
