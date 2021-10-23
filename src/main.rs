use colored::*;
use std::env;
use std::fs;
use std::io;
use std::process;
use texcraft::tex::command::library::execwhitespace;
use texcraft::tex::command::library::WholeLibraryState;
use texcraft::tex::driver;
use texcraft::tex::prelude::*;
use texcraft::tex::token;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        None => {
            println!("Need to specify a command: either expand or docs.");
            process::exit(1);
        }
        Some("expand") => match args.get(2) {
            None => {
                println!("The expand command requires an input TeX file as an argument");
                process::exit(1);
            }
            Some(file_name) => {
                let r = expand(file_name);
                if let Some(err) = r.err() {
                    print!("{}", err);
                    process::exit(1);
                }
            }
        },
        Some("docs") => match args.get(2) {
            None => {
                println!("The docs command requires the name of TeX control sequence.");
                process::exit(1);
            }
            Some(cs_name) => {
                let r = docs(cs_name, args.get(3));
                if let Some(err) = r.err() {
                    print!("{}", err);
                    process::exit(1);
                }
            }
        },
        Some(invalid_command) => {
            println!(
                "Invalid command `{}`. Available commands: expand, docs.",
                invalid_command
            );
            process::exit(1);
        }
    }
}

fn expand(file_name: &str) -> Result<(), anyhow::Error> {
    let mut execution_input = driver::ExecutionInput::new_with_source(
        init_state(),
        Box::new(io::BufReader::new(fs::File::open(file_name)?)),
    );
    let tokens = driver::exec(&mut execution_input, true)?;
    let s = execution_input.take_base();
    let pretty = token::write_tokens(&tokens, s.cs_name_interner());
    print!("{}", pretty);
    Ok(())
}

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
}

fn init_state() -> Base<WholeLibraryState> {
    let mut s = WholeLibraryState::new();
    s.set_command("par", execwhitespace::get_par());
    s.set_command("newline", execwhitespace::get_newline());
    s.set_command("\\", command::Command::Character(Token::new_other('\\')));
    s
}
