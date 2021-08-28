use colored::*;
use std::env;
use std::fs;
use std::io;
use std::process;
use texcraft::tex::command::library::catcodecmd;
use texcraft::tex::command::library::conditional;
use texcraft::tex::command::library::def;
use texcraft::tex::command::library::registers;
use texcraft::tex::command::library::the;
use texcraft::tex::command::library::time;
use texcraft::tex::command::library::variableops;
use texcraft::tex::command::library::WholeLibraryState;
use texcraft::tex::driver;
use texcraft::tex::input;
use texcraft::tex::state::Base;
use texcraft::tex::token;
use texcraft::tex::token::catcode;

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
    let mut s = init_state();

    let mut input = input::Unit::new();
    let f = Box::new(io::BufReader::new(fs::File::open(file_name)?));
    input.push_new_source(f);

    let tokens = driver::expand(&mut s, &mut input)?;
    let pretty = token::write_tokens(&tokens, true);
    print!("{}", pretty);
    Ok(())
}

fn docs(cs_name: &String, optional_file_name: Option<&String>) -> Result<(), anyhow::Error> {
    let mut s = init_state();

    if let Some(file_name) = optional_file_name {
        let mut input = input::Unit::new();
        let f = Box::new(io::BufReader::new(fs::File::open(file_name)?));
        input.push_new_source(f);
        driver::expand(&mut s, &mut input)?;
    }

    if cs_name == "list" {
        let primitives = &s.primitives;
        let mut cs_names = Vec::new();
        for name in primitives.keys() {
            cs_names.push(name);
        }
        cs_names.sort();
        let mut last_prefix = None;
        for cs_name in cs_names.iter() {
            let new_last_prefix = cs_name.chars().nth(0);
            if last_prefix != new_last_prefix {
                println![""];
                last_prefix = new_last_prefix;
            }
            let doc = primitives.get(&cs_name).unwrap().doc();
            let first_line = doc.split("\n").nth(0).unwrap_or("");
            println!["\\{}  {}", cs_name.bold(), first_line];
        }
        return Ok(());
    }

    match s.get_command(cs_name) {
        None => {
            println!("Unknown command \\{}", cs_name);
            process::exit(1);
        }
        Some(command) => {
            println!("{}", command.doc());
        }
    }
    Ok(())
}

fn init_state() -> Base<WholeLibraryState> {
    let mut s = Base::<WholeLibraryState>::new(catcode::tex_defaults(), WholeLibraryState::new());
    conditional::add_all_conditionals(&mut s);
    def::add_all_commands(&mut s);
    s.set_command("the", the::get_the());
    s.set_command("count", registers::get_count());
    s.set_command("countdef", registers::get_countdef());
    s.set_command("catcode", catcodecmd::get_catcode());
    s.set_command("advance", variableops::get_advance());
    s.set_command("multiply", variableops::get_multiply());
    s.set_command("divide", variableops::get_divide());
    s.set_command("time", time::get_time());
    s.set_command("day", time::get_day());
    s.set_command("month", time::get_month());
    s.set_command("year", time::get_year());
    s
}
