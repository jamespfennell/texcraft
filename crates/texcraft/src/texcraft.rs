use clap::Parser;
use colored::Colorize;
use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;
use texlang::traits::*;
use texlang::*;
use texlang_stdlib::job;
use texlang_stdlib::repl;
use texlang_stdlib::script;

/// This is a program that demonstrates some of the features of the
///   Texcraft project.
/// See the subcommands for things it can do.
///
/// Website: `<https://texcraft.dev>`.
#[derive(Parser)]
#[clap(version)]
struct Cli {
    /// Path to a format file
    #[arg(long)]
    format_file: Option<PathBuf>,

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
    let is_repl = matches!(args.sub_command, SubCommand::Repl);
    let mut vm = new_vm(args.format_file, is_repl);
    let result = match args.sub_command {
        SubCommand::Doc(d) => doc(&vm, d.name),
        SubCommand::Repl => {
            repl(&mut vm);
            Ok(())
        }
        SubCommand::Run(run_args) => {
            let result = run(&mut vm, run_args.file_path);
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

fn run(vm: &mut vm::VM<State>, mut path: PathBuf) -> Result<(), Box<error::TracedTexError>> {
    if path.extension().is_none() {
        path.set_extension("tex");
    }
    vm.state.job.set_job_name(&path);
    let source_code = match fs::read_to_string(&path) {
        Ok(source_code) => source_code,
        Err(err) => {
            println!["Failed to open file {:?}: {err}", &path];
            std::process::exit(1);
        }
    };
    // The only error possible is input stack size exceeded, which can't be hit.
    // TODO: the external VM method shouldn't error
    let _ = vm.push_source(path.to_string_lossy().to_string(), source_code);
    script::set_io_writer(vm, std::io::stdout());
    script::run(vm)?;
    Ok(())
}

fn repl(vm: &mut vm::VM<State>) {
    println!("{}\n", REPL_START.trim());
    repl::run::run(
        vm,
        repl::RunOptions {
            prompt: "TeX> ",
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

TeX> \def\hello{Hello, World}

and then running it:

TeX> \hello
Hello, World

TeX supports variables (see the \count and \countdef commands), math (\advance, \multiple, \divide),
and conditional control flow (\if, \else, \fi). You can input external TeX scripts using \input.

Tips

* The terminal includes tab autocompletion for TeX commands. Type \<tab><tab> to see all commands.
* Type \doc \command to read the documentation for a TeX command.

Website: https://texcraft.dev
";

fn doc(vm: &vm::VM<State>, cs_name: Option<String>) -> Result<(), String> {
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
                let command_ref = token::CommandRef::ControlSequence(cs_name_s);
                let doc = match vm.commands_map.get_command_slow(&command_ref) {
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
            let command_ref = token::CommandRef::ControlSequence(cs_name);
            let cmd = match vm.commands_map.get_command_slow(&command_ref) {
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

fn new_vm(format_file_path: Option<PathBuf>, repl: bool) -> Box<vm::VM<State>> {
    let mut m = texlang_stdlib::built_in_commands();
    m.insert("par", script::get_par());
    m.insert("newline", script::get_newline());
    m.insert(
        "\\",
        command::Command::CharacterTokenAlias(token::Value::Other('\\')).into(),
    );
    if repl {
        m.insert("doc", repl::get_doc());
        m.insert("help", repl::get_help());
        m.insert("exit", repl::get_exit());
        m.insert("quit", repl::get_exit());
        m.insert("q", repl::get_exit());
    } else {
        m.insert("dump", job::get_dump());
    }

    m.insert("font", texlang_font::get_font());
    m.insert("nullfont", texlang_font::get_nullfont());

    match format_file_path {
        None => Box::new(vm::VM::<State>::new_with_built_in_commands(m)),
        Some(path) => {
            let format_file = std::fs::read(&path).unwrap();
            if path.extension().map(OsStr::to_str) == Some(Some("json")) {
                let mut deserializer = serde_json::Deserializer::from_slice(&format_file);
                // TODO: shouldn't panic if this fails!
                Box::new(vm::VM::deserialize_with_built_in_commands(&mut deserializer, m).unwrap())
            } else {
                let mut deserializer = rmp_serde::decode::Deserializer::from_read_ref(&format_file);
                Box::new(vm::VM::deserialize_with_built_in_commands(&mut deserializer, m).unwrap())
            }
        }
    }
}

use texlang::types::CatCode;
use texlang_stdlib::*;

#[derive(Default, serde::Serialize, serde::Deserialize)]
pub struct State {
    pub alloc: alloc::Component,
    pub codes_cat_code: codes::Component<CatCode>,
    pub codes_math_code: codes::Component<types::MathCode>,
    pub conditional: conditional::Component,
    pub end_line_char: endlinechar::Component,
    pub error_mode: errormode::Component,
    pub font_repo: texlang_font::NoOpFontRepo<tfm::File>,
    pub fonts: texlang_font::FontComponent,
    pub input: input::Component<16>,
    pub job: job::Component,
    pub prefix: prefix::Component,
    pub registers_i32: registers::Component<i32, 32768>,
    pub registers_token_list: registers::Component<Vec<token::Token>, 256>,
    pub repl: repl::Component,
    pub script: script::Component,
    pub time: time::Component,
    pub tracing_macros: tracingmacros::Component,
}

impl TexlangState for State {
    #[inline]
    fn cat_code(&self, c: char) -> texlang::types::CatCode {
        codes::cat_code(self, c)
    }

    #[inline]
    fn end_line_char(&self) -> Option<char> {
        endlinechar::end_line_char(self)
    }

    #[inline]
    fn post_macro_expansion_hook(
        token: texlang::token::Token,
        input: &vm::ExpansionInput<Self>,
        tex_macro: &texlang::texmacro::Macro,
        arguments: &[&[texlang::token::Token]],
        reversed_expansion: &[texlang::token::Token],
    ) {
        tracingmacros::hook(token, input, tex_macro, arguments, reversed_expansion)
    }

    #[inline]
    fn expansion_override_hook(
        token: texlang::token::Token,
        input: &mut vm::ExpansionInput<Self>,
        tag: Option<texlang::command::Tag>,
    ) -> texlang::prelude::Result<Option<texlang::token::Token>> {
        expansion::noexpand_hook(token, input, tag)
    }

    #[inline]
    fn variable_assignment_scope_hook(
        state: &mut Self,
    ) -> texcraft_stdext::collections::groupingmap::Scope {
        prefix::variable_assignment_scope_hook(state)
    }

    fn recoverable_error_hook(
        &self,
        recoverable_error: texlang::error::TracedTexError,
    ) -> Result<(), Box<dyn error::TexError>> {
        errormode::recoverable_error_hook(self, recoverable_error)
    }
}

implement_has_component![State{
    alloc: alloc::Component,
    codes_cat_code: codes::Component<CatCode>,
    codes_math_code: codes::Component<types::MathCode>,
    conditional: conditional::Component,
    end_line_char: endlinechar::Component,
    error_mode: errormode::Component,
    fonts: texlang_font::FontComponent,
    input: input::Component<16>,
    job: job::Component,
    prefix: prefix::Component,
    registers_i32: registers::Component<i32, 32768>,
    registers_token_list: registers::Component<Vec<token::Token>, 256>,
    repl: repl::Component,
    script: script::Component,
    time: time::Component,
    tracing_macros: tracingmacros::Component,
}];

impl texlang_common::HasLogging for State {}
impl texlang_common::HasFileSystem for State {}
impl texlang_common::HasTerminalIn for State {
    fn terminal_in(&self) -> std::rc::Rc<std::cell::RefCell<dyn texlang_common::TerminalIn>> {
        self.error_mode.terminal_in()
    }
}
impl texlang_stdlib::the::TheCompatible for State {
    fn get_command_ref_for_font(&self, font: types::Font) -> Option<token::CommandRef> {
        texlang_font::FontComponent::get_command_ref_for_font(self, font)
    }
}

impl texlang_font::HasFontRepo for State {
    type FontRepo = texlang_font::NoOpFontRepo<tfm::File>;

    fn font_repo_mut(&mut self) -> &mut Self::FontRepo {
        &mut self.font_repo
    }
}
