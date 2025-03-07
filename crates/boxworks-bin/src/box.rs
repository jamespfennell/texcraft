use boxworks_lang as bwl;
use clap::Parser;
use std::fs;
use std::path::PathBuf;

/// Box: a CLI for Boxworks
#[derive(Parser)]
#[clap(version)]
struct Cli {
    #[clap(subcommand)]
    sub_command: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Check(Check),
    Tex(Tex),
}

fn main() {
    let args: Cli = Cli::parse();
    let result = match args.sub_command {
        SubCommand::Check(c) => c.run(),
        SubCommand::Tex(tex) => match tex.sub_command {
            TexSubCommand::Hlist(tex_convert_text) => tex_convert_text.run(),
        },
    };
    if let Err(err) = result {
        println!["{err}"];
        std::process::exit(1);
    }
}

/// Check a Boxworks file is valid.
#[derive(Parser)]
struct Check {
    /// Path to the Boxworks file.
    path: PathBuf,
}

impl Check {
    fn run(self) -> Result<(), String> {
        let source = match fs::read_to_string(&self.path) {
            Ok(source) => source,
            Err(err) => {
                return Err(format!["failed to open file {:?}: {err}", &self.path]);
            }
        };
        if let Err(errs) = bwl::parse_horizontal_list(&source) {
            let path = self.path.to_string_lossy();
            let cache: (&str, _) = (&path, ariadne::Source::from(source.clone()));
            for err in &errs {
                err.ariadne_report(&path).print(cache.clone()).unwrap();
            }
            return Err(format!("Input file had {} errors", errs.len()));
        }
        Ok(())
    }
}

/// Perform operations related to TeX.
#[derive(Parser)]
struct Tex {
    #[clap(subcommand)]
    sub_command: TexSubCommand,
}

#[derive(Parser)]
enum TexSubCommand {
    Hlist(TexHlist),
}

/// Print the horizontal list that TeX builds for some text.
#[derive(Parser)]
struct TexHlist {
    /// The text used to build the list.
    text: String,
}

impl TexHlist {
    fn run(self) -> Result<(), String> {
        let (fonts, list) = boxworks::tex::build_horizontal_list(&self.text);
        println!("# fonts:");
        for (i, font) in fonts.into_iter().enumerate() {
            println!("# {i}: {font}");
        }
        let s = boxworks_lang::write_horizontal_list(&list);
        print!("{s}");
        Ok(())
    }
}
