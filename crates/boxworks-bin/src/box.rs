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
    Fmt(Fmt),
    Tex(Tex),
}

fn main() {
    let args: Cli = Cli::parse();
    let result = match args.sub_command {
        SubCommand::Check(check) => check.run(),
        SubCommand::Fmt(fmt) => fmt.run(),
        SubCommand::Tex(tex) => tex.run(),
    };
    if let Err(err) = result {
        println!["Error: {err}"];
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

/// Format a Box file.
#[derive(Parser)]
struct Fmt {
    /// Path to the Box file.
    path: PathBuf,
}

impl Fmt {
    fn run(self) -> Result<(), String> {
        let source = match fs::read_to_string(&self.path) {
            Ok(source) => source,
            Err(err) => {
                return Err(format!["failed to open file {:?}: {err}", &self.path]);
            }
        };
        match bwl::format(&source) {
            Ok(s) => print!["{s}"],
            Err(errs) => {
                let path = self.path.to_string_lossy();
                let cache: (&str, _) = (&path, ariadne::Source::from(source.clone()));
                for err in &errs {
                    err.ariadne_report(&path).print(cache.clone()).unwrap();
                }
                return Err(format!("Input file had {} errors", errs.len()));
            }
        };
        Ok(())
    }
}

/// Perform operations related to TeX.
#[derive(Parser)]
struct Tex {
    #[clap(subcommand)]
    sub_command: TexSubCommand,

    /// The TeX engine to use (e.g. `tex`, `pdftex`).
    ///
    /// Defaults to `tex`.
    #[clap(short, long)]
    tex_engine: Option<String>,
}

impl Tex {
    fn run(self) -> Result<(), String> {
        match boxworks::tex::new_tex_engine_binary(self.tex_engine.unwrap_or("tex".to_string())) {
            Ok(tex_engine) => match self.sub_command {
                TexSubCommand::Hlists(tex_convert_text) => {
                    tex_convert_text.run(tex_engine.as_ref())
                }
            },
            Err(err) => Err(format!["{err}"]),
        }
    }
}

#[derive(Parser)]
enum TexSubCommand {
    Hlists(TexHlists),
}

/// Print the horizontal lists that TeX builds for some contents.
#[derive(Parser)]
struct TexHlists {
    /// The contents used to build the lists.
    ///
    /// Each element of contents is used to build a separate horizontal list.
    contents: Vec<String>,
}

impl TexHlists {
    fn run(self, tex_engine: &dyn boxworks::tex::TexEngine) -> Result<(), String> {
        let (fonts, hlists) =
            boxworks::tex::build_horizontal_lists(tex_engine, &mut self.contents.iter());
        println!("# fonts:");
        for (font_name, font_number) in fonts.into_iter() {
            println!("# - {font_name}={font_number}");
        }
        for (i, hlist) in hlists.into_iter().enumerate() {
            println!("#");
            println!("# hlist {}", i + 1);
            print!("{}", boxworks_lang::write_horizontal_list(&hlist));
        }
        Ok(())
    }
}
