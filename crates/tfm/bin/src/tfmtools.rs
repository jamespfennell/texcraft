use clap::Parser;

#[derive(Debug, Parser)]
enum Command {
    Check(CheckArgs),
}

#[derive(Debug, Parser)]
struct CheckArgs {
    path: std::path::PathBuf,
}

fn main() {
    let args = Command::parse();
    match args {
        Command::Check(args) => check(args),
    }
}

fn check(args: CheckArgs) {
    let source = match std::fs::read_to_string(&args.path) {
        Ok(source) => source,
        Err(err) => {
            println!["Failed to open file {:?}: {err}", &args.path];
            std::process::exit(1);
        }
    };
    let (ast, errors) = tfm::pl::ast::Ast::build(&source);
    for error in errors {
        let s = Source {
            path: args.path.clone(),
            source: ariadne::Source::from(source.clone()),
        };
        error.ariadne_report().eprint(s).unwrap();
    }
    let _ = ast;
}

struct Source {
    path: std::path::PathBuf,
    source: ariadne::Source,
}

impl ariadne::Cache<()> for Source {
    fn fetch(&mut self, _: &()) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.source)
    }

    fn display<'a>(&self, _: &'a ()) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(format!["{}", self.path.display()]))
    }
}
