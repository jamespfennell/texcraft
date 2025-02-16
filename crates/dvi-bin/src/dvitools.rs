use clap::Parser;

fn main() {
    if let Err(err) = Cli::parse().run() {
        if !err.is_empty() {
            eprintln!("Error: {err}");
        }
        std::process::exit(1);
    }
}

#[derive(Debug, clap::Parser)]
#[command(
    name = "DVI tools",
    author = "The Texcraft Project",
    version = "0.1",
    about,
    long_about,
    max_term_width(100)
)]
struct Cli {
    /// Path to the DVI file (.dvi).
    path: std::path::PathBuf,

    /// Pages of the DVI file to output (TODO)
    #[arg(short = 'p', long, default_value = "-")]
    pages: Option<String>, // byte_offset
                           // op index
                           // opcodes
                           // max ops
}

impl Cli {
    fn run(self) -> Result<(), String> {
        let b = match std::fs::read(&self.path) {
            Ok(b) => b,
            Err(err) => return Err(format!("failed to read `{}`: {}", self.path.display(), err)),
        };
        for op_or_err in dvi::OpIter::new(&b) {
            let op = match op_or_err {
                Ok(op) => op,
                Err(err) => return Err(format!("{}", err)),
            };
            println!("{:?}", op);
        }
        Ok(())
    }
}
