use clap::Parser;

fn main() {
    if let Err(err) = Cli::parse().run() {
        if !err.is_empty() {
            eprintln!("Error: {err}");
        }
        std::process::exit(1);
    }
}

/// Tools for working with DVI files.
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
    #[command(subcommand)]
    command: Command,
}

impl Cli {
    fn run(self) -> Result<(), String> {
        match self.command {
            Command::Inspect(inspect) => inspect.run(),
            Command::Normalize(normalize) => normalize.run(),
        }
    }
}

#[derive(Clone, Debug, clap::Subcommand)]
enum Command {
    /// Print the contents of a DVI file in human-readable format.
    Inspect(Inspect),
    /// Normalize a DVI file.
    Normalize(Normalize),
}

#[derive(Clone, Debug, Parser)]
struct Inspect {
    /// Path to the DVI file.
    path: std::path::PathBuf,
    /*
    /// Pages of the DVI file to output (TODO)
    #[arg(short = 'p', long, default_value = "-")]
    pages: Option<String>, // byte_offset
                           // op index
                           // opcodes
                           // max ops
     */
}

impl Inspect {
    fn run(self) -> Result<(), String> {
        let b = match std::fs::read(&self.path) {
            Ok(b) => b,
            Err(err) => return Err(format!("failed to read `{}`: {}", self.path.display(), err)),
        };
        let mut result = Ok(());
        for op in dvi::Deserializer::new(&b, &mut result) {
            println!("{:?}", op);
        }
        result.map_err(|err| format!("{}", err))
    }
}

#[derive(Clone, Debug, Parser)]
struct Normalize {
    /// Path to the DVI file to normalize.
    input: std::path::PathBuf,

    /// Path to write the normalized DVI file.
    output: std::path::PathBuf,
}

impl Normalize {
    fn run(self) -> Result<(), String> {
        let b = match std::fs::read(&self.input) {
            Ok(b) => b,
            Err(err) => {
                return Err(format!(
                    "failed to read `{}`: {}",
                    self.input.display(),
                    err
                ))
            }
        };
        let mut result = Ok(());
        let mut i1 = dvi::Deserializer::new(&b, &mut result);
        let i2 = dvi::transforms::VarRemover::new(&mut i1);
        let b = dvi::serialize(i2);
        result.map_err(|err| format!("{}", err))?;
        std::fs::write(&self.output, b)
            .map_err(|err| format!("failed to write `{}`: {}", self.output.display(), err))
    }
}
