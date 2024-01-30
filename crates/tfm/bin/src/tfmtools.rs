use clap::Parser;

mod argtypes;
use argtypes::*;

fn main() {
    let args = Cli::parse();
    let result = match args.command {
        Command::Check(check) => check.run(),
        Command::Convert(convert) => convert.run(),
        Command::ConvertBatch(convert_batch) => convert_batch.run(),
        Command::Describe(describe) => describe.run(),
        Command::Fmt(format) => format.run(),
    };
    if let Err(err) = result {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

/// Tools for working with TeX font metric data.
#[derive(Debug, Parser)]
#[command(
    name = "tfmtools",
    author = "The Texcraft Project",
    version = "0.1",
    about,
    long_about,
    max_term_width(100)
)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    /// Indent to use when outputting .pl files.
    #[arg(long, default_value_t = 3)]
    pl_indent: usize,

    /// Specification for how to output characters in .pl files.
    #[arg(short, long, default_value = "default")]
    pl_charcode_format: CharcodeFormat,
}

#[derive(Clone, Debug, clap::Subcommand)]
enum Command {
    /// Check that a .tfm or .pl file is valid.
    Check(Check),
    /// Convert a single .tfm or .pl file.
    Convert(Convert),
    /// Convert a batch of .tfm and .pl files.
    ConvertBatch(ConvertBatch),
    /// Describe a .tfm or .pl file in tabular format.
    Describe(Describe),
    /// Format a .pl file.
    Fmt(Format),
}

#[derive(Clone, Debug, Parser)]
struct Check {
    /// Path to the .tf or .pl file to validate.
    path: TfOrPlPath,
}

impl Check {
    fn run(&self) -> Result<(), String> {
        todo!()
        /*
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
         */
    }
}

#[derive(Clone, Debug, Parser)]
struct Convert {
    /// Path to the .tf or .pl file to convert.
    path: TfOrPlPath,

    /// Output path for the converted file.
    ///
    /// This must have the correct file extension.
    /// If the input file is a .pl file, this must be a .tfm path,
    ///     and vice-versa.
    ///
    /// If not provided, and the input file is a .tfm file,
    ///     the converted .pl file will be printed to standard out.
    /// If the input file is a .pl file,
    ///     the converted TFM file will be written to the same path
    ///     as the input file but with a .tfm file extension.
    /// Thus
    ///
    /// $ tfmtools convert path/to/file.pl
    ///
    /// writes the output to path/to/file.tfm.
    #[arg(short, long)]
    output: Option<TfOrPlPath>,

    /// Specification for how to output characters.
    #[arg(short, long, default_value = "default")]
    charcode_format: argtypes::CharcodeFormat,
}

impl Convert {
    fn run(&self) -> Result<(), String> {
        match &self.path {
            TfOrPlPath::Tf(tf_path) => {
                let (tfm_file, _) = tf_path.read()?;
                let pl_file = tfm::pl::File::from_tfm_file(tfm_file);
                let pl_output = format![
                    "{}",
                    pl_file.display(
                        3,
                        self.charcode_format
                            .to_display_format(&pl_file.header.character_coding_scheme)
                    )
                ];
                match &self.output {
                    None => print!("{pl_output}"),
                    Some(TfOrPlPath::Pl(pl_path)) => {
                        pl_path.write(&pl_output)?;
                    }
                    Some(TfOrPlPath::Tf(_tfm_path)) => todo!(),
                }
                Ok(())
            }
            TfOrPlPath::Pl(_pl_path) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Parser)]
struct ConvertBatch {
    /// Paths of the files to convert.
    ///
    /// Each path must have either a .tfm or a .pl file extension.
    paths: Vec<TfOrPlPath>,

    /// Output directory for the converted files.
    ///
    /// By default, all of the converted files are written to the specified directory.
    /// The name of the converted file is the name of the original file with the extension changed accordingly.
    /// Thus
    ///
    ///     $ tfmtools convert-batch -o output file1.pl dir/file2.tfm
    ///
    /// will create two files, `output/file1.tfm` and `output/file2.pl`.
    ///
    /// Alternatively, the `--preserve-paths` option can be used to preserve directory structure.
    #[arg(short, long)]
    output: std::path::PathBuf,

    /// Preserve the relative paths of input files.
    ///
    /// With this option, the output path for a converted file is the
    ///     output directory specified in `--output-directory` followed by the path to the original file.
    /// Thus
    ///
    ///     $ tfmtools convert-batch --preserve-paths -o output file1.pl dir/file2.tfm
    ///
    /// will create two files, `output/file1.tfm` and `output/dir/file2.pl`.
    ///
    /// Using this option it is possible to write the converted files to the same directory
    ///     as the original files even if the files are in multiple directories.
    /// This can be down with:
    ///
    ///     $ tfmtools convert-batch --preserve-paths -o output . [PATHS]...
    #[arg(short, long)]
    preserve_paths: bool,
    // TODO: allow collisions
}

impl ConvertBatch {
    fn run(&self) -> Result<(), String> {
        todo!()
    }
}

#[derive(Clone, Debug, Parser)]
struct Describe {
    // TODO: we should have a ValidTfFile or ValidPlFile type that reads it in
    path: TfOrPlPath,
}

impl Describe {
    fn run(&self) -> Result<(), String> {
        todo!()
    }
}

#[derive(Clone, Debug, Parser)]
struct Format {
    /// Instead of overwriting the .pl file, write the result to the provided path.
    #[arg(short, long)]
    output: Option<std::path::PathBuf>,

    /// Instead of overwriting the .pl file, print the result to standard out.
    #[arg(short, long)]
    print: bool,

    /// Just check that the .pl file is already formatted correctly, and don't output anything.
    #[arg(short, long)]
    check: bool,
}

impl Format {
    fn run(&self) -> Result<(), String> {
        todo!()
    }
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
