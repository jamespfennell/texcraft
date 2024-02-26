use std::collections::HashSet;

use clap::Parser;

mod common;
use common::*;

fn main() {
    if let Err(err) = Cli::parse().run() {
        if !err.is_empty() {
            eprintln!("{err}");
        }
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

impl Cli {
    fn run(self) -> Result<(), String> {
        match self.command {
            Command::Check(check) => check.run(),
            Command::Convert(convert) => convert.run(),
            Command::ConvertBatch(convert_batch) => convert_batch.run(),
            Command::Debug(debug) => debug.run(),
            Command::Fmt(format) => format.run(),
            Command::Undebug(undebug) => undebug.run(),
        }
    }
}

#[derive(Clone, Debug, clap::Subcommand)]
enum Command {
    /// Check that a .tfm or .pl file is valid.
    Check(Check),
    /// Convert a single .tfm or .pl file.
    Convert(Convert),
    /// Convert a batch of .tfm and .pl files.
    ConvertBatch(ConvertBatch),
    /// Print debugging information about a .tfm file.
    ///
    /// This subcommand is used to debug the contents of a .tfm file.
    /// Unlike .pl files - which are regular ASCII, human-readable,
    ///   and can thus be debugged by opening them in a text editor -
    ///   .tfm are binary files and can't be easily debugged.
    ///
    /// Invoking the subcommand with no arguments will print all
    ///   sections of the .tfm file in both "raw" and "parsed" formats:
    ///
    ///   $ tfmtools debug path/to/file.tfm
    ///
    /// The raw format prints the bytes of the .tfm file,
    ///   grouped by tfm section and with 1 word (4 bytes) per line.
    /// (In general .tfm files are word-oriented rather than byte-oriented.)
    /// The parsed format is a debug output of the fully parsed .tfm
    ///   data structure, again printed per-section.
    /// To print only one of the forms pass `--raw` or `--parsed`.
    ///
    /// As alluded to above, .tfm files have sections.
    /// There are 10 sections in a .tfm file, each containing data of a specific type.
    /// To just print one or more of the sections, use the `-s` or `--section` flag:
    ///
    ///     $ tfmtools debug path/to/file.tfm --section widths --section heights
    Debug(Debug),
    /// Format a .pl file.
    Fmt(Format),
    /// Create a .tfm file from the (potentially modified) output of `tfmtools debug`
    ///
    /// The `tfmtools debug` command outputs a plain text representation of the binary
    ///     data in a .tfm file.
    /// This command reconstructs .tfm binary files from this plain text output.
    ///
    /// There are at least two use cases for this command:
    ///
    /// 1.
    ///     If you have a corrupted .tfm file and know enough about the format to fix it,
    ///     you can run `tfmtools debug`,
    ///     manually fix the bytes in the text output,
    ///     and then create a hopefully valid .tfm from the modified output using `tfmtools undebug`.
    ///
    /// 2.
    ///     If you're writing code that reads .tfm files (potentially using Texcraft's tfm crate)
    ///     you can use this command to create invalid or otherwise unusual .tfm files
    ///     for testing.
    ///
    /// If you're modifying the debug output it may be useful to some of the parsing rules:
    ///
    /// 1.
    ///     You can omit .tfm sections.
    ///     In the outputted .tfm file, all unspecified sections will be output as empty
    ///     sections with the exception of the sub file sizes section, which is calculated
    ///     correctly.
    ///
    /// 2.
    ///     Only data within binary data sections is parsed.
    ///     Text in other sections is completely ignored.
    ///
    /// 3.
    ///     Within binary data sections, all lines beginning with // are ignored.
    ///
    /// 4.
    ///     Within binary data sections, all data before the first | character is ignored.
    Undebug(Undebug),
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
    /// Path to the .tfm or .pl file to convert.
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
    charcode_format: common::CharcodeFormat,
}

impl Convert {
    fn run(&self) -> Result<(), String> {
        match &self.path {
            TfOrPlPath::Tf(tf_path) => {
                let (_, tfm_file, warnings) = tf_path.read()?;
                let pl_file = tfm::pl::File::from_tfm_file(tfm_file);
                let pl_output = format![
                    "{}",
                    pl_file.display(
                        3,
                        self.charcode_format
                            .to_display_format(&pl_file.header.character_coding_scheme)
                    )
                ];
                let tfm_modified = warnings
                    .iter()
                    .map(tfm::format::Warning::tfm_file_modified)
                    .any(|t| t);
                let suffix = if tfm_modified {
                    "(COMMENT THE TFM FILE WAS BAD, SO THE DATA HAS BEEN CHANGED!)\n"
                } else {
                    ""
                };
                match &self.output {
                    None => print!("{pl_output}{suffix}"),
                    Some(TfOrPlPath::Pl(pl_path)) => {
                        pl_path.write(&pl_output)?;
                    }
                    Some(TfOrPlPath::Tf(_tfm_path)) => todo!(),
                }
                Ok(())
            }
            TfOrPlPath::Pl(pl_path) => {
                let pl_file = pl_path.read()?;
                let tfm_file = tfm::format::File::from_pl_file(&pl_file);
                let tfm_bytes = tfm_file.serialize();
                let tfm_path: TfPath = match &self.output {
                    Some(TfOrPlPath::Pl(_pl_path)) => todo!(),
                    Some(TfOrPlPath::Tf(tfm_path)) => tfm_path.clone(),
                    None => {
                        let mut tfm_path: std::path::PathBuf = pl_path.0.clone();
                        tfm_path.set_extension("tfm");
                        TfPath(tfm_path)
                    }
                };
                tfm_path.write(&tfm_bytes)?;
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, Parser)]
struct ConvertBatch {
    /// Paths of the files to convert.
    ///
    /// Each path must have either a .tfm or a .pl file extension.
    paths: Vec<TfOrPlPath>,
    /* TODO
    /// Output directory for the converted files.
    ///
    /// (At least) three ways to do it:
    ///
    /// - Relative to each original file. Defaults to being next to each file but can be customized
    /// - Relative to the current working directory, with the directory structure preserved
    /// - Relative to the current working directory, but all flattened (--flatten)
    ///
    /// By default, all of the converted files are written to this directory.
    /// The name of the converted file is the name of the original file with the extension changed accordingly.
    /// Thus
    ///
    ///     $ tfmtools convert-batch -o output file1.pl dir/file2.tfm
    ///
    /// will create two files, `output/file1.tfm` and `output/file2.pl`.
    ///
    /// Alternatively, the `--preserve-paths` option can be used to preserve directory structure.
    #[arg(short, long)]
    output_directory: std::path::PathBuf,

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
     */
}

impl ConvertBatch {
    fn run(&self) -> Result<(), String> {
        todo!()
    }
}

#[derive(Clone, Debug, Parser)]
struct Debug {
    /// Path to the .tfm to debug.
    path: TfPath,

    /// Sections of the .tfm file to display.
    ///
    /// If no sections are provided, all sections will be displayed.
    #[arg(short, long, value_enum)]
    section: Vec<Section>,

    /// Only display raw data.
    #[arg(long)]
    raw: bool,

    /// Only display parsed data.
    #[arg(long)]
    parsed: bool,

    /// Omit the .tfm path in the output.
    ///
    /// This is useful if you're using the output to diff two .tfm files.
    #[arg(long)]
    omit_tfm_path: bool,
}

#[derive(Clone, Debug)]
struct Section(tfm::format::Section);

impl Section {
    const ALL_VARIANTS: [Section; 11] = [
        Section(tfm::format::Section::SubFileSizes),
        Section(tfm::format::Section::Header),
        Section(tfm::format::Section::CharInfos),
        Section(tfm::format::Section::Widths),
        Section(tfm::format::Section::Heights),
        Section(tfm::format::Section::Depths),
        Section(tfm::format::Section::ItalicCorrections),
        Section(tfm::format::Section::LigKern),
        Section(tfm::format::Section::Kerns),
        Section(tfm::format::Section::ExtensibleRecipes),
        Section(tfm::format::Section::Params),
    ];
}

impl clap::ValueEnum for Section {
    fn value_variants<'a>() -> &'a [Self] {
        &Section::ALL_VARIANTS
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(clap::builder::PossibleValue::new(
            tfm::format::Section::NAMES[self.0 as usize],
        ))
    }
}

impl Debug {
    fn run(&self) -> Result<(), String> {
        let (tfm_bytes, tfm_file, _) = self.path.read()?;
        let raw_file = tfm::format::RawFile::deserialize(&tfm_bytes).0.unwrap();

        let path = if self.omit_tfm_path {
            None
        } else {
            self.path.0.to_str()
        };

        let sub_file_sizes = raw_file.sub_file_sizes.clone();
        let (raw_file, tfm_file) = match (self.raw, self.parsed) {
            (false, false) | (true, true) => (Some(&raw_file), Some(&tfm_file)),
            (true, false) => (Some(&raw_file), None),
            (false, true) => (None, Some(&tfm_file)),
        };
        let sections: Vec<tfm::format::Section> = {
            let s: HashSet<tfm::format::Section> = self.section.iter().map(|s| s.0).collect();
            match s.is_empty() {
                true => tfm::format::Section::ALL_SECTIONS.into(),
                false => {
                    let mut v: Vec<tfm::format::Section> = s.into_iter().collect();
                    v.sort();
                    v
                }
            }
        };
        print!(
            "{}",
            tfm::format::debug(path, sub_file_sizes, tfm_file, raw_file, sections)
        );
        Ok(())
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

#[derive(Clone, Debug, Parser)]
struct Undebug {
    /// Path to the output of `tfmtools debug`.
    input: std::path::PathBuf,

    /// Path to write the .tfm file.
    output: TfPath,
}

impl Undebug {
    fn run(&self) -> Result<(), String> {
        let data = match std::fs::read_to_string(&self.input) {
            Ok(data) => data,
            Err(err) => {
                return Err(format!(
                    "Failed to read `{}`: {}",
                    self.input.display(),
                    err
                ))
            }
        };
        match tfm::format::RawFile::from_debug_output(&data) {
            Ok(b) => {
                self.output.write(&b)?;
                Ok(())
            }
            Err((err, n)) => {
                // TODO: this logic should be behind an arg type like TfmFile?
                let source = common::AriadneSource {
                    path: self.input.clone(),
                    source: data.clone().into(),
                };

                let range = line_range(&data, n);
                let builder = ariadne::Report::build(ariadne::ReportKind::Error, (), range.start)
                    .with_message("failed to parse `tfmtool debug` output");
                let builder = builder.with_label(
                    ariadne::Label::new(range)
                        .with_message(err)
                        .with_color(ariadne::Color::Red),
                );
                let report = builder.finish();
                report.eprint(&source).unwrap();
                Err("".into())
            }
        }
    }
}

fn line_range(s: &str, n: usize) -> std::ops::Range<usize> {
    let mut consumed = 0;
    for (m, line) in s.lines().enumerate() {
        if m == n {
            return consumed..consumed + line.len();
        }
        consumed += 1 + line.len();
    }
    panic!("s contains a line with index n");
}
