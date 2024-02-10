use std::collections::HashSet;

use clap::Parser;

mod common;
use common::*;

fn main() {
    if let Err(err) = Cli::parse().run() {
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

impl Cli {
    fn run(self) -> Result<(), String> {
        match self.command {
            Command::Check(check) => check.run(),
            Command::Convert(convert) => convert.run(),
            Command::ConvertBatch(convert_batch) => convert_batch.run(),
            Command::Debug(debug) => debug.run(),
            Command::Fmt(format) => format.run(),
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
    /// Debug a .tfm file.
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
    ///     $ tfmtools debug path/to/file.tfml --section widths --section heights
    Debug(Debug),
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
                let (_, tfm_file, _) = tf_path.read()?;
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
            TfOrPlPath::Pl(pl_path) => {
                let mut pl_file = pl_path.read()?;
                if let Err(err) = tfm::ligkern::CompiledProgram::compile(
                    &pl_file.lig_kern_instructions,
                    &[],
                    pl_file.lig_kern_entrypoints(),
                ) {
                    eprintln!("{}", err.pltotf_message());
                    eprintln!("All ligatures will be cleared.");
                    pl_file.clear_lig_kern_data();
                }
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
    #[arg(short, long)]
    section: Vec<Section>,

    /// Only display raw data.
    #[arg(long)]
    raw: bool,

    /// Only display parsed data.
    #[arg(long)]
    parsed: bool,
}

impl Debug {
    fn run(&self) -> Result<(), String> {
        let (tfm_bytes, tfm_file, _) = self.path.read()?;
        let (raw_file, _) = tfm::format::RawFile::deserialize(&tfm_bytes).unwrap();

        let sections = {
            let mut s: HashSet<Section> = self.section.iter().copied().collect();
            match s.remove(&Section::All) || s.is_empty() {
                true => Section::all(),
                false => {
                    let mut v: Vec<Section> = s.into_iter().collect();
                    v.sort();
                    v
                }
            }
        };
        let (raw, parsed) = match (self.raw, self.parsed) {
            (false, false) => (true, true),
            t => t,
        };
        for section in sections {
            let (raw_data, parsed_data) = section.get(&raw_file, &tfm_file);
            if raw {
                println!("────────────────────────────[{:?} - raw]", section);
                if raw_data.is_empty() {
                    println!("[empty]")
                }
                for (j, word) in WordIter(raw_data).enumerate() {
                    println!(
                        "{} │ {:4}{:4}{:4}{:4}",
                        section.index(j),
                        word[0],
                        word[1],
                        word[2],
                        word[3]
                    );
                }
            }
            if parsed {
                println!("────────────────────────────[{:?} - parsed]", section);
                println!("{:#?}", parsed_data);
            }
        }
        Ok(())
    }
}

#[derive(Debug, Hash, Clone, Copy, clap::ValueEnum, PartialEq, Eq, PartialOrd, Ord)]
enum Section {
    /// Output all sections
    All,
    /// Sub-file sizes
    SubFileSizes,
    /// Character data
    CharInfos,
    /// Widths array
    Widths,
    /// Heights array
    Heights,
    /// Depths array
    Depths,
    /// Italic corrections array
    ItalicCorrections,
    /// Lig/kern instructions
    LigKern,
    /// Kerns array
    Kerns,
    /// Extensible recipes
    ExtensibleRecipes,
    /// Params array
    Params,
}

impl Section {
    fn all() -> Vec<Section> {
        use Section::*;
        vec![
            SubFileSizes,
            CharInfos,
            Widths,
            Heights,
            Depths,
            ItalicCorrections,
            LigKern,
            Kerns,
            ExtensibleRecipes,
            Params,
        ]
    }
    fn get<'a>(
        &self,
        raw_file: &'a tfm::format::RawFile<'a>,
        file: &'a tfm::format::File,
    ) -> (&'a [u8], Box<dyn std::fmt::Debug + 'a>) {
        match self {
            Section::All => panic!("this method doesn't work with Section::All"),
            Section::SubFileSizes => (
                raw_file.raw_sub_file_sizes,
                Box::new(&raw_file.sub_file_sizes),
            ),
            Section::CharInfos => (raw_file.char_infos, Box::new(&file.char_infos)),
            Section::Widths => (raw_file.widths, Box::new(&file.widths)),
            Section::Heights => (raw_file.heights, Box::new(&file.heights)),
            Section::Depths => (raw_file.depths, Box::new(&file.depths)),
            Section::ItalicCorrections => (
                raw_file.italic_corrections,
                Box::new(&file.italic_corrections),
            ),
            Section::LigKern => (
                raw_file.lig_kern_instructions,
                Box::new(&file.lig_kern_instructions),
            ),
            Section::Kerns => (raw_file.kerns, Box::new(&file.kerns)),
            Section::ExtensibleRecipes => (
                raw_file.extensible_recipes,
                Box::new(&file.extensible_chars),
            ),
            Section::Params => (raw_file.params, Box::new(&file.params)),
        }
    }
    fn index(&self, j: usize) -> String {
        let default = format!("{:5}", j);
        if *self != Self::CharInfos {
            return default;
        }
        let raw: u8 = match j.try_into() {
            Ok(raw) => raw,
            Err(_) => return default,
        };
        let c = raw as char;
        if c.is_ascii_alphanumeric() || c.is_ascii_graphic() {
            format!("    {}", c)
        } else {
            format!(" 0x{:02x}", j)
        }
    }
}

struct WordIter<'a>(&'a [u8]);

impl<'a> Iterator for WordIter<'a> {
    type Item = [u8; 4];

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.get(..4) {
            None => None,
            Some(head) => {
                self.0 = &self.0[4..];
                Some([head[0], head[1], head[2], head[3]])
            }
        }
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
