use std::collections::HashSet;

use clap::Parser;

mod shared;
use shared::*;

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

    /// Indent to use when outputting property list files.
    #[arg(short = 'i', long, default_value_t = 3)]
    pl_indent: usize,

    /// Specification for how to output characters in property list files.
    #[arg(short = 'c', long, default_value = "default")]
    pl_charcode_format: CharcodeFormat,

    /// Default extension to use when writing property list files.
    #[arg(short = 'e', long, default_value = "plst")]
    pl_extension: PlExtension,
}

impl Cli {
    fn run(self) -> Result<(), String> {
        match self.command {
            Command::Check(check) => check.run(),
            Command::Convert(convert) => convert.run(self.pl_indent, self.pl_charcode_format),
            Command::Debug(debug) => debug.run(),
            Command::Fmt(format) => format.run(),
            Command::Ligkern { action: lig_kern } => lig_kern.run(),
            Command::Undebug(undebug) => undebug.run(),
        }
    }
}

#[derive(Clone, Debug, clap::Subcommand)]
enum Command {
    /// Check that a .tfm or property list file is valid.
    Check(Check),

    /// Convert a single .tfm or property list file.
    Convert(Convert),

    /// Print debugging information about a .tfm file.
    ///
    /// This subcommand is used to debug the contents of a .tfm file.
    /// Unlike property list files - which are regular ASCII, human-readable,
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

    /// Format a property list file.
    Fmt(Format),

    /// Perform operations related to the lig/kern program in a .tfm or property list file.
    Ligkern {
        #[command(subcommand)]
        action: LigKern,
    },

    /// Create a .tfm file from the (potentially modified) output of `tfmtools debug`
    ///
    /// The `tfmtools debug` command outputs a plain text representation of the binary
    ///     and parsed data in a .tfm file.
    /// This command reconstructs .tfm binary files from the raw data in this plain text output.
    ///
    /// There are at least two use cases for this command:
    ///
    /// 1.
    ///        If you have a corrupted .tfm file and know enough about the format to fix it,
    ///        you can run `tfmtools debug`,
    ///        manually fix the bytes in the text output,
    ///        and then create a hopefully valid .tfm from the modified output using `tfmtools undebug`.
    ///
    /// 2.
    ///        If you're writing code that reads .tfm files (potentially using Texcraft's tfm crate)
    ///        you can use this command to create invalid or otherwise unusual .tfm files
    ///        for testing.
    ///
    /// When modifying the debug output it may be useful to know some of the parsing rules:
    ///
    /// 1.
    ///        Any of the .tfm sections can be omitted.
    ///        All omitted sections are output as empty
    ///        sections with the exception of the sub file sizes section, which is calculated
    ///        correctly.
    ///
    /// 2.
    ///        Only data within binary data sections is parsed.
    ///        Text in other sections is completely ignored.
    ///
    /// 3.
    ///        Within binary data sections, all lines beginning with // are ignored.
    ///
    /// 4.
    ///        Within binary data sections, all data before the first | character is generally ignored.
    ///        There is a single exception: in the first line of the character data section,
    ///        any text before the first | is parsed as the first character in the file.
    Undebug(Undebug),
}

#[derive(Clone, Debug, Parser)]
struct Check {
    /// Path to the .tf or property list file to validate.
    path: TfOrPlPath,
}

impl Check {
    fn run(&self) -> Result<(), String> {
        let num_warnings = match &self.path {
            TfOrPlPath::Tf(tfm_path) => {
                let Tfm {
                    deserialization_warnings,
                    validation_warnings,
                    ..
                } = tfm_path.read(false)?;
                deserialization_warnings.len() + validation_warnings.len()
            }
            TfOrPlPath::Pl(pl_path) => pl_path.read()?.1.len(),
        };
        if num_warnings > 0 {
            Err(format!("Check failure: {} warnings", num_warnings))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug, Parser)]
struct Convert {
    /// Path to the .tfm or property list file to convert.
    path: TfOrPlPath,

    /// Output path for the converted file.
    ///
    /// This must have the correct file extension.
    /// If the input file is a property list file, this must be a .tfm path.
    /// If the input file is a .tfm file, this must be a .pl or .plst path.
    ///
    /// If not provided, and the input file is a .tfm file,
    ///     the converted property list file will be printed to standard out.
    /// If the input file is a property list file,
    ///     the converted TFM file will be written to the same path
    ///     as the input file but with a .tfm file extension.
    /// Thus
    ///
    /// $ tfmtools convert path/to/file.pl
    ///
    /// writes the output to path/to/file.tfm.
    #[arg(short, long)]
    output: Option<TfOrPlPath>,

    /// Remove the lig/kern program from the output file.
    ///
    /// This clears all lig/kern instructions and removes ligature tags from characters.
    #[arg(long)]
    remove_lig_kern_program: bool,
}

fn remove_lig_kern_program(file: &mut tfm::File) {
    file.lig_kern_program = Default::default();
    file.char_tags
        .retain(|_, tag| !matches!(tag, tfm::CharTag::Ligature(_)));
}

impl Convert {
    fn run(&self, indent: usize, charcode_format: CharcodeFormat) -> Result<(), String> {
        match &self.path {
            TfOrPlPath::Tf(tf_path) => {
                let mut bytes = tf_path.read_bytes()?;
                if self.remove_lig_kern_program {
                    let (tfm_file_or, _) = tfm::File::deserialize(&bytes);
                    if let Ok(mut tfm_file) = tfm_file_or {
                        remove_lig_kern_program(&mut tfm_file);
                        bytes = tfm_file.serialize();
                    }
                }
                let output = tfm::algorithms::tfm_to_pl(&bytes, indent, &|pl_file| {
                    charcode_format.to_display_format(&pl_file.header.character_coding_scheme)
                })
                .unwrap();
                for error_message in output.error_messages {
                    eprintln!("{}", error_message.tftopl_message());
                }
                let pl_data = match output.pl_data {
                    Ok(pl_data) => pl_data,
                    Err(err) => return Err(err.tftopl_message()),
                };
                match &self.output {
                    None => print!("{pl_data}"),
                    Some(TfOrPlPath::Pl(pl_path)) => {
                        pl_path.write(&pl_data)?;
                    }
                    Some(TfOrPlPath::Tf(_tfm_path)) => todo!(),
                }
                Ok(())
            }
            TfOrPlPath::Pl(pl_path) => {
                let (pl_file, _) = pl_path.read()?;
                let mut tfm_file: tfm::File = pl_file.into();
                if self.remove_lig_kern_program {
                    remove_lig_kern_program(&mut tfm_file);
                }
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
    /// This is useful if the tool is being used to diff two .tfm files.
    /// In this case the .tfm file paths being different would return a false positive.
    #[arg(long)]
    omit_tfm_path: bool,

    /// Skip validating and fixing the .tfm file.
    #[arg(long)]
    skip_validation: bool,
}

#[derive(Clone, Debug)]
struct Section(tfm::Section);

impl Section {
    const ALL_VARIANTS: [Section; 11] = [
        Section(tfm::Section::SubFileSizes),
        Section(tfm::Section::Header),
        Section(tfm::Section::CharInfos),
        Section(tfm::Section::Widths),
        Section(tfm::Section::Heights),
        Section(tfm::Section::Depths),
        Section(tfm::Section::ItalicCorrections),
        Section(tfm::Section::LigKern),
        Section(tfm::Section::Kerns),
        Section(tfm::Section::ExtensibleRecipes),
        Section(tfm::Section::Params),
    ];
}

impl clap::ValueEnum for Section {
    fn value_variants<'a>() -> &'a [Self] {
        &Section::ALL_VARIANTS
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(clap::builder::PossibleValue::new(
            tfm::Section::NAMES[self.0 as usize],
        ))
    }
}

impl Debug {
    fn run(&self) -> Result<(), String> {
        let Tfm {
            bytes: tfm_bytes,
            file: tfm_file,
            ..
        } = self.path.read(self.skip_validation)?;
        let raw_file = tfm::RawFile::deserialize(&tfm_bytes).0.unwrap();

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
        let sections: Vec<tfm::Section> = {
            let s: HashSet<tfm::Section> = self.section.iter().map(|s| s.0).collect();
            match s.is_empty() {
                true => tfm::Section::ALL_SECTIONS.into(),
                false => {
                    let mut v: Vec<tfm::Section> = s.into_iter().collect();
                    v.sort();
                    v
                }
            }
        };
        print!(
            "{}",
            tfm::debug(path, sub_file_sizes, tfm_file, raw_file, sections)
        );
        Ok(())
    }
}

#[derive(Clone, Debug, Parser)]
struct Format {
    /// Instead of overwriting the property list file, write the result to the provided path.
    #[arg(short, long)]
    output: Option<std::path::PathBuf>,

    /// Instead of overwriting the property list file, print the result to standard out.
    #[arg(short, long)]
    print: bool,

    /// Just check that the property list file is already formatted correctly, and don't output anything.
    #[arg(short, long)]
    check: bool,
}

impl Format {
    fn run(&self) -> Result<(), String> {
        todo!()
    }
}

#[derive(Clone, Debug, clap::Subcommand)]
enum LigKern {
    Describe(LigKernDescribe),
    Replace(LigKernReplace),
    Run(LigKernRun),
}

impl LigKern {
    fn run(&self) -> Result<(), String> {
        use LigKern::*;
        match self {
            Describe(describe) => describe.run(),
            Replace(replace) => replace.run(),
            Run(run) => run.run(),
        }
    }
}

type RawProgram = (
    tfm::ligkern::lang::Program,
    std::collections::HashMap<tfm::Char, u16>,
);

fn raw_lig_kern_program(path: &std::path::Path) -> Result<RawProgram, String> {
    match path.extension().and_then(|e| e.to_str()) {
        Some("tfm") => {
            let mut tfm_file = TfPath(path.to_path_buf()).read(false)?.file;
            let entrypoints = tfm_file
                .lig_kern_entrypoints()
                .into_iter()
                .filter_map(|(c, e)| {
                    tfm_file
                        .lig_kern_program
                        .unpack_entrypoint(e)
                        .ok()
                        .map(|e| (c, e))
                })
                .collect();
            Ok((tfm_file.lig_kern_program, entrypoints))
        }
        Some("pl") | Some("plst") => {
            let (pl_file, _) = PlPath(path.to_path_buf()).read()?;
            let entrypoints = pl_file.lig_kern_entrypoints(true);
            Ok((pl_file.lig_kern_program, entrypoints))
        }
        _ => {
            let text = std::fs::read_to_string(path)
                .map_err(|e| format!("Failed to read `{}`: {}", path.display(), e))?;
            tfm::ligkern::lang::Program::parse_compact(&text)
                .map_err(|e| format!("Failed to parse lig/kern program: {e:?}"))
        }
    }
}

fn compile_lig_kern_program(
    path: &std::path::Path,
) -> Result<tfm::ligkern::CompiledProgram, String> {
    Ok(match path.extension().and_then(|e| e.to_str()) {
        Some("tfm") => {
            let mut tfm_file = TfPath(path.to_path_buf()).read(false)?.file;
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0
        }
        Some("pl") | Some("plst") => {
            let (pl_file, _) = PlPath(path.to_path_buf()).read()?;
            let entrypoints = pl_file.lig_kern_entrypoints(true);
            tfm::ligkern::CompiledProgram::compile(
                &pl_file.lig_kern_program,
                pl_file.header.design_size,
                &[],
                entrypoints,
            )
            .0
        }
        _ => {
            let text = std::fs::read_to_string(path)
                .map_err(|e| format!("Failed to read `{}`: {}", path.display(), e))?;
            let (program, entrypoints) = tfm::ligkern::lang::Program::parse_compact(&text)
                .map_err(|e| format!("Failed to parse lig/kern program: {e:?}"))?;
            tfm::ligkern::CompiledProgram::compile(&program, tfm::FixWord::ONE, &[], entrypoints).0
        }
    })
}

#[derive(Clone, Debug, Parser)]
struct LigKernDescribe {
    /// Path to a .tfm file, property list file, or compact lig/kern program.
    path: std::path::PathBuf,

    /// Output the raw (uncompiled) lig/kern program instead of the compiled program.
    #[arg(long)]
    raw: bool,
}

impl LigKernDescribe {
    fn run(&self) -> Result<(), String> {
        if self.raw {
            let (program, entrypoints) = raw_lig_kern_program(&self.path)?;
            let mut sorted: Vec<_> = entrypoints.into_iter().collect();
            sorted.sort_by_key(|(c, _)| *c);
            for (left_char, entrypoint) in sorted {
                for (_, instruction) in program.instructions_for_entrypoint(entrypoint) {
                    println!(
                        "{}",
                        instruction
                            .operation
                            .display_compact(left_char.into(), instruction.right_char.into())
                    );
                }
            }
            return Ok(());
        }
        let lig_kern_program = compile_lig_kern_program(&self.path)?;
        for (l, r) in lig_kern_program.all_pairs_with_replacements() {
            let s = match l {
                None => {
                    print!("     {r} -> ");
                    format!["{r}"]
                }
                Some(l) => {
                    print!("   {l}{r} -> ");
                    format!["{l}{r}"]
                }
            };
            struct Emitter;
            impl tfm::ligkern::Emitter for Emitter {
                fn emit_character(&mut self, c: char) {
                    print!("{}", c.escape_debug())
                }

                fn emit_kern(&mut self, kern: common::Scaled) {
                    print!("[{}]", kern.0);
                }

                fn emit_ligature(&mut self, ligature: tfm::ligkern::Ligature) {
                    print!("{}", ligature.c.escape_debug());
                }
            }
            let mut emitter = Emitter {};
            lig_kern_program.run(&s, &mut emitter);
            println!();
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Parser)]
struct LigKernRun {
    /// Path to a .tfm file, property list file, or compact lig/kern program.
    path: std::path::PathBuf,

    /// The string to run through the lig/kern program.
    input: String,
}

impl LigKernRun {
    fn run(&self) -> Result<(), String> {
        let lig_kern_program = compile_lig_kern_program(&self.path)?;
        struct Emitter;
        impl tfm::ligkern::Emitter for Emitter {
            fn emit_character(&mut self, c: char) {
                print!("{c}")
            }
            fn emit_kern(&mut self, kern: common::Scaled) {
                print!("[{}]", kern.0);
            }
            fn emit_ligature(&mut self, ligature: tfm::ligkern::Ligature) {
                print!("{}", ligature.c);
            }
        }
        lig_kern_program.run(&self.input, &mut Emitter);
        println!();
        Ok(())
    }
}

#[derive(Clone, Debug, Parser)]
struct LigKernReplace {
    /// Path to the .tfm or property list file.
    path: TfOrPlPath,

    /// Path to the lig/kern program file to use as a replacement.
    #[arg(long)]
    program: Option<std::path::PathBuf>,

    /// Remove the lig/kern program entirely.
    #[arg(long)]
    remove: bool,

    /// Modify the input file in place.
    #[arg(long)]
    in_place: bool,

    /// Path to write the output file.
    #[arg(long)]
    output: Option<TfOrPlPath>,
}

impl LigKernReplace {
    fn run(&self) -> Result<(), String> {
        if !self.in_place && self.output.is_none() {
            return Err("one of --in-place or --output must be provided".into());
        }
        let (new_program, entrypoints) = match (&self.program, self.remove) {
            (None, false) => return Err("one of --program or --remove must be provided".into()),
            (Some(_), true) => return Err("--program and --remove are mutually exclusive".into()),
            (None, true) => (Default::default(), Default::default()),
            (Some(program_path), false) => {
                let program_text = std::fs::read_to_string(program_path)
                    .map_err(|e| format!("Failed to read `{}`: {}", program_path.display(), e))?;
                tfm::ligkern::lang::Program::parse_compact(&program_text)
                    .map_err(|e| format!("Failed to parse lig/kern program: {e:?}"))?
            }
        };
        match &self.path {
            TfOrPlPath::Tf(tf_path) => {
                let mut tfm = tf_path.read(false)?;
                tfm.file.replace_lig_kern_program(new_program, entrypoints);
                let bytes = tfm.file.serialize();
                let out_path = if self.in_place {
                    tf_path.clone()
                } else if let Some(TfOrPlPath::Tf(out_tf)) = &self.output {
                    out_tf.clone()
                } else {
                    return Err("cannot write a .tfm file to a .pl path".into());
                };
                out_path.write(&bytes)
            }
            TfOrPlPath::Pl(pl_path) => {
                let (mut pl_file, _) = pl_path.read()?;
                pl_file.replace_lig_kern_program(new_program, entrypoints);
                let pl_text = format!(
                    "{}",
                    pl_file.display(0, tfm::pl::CharDisplayFormat::Default)
                );
                let out_path = if self.in_place {
                    pl_path.clone()
                } else if let Some(TfOrPlPath::Pl(out_pl)) = &self.output {
                    out_pl.clone()
                } else {
                    return Err("cannot write a .pl file to a .tfm path".into());
                };
                out_path.write(&pl_text)
            }
        }
    }
}

#[derive(Clone, Debug, Parser)]
struct Undebug {
    /// Path to the output of `tfmtools debug`.
    input: std::path::PathBuf,

    /// Path to write the .tfm file.
    output: TfPath,

    /// By default the sub-file sizes in the debug output are ignored and the
    ///     correct sizes are calculated, based on the sizes of the data sections.
    /// If true, the sizes in the debug output will be used instead.
    #[arg(short, long)]
    keep_sub_file_sizes: bool,
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
        match tfm::RawFile::from_debug_output(&data, self.keep_sub_file_sizes) {
            Ok(b) => {
                self.output.write(&b)?;
                Ok(())
            }
            Err((err, n)) => {
                let source = ariadne::Source::from(data.clone());
                let range = line_range(&data, n);
                let builder = ariadne::Report::build(ariadne::ReportKind::Error, range.clone())
                    .with_message("failed to parse `tfmtools debug` output");
                let builder = builder.with_label(
                    ariadne::Label::new(range)
                        .with_message(err)
                        .with_color(ariadne::Color::Red),
                );
                let report = builder.finish();
                report.eprint(source).unwrap();
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

#[derive(Debug, Clone, clap::ValueEnum)]
pub enum PlExtension {
    /// The .pl file extension is used in Knuth's implementation of tftopl and pltotf.
    /// The problem with this extension is that it collides with Perl scripts,
    ///     which are very common in the CTAN repository,
    ///     and this makes it hard to find real property list files.
    Pl,
    /// The .plst file extension is the default in tfmtools.
    Plst,
}

#[derive(Clone, Debug)]
pub struct TfPath(pub std::path::PathBuf);

pub struct Tfm {
    pub bytes: Vec<u8>,
    pub file: tfm::File,
    pub deserialization_warnings: Vec<tfm::DeserializationWarning>,
    pub validation_warnings: Vec<tfm::ValidationWarning>,
}

impl TfPath {
    pub fn read_bytes(&self) -> Result<Vec<u8>, String> {
        match std::fs::read(&self.0) {
            Ok(data) => Ok(data),
            Err(err) => Err(format!("Failed to read `{}`: {}", self.0.display(), err)),
        }
    }
    pub fn read(&self, skip_validation: bool) -> Result<Tfm, String> {
        let bytes = self.read_bytes()?;
        let (tfm_file_or, deserialization_warnings) = tfm::File::deserialize(&bytes);
        for warning in &deserialization_warnings {
            eprintln!("{}", warning.tftopl_message())
        }
        let mut tfm_file = match tfm_file_or {
            Ok(t) => t,
            Err(err) => return Err(err.tftopl_message()),
        };
        let validation_warnings = if skip_validation {
            vec![]
        } else {
            tfm_file.validate_and_fix()
        };
        for warning in &validation_warnings {
            eprintln!("{}", warning.tftopl_message())
        }
        Ok(Tfm {
            bytes,
            file: tfm_file,
            deserialization_warnings,
            validation_warnings,
        })
    }
    fn parse(input: &str) -> Result<Self, InvalidExtension> {
        let path_buf: std::path::PathBuf = input.into();
        match path_buf.extension().and_then(std::ffi::OsStr::to_str) {
            Some("tfm") => Ok(TfPath(path_buf)),
            extension => Err(InvalidExtension {
                provided: extension.map(str::to_string),
                allowed: vec!["tfm"],
            }),
        }
    }

    pub fn write(&self, content: &[u8]) -> Result<(), String> {
        match std::fs::write(&self.0, content) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Failed to write `{}`: {}", self.0.display(), err)),
        }
    }
}

impl clap::builder::ValueParserFactory for TfPath {
    type Parser = clap::builder::ValueParser;

    fn value_parser() -> Self::Parser {
        clap::builder::ValueParser::new(TfPath::parse)
    }
}

#[derive(Clone, Debug)]
pub struct PlPath(pub std::path::PathBuf);

impl PlPath {
    pub fn read(&self) -> Result<(tfm::pl::File, Vec<tfm::pl::ParseWarning>), String> {
        let data = match std::fs::read_to_string(&self.0) {
            Ok(data) => data,
            Err(err) => return Err(format!("Failed to read `{}`: {}", self.0.display(), err)),
        };
        let (pl_file, warnings) = tfm::pl::File::from_pl_source_code(&data);
        let source = ariadne::Source::from(data.as_str());
        let path = self.0.as_os_str().to_string_lossy();
        let cache: (&str, _) = (&path, source);
        for warning in &warnings {
            // Note that the cache contains the Source type, which contains
            // a vector of lines in the file, and this is cloned for each error.
            // This is not great, but is the only default behavior the ariadne
            // crate supports. An alternative would be to implement ariadne::Cache
            // for &Source and then there would be no cloning.
            warning.ariadne_report(&path).eprint(cache.clone()).unwrap();
        }
        Ok((pl_file, warnings))
    }
    pub fn write(&self, content: &str) -> Result<(), String> {
        match std::fs::write(&self.0, content) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Failed to write `{}`: {}", self.0.display(), err)),
        }
    }
}

#[derive(Debug)]
pub struct InvalidExtension {
    pub provided: Option<String>,
    pub allowed: Vec<&'static str>,
}

impl std::fmt::Display for InvalidExtension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let allowed = self
            .allowed
            .iter()
            .map(|s| format![".{s}"])
            .collect::<Vec<String>>()
            .join(" or ");
        match &self.provided {
            None => write!(
                f,
                "the file extension must be {} but it is missing",
                allowed
            ),
            Some(extension) => write!(
                f,
                "the file extension must be {} but it is .{}",
                allowed, extension
            ),
        }
    }
}

impl std::error::Error for InvalidExtension {}

#[derive(Clone, Debug)]
pub enum TfOrPlPath {
    Tf(TfPath),
    Pl(PlPath),
}

impl TfOrPlPath {
    pub fn parse(input: &str) -> Result<Self, InvalidExtension> {
        let path_buf: std::path::PathBuf = input.into();
        match path_buf.extension().and_then(std::ffi::OsStr::to_str) {
            Some("pl") | Some("plst") => Ok(TfOrPlPath::Pl(PlPath(path_buf))),
            Some("tfm") => Ok(TfOrPlPath::Tf(TfPath(path_buf))),
            extension => Err(InvalidExtension {
                provided: extension.map(str::to_string),
                allowed: vec!["pl", "plst", "tfm"],
            }),
        }
    }
}

impl clap::builder::ValueParserFactory for TfOrPlPath {
    type Parser = clap::builder::ValueParser;

    fn value_parser() -> Self::Parser {
        clap::builder::ValueParser::new(TfOrPlPath::parse)
    }
}
