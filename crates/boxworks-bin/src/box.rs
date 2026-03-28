use boxworks::TextPreprocessor;
use boxworks_lang as bwl;
use clap::Parser;
use std::collections::HashMap;
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
    Build(Build),
}

fn main() {
    let args: Cli = Cli::parse();
    let result = match args.sub_command {
        SubCommand::Check(check) => check.run(),
        SubCommand::Fmt(fmt) => fmt.run(),
        SubCommand::Build(hlists) => hlists.run(),
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

/// What kind of output to produce.
#[derive(clap::ValueEnum, Clone, Default)]
enum Mode {
    /// Build horizontal lists.
    #[default]
    Hlists,
    /// Build vertical lists. Requires --tex-engine.
    Vlists,
}

/// Print the horizontal lists built for some text.
///
/// By default, Box builds the lists. If --tex_engine is specified, the given TeX engine is used instead.
#[derive(Parser)]
struct Build {
    /// The texts for which to build the lists.
    ///
    /// Each element of texts is used to build a separate horizontal list.
    texts: Vec<String>,

    /// Font metrics file to use.
    #[clap(short, long)]
    font_metrics: Option<PathBuf>,

    /// Use a TeX engine to build the lists (e.g. `tex`, `pdftex`).
    ///
    /// If not specified, Box builds the lists using CMR10 as the default font.
    #[clap(long)]
    tex_engine: Option<String>,

    /// Path to a file containing texts to convert, one per line.
    ///
    /// Empty lines are ignored. Each non-empty line is converted into a separate horizontal list.
    #[clap(long)]
    texts_file: Option<PathBuf>,

    /// What kind of output to produce.
    #[clap(long, default_value_t, value_enum)]
    mode: Mode,
}

impl Build {
    fn run(mut self) -> Result<(), String> {
        let num_direct = self.texts.len();
        let mut file_line_numbers: Vec<usize> = vec![];
        if let Some(ref path) = self.texts_file.clone() {
            let content = match fs::read_to_string(path) {
                Ok(s) => s,
                Err(err) => return Err(format!["failed to open file {:?}: {err}", path]),
            };
            for (i, line) in content.lines().enumerate() {
                if !line.is_empty() {
                    self.texts.push(line.to_owned());
                    file_line_numbers.push(i + 1);
                }
            }
        }
        let tex_engine: Option<Box<dyn boxworks::tex::TexEngine>> =
            if let Some(ref name) = self.tex_engine {
                Some(
                    boxworks::tex::new_tex_engine_binary(name.clone())
                        .map_err(|err| format!["{err}"])?,
                )
            } else {
                None
            };
        let labels = make_labels(self.texts.len(), num_direct, &file_line_numbers);
        match self.mode {
            Mode::Hlists => {
                let hlists = match tex_engine {
                    Some(engine) => run_tex_hlists(engine.as_ref(), self.texts, self.font_metrics)?,
                    None => run_box_hlists(self.texts, self.font_metrics)?,
                };
                print_hlists(hlists, labels);
            }
            Mode::Vlists => {
                let engine =
                    tex_engine.ok_or_else(|| "--mode=vlists requires --tex-engine".to_string())?;
                let vlists = run_tex_vlists(engine.as_ref(), self.texts, self.font_metrics)?;
                print_vlists(vlists, labels);
            }
        }
        Ok(())
    }
}

fn make_labels(n: usize, num_direct: usize, file_line_numbers: &[usize]) -> Vec<Option<usize>> {
    (0..n)
        .map(|i| {
            if i >= num_direct {
                Some(file_line_numbers[i - num_direct])
            } else {
                None
            }
        })
        .collect()
}

fn build_tex_context(
    font_metrics: Option<PathBuf>,
) -> Result<(HashMap<PathBuf, Vec<u8>>, String), String> {
    let mut auxiliary_files: HashMap<PathBuf, Vec<u8>> = Default::default();
    let mut preamble: String = Default::default();
    let (source, file_name) = load_font_metrics(font_metrics)?;
    let file_stem = file_name.file_stem().unwrap().to_string_lossy();
    preamble.push_str(&format![
        r"

            \font \customFont {file_stem}

            \customFont
            "
    ]);
    auxiliary_files.insert(file_name, source);
    Ok((auxiliary_files, preamble))
}

fn run_box_hlists(
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
) -> Result<Vec<bwl::ast::Hlist<'static>>, String> {
    let (tfm_bytes, _) = load_font_metrics(font_metrics)?;
    let mut tfm_file = tfm::File::deserialize(&tfm_bytes).0.unwrap();
    let lig_kern_program = tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
    let mut tp: boxworks_text::TextPreprocessorImpl = Default::default();
    tp.register_font(0, &tfm_file, lig_kern_program);
    tp.activate_font(0);
    let raw: Vec<Vec<_>> = texts
        .into_iter()
        .map(|text| {
            let mut got = vec![];
            tp.add_text(&text, &mut got);
            got
        })
        .collect();
    use bwl::convert::ToBoxLang;
    Ok(raw
        .into_iter()
        .map(|got| bwl::ast::Hlist {
            width: Default::default(),
            content: got.to_box_lang().into(),
        })
        .collect())
}

fn run_tex_hlists(
    tex_engine: &dyn boxworks::tex::TexEngine,
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
) -> Result<Vec<bwl::ast::Hlist<'static>>, String> {
    let (auxiliary_files, preamble) = build_tex_context(font_metrics)?;
    let (fonts, hlists) = boxworks::tex::build_horizontal_lists(
        tex_engine,
        &auxiliary_files,
        &preamble,
        &mut texts.iter(),
    );
    _ = fonts;
    use bwl::convert::ToBoxLang;
    Ok(hlists.to_box_lang())
}

fn run_tex_vlists(
    tex_engine: &dyn boxworks::tex::TexEngine,
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
) -> Result<Vec<bwl::ast::Vlist<'static>>, String> {
    let (auxiliary_files, preamble) = build_tex_context(font_metrics)?;
    let (_, vlists) = boxworks::tex::build_vertical_lists(
        tex_engine,
        &auxiliary_files,
        &preamble,
        &mut texts.iter(),
    );
    use bwl::convert::ToBoxLang;
    Ok(vlists.to_box_lang())
}

fn print_hlists(hlists: Vec<bwl::ast::Hlist<'static>>, labels: Vec<Option<usize>>) {
    for (i, (mut hlist, label)) in hlists.into_iter().zip(labels).enumerate() {
        hlist.width = core::Scaled::ZERO.into();
        terse(&mut hlist.content.value);
        println!("#");
        match label {
            Some(line_num) => println!("# hlist {} (line {})", i + 1, line_num),
            None => println!("# hlist {}", i + 1),
        }
        println!("{}", bwl::ast::Horizontal::Hlist(hlist));
    }
}

fn print_vlists(vlists: Vec<bwl::ast::Vlist<'static>>, labels: Vec<Option<usize>>) {
    for (i, (vlist, label)) in vlists.into_iter().zip(labels).enumerate() {
        println!("#");
        match label {
            Some(line_num) => println!("# vlist {} (line {})", i + 1, line_num),
            None => println!("# vlist {}", i + 1),
        }
        println!("{}", bwl::ast::Horizontal::Vlist(vlist));
    }
}

fn load_font_metrics(font_metrics: Option<PathBuf>) -> Result<(Vec<u8>, PathBuf), String> {
    let Some(path) = font_metrics else {
        let bytes = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm").to_vec();
        return Ok((bytes, PathBuf::from("cmr10.tfm")));
    };
    let bytes = match path.extension().and_then(|s| s.to_str()) {
        Some("pl" | "plst") => {
            let pl_data = match fs::read_to_string(&path) {
                Ok(source) => source,
                Err(err) => return Err(format!["failed to open file {:?}: {err}", &path]),
            };
            tfm::algorithms::pl_to_tfm(&pl_data).0
        }
        Some("tfm") => match fs::read(&path) {
            Ok(source) => source,
            Err(err) => return Err(format!["failed to open file {:?}: {err}", &path]),
        },
        _ => {
            return Err(format![
                "unsupported font metrics file extension: must be pl, plst or tfm, is {:?}",
                path.extension()
            ])
        }
    };
    let mut file_name: PathBuf = path.file_name().unwrap().into();
    file_name.set_extension("tfm");
    Ok((bytes, file_name))
}

fn terse(v: &mut Vec<bwl::ast::Horizontal<'static>>) {
    let mut src: usize = 0;
    let mut dest = 0;
    let mut buf: String = Default::default();
    while let Some(elem) = v.get_mut(src) {
        match elem {
            bwl::ast::Horizontal::Text(text) => {
                let first_text = buf.is_empty();
                buf.push_str(&text.content.value);
                text.content.value = buf.clone().into();
                if !first_text {
                    dest -= 1;
                }
            }
            _ => {
                buf.clear();
            }
        }
        v.swap(src, dest);
        src += 1;
        dest += 1;
    }
    v.truncate(dest);
}
