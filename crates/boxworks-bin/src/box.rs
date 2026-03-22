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
    Hlists(Hlists),
    Tex(Tex),
}

fn main() {
    let args: Cli = Cli::parse();
    let result = match args.sub_command {
        SubCommand::Check(check) => check.run(),
        SubCommand::Fmt(fmt) => fmt.run(),
        SubCommand::Hlists(hlists) => hlists.run(),
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

/// Print the horizontal lists that Box builds for some text.
///
/// To use TeX of pdfTeX to build the lists use `box tex hlists`instead.
#[derive(Parser)]
struct Hlists {
    /// The texts for which to build the lists.
    ///
    /// Each element of texts is used to build a separate horizontal list.
    texts: Vec<String>,

    /// Font metrics file to use.
    ///
    /// If not provided, the command uses CMR10.
    #[clap(short, long)]
    font_metrics: Option<PathBuf>,
}

impl Hlists {
    fn run(self) -> Result<(), String> {
        let tfm_bytes: Vec<u8> = match self.font_metrics {
            None => include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm").into(),
            Some(font_metrics) => match font_metrics.extension().and_then(|s| s.to_str()) {
                Some("pl" | "plst") => {
                    let pl_data = match fs::read_to_string(&font_metrics) {
                        Ok(source) => source,
                        Err(err) => {
                            return Err(format!["failed to open file {:?}: {err}", &font_metrics]);
                        }
                    };
                    tfm::algorithms::pl_to_tfm(&pl_data).0
                }
                Some("tfm") => match fs::read(&font_metrics) {
                    Ok(source) => source,
                    Err(err) => {
                        return Err(format!["failed to open file {:?}: {err}", &font_metrics]);
                    }
                },
                _ => {
                    return Err(format![
                        "unsupported font metrics file extension: must be pl, plst or tfm, is {:?}",
                        font_metrics.extension()
                    ])
                }
            },
        };
        let mut tfm_file = tfm::File::deserialize(&tfm_bytes).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
        let mut tp: boxworks_text::TextPreprocessorImpl = Default::default();
        tp.register_font(0, &tfm_file, lig_kern_program);
        tp.activate_font(0);
        for (i, text) in self.texts.into_iter().enumerate() {
            let mut got = vec![];
            for word in text.split_inclusive(' ') {
                tp.add_text(word.trim_matches(' '), &mut got);
                if word.ends_with(" ") {
                    tp.add_space(&mut got);
                }
            }
            use bwl::convert::ToBoxLang;
            let mut lang: Vec<bwl::ast::Horizontal<'static>> = got.to_box_lang();
            terse(&mut lang);
            println!("#");
            println!("# hlist {}", i + 1);
            println!(
                "{}",
                bwl::ast::Horizontal::Hlist(bwl::ast::Hlist {
                    width: Default::default(),
                    content: lang.into(),
                })
            );
        }
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

/// Print the horizontal lists that TeX builds for some text.
///
/// To use Box to build the lists use `box hlists` instead.
#[derive(Parser)]
struct TexHlists {
    /// The texts for which to build the lists.
    ///
    /// Each element of texts is used to build a separate horizontal list.
    texts: Vec<String>,

    /// Font metrics file to use.
    ///
    /// If not provided, the command uses the metrics for the default font for TeX
    /// (or whatever engine is being used).
    #[clap(short, long)]
    font_metrics: Option<PathBuf>,
}

impl TexHlists {
    fn run(self, tex_engine: &dyn boxworks::tex::TexEngine) -> Result<(), String> {
        let mut auxiliary_files: HashMap<PathBuf, Vec<u8>> = Default::default();
        let mut preamble: String = Default::default();
        if let Some(font_metrics) = self.font_metrics {
            let source = match font_metrics.extension().and_then(|s| s.to_str()) {
                Some("pl" | "plst") => {
                    let pl_data = match fs::read_to_string(&font_metrics) {
                        Ok(source) => source,
                        Err(err) => {
                            return Err(format!["failed to open file {:?}: {err}", &font_metrics]);
                        }
                    };
                    tfm::algorithms::pl_to_tfm(&pl_data).0
                }
                Some("tfm") => match fs::read(&font_metrics) {
                    Ok(source) => source,
                    Err(err) => {
                        return Err(format!["failed to open file {:?}: {err}", &font_metrics]);
                    }
                },
                _ => {
                    return Err(format![
                        "unsupported font metrics file extension: must be pl, plst or tfm, is {:?}",
                        font_metrics.extension()
                    ])
                }
            };
            let mut file_name: PathBuf = font_metrics.file_name().unwrap().into();
            file_name.set_extension("tfm");
            let file_stem = file_name.file_stem().unwrap().to_string_lossy();
            preamble.push_str(&format![
                r"

                    \font \customFont {file_stem}

                    \customFont
                    "
            ]);
            auxiliary_files.insert(file_name, source);
        }
        let (fonts, hlists) = boxworks::tex::build_horizontal_lists(
            tex_engine,
            &auxiliary_files,
            &preamble,
            &mut self.texts.iter(),
        );
        println!("# fonts:");
        for (font_name, font_number) in fonts.into_iter() {
            println!("# - {font_name}={font_number}");
        }

        use bwl::convert::ToBoxLang;
        let lang: Vec<bwl::ast::Hlist<'static>> = hlists.to_box_lang();
        for (i, mut hlist) in lang.into_iter().enumerate() {
            terse(&mut hlist.content.value);
            println!("#");
            println!("# hlist {}", i + 1);
            println!("{}", bwl::ast::Horizontal::Hlist(hlist));
        }
        Ok(())
    }
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
