use boxworks::lang as bwl;
use boxworks::tex as bwt;
use boxworks::LineBreaker;
use boxworks::TextPreprocessor;
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
    Hbox(Hbox),
    Linebreak(Linebreak),
}

fn main() {
    let args: Cli = Cli::parse();
    let result = match args.sub_command {
        SubCommand::Check(check) => check.run(),
        SubCommand::Fmt(fmt) => fmt.run(),
        SubCommand::Hbox(hbox) => hbox.run(),
        SubCommand::Linebreak(linebreak) => linebreak.run(),
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

/// Print the horizontal lists built for some text.
///
/// By default, Box builds the lists. If --tex_engine is specified, the given TeX engine is used instead.
#[derive(Parser)]
struct Hbox {
    /// The texts for which to build the lists.
    ///
    /// Each element of texts is used to build a separate horizontal list.
    texts: Vec<String>,

    /// Font metrics file to use.
    ///
    /// If not specified, Box builds the lists using CMR10 as the default font.
    #[clap(short, long)]
    font_metrics: Option<PathBuf>,

    /// Use a TeX engine to build the lists (e.g. `tex`, `pdftex`).
    #[clap(long)]
    tex_engine: Option<String>,

    /// Path to a file containing texts to convert, one per line.
    ///
    /// Empty lines are ignored. Each non-empty line is converted into a separate horizontal list.
    #[clap(long)]
    texts_file: Option<PathBuf>,

    /// Run the hyphenation algorithm on the text before boxing it.
    #[clap(long)]
    hyphenate: bool,

    /// Glue between words.
    ///
    /// If zero (the default), TeX uses the current font's default spacing, potentially modified by the space factor.
    ///
    /// Corresponds to the TeX primitive \spaceskip.
    #[clap(long)]
    space_skip: Option<String>,

    /// Extra wide glue between words - e.g. after a period.
    ///
    /// Corresponds to the TeX primitive \xspaceskip.
    #[clap(long)]
    extra_space_skip: Option<String>,
}

impl Hbox {
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
        let mut params = boxworks_text::Params::plain_tex_defaults();
        if let Some(ref s) = self.space_skip {
            params.space_skip = parse_glue(s)?;
        }
        if let Some(ref s) = self.extra_space_skip {
            params.extra_space_skip = parse_glue(s)?;
        }
        let tex_engine: Option<Box<dyn bwt::TexEngine>> = if let Some(ref name) = self.tex_engine {
            Some(bwt::new_tex_engine_binary(name.clone()).map_err(|err| format!["{err}"])?)
        } else {
            None
        };
        let labels = make_labels(self.texts.len(), num_direct, &file_line_numbers);
        let hboxs = match tex_engine {
            Some(engine) => run_tex_hboxs(
                engine.as_ref(),
                self.texts,
                self.font_metrics,
                self.hyphenate,
                params,
            )?,
            None => run_box_hboxs(self.texts, self.font_metrics, self.hyphenate, params)?,
        };
        print_hboxs(hboxs, labels);
        Ok(())
    }
}

/// Break text into lines using a TeX engine and print the result.
#[derive(Parser)]
struct Linebreak {
    /// The texts to break into lines.
    texts: Vec<String>,

    /// Font metrics file to use.
    #[clap(short, long)]
    font_metrics: Option<PathBuf>,

    /// Use a TeX engine to build the lists (e.g. `tex`, `pdftex`).
    #[clap(long)]
    tex_engine: Option<String>,

    /// Path to a file containing texts to convert, one per line.
    ///
    /// Empty lines are ignored. Each non-empty line is converted into a separate horizontal list.
    #[clap(long)]
    texts_file: Option<PathBuf>,

    /// Width of the vertical list, e.g. "100pt" or "6.5in".
    #[clap(long)]
    width: Option<String>,

    /// Widths of successive lines as a comma-separated list, e.g. "100pt,200pt,300pt". The last value repeats for all remaining lines.
    #[clap(long)]
    widths: Option<String>,

    /// Demerits for adjacent lines with incompatible fitness classes (default: 10000).
    #[clap(long)]
    adj_demerits: Option<i32>,

    /// Demerits for two consecutive hyphenated lines (default: 10000).
    #[clap(long)]
    double_hyphen_demerits: Option<i32>,

    /// Penalty for an explicit hyphen (default: 50).
    #[clap(long)]
    ex_hyphen_penalty: Option<i32>,

    /// Demerits if the second-to-last line ends with a hyphen (default: 5000).
    #[clap(long)]
    final_hyphen_demerits: Option<i32>,

    /// Penalty for a discretionary hyphen (default: 50).
    #[clap(long)]
    hyphen_penalty: Option<i32>,

    /// Glue added to the left of every line, e.g. "6pt" or "0pt plus 1fil" (default: "0pt").
    #[clap(long)]
    left_skip: Option<String>,

    /// Penalty added to each line's badness before squaring (default: 10).
    #[clap(long)]
    line_penalty: Option<i32>,

    /// Desired number of extra lines relative to the optimum (default: 0).
    #[clap(long)]
    looseness: Option<i32>,

    /// Glue appended to the last line of a paragraph (default: "0pt plus 1fil").
    #[clap(long)]
    par_fill_skip: Option<String>,

    /// Badness tolerance for the first pass (no hyphenation) (default: 100).
    #[clap(long)]
    pre_tolerance: Option<i32>,

    /// Glue added to the right of every line, e.g. "6pt" or "0pt plus 1fil" (default: "0pt").
    #[clap(long)]
    right_skip: Option<String>,

    /// Badness tolerance for the second pass (with hyphenation) (default: 200).
    #[clap(long)]
    tolerance: Option<i32>,

    /// Glue between words.
    ///
    /// If zero (the default), TeX uses the current font's default spacing, potentially modified by the space factor.
    ///
    /// Corresponds to the TeX primitive \spaceskip.
    #[clap(long)]
    space_skip: Option<String>,

    /// Extra wide glue between words - e.g. after a period.
    ///
    /// Corresponds to the TeX primitive \xspaceskip.
    #[clap(long)]
    extra_space_skip: Option<String>,

    /// Print the broken text instead of the box representation.
    #[clap(long)]
    output_text: bool,
}

impl Linebreak {
    fn run(mut self) -> Result<(), String> {
        if let Some(ref path) = self.texts_file.clone() {
            let content = match fs::read_to_string(path) {
                Ok(s) => s,
                Err(err) => return Err(format!["failed to open file {:?}: {err}", path]),
            };
            for line in content.lines() {
                if !line.is_empty() {
                    self.texts.push(line.to_owned());
                }
            }
        }
        let mut text_params = boxworks_text::Params::plain_tex_defaults();
        if let Some(ref s) = self.space_skip {
            text_params.space_skip = parse_glue(s)?;
        }
        if let Some(ref s) = self.extra_space_skip {
            text_params.extra_space_skip = parse_glue(s)?;
        }

        let mut params = boxworks_knuthplass::Params::plain_tex_defaults();
        if let Some(v) = self.adj_demerits {
            params.adj_demerits = v;
        }
        if let Some(v) = self.double_hyphen_demerits {
            params.double_hyphen_demerits = v;
        }
        if let Some(v) = self.ex_hyphen_penalty {
            params.ex_hyphen_penalty = v;
        }
        if let Some(v) = self.final_hyphen_demerits {
            params.final_hyphen_demerits = v;
        }
        if let Some(v) = self.hyphen_penalty {
            params.hyphen_penalty = v;
        }
        if let Some(ref s) = self.left_skip {
            params.left_skip = parse_glue(s)?;
        }
        if let Some(v) = self.line_penalty {
            params.line_penalty = v;
        }
        if let Some(v) = self.looseness {
            params.looseness = v;
        }
        if let Some(ref s) = self.par_fill_skip {
            params.par_fill_skip = parse_glue(s)?;
        }
        if let Some(v) = self.pre_tolerance {
            params.pre_tolerance = v;
        }
        if let Some(ref s) = self.right_skip {
            params.right_skip = parse_glue(s)?;
        }
        if let Some(v) = self.tolerance {
            params.tolerance = v;
        }

        let widths: Vec<common::Scaled> = match (self.width, self.widths) {
            (Some(_), Some(_)) => return Err("--width and --widths are mutually exclusive".into()),
            (None, None) => return Err("one of --width or --widths is required".into()),
            (Some(w), None) => vec![common::Scaled::parse_from_string(&w)?],
            (None, Some(s)) => s
                .split(',')
                .map(|s| common::Scaled::parse_from_string(s.trim()))
                .collect::<Result<_, _>>()?,
        };

        let vlists = match self.tex_engine {
            None => run_box_vlists(self.texts, self.font_metrics, &widths, text_params, &params)?,
            Some(tex_engine) => {
                let engine =
                    bwt::new_tex_engine_binary(tex_engine).map_err(|err| format!["{err}"])?;
                run_tex_vlists(
                    engine.as_ref(),
                    self.texts,
                    self.font_metrics,
                    &widths,
                    text_params,
                    &params,
                )?
            }
        };
        if self.output_text {
            for vlist in vlists {
                print_vlist_text(&vlist);
            }
        } else {
            let labels = vec![None; vlists.len()];
            print_vlists(vlists, labels);
        }
        Ok(())
    }
}

fn print_vlist_text(vlist: &bwl::ast::VBox<'_>) {
    for elem in &vlist.content.value {
        if let bwl::ast::Vertical::HBox(hbox) = elem {
            let mut line = String::new();
            for h in &hbox.content.value {
                match h {
                    bwl::ast::Horizontal::Chars(c) => line.push_str(&c.content.value),
                    bwl::ast::Horizontal::Ligature(l) => line.push_str(&l.original_chars.value),
                    bwl::ast::Horizontal::Glue(_) => line.push(' '),
                    _ => {}
                }
            }
            println!("{line}");
        }
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

\tracingparagraphs=1

% Boxworks does not yet append the \overfullrule rule to overfull boxes
% (TeX.2021.666), so suppress it in TeX's output for now.
\hfuzz=\maxdimen

            \font \customFont {file_stem}

            \customFont
            "
    ]);
    auxiliary_files.insert(file_name, source);
    Ok((auxiliary_files, preamble))
}

fn run_box_hboxs(
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    hyphenated: bool,
    params: boxworks_text::Params,
) -> Result<Vec<bwl::ast::HBox<'static>>, String> {
    let (tfm_bytes, _) = load_font_metrics(font_metrics)?;
    let mut tfm_file = tfm::File::deserialize(&tfm_bytes).0.unwrap();
    let lig_kern_program = tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
    let mut tp = boxworks_text::TextPreprocessorImpl::new(params);
    tp.register_font(0, &tfm_file, lig_kern_program.clone());
    tp.activate_font(0);
    let mut font_repo: boxworks_text::TfmFontRepo = Default::default();
    font_repo.register_font(0, tfm_file);
    use boxworks::ds;
    let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);
    let raw: Vec<ds::HBox> = texts
        .into_iter()
        .map(|text| {
            let mut got = vec![];
            tp.add_text(&text, &mut got);
            let box_1 = ds::HBox::pack(
                &font_repo,
                got.clone(),
                ds::PackWidth::Additional(common::Scaled::ZERO),
            );
            if !hyphenated {
                return box_1;
            }
            use boxworks::Hyphenator;
            hyphenator.hyphenate(&mut got);
            let mut box_2 = ds::HBox::pack(
                &font_repo,
                got.clone(),
                ds::PackWidth::Additional(common::Scaled::ZERO),
            );
            box_2.width = box_1.width;
            box_2.glue_order = box_1.glue_order;
            box_2.glue_ratio = box_1.glue_ratio;
            box_2
        })
        .collect();
    use bwl::convert::ToBoxLang;
    Ok(raw.into_iter().map(|got| got.to_box_lang()).collect())
}

fn run_tex_hboxs(
    tex_engine: &dyn bwt::TexEngine,
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    hyphenated: bool,
    params: boxworks_text::Params,
) -> Result<Vec<bwl::ast::HBox<'static>>, String> {
    let (auxiliary_files, mut preamble) = build_tex_context(font_metrics)?;
    preamble.push_str(&params.tex());
    let (fonts, hboxs) = bwt::build_horizontal_lists(
        tex_engine,
        &auxiliary_files,
        &preamble,
        &mut texts.iter(),
        hyphenated,
    );
    _ = fonts;
    use bwl::convert::ToBoxLang;
    Ok(hboxs.into_iter().map(|l| l.to_box_lang()).collect())
}

fn run_box_vlists(
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    widths: &[common::Scaled],
    text_params: boxworks_text::Params,
    params: &boxworks_knuthplass::Params,
) -> Result<Vec<bwl::ast::VBox<'static>>, String> {
    let (tfm_bytes, _) = load_font_metrics(font_metrics)?;
    let mut tfm_file = tfm::File::deserialize(&tfm_bytes).0.unwrap();
    let lig_kern_program = tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
    let mut tp = boxworks_text::TextPreprocessorImpl::new(text_params);
    tp.register_font(0, &tfm_file, lig_kern_program.clone());
    tp.activate_font(0);
    let mut font_repo: boxworks_text::TfmFontRepo = Default::default();
    font_repo.register_font(0, tfm_file);

    let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);
    use boxworks::ds;
    let raw: Vec<ds::VBox> = texts
        .into_iter()
        .map(|text| {
            let lb = boxworks_knuthplass::LineBreaker {
                params,
                line_widths: widths,
                line_indents: &[],
                debug_logger: None,
                hyphenator: &hyphenator,
            };
            let mut vlist = vec![];

            let mut h_list = vec![];
            tp.add_text(&text, &mut h_list);
            lb.break_line(&font_repo, &mut vlist, &mut h_list);
            ds::VBox {
                height: common::Scaled::ZERO,
                width: common::Scaled::ZERO,
                depth: common::Scaled::ZERO,
                shift_amount: common::Scaled::ZERO,
                list: vlist,
                glue_ratio: Default::default(),
                glue_order: Default::default(),
            }
        })
        .collect();
    use bwl::convert::ToBoxLang;

    Ok(raw.into_iter().map(|got| got.to_box_lang()).collect())
}

fn run_tex_vlists(
    tex_engine: &dyn bwt::TexEngine,
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    widths: &[common::Scaled],
    text_params: boxworks_text::Params,
    params: &boxworks_knuthplass::Params,
) -> Result<Vec<bwl::ast::VBox<'static>>, String> {
    let (auxiliary_files, mut preamble) = build_tex_context(font_metrics)?;
    preamble.push_str(&text_params.tex());
    preamble.push_str(&params.tex());
    let (_, vlists) = bwt::build_vertical_lists(
        tex_engine,
        &auxiliary_files,
        &preamble,
        widths,
        &mut texts.iter(),
    );
    use bwl::convert::ToBoxLang;
    Ok(vlists.into_iter().map(|l| l.to_box_lang()).collect())
}

fn print_hboxs(hboxs: Vec<bwl::ast::HBox<'static>>, labels: Vec<Option<usize>>) {
    for (i, (hbox, label)) in hboxs.into_iter().zip(labels).enumerate() {
        println!("#");
        match label {
            Some(line_num) => println!("# hbox {} (line {})", i + 1, line_num),
            None => println!("# hbox {}", i + 1),
        }
        println!("{}", bwl::ast::Horizontal::HBox(hbox));
    }
}

fn print_vlists(vlists: Vec<bwl::ast::VBox<'static>>, labels: Vec<Option<usize>>) {
    for (i, (vlist, label)) in vlists.into_iter().zip(labels).enumerate() {
        println!("#");
        match label {
            Some(line_num) => println!("# vlist {} (line {})", i + 1, line_num),
            None => println!("# vlist {}", i + 1),
        }
        println!("{}", bwl::ast::Horizontal::VBox(vlist));
    }
}

fn parse_glue(s: &str) -> Result<common::Glue, String> {
    let mut glue = common::Glue::ZERO;
    let (width_str, rest) = match s.find(" plus ").or_else(|| s.find(" minus ")) {
        Some(pos) => (&s[..pos], s[pos..].trim()),
        None => (s, ""),
    };
    glue.width = common::Scaled::parse_from_string(width_str.trim())?;
    let rest = if let Some(r) = rest.strip_prefix("plus ") {
        let (stretch_str, minus_rest) = match r.find(" minus ") {
            Some(pos) => (&r[..pos], r[pos..].trim()),
            None => (r, ""),
        };
        let (stretch, order) = parse_scaled_inf(stretch_str.trim())?;
        glue.stretch = stretch;
        glue.stretch_order = order;
        minus_rest
    } else {
        rest
    };
    if let Some(shrink_str) = rest.strip_prefix("minus ") {
        let (shrink, order) = parse_scaled_inf(shrink_str.trim())?;
        glue.shrink = shrink;
        glue.shrink_order = order;
    } else if !rest.is_empty() {
        return Err(format!("invalid glue {s:?}"));
    }
    Ok(glue)
}

fn parse_scaled_inf(s: &str) -> Result<(common::Scaled, common::GlueOrder), String> {
    for (suffix, order) in [
        ("filll", common::GlueOrder::Filll),
        ("fill", common::GlueOrder::Fill),
        ("fil", common::GlueOrder::Fil),
    ] {
        if let Some(num_str) = s.strip_suffix(suffix) {
            return Ok((
                common::Scaled::parse_from_string(&format!("{num_str}pt"))?,
                order,
            ));
        }
    }
    Ok((
        common::Scaled::parse_from_string(s)?,
        common::GlueOrder::Normal,
    ))
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
