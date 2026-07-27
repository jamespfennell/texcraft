use crate::shared;
use boxworks::ds;
use boxworks::tex as bwt;
use boxworks::LineBreaker;
use boxworks::TextPreprocessor;
use clap::Parser;
use std::fs;
use std::path::PathBuf;

/// Break text into lines and print the result.
#[derive(Parser)]
pub struct Command {
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

    /// Penalty added to the vertical list after a line ending in a hyphen (default: 100).
    #[clap(long)]
    broken_penalty: Option<i32>,

    /// Penalty added to the vertical list after the first line (default: 150).
    #[clap(long)]
    club_penalty: Option<i32>,

    /// Demerits for two consecutive hyphenated lines (default: 10000).
    #[clap(long)]
    double_hyphen_demerits: Option<i32>,

    /// Extra stretchability given to every line, e.g. "10pt", if line breaking
    /// fails otherwise (default: "0pt").
    #[clap(long)]
    emergency_stretch: Option<String>,

    /// Penalty for an explicit hyphen (default: 50).
    #[clap(long)]
    ex_hyphen_penalty: Option<i32>,

    /// Demerits if the second-to-last line ends with a hyphen (default: 5000).
    #[clap(long)]
    final_hyphen_demerits: Option<i32>,

    /// Penalty added to the vertical list before the last line (default: 150).
    #[clap(long)]
    final_widow_penalty: Option<i32>,

    /// Penalty for a discretionary hyphen (default: 50).
    #[clap(long)]
    hyphen_penalty: Option<i32>,

    /// Penalty added to the vertical list between each pair of lines (default: 0).
    #[clap(long)]
    inter_line_penalty: Option<i32>,

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

impl Command {
    pub fn run(mut self) -> Result<(), String> {
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
            text_params.space_skip = common::Glue::parse_from_string(s)?;
        }
        if let Some(ref s) = self.extra_space_skip {
            text_params.extra_space_skip = common::Glue::parse_from_string(s)?;
        }

        let mut params = boxworks_knuthplass::Params::plain_tex_defaults();
        if let Some(v) = self.adj_demerits {
            params.adj_demerits = v;
        }
        if let Some(v) = self.broken_penalty {
            params.broken_penalty = v;
        }
        if let Some(v) = self.club_penalty {
            params.club_penalty = v;
        }
        if let Some(v) = self.double_hyphen_demerits {
            params.double_hyphen_demerits = v;
        }
        if let Some(ref s) = self.emergency_stretch {
            params.emergency_stretch = common::Scaled::parse_from_string(s)?;
        }
        if let Some(v) = self.ex_hyphen_penalty {
            params.ex_hyphen_penalty = v;
        }
        if let Some(v) = self.final_hyphen_demerits {
            params.final_hyphen_demerits = v;
        }
        if let Some(v) = self.final_widow_penalty {
            params.final_widow_penalty = v;
        }
        if let Some(v) = self.hyphen_penalty {
            params.hyphen_penalty = v;
        }
        if let Some(v) = self.inter_line_penalty {
            params.inter_line_penalty = v;
        }
        if let Some(ref s) = self.left_skip {
            params.left_skip = common::Glue::parse_from_string(s)?;
        }
        if let Some(v) = self.line_penalty {
            params.line_penalty = v;
        }
        if let Some(v) = self.looseness {
            params.looseness = v;
        }
        if let Some(ref s) = self.par_fill_skip {
            params.par_fill_skip = common::Glue::parse_from_string(s)?;
        }
        if let Some(v) = self.pre_tolerance {
            params.pre_tolerance = v;
        }
        if let Some(ref s) = self.right_skip {
            params.right_skip = common::Glue::parse_from_string(s)?;
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
                let mut engine =
                    bwt::new_tex_engine_binary(tex_engine).map_err(|err| format!["{err}"])?;
                run_tex_vlists(
                    engine.as_mut(),
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

fn run_box_vlists(
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    widths: &[common::Scaled],
    text_params: boxworks_text::Params,
    params: &boxworks_knuthplass::Params,
) -> Result<Vec<ds::VBox>, String> {
    let (tfm_bytes, _) = shared::load_font_metrics(font_metrics)?;
    let mut tfm_file = tfm::File::deserialize(&tfm_bytes).0.unwrap();
    let lig_kern_program = tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
    let mut tp = boxworks_text::TextPreprocessorImpl::new(text_params);
    tp.register_font(0, &tfm_file, lig_kern_program.clone());
    tp.activate_font(0);
    let mut font_repo: boxworks_text::TfmFontRepo = Default::default();
    font_repo.register_font(0, tfm_file);

    let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);
    use boxworks::ds;
    Ok(texts
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
        .collect())
}

fn print_vlist_text(vlist: &ds::VBox) {
    for elem in &vlist.list {
        if let ds::Vertical::HBox(hbox) = elem {
            let mut line = String::new();
            for h in &hbox.list {
                match h {
                    ds::Horizontal::Char(c) => line.push(c.char),
                    ds::Horizontal::Ligature(l) => line.push_str(&l.original_chars),
                    ds::Horizontal::Glue(_) => line.push(' '),
                    _ => {}
                }
            }
            println!("{line}");
        }
    }
}

fn print_vlists(vlists: Vec<ds::VBox>, labels: Vec<Option<usize>>) {
    for (i, (vlist, label)) in vlists.into_iter().zip(labels).enumerate() {
        println!("#");
        match label {
            Some(line_num) => println!("# vlist {} (line {})", i + 1, line_num),
            None => println!("# vlist {}", i + 1),
        }
        println!("{vlist}");
    }
}

fn run_tex_vlists(
    tex_engine: &mut dyn bwt::TexEngine,
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    widths: &[common::Scaled],
    text_params: boxworks_text::Params,
    params: &boxworks_knuthplass::Params,
) -> Result<Vec<ds::VBox>, String> {
    let (auxiliary_files, mut preamble) = shared::build_tex_context(font_metrics)?;
    preamble.push_str(&text_params.tex());
    preamble.push_str(&params.tex());
    // \looseness resets after each paragraph, so the value in the preamble
    // only applies to the first text.
    let texts: Vec<String> = texts
        .into_iter()
        .map(|text| bwt::prepend_looseness(params.looseness, &text))
        .collect();
    let (_, vlists) = bwt::build_vertical_lists(
        tex_engine,
        &auxiliary_files,
        &preamble,
        widths,
        &mut texts.iter(),
    );
    Ok(vlists)
}
