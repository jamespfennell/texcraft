use crate::shared;
use boxworks::ds;
use boxworks::tex as bwt;
use boxworks::TextPreprocessor;
use clap::Parser;
use common::font;
use std::fs;
use std::path::PathBuf;

/// Preprocess some text into horizontal lists and print them.
///
/// By default, Box builds the lists. If --tex_engine is specified, the given TeX engine is used instead.
#[derive(Parser)]
pub struct Command {
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

impl Command {
    pub fn run(mut self) -> Result<(), String> {
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
            params.space_skip = common::Glue::parse_from_string(s)?;
        }
        if let Some(ref s) = self.extra_space_skip {
            params.extra_space_skip = common::Glue::parse_from_string(s)?;
        }
        let tex_engine: Option<Box<dyn bwt::TexEngine>> = if let Some(ref name) = self.tex_engine {
            Some(bwt::new_tex_engine_binary(name.clone()).map_err(|err| format!["{err}"])?)
        } else {
            None
        };
        let labels = make_labels(self.texts.len(), num_direct, &file_line_numbers);
        let hboxs = match tex_engine {
            Some(mut engine) => run_tex_hboxs(
                engine.as_mut(),
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

fn run_box_hboxs(
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    hyphenated: bool,
    params: boxworks_text::Params,
) -> Result<Vec<ds::HBox>, String> {
    let (tfm_bytes, _) = shared::load_font_metrics(font_metrics)?;
    let mut tfm_file = tfm::File::deserialize(&tfm_bytes).0.unwrap();
    let lig_kern_program = tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
    let mut tp = boxworks_text::TextPreprocessorImpl::new(params);
    tp.register_font(font::Id::ONE, &tfm_file, lig_kern_program.clone());
    tp.activate_font(font::Id::ONE);
    let mut font_repo: boxworks_text::TfmFontRepo = Default::default();
    font_repo.register_font(font::Id::ONE, tfm_file);
    use boxworks::ds;
    let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);
    Ok(texts
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
        .collect())
}

fn run_tex_hboxs(
    tex_engine: &mut dyn bwt::TexEngine,
    texts: Vec<String>,
    font_metrics: Option<PathBuf>,
    hyphenated: bool,
    params: boxworks_text::Params,
) -> Result<Vec<ds::HBox>, String> {
    let (auxiliary_files, mut preamble) = shared::build_tex_context(font_metrics)?;
    preamble.push_str(&params.tex());
    Ok(bwt::build_horizontal_lists(
        tex_engine,
        &auxiliary_files,
        &preamble,
        &mut texts.iter(),
        hyphenated,
    )
    .1)
}

fn print_hboxs(hboxs: Vec<ds::HBox>, labels: Vec<Option<usize>>) {
    for (i, (hbox, label)) in hboxs.into_iter().zip(labels).enumerate() {
        println!("#");
        match label {
            Some(line_num) => println!("# hbox {} (line {})", i + 1, line_num),
            None => println!("# hbox {}", i + 1),
        }
        println!("{hbox}");
    }
}
