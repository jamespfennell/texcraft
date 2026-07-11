//! WebAssembly bindings for knuthplass.dev.
//!
//! Exposes the Knuth-Plass line breaker from `boxworks-knuthplass` as a single
//! `break_paragraph` function that returns positioned elements as JSON. All
//! positions are computed here in pt so the JavaScript side only places
//! elements; it never measures text.

use boxworks::ds;
use boxworks::LineBreaker as _;
use boxworks::TextPreprocessor as _;
use common::Scaled;
use wasm_bindgen::prelude::*;

const CMR10_TFM: &[u8] = include_bytes!("../../../../crates/tfm/corpus/computer-modern/cmr10.tfm");

// Plain TeX vertical spacing defaults, used to compute line baselines.
const BASELINE_SKIP_PT: f64 = 12.0;
const LINE_SKIP_PT: f64 = 1.0;
const LINE_SKIP_LIMIT_PT: f64 = 0.0;

/// Breaks a paragraph of text into lines and returns the typeset result as JSON.
///
/// `params_json` holds the fields of [`boxworks_knuthplass::Params`] plus
/// `width_pt` and the [`boxworks_text::Params`] glue fields `space_skip` and
/// `extra_space_skip`; omitted fields take their plain TeX default values.
/// Glue parameters are strings in TeX glue syntax, e.g. `"0pt plus 1fil"`.
///
/// Note: the line breaker panics if no feasible solution exists within
/// `tolerance` (the emergency pass is not yet implemented). A panic surfaces
/// in JavaScript as a thrown `RuntimeError`, so callers should wrap this in
/// try/catch until that is fixed upstream.
#[wasm_bindgen]
pub fn break_paragraph(text: &str, params_json: &str) -> String {
    match break_paragraph_impl(text, params_json) {
        Ok(output) => serde_json::to_string(&output).unwrap(),
        Err(msg) => serde_json::to_string(&ErrorOutput { error: msg }).unwrap(),
    }
}

#[derive(serde::Deserialize, Default)]
#[serde(deny_unknown_fields)]
struct ParamsInput {
    width_pt: Option<f64>,
    adj_demerits: Option<i32>,
    broken_penalty: Option<i32>,
    club_penalty: Option<i32>,
    double_hyphen_demerits: Option<i32>,
    ex_hyphen_penalty: Option<i32>,
    final_hyphen_demerits: Option<i32>,
    final_widow_penalty: Option<i32>,
    hyphen_penalty: Option<i32>,
    inter_line_penalty: Option<i32>,
    left_skip: Option<String>,
    line_penalty: Option<i32>,
    looseness: Option<i32>,
    par_fill_skip: Option<String>,
    pre_tolerance: Option<i32>,
    right_skip: Option<String>,
    space_skip: Option<String>,
    extra_space_skip: Option<String>,
    tolerance: Option<i32>,
}

#[derive(serde::Serialize)]
struct ErrorOutput {
    error: String,
}

#[derive(serde::Serialize)]
struct Output {
    lines: Vec<Line>,
    /// How many passes the breaker ran: 1 if the first (no-hyphenation)
    /// pass succeeded, 2 if the hyphenating pass ran. 0 for empty input.
    passes: u8,
}

#[derive(serde::Serialize)]
struct Line {
    /// Baseline y position, growing downwards from 0 at the top of the paragraph.
    baseline_pt: f64,
    width_pt: f64,
    height_pt: f64,
    depth_pt: f64,
    /// Badness of the line's glue setting, approximated as 100·|ratio|³
    /// (TeX's formula, without its fixed-point rounding). 1000000 for an
    /// overfull line; 0 when the glue is infinitely stretchable.
    badness: i64,
    /// The line's glue set ratio: positive when stretching, negative when
    /// shrinking. Clamped to -1 for overfull lines.
    glue_ratio: f64,
    /// The glue order the ratio applies to: "normal", "fil", "fill" or
    /// "filll".
    glue_order: &'static str,
    /// The penalty node inserted after this line (TeX.2021.890): the sum
    /// of inter_line_penalty, plus club_penalty after the first line,
    /// final_widow_penalty after the second-to-last, and broken_penalty
    /// after a hyphenated line. 0 when no node was inserted — TeX omits
    /// zero penalties, and nothing follows the last line.
    penalty: i32,
    /// The demerits the breaker charged for this line (TeX.2021.859):
    /// (line_penalty + badness)² plus the break penalty's contribution,
    /// plus the adj/double-hyphen/final-hyphen extras. Recovered from the
    /// chosen active-node chain, so it is the algorithm's own arithmetic,
    /// not a UI approximation.
    demerits: i32,
    /// True when the demerits were artificially zeroed (TeX.2021.854):
    /// the break was forced because no feasible break existed, so the 0
    /// says nothing about the line's quality. False for a genuine 0.
    artificial_demerits: bool,
    elements: Vec<Element>,
}

#[derive(serde::Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum Element {
    Char {
        x_pt: f64,
        width_pt: f64,
        unicode: char,
    },
    Lig {
        x_pt: f64,
        width_pt: f64,
        unicode: char,
        original: String,
    },
    Kern {
        x_pt: f64,
        width_pt: f64,
    },
    Glue {
        x_pt: f64,
        width_pt: f64,
        natural_pt: f64,
        stretch_pt: f64,
        /// "" for normal (finite) stretch, else "fil"/"fill"/"filll".
        stretch_order: &'static str,
        shrink_pt: f64,
        shrink_order: &'static str,
    },
}

/// A debug logger that records which passes ran, plus the active-node data
/// needed to recover the chosen breaks' demerits.
#[derive(Default)]
struct PassRecorder {
    attempts: u8,
    /// node index -> (total_demerits, previous_node_index,
    /// artificial_demerits). Cleared each attempt: node numbering
    /// restarts, and only the successful attempt's nodes matter.
    nodes: std::collections::HashMap<usize, (i32, usize, bool)>,
    /// The node the breaker chose; the chosen breakpoints are its chain of
    /// predecessors.
    selected: Option<usize>,
}

impl PassRecorder {
    /// Demerits charged for each chosen line — a node's total_demerits
    /// minus its predecessor's — paired with whether they were
    /// artificially zeroed (TeX.2021.854). Node 0 is the paragraph start:
    /// it is never logged and its total is 0.
    fn line_demerits(&self) -> Vec<(i32, bool)> {
        let mut v = vec![];
        let Some(mut index) = self.selected else {
            return v;
        };
        while index > 0 {
            let Some(&(total, prev, artificial)) = self.nodes.get(&index) else {
                break;
            };
            let prev_total = match prev {
                0 => 0,
                _ => self.nodes.get(&prev).map_or(0, |&(t, _, _)| t),
            };
            v.push((total - prev_total, artificial));
            index = prev;
        }
        v.reverse();
        v
    }
}

impl boxworks_knuthplass::debug::Logger for PassRecorder {
    fn log_attempt(&mut self, attempt_number: u8) {
        self.attempts = self.attempts.max(attempt_number);
        self.nodes.clear();
        self.selected = None;
    }
    fn log_feasible_breakpoint(
        &mut self,
        _: &[ds::Horizontal],
        _: boxworks_knuthplass::debug::FeasibleBreakpoint,
    ) {
    }
    fn log_new_active_node(&mut self, an: boxworks_knuthplass::debug::NewActiveNode) {
        self.nodes.insert(
            an.node_index,
            (
                an.total_demerits,
                an.previous_node_index,
                an.artificial_demerits,
            ),
        );
    }
    fn log_selected_node(&mut self, node_index: usize) {
        self.selected = Some(node_index);
    }
}

fn order_suffix(order: common::GlueOrder) -> &'static str {
    match order {
        common::GlueOrder::Normal => "",
        common::GlueOrder::Fil => "fil",
        common::GlueOrder::Fill => "fill",
        common::GlueOrder::Filll => "filll",
    }
}

fn break_paragraph_impl(text: &str, params_json: &str) -> Result<Output, String> {
    if text.trim().is_empty() {
        return Ok(Output {
            lines: vec![],
            passes: 0,
        });
    }
    let input: ParamsInput =
        serde_json::from_str(params_json).map_err(|err| format!("invalid parameters: {err}"))?;
    let params = build_params(&input)?;
    let width = match input.width_pt {
        None => return Err("width_pt is required".into()),
        Some(w) if !(1.0..=10000.0).contains(&w) => {
            return Err("width_pt must be between 1 and 10000".into())
        }
        Some(w) => Scaled((w * 65536.0).round() as i32),
    };

    let (tfm_result, _) = tfm::File::deserialize(CMR10_TFM);
    let mut tfm_file = tfm_result.map_err(|err| format!("{err:?}"))?;
    let lig_kern_program = tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
    let mut text_params = boxworks_text::Params::plain_tex_defaults();
    if let Some(ref s) = input.space_skip {
        text_params.space_skip = parse_named_glue("space_skip", s)?;
    }
    if let Some(ref s) = input.extra_space_skip {
        text_params.extra_space_skip = parse_named_glue("extra_space_skip", s)?;
    }
    let mut tp = boxworks_text::TextPreprocessorImpl::new(text_params);
    tp.register_font(0, &tfm_file, lig_kern_program.clone());
    tp.activate_font(0);
    let mut font_repo: boxworks_text::TfmFontRepo = Default::default();
    font_repo.register_font(0, tfm_file);

    // Reject characters the font has no glyph for; they would otherwise be
    // silently dropped or typeset with zero width.
    {
        use boxworks::FontRepo;
        let mut missing: Vec<char> = vec![];
        for c in text.chars() {
            if c.is_whitespace() || missing.contains(&c) {
                continue;
            }
            if font_repo.width(c, 0).is_none() {
                missing.push(c);
            }
        }
        if !missing.is_empty() {
            let list: Vec<String> = missing
                .iter()
                .take(5)
                .map(|c| format!("\"{c}\" (U+{:04X})", *c as u32))
                .collect();
            return Err(format!(
                "the text contains character{} not in the Computer Modern font: {}",
                if missing.len() == 1 { "" } else { "s" },
                list.join(", "),
            ));
        }
    }

    let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);
    let line_widths = [width];
    let mut pass_recorder = PassRecorder::default();
    let lb = boxworks_knuthplass::LineBreaker {
        params: &params,
        line_widths: &line_widths,
        line_indents: &[],
        debug_logger: Some(&mut pass_recorder),
        hyphenator: &hyphenator,
    };

    let mut h_list = vec![];
    tp.add_text(text, &mut h_list);
    let mut v_list = vec![];
    lb.break_line(&font_repo, &mut v_list, &mut h_list);

    Ok(build_output(
        &v_list,
        &font_repo,
        pass_recorder.attempts,
        &pass_recorder.line_demerits(),
    ))
}

fn build_params(input: &ParamsInput) -> Result<boxworks_knuthplass::Params, String> {
    let mut params = boxworks_knuthplass::Params::plain_tex_defaults();
    macro_rules! set_int {
        ( $( $field: ident, )+ ) => {
            $(
            if let Some(v) = input.$field {
                params.$field = v;
            }
            )+
        };
    }
    set_int!(
        adj_demerits,
        broken_penalty,
        club_penalty,
        double_hyphen_demerits,
        ex_hyphen_penalty,
        final_hyphen_demerits,
        final_widow_penalty,
        hyphen_penalty,
        inter_line_penalty,
        line_penalty,
        looseness,
        pre_tolerance,
        tolerance,
    );
    if let Some(ref s) = input.left_skip {
        params.left_skip = parse_named_glue("left_skip", s)?;
    }
    if let Some(ref s) = input.par_fill_skip {
        params.par_fill_skip = parse_named_glue("par_fill_skip", s)?;
    }
    if let Some(ref s) = input.right_skip {
        params.right_skip = parse_named_glue("right_skip", s)?;
    }
    Ok(params)
}

/// Parses a glue parameter, prefixing any error with the parameter name so
/// the UI can say (and mark) which input is at fault.
fn parse_named_glue(name: &str, s: &str) -> Result<common::Glue, String> {
    parse_glue(s).map_err(|err| format!("{name}: {err}"))
}

fn build_output(
    v_list: &[ds::Vertical],
    font_repo: &boxworks_text::TfmFontRepo,
    passes: u8,
    line_demerits: &[(i32, bool)],
) -> Output {
    let mut lines: Vec<Line> = vec![];
    // Vertical placement follows TeX's append_to_vlist (TeX.2021.679): the
    // gap between a line's baseline and the previous baseline is
    // \baselineskip, unless that would put the boxes closer than
    // \lineskiplimit, in which case \lineskip separates them.
    let mut prev_depth_pt: Option<f64> = None;
    let mut baseline_pt = 0.0;
    for elem in v_list {
        let hbox = match elem {
            ds::Vertical::HBox(hbox) => hbox,
            ds::Vertical::Penalty(p) => {
                if let Some(line) = lines.last_mut() {
                    line.penalty = p.0;
                }
                continue;
            }
            _ => continue,
        };
        let height_pt = pt(hbox.height);
        baseline_pt += match prev_depth_pt {
            None => height_pt,
            Some(prev_depth_pt) => {
                if BASELINE_SKIP_PT - prev_depth_pt - height_pt < LINE_SKIP_LIMIT_PT {
                    prev_depth_pt + LINE_SKIP_PT + height_pt
                } else {
                    BASELINE_SKIP_PT
                }
            }
        };
        prev_depth_pt = Some(pt(hbox.depth));
        let width_pt = pt(hbox.width);
        let (elements, end_x_pt) = build_elements(hbox, font_repo);
        lines.push(Line {
            baseline_pt,
            width_pt,
            height_pt,
            depth_pt: pt(hbox.depth),
            badness: badness(hbox, end_x_pt - width_pt),
            glue_ratio: if hbox.glue_ratio.den == Scaled::ZERO {
                0.0
            } else {
                hbox.glue_ratio.as_float() as f64
            },
            glue_order: match hbox.glue_order {
                common::GlueOrder::Normal => "normal",
                common::GlueOrder::Fil => "fil",
                common::GlueOrder::Fill => "fill",
                common::GlueOrder::Filll => "filll",
            },
            penalty: 0,
            demerits: line_demerits.get(lines.len()).map_or(0, |&(d, _)| d),
            artificial_demerits: line_demerits.get(lines.len()).is_some_and(|&(_, a)| a),
            elements,
        });
    }
    Output { lines, passes }
}

/// Returns the elements of the line and the x position at which they end.
fn build_elements(hbox: &ds::HBox, font_repo: &boxworks_text::TfmFontRepo) -> (Vec<Element>, f64) {
    use boxworks::FontRepo;
    let mut elements = vec![];
    let mut x_pt = 0.0;
    for elem in &hbox.list {
        match elem {
            ds::Horizontal::Char(c) => {
                let width_pt = pt(font_repo.width(c.char, c.font).unwrap_or(Scaled::ZERO));
                elements.push(Element::Char {
                    x_pt,
                    width_pt,
                    unicode: ot1_to_unicode(c.char),
                });
                x_pt += width_pt;
            }
            ds::Horizontal::Ligature(l) => {
                let width_pt = pt(font_repo.width(l.char, l.font).unwrap_or(Scaled::ZERO));
                elements.push(Element::Lig {
                    x_pt,
                    width_pt,
                    unicode: ot1_to_unicode(l.char),
                    original: l.original_chars.to_string(),
                });
                x_pt += width_pt;
            }
            ds::Horizontal::Kern(k) => {
                let width_pt = pt(k.width);
                elements.push(Element::Kern { x_pt, width_pt });
                x_pt += width_pt;
            }
            ds::Horizontal::Glue(g) => {
                let width_pt = set_glue_width_pt(hbox, &g.value);
                elements.push(Element::Glue {
                    x_pt,
                    width_pt,
                    natural_pt: pt(g.value.width),
                    stretch_pt: pt(g.value.stretch),
                    stretch_order: order_suffix(g.value.stretch_order),
                    shrink_pt: pt(g.value.shrink),
                    shrink_order: order_suffix(g.value.shrink_order),
                });
                x_pt += width_pt;
            }
            // Unchosen discretionaries, penalties, etc. take up no space.
            _ => {}
        }
    }
    (elements, x_pt)
}

/// The width of a glue element after the line's stretch/shrink ratio is applied.
fn set_glue_width_pt(hbox: &ds::HBox, glue: &common::Glue) -> f64 {
    let mut width_pt = pt(glue.width);
    let ratio = hbox.glue_ratio;
    if ratio.den == Scaled::ZERO {
        return width_pt;
    }
    let r = ratio.as_float() as f64;
    if ratio.num > Scaled::ZERO {
        if glue.stretch_order == hbox.glue_order {
            width_pt += r * pt(glue.stretch);
        }
    } else if ratio.num < Scaled::ZERO && glue.shrink_order == hbox.glue_order {
        // r is negative here, so this shrinks the glue.
        width_pt += r * pt(glue.shrink);
    }
    width_pt
}

fn badness(hbox: &ds::HBox, overflow_pt: f64) -> i64 {
    // Overfull boxes have their glue ratio clamped to unity during packing,
    // so they are detected by their content overflowing the line width.
    if overflow_pt > 0.001 {
        return 1000000;
    }
    if hbox.glue_order != common::GlueOrder::Normal || hbox.glue_ratio.den == Scaled::ZERO {
        return 0;
    }
    let r = hbox.glue_ratio.as_float() as f64;
    (100.0 * r.abs().powi(3)).round().min(10000.0) as i64
}

fn pt(v: Scaled) -> f64 {
    v.0 as f64 / 65536.0
}

/// Maps a cmr10 character code to the Unicode character with the same glyph,
/// for display in a Unicode Computer Modern webfont (Latin Modern).
///
/// cmr10 uses the OT1 text encoding, in which most ASCII codes are themselves
/// but ligatures, dashes and a few other glyphs live at other positions.
fn ot1_to_unicode(c: char) -> char {
    match c {
        '\u{0B}' => 'ﬀ',
        '\u{0C}' => 'ﬁ',
        '\u{0D}' => 'ﬂ',
        '\u{0E}' => 'ﬃ',
        '\u{0F}' => 'ﬄ',
        '\u{10}' => 'ı',
        '\u{11}' => 'ȷ',
        '\u{19}' => 'ß',
        '\u{1A}' => 'æ',
        '\u{1B}' => 'œ',
        '\u{1C}' => 'ø',
        '\u{1D}' => 'Æ',
        '\u{1E}' => 'Œ',
        '\u{1F}' => 'Ø',
        '"' => '\u{201D}', // '' ligature produces the code of "
        '<' => '¡',
        '>' => '¿',
        '\\' => '\u{201C}', // `` ligature produces the code of \
        '_' => '˙',
        '`' => '\u{2018}',
        '\'' => '\u{2019}',
        '{' => '\u{2013}', // -- ligature produces the code of {
        '|' => '\u{2014}', // --- ligature produces the code of |
        '}' => '˝',
        '~' => '˜',
        c => c,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ALICE: &str = "Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, \"and what is the use of a book,\" thought Alice \"without pictures or conversations?\"";

    fn run(text: &str, params: &str) -> serde_json::Value {
        serde_json::from_str(&break_paragraph(text, params)).unwrap()
    }

    #[test]
    fn error_on_missing_width() {
        let v = run("hello world", "{}");
        assert!(v["error"].as_str().unwrap().contains("width_pt"));
    }

    #[test]
    fn error_on_unknown_param() {
        let v = run("hello world", r#"{"width_pt":300,"tollerance":10}"#);
        assert!(v["error"].as_str().unwrap().contains("invalid parameters"));
    }

    #[test]
    fn error_on_characters_not_in_font() {
        let v = run("cafés and a goat’s chin tuft", r#"{"width_pt":300}"#);
        let error = v["error"].as_str().unwrap();
        assert!(error.contains("not in the Computer Modern font"), "{error}");
        assert!(error.contains("U+00E9"), "{error}");
        assert!(error.contains("U+2019"), "{error}");
    }

    #[test]
    fn ascii_punctuation_is_accepted() {
        // These map to other glyphs in the OT1 encoding but are in the font.
        let v = run("so-called `quotes' and --- dashes?!", r#"{"width_pt":300}"#);
        assert!(v["error"].is_null(), "{v}");
        assert_eq!(v["lines"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn empty_text_gives_no_lines() {
        let v = run("  ", r#"{"width_pt":300}"#);
        assert_eq!(v["lines"].as_array().unwrap().len(), 0);
    }

    #[test]
    fn alice_breaks_into_justified_lines() {
        let v = run(ALICE, r#"{"width_pt":250}"#);
        let lines = v["lines"].as_array().unwrap();
        assert!(
            lines.len() > 5,
            "expected several lines, got {}",
            lines.len()
        );

        for (i, line) in lines.iter().enumerate() {
            let width = line["width_pt"].as_f64().unwrap();
            assert_eq!(width, 250.0);
            let elements = line["elements"].as_array().unwrap();
            assert!(!elements.is_empty());

            // x positions must advance by exactly each element's width.
            let mut x = 0.0;
            for e in elements {
                let got = e["x_pt"].as_f64().unwrap();
                assert!(
                    (got - x).abs() < 1e-9,
                    "line {i}: expected x {x}, got {got}"
                );
                x += e["width_pt"].as_f64().unwrap();
            }
            // Every line except the last is justified: the elements must fill
            // the line exactly. (The last line ends with \parfillskip glue
            // whose set width absorbs the remainder, so it also sums to the
            // line width.)
            assert!(
                (x - width).abs() < 0.01,
                "line {i}: elements end at {x}, want {width}"
            );
        }

        // Baselines are single-spaced at \baselineskip=12pt for cmr10 text.
        let b0 = lines[0]["baseline_pt"].as_f64().unwrap();
        let b1 = lines[1]["baseline_pt"].as_f64().unwrap();
        assert!((b1 - b0 - 12.0).abs() < 1e-9);
    }

    #[test]
    fn ligatures_are_reported_with_originals() {
        let v = run("the office is difficult to fly to", r#"{"width_pt":500}"#);
        let lines = v["lines"].as_array().unwrap();
        assert_eq!(lines.len(), 1);
        let ligs: Vec<(&str, &str)> = lines[0]["elements"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|e| e["type"] == "lig")
            .map(|e| {
                (
                    e["unicode"].as_str().unwrap(),
                    e["original"].as_str().unwrap(),
                )
            })
            .collect();
        assert!(ligs.contains(&("ﬃ", "ffi")), "got {ligs:?}");
        assert!(ligs.contains(&("ﬃ", "ffi")) || ligs.contains(&("ﬁ", "fi")));
        assert!(ligs.contains(&("ﬂ", "fl")), "got {ligs:?}");
    }

    #[test]
    fn kerns_are_reported() {
        // cmr10 kerns the pair "av".
        let v = run("avast", r#"{"width_pt":500}"#);
        let elements = v["lines"][0]["elements"].as_array().unwrap();
        assert!(
            elements.iter().any(|e| e["type"] == "kern"),
            "expected a kern in {elements:?}"
        );
    }

    #[test]
    fn space_skip_replaces_interword_glue() {
        let default = run("a b", r#"{"width_pt":500}"#);
        let custom = run(
            "a b",
            r#"{"width_pt":500,"space_skip":"20pt plus 5pt minus 2pt"}"#,
        );
        let glue_natural = |v: &serde_json::Value| -> f64 {
            v["lines"][0]["elements"]
                .as_array()
                .unwrap()
                .iter()
                .find(|e| e["type"] == "glue")
                .unwrap()["natural_pt"]
                .as_f64()
                .unwrap()
        };
        assert_ne!(glue_natural(&default), 20.0);
        assert_eq!(glue_natural(&custom), 20.0);
    }

    #[test]
    fn extra_space_skip_applies_after_periods() {
        // The space after the period has space factor >= 2000, so
        // extra_space_skip replaces it; the space between "a b" is unaffected.
        let v = run("end. a b", r#"{"width_pt":500,"extra_space_skip":"30pt"}"#);
        let naturals: Vec<f64> = v["lines"][0]["elements"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|e| e["type"] == "glue")
            .map(|e| e["natural_pt"].as_f64().unwrap())
            .collect();
        // The first two glues are the word spaces; the rest are zero-width
        // line glues (right_skip, par_fill_skip).
        assert_eq!(naturals[0], 30.0, "{naturals:?}");
        assert!(naturals[1] > 0.0, "{naturals:?}");
        assert_ne!(naturals[1], 30.0, "{naturals:?}");
    }

    #[test]
    fn glue_errors_name_the_parameter() {
        let v = run("hello world", r#"{"width_pt":300,"right_skip":"garbage"}"#);
        let error = v["error"].as_str().unwrap();
        assert!(error.starts_with("right_skip:"), "{error}");
    }

    #[test]
    fn passes_reports_whether_hyphenation_ran() {
        let first_pass = run(ALICE, r#"{"width_pt":250}"#);
        assert_eq!(first_pass["passes"], 1);
        // pre_tolerance -1 makes the first pass always fail.
        let second_pass = run(ALICE, r#"{"width_pt":250,"pre_tolerance":-1}"#);
        assert_eq!(second_pass["passes"], 2);
        let empty = run(" ", r#"{"width_pt":250}"#);
        assert_eq!(empty["passes"], 0);
    }

    #[test]
    fn per_line_penalties_are_reported() {
        // Width 250 succeeds on the first pass (no hyphenated lines, so
        // broken_penalty never applies): the first line carries
        // club_penalty, the second-to-last final_widow_penalty, every
        // non-final line inter_line_penalty, and the last line nothing.
        let v = run(
            ALICE,
            r#"{"width_pt":250,"inter_line_penalty":7,"club_penalty":150,"final_widow_penalty":150}"#,
        );
        let penalties: Vec<i64> = v["lines"]
            .as_array()
            .unwrap()
            .iter()
            .map(|l| l["penalty"].as_i64().unwrap())
            .collect();
        let n = penalties.len();
        assert!(n >= 4, "{penalties:?}");
        assert_eq!(penalties[0], 157, "{penalties:?}");
        assert_eq!(penalties[1], 7, "{penalties:?}");
        assert_eq!(penalties[n - 2], 157, "{penalties:?}");
        assert_eq!(penalties[n - 1], 0, "{penalties:?}");
    }

    #[test]
    fn per_line_demerits_are_reported() {
        // A single line breaks at the forced final break (eject penalty,
        // which contributes nothing) with badness 0 from \parfillskip, so
        // the demerits are exactly (line_penalty + badness)² = (10+0)².
        let v = run("hello world", r#"{"width_pt":500}"#);
        assert_eq!(v["lines"][0]["demerits"], 100);
        // Multi-line: every line carries the algorithm's demerits.
        let v = run(ALICE, r#"{"width_pt":250}"#);
        for line in v["lines"].as_array().unwrap() {
            assert!(line["demerits"].as_i64().unwrap() > 0, "{line}");
            assert_eq!(line["artificial_demerits"], false, "{line}");
        }
    }

    #[test]
    fn artificial_demerits_are_flagged() {
        // An impossible tolerance forces a solution; the forced breaks get
        // artificially zeroed demerits (TeX.2021.854) and are flagged so
        // the UI can distinguish them from genuine zeros.
        let v = run(
            ALICE,
            r#"{"width_pt":100,"tolerance":1,"pre_tolerance":1}"#,
        );
        let lines = v["lines"].as_array().unwrap();
        assert!(
            lines.iter().any(|l| l["artificial_demerits"] == true),
            "expected at least one forced break"
        );
        for line in lines {
            if line["artificial_demerits"] == true {
                assert_eq!(line["demerits"], 0, "{line}");
            }
        }
    }

    #[test]
    fn narrower_width_gives_more_lines() {
        let narrow = run(ALICE, r#"{"width_pt":150}"#);
        let wide = run(ALICE, r#"{"width_pt":400}"#);
        assert!(
            narrow["lines"].as_array().unwrap().len() > wide["lines"].as_array().unwrap().len()
        );
    }
}

// The glue parsing below is copied from `box linebreak`
// (crates/boxworks-bin/src/box.rs); the two should stay in sync.

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
