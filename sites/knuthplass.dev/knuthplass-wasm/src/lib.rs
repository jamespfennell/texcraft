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
/// `width_pt`; omitted fields take their plain TeX default values. Glue
/// parameters are strings in TeX glue syntax, e.g. `"0pt plus 1fil"`.
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
    tolerance: Option<i32>,
}

#[derive(serde::Serialize)]
struct ErrorOutput {
    error: String,
}

#[derive(serde::Serialize)]
struct Output {
    lines: Vec<Line>,
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
        shrink_pt: f64,
    },
}

fn break_paragraph_impl(text: &str, params_json: &str) -> Result<Output, String> {
    if text.trim().is_empty() {
        return Ok(Output { lines: vec![] });
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
    let mut tp: boxworks_text::TextPreprocessorImpl = Default::default();
    tp.register_font(0, &tfm_file, lig_kern_program.clone());
    tp.activate_font(0);
    let mut font_repo: boxworks_text::TfmFontRepo = Default::default();
    font_repo.register_font(0, tfm_file);

    let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);
    let line_widths = [width];
    let lb = boxworks_knuthplass::LineBreaker {
        params: &params,
        line_widths: &line_widths,
        line_indents: &[],
        debug_logger: None,
        hyphenator: &hyphenator,
    };

    let mut h_list = vec![];
    tp.add_text(text, &mut h_list);
    let mut v_list = vec![];
    lb.break_line(&font_repo, &mut v_list, &mut h_list);

    Ok(build_output(&v_list, &font_repo))
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
        params.left_skip = parse_glue(s)?;
    }
    if let Some(ref s) = input.par_fill_skip {
        params.par_fill_skip = parse_glue(s)?;
    }
    if let Some(ref s) = input.right_skip {
        params.right_skip = parse_glue(s)?;
    }
    Ok(params)
}

fn build_output(v_list: &[ds::Vertical], font_repo: &boxworks_text::TfmFontRepo) -> Output {
    let mut lines = vec![];
    // Vertical placement follows TeX's append_to_vlist (TeX.2021.679): the
    // gap between a line's baseline and the previous baseline is
    // \baselineskip, unless that would put the boxes closer than
    // \lineskiplimit, in which case \lineskip separates them.
    let mut prev_depth_pt: Option<f64> = None;
    let mut baseline_pt = 0.0;
    for elem in v_list {
        let ds::Vertical::HBox(hbox) = elem else {
            continue;
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
            elements,
        });
    }
    Output { lines }
}

/// Returns the elements of the line and the x position at which they end.
fn build_elements(
    hbox: &ds::HBox,
    font_repo: &boxworks_text::TfmFontRepo,
) -> (Vec<Element>, f64) {
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
                    shrink_pt: pt(g.value.shrink),
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
