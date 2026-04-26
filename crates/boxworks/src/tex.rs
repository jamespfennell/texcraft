//! Tools for converting TeX's internal data structures to Boxworks.
//!
//! Knuth's TeX has a bunch of diagnostic tooling for
//!     inspecting the internal data structures in its typesetting engine.
//! This module contains functions that read the diagnostic output of Knuth's TeX
//!     and reconstruct TeX's internal data structures
//!     as Boxworks data structures.
//! The initial motivation is to verify that
//!     subsystems of TeX and Boxworks
//!     -- like the subsystem that converts text into a horizontal list --
//!     are doing the same thing.
//!
//! These functions require that TeX is installed
//!     because they ultimately invoke TeX to generate the right diagnostic information.

use crate::ds;
use std::{collections::HashMap, path::PathBuf};

/// Implementations of this trait can run TeX source code and return stdout.
pub trait TexEngine {
    /// Run the provided TeX source code and return stdout.
    fn run(&self, tex_source_code: &str, auxiliary_files: &HashMap<PathBuf, Vec<u8>>) -> String;
}

/// Error return when a binary on the host computer was not found.
#[derive(Clone, Debug)]
pub struct BinaryNotFound {
    /// Name of the binary e.g. `tex` or `pdftex`.
    pub binary_name: String,
}

impl std::fmt::Display for BinaryNotFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "binary `{}` not found (`which {}` failed)",
            &self.binary_name, &self.binary_name
        )
    }
}

impl std::error::Error for BinaryNotFound {}

/// A TeX engine binary on the host computer, like `tex` or `pdftex`.
pub fn new_tex_engine_binary(binary_name: String) -> Result<Box<dyn TexEngine>, BinaryNotFound> {
    #[allow(clippy::expect_fun_call)]
    if std::process::Command::new("which")
        .arg(&binary_name)
        .stdout(std::process::Stdio::null())
        .spawn()
        .expect(&format!["`which {binary_name}` command failed to start"])
        .wait()
        .expect(&format!["failed to run `which {binary_name}`"])
        .success()
    {
        Ok(Box::new(TexEngineBinary(binary_name)))
    } else {
        Err(BinaryNotFound { binary_name })
    }
}

struct TexEngineBinary(String);

impl TexEngine for TexEngineBinary {
    fn run(&self, tex_source_code: &str, auxiliary_files: &HashMap<PathBuf, Vec<u8>>) -> String {
        let dir = std::env::temp_dir();

        for (file_name, content) in auxiliary_files {
            let mut path = dir.clone();
            path.push(file_name);
            eprintln!("writing to {}", path.as_os_str().to_string_lossy());
            std::fs::write(&path, content).unwrap_or_else(|_| {
                panic![
                    "Unable to write auxiliary file {}",
                    file_name.to_string_lossy()
                ]
            });
        }

        let mut input_path = dir.clone();
        input_path.push("tex-input");
        input_path.set_extension("tex");
        eprintln!("writing to {}", input_path.as_os_str().to_string_lossy());
        std::fs::write(&input_path, tex_source_code).expect("Unable to write file");

        let output = std::process::Command::new(&self.0)
            .current_dir(&dir)
            .arg(&input_path)
            .output()
            .expect("failed to run tex command");
        eprintln!("{}", String::from_utf8(output.stderr).unwrap());

        let stdout = String::from_utf8(output.stdout).expect("stdout output of TeX is utf-8");
        if !output.status.success() {
            eprintln!("Warning: TeX command seems to have failed. Consult the logs in the log file (replace .tex input file with .log)");
        }
        stdout
    }
}

/// Build horizontal lists from some text.
///
/// This function works by putting the text inside a TeX `\hbox{}`,
///     and then instructing TeX to describe the contents of the box.
///
/// This function is the inverse of TeX.2021.173 and onwards
/// (part 12 of TeX: displaying boxes).
pub fn build_horizontal_lists(
    tex_engine: &dyn TexEngine,
    auxiliary_files: &HashMap<PathBuf, Vec<u8>>,
    preamble: &str,
    contents: &mut dyn Iterator<Item = &String>,
) -> (HashMap<String, u32>, Vec<ds::HList>) {
    let macro_calls: Vec<String> = contents.map(|s| format!(r#"\printBox{{{s}}}"#)).collect();
    let tex_source_code = CONVERT_TEXT_TEMPLATE
        .replace("<preamble>", preamble)
        .replace("<box_template>", r"\hbox{#1}")
        .replace("<print_calls>", &macro_calls.join("\n\n"));
    let output = tex_engine.run(&tex_source_code, auxiliary_files);
    let segments = extract_texcraft_segments(&output);
    let mut fonts: HashMap<String, u32> = Default::default();
    let hlists = segments
        .map(|s| parse_hlist(&mut TexOutputIter::new(s), &mut fonts))
        .collect();
    (fonts, hlists)
}

/// Build verticals list from some text.
///
/// This function works by putting the text inside a TeX
///     `\vbox{\noindent \hsize=<>pt}`
///     and then instructing TeX to describe the contents of the box.
///
/// This function is the inverse of TeX.2021.173 and onwards
/// (part 12 of TeX: displaying boxes).
pub fn build_vertical_lists(
    tex_engine: &dyn TexEngine,
    auxiliary_files: &HashMap<PathBuf, Vec<u8>>,
    preamble: &str,
    width: common::Scaled,
    contents: &mut dyn Iterator<Item = &String>,
) -> (HashMap<String, u32>, Vec<ds::VList>) {
    let macro_calls: Vec<String> = contents.map(|s| format!(r#"\printBox{{{s}}}"#)).collect();
    let tex_source_code = CONVERT_TEXT_TEMPLATE
        .replace("<preamble>", preamble)
        .replace(
            "<box_template>",
            &format![r"\vbox{{\noindent \hsize={} #1}}", width],
        )
        .replace("<print_calls>", &macro_calls.join("\n\n"));
    let output = tex_engine.run(&tex_source_code, auxiliary_files);
    let segments = extract_texcraft_segments(&output);
    let mut fonts: HashMap<String, u32> = Default::default();
    let vlists = segments
        .map(|s| parse_vlist(&mut TexOutputIter::new(s), &mut fonts))
        .collect();
    (fonts, vlists)
}

struct TexOutputIter<'tex> {
    s: &'tex str,
    depth: usize,
}

impl<'tex> Iterator for TexOutputIter<'tex> {
    type Item = &'tex str;

    fn next(&mut self) -> Option<Self::Item> {
        let (line, n) = self.peek_impl()?;
        self.s = &self.s[n..];
        Some(line)
    }
}

impl<'tex> TexOutputIter<'tex> {
    fn new(mut s: &'tex str) -> Self {
        loop {
            let line = s
                .split_inclusive('\n')
                .next()
                .expect("still searching for start of output");
            s = &s[line.len()..];
            if !line.starts_with(r"> \box0=") {
                continue;
            }
            return Self { s, depth: 0 };
        }
    }
    fn inner(&self) -> Self {
        Self {
            s: self.s,
            depth: self.depth + 1,
        }
    }
    fn peek(&mut self) -> Option<&'tex str> {
        let (line, _) = self.peek_impl()?;
        Some(line)
    }
    fn peek_impl(&mut self) -> Option<(&'tex str, usize)> {
        loop {
            let line = self.s.split_inclusive('\n').next()?;
            let n = line.len();
            let line = line.trim_end();
            if line.is_empty() {
                self.s = "";
                return None;
            }
            let line_depth = line.chars().take_while(|&c| c == '.').count();
            use std::cmp::Ordering::*;
            match line_depth.cmp(&self.depth) {
                Less => {
                    self.s = "";
                    return None;
                }
                Equal => {
                    return Some((&line[line_depth..], n));
                }
                Greater => {
                    // skip this line
                    self.s = &self.s[n..];
                }
            }
        }
    }
}

/// Extract the raw content between each pair of "Texcraft: begin" / "Texcraft: end" markers.
///
/// Yields one `&str` slice per begin/end pair, borrowing directly from `output`.
fn extract_texcraft_segments(mut s: &str) -> impl Iterator<Item = &str> {
    std::iter::from_fn(move || {
        // Scan forward to the next "Texcraft: begin" line and record the segment start.
        loop {
            let line = s.split_inclusive('\n').next()?;
            s = &s[line.len()..];
            if line.starts_with("Texcraft: begin") {
                break;
            }
        }
        let next = s;
        let mut next_len = 0_usize;
        // Scan forward to the next "Texcraft: end" line and record the segment end.
        loop {
            let line = s.split_inclusive('\n').next()?;
            s = &s[line.len()..];
            next_len += line.len();
            if line.starts_with("Texcraft: end") {
                return Some(&next[0..next_len]);
            }
        }
    })
}

/// Parse a single raw hlist segment (as produced by [`extract_hlist_segments`]) into an hlist.
///
/// The `fonts` map is updated in place as new fonts are encountered.
fn parse_hlist(iter: &mut TexOutputIter, fonts: &mut HashMap<String, u32>) -> ds::HList {
    let mut hlist = {
        let line = iter.next().expect("hlist must be non-empty");
        let s = line
            .strip_prefix(r"\hbox(")
            .expect("first line must be a hlist spec");
        let i = s
            .find('+')
            .expect("hbox dimension spec has a + between height and depth");
        let height = parse_scaled(&s[..i]);
        let s = &s[i + 1..];
        let i = s
            .find(")x")
            .expect("hbox dimension spec has a )x between depth and width");
        let depth = parse_scaled(&s[..i]);
        let rest = &s[i + 2..];
        let (width_str, glue_set_str) = if let Some(j) = rest.find(", glue set ") {
            (&rest[..j], Some(&rest[j + 11..]))
        } else {
            (rest, None)
        };
        let width = parse_scaled(width_str);
        let (glue_ratio, glue_sign, glue_order) = glue_set_str.map(parse_glue_set).unwrap_or((
            ds::GlueRatio(0.0),
            ds::GlueSign::Normal,
            common::GlueOrder::Normal,
        ));
        ds::HList {
            height,
            width,
            depth,
            glue_ratio,
            glue_sign,
            glue_order,
            ..Default::default()
        }
    };
    let mut iter = iter.inner();
    while let Some(line) = iter.peek() {
        let (keyword, tail) = keyword_and_tail(line);
        let mut consume_line = true;
        let elem: ds::Horizontal = match keyword {
            "glue" => ds::Glue {
                kind: ds::GlueKind::Normal,
                value: parse_glue_value(tail),
            }
            .into(),
            "kern" => {
                let mut words = tail.split_ascii_whitespace();
                let width = parse_scaled(words.next().expect("kern has 1 word"));
                ds::Kern {
                    kind: ds::KernKind::Normal,
                    width,
                }
                .into()
            }
            "penalty" => {
                let value: i32 = tail.trim().parse().expect("penalty value is i32");
                ds::Penalty(value).into()
            }
            "rule" => {
                let i = tail.find('x').expect("rule has a width");
                ds::Rule {
                    width: parse_scaled(&tail[i + 1..]),
                    ..Default::default()
                }
                .into()
            }
            "discretionary" => {
                // TODO: handle replacing_spec
                _ = tail;
                ds::Discretionary {
                    pre_break: vec![],
                    post_break: vec![],
                    replace_count: 1,
                }
                .into()
            }
            "hbox" => {
                consume_line = false;
                parse_hlist(&mut iter, fonts).into()
            }
            "vbox" => {
                consume_line = false;
                parse_vlist(&mut iter, fonts).into()
            }
            font_name => {
                use std::collections::hash_map::Entry;
                let num_fonts: u32 = fonts.len().try_into().expect("no more than 2^32 fonts");
                let font = match fonts.entry(font_name.to_string()) {
                    Entry::Occupied(occupied_entry) => *occupied_entry.get(),
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(num_fonts);
                        num_fonts
                    }
                };
                let mut words = tail.split_ascii_whitespace();
                let char =
                    parse_char(words.next().unwrap_or_else(|| {
                        panic!("expected char after font command \\{font_name}")
                    }));
                if words.next() == Some("(ligature") {
                    let og_chars = words.next().expect("lig has 4 words");
                    let og_chars = og_chars.strip_suffix(")").expect("lig ends with ')'");
                    ds::Ligature {
                        included_left_boundary: false,
                        included_right_boundary: false,
                        char,
                        font,
                        original_chars: og_chars.into(),
                    }
                    .into()
                } else {
                    ds::Char { char, font }.into()
                }
            }
        };
        hlist.list.push(elem);
        if consume_line {
            iter.next();
        }
    }
    hlist
}

/// Parse a single raw vlist segment into a vlist.
fn parse_vlist(iter: &mut TexOutputIter, fonts: &mut HashMap<String, u32>) -> ds::VList {
    let mut vlist = {
        let line = iter.next().expect("vlist must be non-empty");
        let s = line
            .strip_prefix(r"\vbox(")
            .expect("first line must be a vlist spec");
        let i = s
            .find('+')
            .expect("vbox dimension spec has a + between height and depth");
        let height = parse_scaled(&s[..i]);
        let s = &s[i + 1..];
        let i = s
            .find(")x")
            .expect("vbox dimension spec has a )x between depth and width");
        let depth = parse_scaled(&s[..i]);
        let width = parse_scaled(&s[i + 2..]);
        ds::VList {
            height,
            width,
            depth,
            shift_amount: common::Scaled::ZERO,
            list: vec![],
            glue_ratio: ds::GlueRatio(0.0),
            glue_sign: ds::GlueSign::Normal,
            glue_order: common::GlueOrder::Normal,
        }
    };
    let mut iter = iter.inner();
    while let Some(line) = iter.peek() {
        let (keyword, tail) = keyword_and_tail(line);
        let mut consume_line = true;
        let elem: ds::Vertical = match keyword {
            "hbox" => {
                consume_line = false;
                parse_hlist(&mut iter, fonts).into()
            }
            "penalty" => {
                let value: i32 = tail.trim().parse().expect("penalty value is i32");
                ds::Penalty(value).into()
            }
            "glue" => ds::Vertical::Glue(ds::Glue {
                kind: ds::GlueKind::Normal,
                value: parse_glue_value(tail),
            }),
            _ => unimplemented!("vlist keyword {keyword} is not implemented"),
        };
        vlist.list.push(elem);
        if consume_line {
            iter.next();
        }
    }
    vlist
}

fn keyword_and_tail(s: &str) -> (&str, &str) {
    let mut c = s.chars();
    assert_eq!(c.next(), Some('\\'), "line expected to begin with \\");
    let mut keyword_len = 0_usize;
    for next in c {
        if next.is_alphabetic() {
            keyword_len += next.len_utf8();
        } else {
            break;
        }
    }
    (&s[1..1 + keyword_len], s[1 + keyword_len..].trim())
}

/// Parse the glue value from the text following `\glue` in TeX's box display.
///
/// `spec` may start with a parenthesised name (named glue, e.g. `(\rightskip) 0.0`)
/// or directly with a space followed by the width (e.g. ` 3.33333 plus 1.66666 minus 1.11111`).
fn parse_glue_value(spec: &str) -> common::Glue {
    let spec = if spec.starts_with('(') {
        let close = spec.find(')').expect("named glue has ')'");
        spec[close + 1..].trim_start()
    } else {
        spec.trim_start()
    };
    let mut words = spec.split_ascii_whitespace();
    let width = parse_scaled(words.next().expect("glue has a width"));
    let (stretch, stretch_order) = if words.next() == Some("plus") {
        parse_glue_amount(words.next().expect("glue has stretch after plus"))
    } else {
        (common::Scaled::ZERO, common::GlueOrder::Normal)
    };
    let (shrink, shrink_order) = if words.next() == Some("minus") {
        parse_glue_amount(words.next().expect("glue has shrink after minus"))
    } else {
        (common::Scaled::ZERO, common::GlueOrder::Normal)
    };
    common::Glue {
        width,
        stretch,
        stretch_order,
        shrink,
        shrink_order,
    }
}

/// Parse a glue component like `"1.66666"`, `"1.0fil"`, `"0.0fill"`.
fn parse_glue_amount(s: &str) -> (common::Scaled, common::GlueOrder) {
    if let Some(s) = s.strip_suffix("filll") {
        (parse_scaled(s), common::GlueOrder::Filll)
    } else if let Some(s) = s.strip_suffix("fill") {
        (parse_scaled(s), common::GlueOrder::Fill)
    } else if let Some(s) = s.strip_suffix("fil") {
        (parse_scaled(s), common::GlueOrder::Fil)
    } else {
        (parse_scaled(s), common::GlueOrder::Normal)
    }
}

/// Parse the `"N[order]"` text that follows `", glue set "` on an hbox line.
fn parse_glue_set(s: &str) -> (ds::GlueRatio, ds::GlueSign, common::GlueOrder) {
    let (sign, s) = if let Some(s) = s.strip_prefix("- ") {
        (ds::GlueSign::Shrinking, s)
    } else {
        (ds::GlueSign::Stretching, s)
    };
    let (s, order) = if let Some(s) = s.strip_suffix("filll") {
        (s, common::GlueOrder::Filll)
    } else if let Some(s) = s.strip_suffix("fill") {
        (s, common::GlueOrder::Fill)
    } else if let Some(s) = s.strip_suffix("fil") {
        (s, common::GlueOrder::Fil)
    } else {
        (s, common::GlueOrder::Normal)
    };
    let ratio: f32 = s.parse().expect("glue set ratio is a float");
    (ds::GlueRatio(ratio), sign, order)
}

fn parse_char(s: &str) -> char {
    let mut cs = s.chars();
    match cs.next().expect("char has one character") {
        '^' => {
            assert_eq!(cs.next(), Some('^'));
            let raw_c = cs.next().expect("char of the form ^^X") as u32;
            if let Some(raw_c) = raw_c.checked_sub(64) {
                raw_c
            } else {
                raw_c + 64
            }
            .try_into()
            .expect("TeX describes a valid character")
        }
        c => c,
    }
}

fn parse_scaled(s: &str) -> common::Scaled {
    let (neg, s) = match s.strip_prefix('-') {
        Some(s) => (true, s),
        None => (false, s),
    };
    let mut parts = s.split('.');
    let i: i32 = parts
        .next()
        .expect("scaled has an integer part")
        .parse()
        .expect("integer part is an integer");
    let mut f = [0_u8; 17];
    for (k, c) in parts
        .next()
        .expect("scaled has a fractional part")
        .chars()
        .enumerate()
    {
        f[k] = c
            .to_digit(10)
            .expect("fractional part are digits")
            .try_into()
            .expect("digits are in the range [0,10) and always fit in u8");
    }
    let f = common::Scaled::from_decimal_digits(&f);
    let sc = common::Scaled::new(i, f, common::ScaledUnit::Point).unwrap_or_else(|_| {
        eprintln!(
            "scaled number '{s}pt' is too big to parse; replacing with the largest scaled value {}",
            common::Scaled::MAX_DIMEN
        );
        common::Scaled::MAX_DIMEN
    });
    if neg {
        -sc
    } else {
        sc
    }
}

const CONVERT_TEXT_TEMPLATE: &str = r"

% User provided preamble.
<preamble>

% After showing a box, TeX stops and waits for user input.
% The following command suppresses that behavior.
\nonstopmode

% Output the box description to the terminal, from which we'll read it.
\tracingonline=1

% Output up to 1 million nodes.
\showboxbreadth=1000000
% Output up to 100 nested boxes.
\showboxdepth=100

% Prints the contents on its own line in the terminal.
\def\fullLineMessage#1{
    {
        \newlinechar=`@
        \message{@#1@}
    }
}

\def\printBox#1{
    % Put the content we want to see in box 0.
    \setbox0=<box_template>
    % Add a start marker so we know where to begin in the log
    \fullLineMessage{Texcraft: begin}
    % Show the box!
    \showbox0

    % Add a start marker so we know where to end in the log
    \fullLineMessage{Texcraft: end}
}

<print_calls>

We add some text at the end.

\end
";

#[cfg(test)]
mod tests {
    use super::*;

    struct MockTexEngine(String);

    impl TexEngine for MockTexEngine {
        fn run(&self, _: &str, _: &HashMap<PathBuf, Vec<u8>>) -> String {
            self.0.clone()
        }
    }

    #[test]
    fn test_build_horizontal_lists() {
        let log = r#"This is TeX, Version 3.141592653 (TeX Live 2024) (preloaded format=tex)
(./tmp/test.tex

Texcraft: begin
> \box0=
\hbox(6.94444+0.0)x56.66678
.\tenrm M
.\tenrm i
.\tenrm n
.\kern-0.27779
.\tenrm t
.\glue 3.33333 plus 1.66666 minus 1.11111
.\tenrm a
.\tenrm n
.\tenrm d
.\glue 3.33333 plus 1.66666 minus 1.11111
.\tenrm m
.\tenrm e

! OK.
\printBox ...Message {Texcraft: begin} \showbox 0 
                                                  \par \fullLineMessage {Tex...
l.31 \printBox{Mint and me}
                           

Texcraft: end
 )
(see the transcript file for additional information)
No pages of output.
Transcript written on test.log.
"#;

        let tex_engine = MockTexEngine(log.to_string());
        let (got_fonts, got_list) = build_horizontal_lists(
            &tex_engine,
            &Default::default(),
            &"",
            &mut vec!["".to_string()].iter(),
        );

        let want_list = ds::HList {
            height: parse_scaled("6.94444"),
            width: parse_scaled("56.66678"),
            depth: common::Scaled::ZERO,
            list: vec![
                ds::Char { char: 'M', font: 0 }.into(),
                ds::Char { char: 'i', font: 0 }.into(),
                ds::Char { char: 'n', font: 0 }.into(),
                ds::Kern {
                    kind: ds::KernKind::Normal,
                    width: parse_scaled("-0.27779"),
                }
                .into(),
                ds::Char { char: 't', font: 0 }.into(),
                ds::Glue {
                    kind: ds::GlueKind::Normal,
                    value: common::Glue {
                        width: parse_scaled("3.33333"),
                        stretch: parse_scaled("1.66666"),
                        stretch_order: common::GlueOrder::Normal,
                        shrink: parse_scaled("1.11111"),
                        shrink_order: common::GlueOrder::Normal,
                    },
                }
                .into(),
                ds::Char { char: 'a', font: 0 }.into(),
                ds::Char { char: 'n', font: 0 }.into(),
                ds::Char { char: 'd', font: 0 }.into(),
                ds::Glue {
                    kind: ds::GlueKind::Normal,
                    value: common::Glue {
                        width: parse_scaled("3.33333"),
                        stretch: parse_scaled("1.66666"),
                        stretch_order: common::GlueOrder::Normal,
                        shrink: parse_scaled("1.11111"),
                        shrink_order: common::GlueOrder::Normal,
                    },
                }
                .into(),
                ds::Char { char: 'm', font: 0 }.into(),
                ds::Char { char: 'e', font: 0 }.into(),
            ],
            ..Default::default()
        };
        let want_fonts = {
            let mut m = HashMap::new();
            m.insert("tenrm".to_string(), 0);
            m
        };

        assert_eq!(got_list, vec![want_list]);
        assert_eq!(got_fonts, want_fonts);
    }

    #[test]
    fn test_build_vertical_lists() {
        let log = r#"
This is TeX, Version 3.141592653 (TeX Live 2024) (preloaded format=tex)
(./test.tex

Texcraft: begin
> \box0=
\vbox(18.94444+0.0)x41.0
.\hbox(6.94444+0.0)x41.0, glue set 0.26662
..\tenrm M
..\tenrm i
..\tenrm n
..\kern-0.27779
..\tenrm t
..\glue 3.33333 plus 1.66666 minus 1.11111
..\tenrm a
..\tenrm n
..\tenrm d
..\glue(\rightskip) 0.0
.\penalty 300
.\glue(\baselineskip) 7.69446
.\hbox(4.30554+0.0)x41.0, glue set 28.2222fil
..\tenrm m
..\tenrm e
..\penalty 10000
..\glue(\parfillskip) 0.0 plus 1.0fil
..\glue(\rightskip) 0.0

! OK.
\printBox ...Message {Texcraft: begin} \showbox 0 
                                                  \par \fullLineMessage {Tex...
l.31 \printBox{Mint and me}
                           

Texcraft: end
 )
(see the transcript file for additional information)
No pages of output.
Transcript written on test.log.
"#;

        let tex_engine = MockTexEngine(log.to_string());
        let (got_fonts, got_list) = build_vertical_lists(
            &tex_engine,
            &Default::default(),
            &"",
            common::Scaled::ONE * 41,
            &mut vec!["".to_string()].iter(),
        );

        let want_list = ds::VList {
            height: parse_scaled("18.94444"),
            width: parse_scaled("41.0"),
            depth: common::Scaled::ZERO,
            shift_amount: common::Scaled::ZERO,
            glue_ratio: ds::GlueRatio(0.0),
            glue_sign: ds::GlueSign::Normal,
            glue_order: common::GlueOrder::Normal,
            list: vec![
                ds::Vertical::HList(ds::HList {
                    height: parse_scaled("6.94444"),
                    width: parse_scaled("41.0"),
                    depth: common::Scaled::ZERO,
                    glue_ratio: ds::GlueRatio(0.26662),
                    glue_sign: ds::GlueSign::Stretching,
                    glue_order: common::GlueOrder::Normal,
                    list: vec![
                        ds::Char { char: 'M', font: 0 }.into(),
                        ds::Char { char: 'i', font: 0 }.into(),
                        ds::Char { char: 'n', font: 0 }.into(),
                        ds::Kern {
                            kind: ds::KernKind::Normal,
                            width: parse_scaled("-0.27779"),
                        }
                        .into(),
                        ds::Char { char: 't', font: 0 }.into(),
                        ds::Glue {
                            kind: ds::GlueKind::Normal,
                            value: common::Glue {
                                width: parse_scaled("3.33333"),
                                stretch: parse_scaled("1.66666"),
                                stretch_order: common::GlueOrder::Normal,
                                shrink: parse_scaled("1.11111"),
                                shrink_order: common::GlueOrder::Normal,
                            },
                        }
                        .into(),
                        ds::Char { char: 'a', font: 0 }.into(),
                        ds::Char { char: 'n', font: 0 }.into(),
                        ds::Char { char: 'd', font: 0 }.into(),
                        ds::Glue {
                            kind: ds::GlueKind::Normal,
                            value: common::Glue {
                                width: common::Scaled::ZERO,
                                stretch: common::Scaled::ZERO,
                                stretch_order: common::GlueOrder::Normal,
                                shrink: common::Scaled::ZERO,
                                shrink_order: common::GlueOrder::Normal,
                            },
                        }
                        .into(),
                    ],
                    ..Default::default()
                }),
                ds::Vertical::Penalty(ds::Penalty(300)),
                ds::Vertical::Glue(ds::Glue {
                    kind: ds::GlueKind::Normal,
                    value: common::Glue {
                        width: parse_scaled("7.69446"),
                        stretch: common::Scaled::ZERO,
                        stretch_order: common::GlueOrder::Normal,
                        shrink: common::Scaled::ZERO,
                        shrink_order: common::GlueOrder::Normal,
                    },
                }),
                ds::Vertical::HList(ds::HList {
                    height: parse_scaled("4.30554"),
                    width: parse_scaled("41.0"),
                    depth: common::Scaled::ZERO,
                    glue_ratio: ds::GlueRatio(28.2222),
                    glue_sign: ds::GlueSign::Stretching,
                    glue_order: common::GlueOrder::Fil,
                    list: vec![
                        ds::Char { char: 'm', font: 0 }.into(),
                        ds::Char { char: 'e', font: 0 }.into(),
                        ds::Penalty(10000).into(),
                        ds::Glue {
                            kind: ds::GlueKind::Normal,
                            value: common::Glue {
                                width: common::Scaled::ZERO,
                                stretch: parse_scaled("1.0"),
                                stretch_order: common::GlueOrder::Fil,
                                shrink: common::Scaled::ZERO,
                                shrink_order: common::GlueOrder::Normal,
                            },
                        }
                        .into(),
                        ds::Glue {
                            kind: ds::GlueKind::Normal,
                            value: common::Glue {
                                width: common::Scaled::ZERO,
                                stretch: common::Scaled::ZERO,
                                stretch_order: common::GlueOrder::Normal,
                                shrink: common::Scaled::ZERO,
                                shrink_order: common::GlueOrder::Normal,
                            },
                        }
                        .into(),
                    ],
                    ..Default::default()
                }),
            ],
        };
        let want_fonts = {
            let mut m = HashMap::new();
            m.insert("tenrm".to_string(), 0);
            m
        };

        assert_eq!(got_list, vec![want_list]);
        assert_eq!(got_fonts, want_fonts);
    }
    #[test]
    fn test_build_vertical_lists_2() {
        let log = r#"
This is TeX, Version 3.141592653 (TeX Live 2024) (preloaded format=tex)
(./test.tex

Texcraft: begin
> \box0=
\vbox(6.83331+0.0)x41.0
.\hbox(6.83331+0.0)x41.0
..\tenrm A
..\hbox(6.83331+0.0)x48.08336
...\tenrm B
...\vbox(6.83331+0.0)x41.0
....\hbox(6.83331+0.0)x41.0, glue set 6.13887fil
.....\hbox(0.0+0.0)x20.0
.....\tenrm C
.....\hbox(6.83331+0.0)x7.6389
......\tenrm D
.....\penalty 10000
.....\glue(\parfillskip) 0.0 plus 1.0fil
.....\glue(\rightskip) 0.0
..\penalty 10000
..\glue(\parfillskip) 0.0 plus 1.0fil
..\glue(\rightskip) 0.0
..\rule(*+*)x5.0

! OK.
\printBox ...Message {Texcraft: begin} \showbox 0 
                                                  \par \fullLineMessage {Tex...
l.31 \printBox{Mint and me}
                           

Texcraft: end
 )
(see the transcript file for additional information)
No pages of output.
Transcript written on test.log.
"#;

        let tex_engine = MockTexEngine(log.to_string());
        let (got_fonts, got_list) = build_vertical_lists(
            &tex_engine,
            &Default::default(),
            &"",
            common::Scaled::ONE * 41,
            &mut vec!["".to_string()].iter(),
        );

        let want_list = ds::VList {
            height: parse_scaled("6.83331"),
            width: parse_scaled("41.0"),
            depth: common::Scaled::ZERO,
            shift_amount: common::Scaled::ZERO,
            glue_ratio: ds::GlueRatio(0.0),
            glue_sign: ds::GlueSign::Normal,
            glue_order: common::GlueOrder::Normal,
            list: vec![ds::Vertical::HList(ds::HList {
                height: parse_scaled("6.83331"),
                width: parse_scaled("41.0"),
                depth: common::Scaled::ZERO,
                list: vec![
                    ds::Char { char: 'A', font: 0 }.into(),
                    ds::Horizontal::HList(ds::HList {
                        height: parse_scaled("6.83331"),
                        width: parse_scaled("48.08336"),
                        depth: common::Scaled::ZERO,
                        list: vec![
                            ds::Char { char: 'B', font: 0 }.into(),
                            ds::Horizontal::VList(ds::VList {
                                height: parse_scaled("6.83331"),
                                width: parse_scaled("41.0"),
                                depth: common::Scaled::ZERO,
                                list: vec![ds::Vertical::HList(ds::HList {
                                    height: parse_scaled("6.83331"),
                                    width: parse_scaled("41.0"),
                                    depth: common::Scaled::ZERO,
                                    glue_ratio: ds::GlueRatio(6.13887),
                                    glue_sign: ds::GlueSign::Stretching,
                                    glue_order: common::GlueOrder::Fil,
                                    list: vec![
                                        ds::Horizontal::HList(ds::HList {
                                            height: common::Scaled::ZERO,
                                            width: parse_scaled("20.0"),
                                            depth: common::Scaled::ZERO,
                                            ..Default::default()
                                        }),
                                        ds::Char { char: 'C', font: 0 }.into(),
                                        ds::Horizontal::HList(ds::HList {
                                            height: parse_scaled("6.83331"),
                                            width: parse_scaled("7.6389"),
                                            depth: common::Scaled::ZERO,
                                            list: vec![ds::Char { char: 'D', font: 0 }.into()],
                                            ..Default::default()
                                        }),
                                        ds::Penalty(10000).into(),
                                        ds::Glue {
                                            kind: ds::GlueKind::Normal,
                                            value: common::Glue {
                                                width: common::Scaled::ZERO,
                                                stretch: parse_scaled("1.0"),
                                                stretch_order: common::GlueOrder::Fil,
                                                shrink: common::Scaled::ZERO,
                                                shrink_order: common::GlueOrder::Normal,
                                            },
                                        }
                                        .into(),
                                        ds::Glue {
                                            kind: ds::GlueKind::Normal,
                                            value: common::Glue {
                                                width: common::Scaled::ZERO,
                                                stretch: common::Scaled::ZERO,
                                                stretch_order: common::GlueOrder::Normal,
                                                shrink: common::Scaled::ZERO,
                                                shrink_order: common::GlueOrder::Normal,
                                            },
                                        }
                                        .into(),
                                    ],
                                    ..Default::default()
                                })],
                                ..Default::default()
                            }),
                        ],
                        ..Default::default()
                    }),
                    ds::Penalty(10000).into(),
                    ds::Glue {
                        kind: ds::GlueKind::Normal,
                        value: common::Glue {
                            width: common::Scaled::ZERO,
                            stretch: parse_scaled("1.0"),
                            stretch_order: common::GlueOrder::Fil,
                            shrink: common::Scaled::ZERO,
                            shrink_order: common::GlueOrder::Normal,
                        },
                    }
                    .into(),
                    ds::Glue {
                        kind: ds::GlueKind::Normal,
                        value: common::Glue {
                            width: common::Scaled::ZERO,
                            stretch: common::Scaled::ZERO,
                            stretch_order: common::GlueOrder::Normal,
                            shrink: common::Scaled::ZERO,
                            shrink_order: common::GlueOrder::Normal,
                        },
                    }
                    .into(),
                    ds::Rule {
                        height: ds::Rule::RUNNING,
                        depth: ds::Rule::RUNNING,
                        width: parse_scaled("5.0"),
                    }
                    .into(),
                ],
                ..Default::default()
            })],
        };
        let want_fonts = {
            let mut m = HashMap::new();
            m.insert("tenrm".to_string(), 0);
            m
        };

        assert_eq!(got_fonts, want_fonts);
        assert_eq!(got_list, vec![want_list]);
    }
}
