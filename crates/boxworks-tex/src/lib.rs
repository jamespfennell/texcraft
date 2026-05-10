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

use boxworks::ds;
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
    hyphenated: bool,
) -> (HashMap<String, u32>, Vec<ds::HBox>) {
    let macro_calls: Vec<String> = contents.map(|s| format!(r#"\printBox{{{s}}}"#)).collect();
    let box_template = r"\vbox{\hbox{#1} \pretolerance=-1 \hsize=16383pt \noindent  #1}";
    let tex_source_code = CONVERT_TEXT_TEMPLATE
        .replace("<preamble>", preamble)
        .replace("<box_template>", box_template)
        .replace("<print_calls>", &macro_calls.join("\n\n"));
    let output = tex_engine.run(&tex_source_code, auxiliary_files);
    let segments = extract_texcraft_segments(&output);
    let mut fonts: HashMap<String, u32> = Default::default();
    let hlists = segments
        .map(|s| {
            let v_box = parse_vlist(&mut TexOutputIter::new(s), &mut fonts).unwrap();

            let mut h_box = None;
            let mut h_box_hyphenated = None;
            for elem in v_box.list {
                if let ds::Vertical::HBox(mut next_h_box) = elem {
                    match &mut h_box {
                        None => h_box = Some(next_h_box),
                        Some(ref mut first_hbox) => {
                            next_h_box.width = first_hbox.width;
                            // remove the 3 items that the line-breaking algorithm sticks on.
                            next_h_box.list.pop();
                            next_h_box.list.pop();
                            next_h_box.list.pop();
                            h_box_hyphenated = Some(next_h_box);
                            break;
                        }
                    }
                }
            }
            if hyphenated {
                h_box_hyphenated.unwrap()
            } else {
                h_box.unwrap()
            }
        })
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
    widths: &[common::Scaled],
    params: &boxworks_knuthplass::Params,
    contents: &mut dyn Iterator<Item = &String>,
) -> (HashMap<String, u32>, Vec<ds::VBox>) {
    let params_preamble = format!(
        "\n\\adjdemerits={}\n\\doublehyphendemerits={}\n\\exhyphenpenalty={}\n\\finalhyphendemerits={}\n\\hyphenpenalty={}\n\\leftskip={}\n\\linepenalty={}\n\\looseness={}\n\\parfillskip={}\n\\pretolerance={}\n\\rightskip={}\n\\tolerance={}\n",
        params.adj_demerits,
        params.double_hyphen_demerits,
        params.ex_hyphen_penalty,
        params.final_hyphen_demerits,
        params.hyphen_penalty,
        params.left_skip,
        params.line_penalty,
        params.looseness,
        params.par_fill_skip,
        params.pre_tolerance,
        params.right_skip,
        params.tolerance,
    );
    let combined_preamble = format!("{preamble}{params_preamble}");
    let last_width = *widths.last().expect("widths is non-empty");
    let box_template = if widths.len() == 1 {
        format!(r"\vbox{{\noindent \hsize={} #1}}", last_width)
    } else {
        let parshape_specs: String = widths.iter().map(|w| format!("0pt {w} ")).collect();
        format!(
            r"\vbox{{\noindent \hsize={} \parshape {} {}#1}}",
            last_width,
            widths.len(),
            parshape_specs,
        )
    };
    let macro_calls: Vec<String> = contents.map(|s| format!(r#"\printBox{{{s}}}"#)).collect();
    let tex_source_code = CONVERT_TEXT_TEMPLATE
        .replace("<preamble>", &combined_preamble)
        .replace("<box_template>", &box_template)
        .replace("<print_calls>", &macro_calls.join("\n\n"));
    let output = tex_engine.run(&tex_source_code, auxiliary_files);
    let segments = extract_texcraft_segments(&output);
    let mut fonts: HashMap<String, u32> = Default::default();
    let vlists = segments
        .map(|s| parse_vlist(&mut TexOutputIter::new(s), &mut fonts).unwrap())
        .collect();
    (fonts, vlists)
}

struct TexOutputIter<'tex> {
    s: &'tex str,
    depth: usize,
    line_number: usize,
}

impl<'tex> Iterator for TexOutputIter<'tex> {
    type Item = (usize, &'tex str);

    fn next(&mut self) -> Option<Self::Item> {
        let (line_number, line, n) = self.peek_impl()?;
        self.s = &self.s[n..];
        self.line_number += 1;
        Some((line_number, line))
    }
}

impl<'tex> TexOutputIter<'tex> {
    fn new(mut s: &'tex str) -> Self {
        let mut line_number = 1_usize;
        loop {
            let line = s
                .split_inclusive('\n')
                .next()
                .expect("still searching for start of output");
            s = &s[line.len()..];
            line_number += 1;
            if !line.starts_with(r"> \box0=") {
                continue;
            }
            return Self {
                s,
                depth: 0,
                line_number,
            };
        }
    }
    fn inner(&self) -> Self {
        Self {
            s: self.s,
            depth: self.depth + 1,
            line_number: self.line_number,
        }
    }
    fn peek(&mut self) -> Option<(usize, &'tex str)> {
        let (line_number, line, _) = self.peek_impl()?;
        Some((line_number, line))
    }
    fn peek_impl(&mut self) -> Option<(usize, &'tex str, usize)> {
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
                    return Some((self.line_number, &line[line_depth..], n));
                }
                Greater => {
                    self.s = &self.s[n..];
                    self.line_number += 1;
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

/// Kind of error when parsing TeX logs.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    /// The iterator was empty when the start of an hlist was expected.
    EmptyHlist,
    /// The first line did not start with `\hbox(` as required.
    MissingHboxPrefix,
    /// The hbox dimension spec had no `+` separating height from depth.
    MissingHboxHeightDepthSeparator,
    /// The hbox dimension spec had no `)x` separating depth from width.
    MissingHboxDepthWidthSeparator,
    /// A `\kern` item had no width value.
    KernMissingWidth,
    /// A `\penalty` value could not be parsed as an integer.
    InvalidPenaltyValue,
    /// A `\rule` item had no `x` separating the height/depth from the width.
    RuleMissingWidthSeparator,
    /// A `\discretionary` item had a word other than `replacing` where `replacing` was expected.
    DiscretionaryExpectedReplacingKeyword,
    /// A `\discretionary replacing` item had no replacement count.
    DiscretionaryMissingReplaceCount,
    /// A `\discretionary replacing N` item had a count that could not be parsed as an integer.
    DiscretionaryInvalidReplaceCount,
    /// A font command was not followed by a character.
    MissingCharAfterFont,
    /// A ligature item had no original chars word after `(ligature`.
    LigatureMissingOriginalChars,
    /// The original chars of a ligature did not end with `)`.
    LigatureMissingClosingParen,
    /// The iterator was empty when the start of a vlist was expected.
    EmptyVlist,
    /// The first line did not start with `\vbox(` as required.
    MissingVboxPrefix,
    /// The vbox dimension spec had no `+` separating height from depth.
    MissingVboxHeightDepthSeparator,
    /// The vbox dimension spec had no `)x` separating depth from width.
    MissingVboxDepthWidthSeparator,
    /// A vlist contained a keyword that is not yet handled.
    UnknownVlistKeyword,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::EmptyHlist => write!(f, "iterator was empty when an hlist was expected"),
            ErrorKind::MissingHboxPrefix => write!(f, r"first line did not start with \hbox("),
            ErrorKind::MissingHboxHeightDepthSeparator => {
                write!(
                    f,
                    r"hbox dimension spec missing '+' between height and depth"
                )
            }
            ErrorKind::MissingHboxDepthWidthSeparator => {
                write!(
                    f,
                    r"hbox dimension spec missing ')x' between depth and width"
                )
            }
            ErrorKind::KernMissingWidth => write!(f, r"\kern item had no width value"),
            ErrorKind::InvalidPenaltyValue => {
                write!(f, r"\penalty value could not be parsed as an integer")
            }
            ErrorKind::RuleMissingWidthSeparator => {
                write!(
                    f,
                    r"\rule item had no 'x' separating height/depth from width"
                )
            }
            ErrorKind::DiscretionaryExpectedReplacingKeyword => {
                write!(
                    f,
                    r"\discretionary item had unexpected word where 'replacing' was expected"
                )
            }
            ErrorKind::DiscretionaryMissingReplaceCount => {
                write!(f, r"\discretionary replacing item had no replacement count")
            }
            ErrorKind::DiscretionaryInvalidReplaceCount => {
                write!(
                    f,
                    r"\discretionary replacing count could not be parsed as an integer"
                )
            }
            ErrorKind::MissingCharAfterFont => {
                write!(f, "font command was not followed by a character")
            }
            ErrorKind::LigatureMissingOriginalChars => {
                write!(f, "ligature item had no original chars after '(ligature'")
            }
            ErrorKind::LigatureMissingClosingParen => {
                write!(f, "ligature original chars did not end with ')'")
            }
            ErrorKind::EmptyVlist => write!(f, "iterator was empty when a vlist was expected"),
            ErrorKind::MissingVboxPrefix => write!(f, r"first line did not start with \vbox("),
            ErrorKind::MissingVboxHeightDepthSeparator => {
                write!(
                    f,
                    r"vbox dimension spec missing '+' between height and depth"
                )
            }
            ErrorKind::MissingVboxDepthWidthSeparator => {
                write!(
                    f,
                    r"vbox dimension spec missing ')x' between depth and width"
                )
            }
            ErrorKind::UnknownVlistKeyword => write!(f, "vlist contained an unhandled keyword"),
        }
    }
}

impl std::error::Error for ErrorKind {}

/// Error returned by internal functions that parse TeX logs
/// These internal parsing functions will eventually be made public
/// and so the errors were made public.
#[derive(Debug, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub line_number: usize,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {}", self.line_number, self.kind)
    }
}

impl std::error::Error for Error {}

fn parse_disc_elem(
    line: &str,
    line_number: usize,
    fonts: &mut HashMap<String, u32>,
) -> Result<ds::DiscretionaryElem, Error> {
    let (keyword, tail) = keyword_and_tail(line);
    match keyword {
        "kern" => {
            let mut words = tail.split_ascii_whitespace();
            let width = parse_scaled(words.next().ok_or(Error {
                kind: ErrorKind::KernMissingWidth,
                line_number,
            })?);
            Ok(ds::DiscretionaryElem::Kern(ds::Kern {
                kind: ds::KernKind::Normal,
                width,
            }))
        }
        font_name => {
            use std::collections::hash_map::Entry;
            let num_fonts: u32 = fonts.len().try_into().expect("no more than 2^32 fonts");
            let font = match fonts.entry(font_name.to_string()) {
                Entry::Occupied(e) => *e.get(),
                Entry::Vacant(e) => {
                    e.insert(num_fonts);
                    num_fonts
                }
            };
            let mut words = tail.split_ascii_whitespace();
            let char = parse_char(words.next().ok_or(Error {
                kind: ErrorKind::MissingCharAfterFont,
                line_number,
            })?);
            Ok(ds::DiscretionaryElem::Char(ds::Char { char, font }))
        }
    }
}

/// Parse a single raw hlist segment (as produced by [`extract_hlist_segments`]) into an hlist.
///
/// The `fonts` map is updated in place as new fonts are encountered.
fn parse_hlist(
    iter: &mut TexOutputIter,
    fonts: &mut HashMap<String, u32>,
) -> Result<ds::HBox, Error> {
    let mut hlist = {
        let line_number_hint = iter.line_number;
        let (line_number, line) = iter.next().ok_or(Error {
            kind: ErrorKind::EmptyHlist,
            line_number: line_number_hint,
        })?;
        let s = line.strip_prefix(r"\hbox(").ok_or(Error {
            kind: ErrorKind::MissingHboxPrefix,
            line_number,
        })?;
        let i = s.find('+').ok_or(Error {
            kind: ErrorKind::MissingHboxHeightDepthSeparator,
            line_number,
        })?;
        let height = parse_scaled(&s[..i]);
        let s = &s[i + 1..];
        let i = s.find(")x").ok_or(Error {
            kind: ErrorKind::MissingHboxDepthWidthSeparator,
            line_number,
        })?;
        let depth = parse_scaled(&s[..i]);
        let rest = &s[i + 2..];
        let (width_str, glue_set_str) = if let Some(j) = rest.find(", glue set ") {
            (&rest[..j], Some(&rest[j + 11..]))
        } else {
            (rest, None)
        };
        let width = parse_scaled(width_str);
        let (glue_ratio, glue_order) = glue_set_str
            .map(parse_glue_set)
            .unwrap_or((ds::GlueRatio::default(), common::GlueOrder::Normal));
        ds::HBox {
            height,
            width,
            depth,
            glue_ratio,
            glue_order,
            ..Default::default()
        }
    };
    let mut iter = iter.inner();
    while let Some((line_number, line)) = iter.peek() {
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
                let width = parse_scaled(words.next().ok_or(Error {
                    kind: ErrorKind::KernMissingWidth,
                    line_number,
                })?);
                ds::Kern {
                    kind: ds::KernKind::Normal,
                    width,
                }
                .into()
            }
            "penalty" => {
                let value: i32 = tail.trim().parse().map_err(|_| Error {
                    kind: ErrorKind::InvalidPenaltyValue,
                    line_number,
                })?;
                ds::Penalty(value).into()
            }
            "rule" => {
                let i = tail.find('x').ok_or(Error {
                    kind: ErrorKind::RuleMissingWidthSeparator,
                    line_number,
                })?;
                ds::Rule {
                    width: parse_scaled(&tail[i + 1..]),
                    ..Default::default()
                }
                .into()
            }
            "discretionary" => {
                // Inverse of TeX.2021.195
                let mut words = tail.split_ascii_whitespace();
                let replace_count = match words.next() {
                    None => 0,
                    Some(replacing) => {
                        if replacing != "replacing" {
                            return Err(Error {
                                kind: ErrorKind::DiscretionaryExpectedReplacingKeyword,
                                line_number,
                            });
                        }
                        let count_str = words.next().ok_or(Error {
                            kind: ErrorKind::DiscretionaryMissingReplaceCount,
                            line_number,
                        })?;
                        count_str.parse().map_err(|_| Error {
                            kind: ErrorKind::DiscretionaryInvalidReplaceCount,
                            line_number,
                        })?
                    }
                };
                // Consume the \discretionary line now so iter.peek() below sees sub-items.
                iter.next();
                consume_line = false;
                // Pre-break items are at depth+1 (normal dot-prefixed sub-lines).
                let mut pre_break = vec![];
                {
                    let mut pre_iter = iter.inner();
                    while let Some((ln, line)) = pre_iter.peek() {
                        pre_break.push(parse_disc_elem(line, ln, fonts)?);
                        pre_iter.next();
                    }
                }
                // Post-break items are at the current depth but with content starting with `|`.
                // iter.peek() lazily skips the depth+1 pre-break lines via peek_impl's Greater branch.
                let mut post_break = vec![];
                while let Some((ln, line)) = iter.peek() {
                    let Some(post_line) = line.strip_prefix('|') else {
                        break;
                    };
                    iter.next();
                    post_break.push(parse_disc_elem(post_line, ln, fonts)?);
                }
                ds::Discretionary {
                    pre_break,
                    post_break,
                    replace_count,
                }
                .into()
            }
            "hbox" => {
                consume_line = false;
                parse_hlist(&mut iter, fonts)?.into()
            }
            "vbox" => {
                consume_line = false;
                parse_vlist(&mut iter, fonts)?.into()
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
                let char = parse_char(words.next().ok_or(Error {
                    kind: ErrorKind::MissingCharAfterFont,
                    line_number,
                })?);
                if words.next() == Some("(ligature") {
                    let og_chars = words.next().ok_or(Error {
                        kind: ErrorKind::LigatureMissingOriginalChars,
                        line_number,
                    })?;
                    let og_chars = og_chars.strip_suffix(')').ok_or(Error {
                        kind: ErrorKind::LigatureMissingClosingParen,
                        line_number,
                    })?;
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
    Ok(hlist)
}

/// Parse a single raw vlist segment into a vlist.
fn parse_vlist(
    iter: &mut TexOutputIter,
    fonts: &mut HashMap<String, u32>,
) -> Result<ds::VBox, Error> {
    let mut vlist = {
        let line_number_hint = iter.line_number;
        let (line_number, line) = iter.next().ok_or(Error {
            kind: ErrorKind::EmptyVlist,
            line_number: line_number_hint,
        })?;
        let s = line.strip_prefix(r"\vbox(").ok_or(Error {
            kind: ErrorKind::MissingVboxPrefix,
            line_number,
        })?;
        let i = s.find('+').ok_or(Error {
            kind: ErrorKind::MissingVboxHeightDepthSeparator,
            line_number,
        })?;
        let _height = parse_scaled(&s[..i]);
        let s = &s[i + 1..];
        let i = s.find(")x").ok_or(Error {
            kind: ErrorKind::MissingVboxDepthWidthSeparator,
            line_number,
        })?;
        let _depth = parse_scaled(&s[..i]);
        let _width = parse_scaled(&s[i + 2..]);
        // TODO: use the real heights when Boxworks has these populated too.
        let (width, height, depth) = (
            common::Scaled::ZERO,
            common::Scaled::ZERO,
            common::Scaled::ZERO,
        );
        ds::VBox {
            height,
            width,
            depth,
            shift_amount: common::Scaled::ZERO,
            list: vec![],
            glue_ratio: Default::default(),
            glue_order: common::GlueOrder::Normal,
        }
    };
    let mut iter = iter.inner();
    while let Some((line_number, line)) = iter.peek() {
        let (keyword, tail) = keyword_and_tail(line);
        let mut consume_line = true;
        let elem: ds::Vertical = match keyword {
            "hbox" => {
                consume_line = false;
                parse_hlist(&mut iter, fonts)?.into()
            }
            "penalty" => {
                let value: i32 = tail.trim().parse().map_err(|_| Error {
                    kind: ErrorKind::InvalidPenaltyValue,
                    line_number,
                })?;
                ds::Penalty(value).into()
            }
            "glue" => ds::Vertical::Glue(ds::Glue {
                kind: ds::GlueKind::Normal,
                value: parse_glue_value(tail),
            }),
            _ => {
                return Err(Error {
                    kind: ErrorKind::UnknownVlistKeyword,
                    line_number,
                })
            }
        };
        vlist.list.push(elem);
        if consume_line {
            iter.next();
        }
    }
    Ok(vlist)
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
fn parse_glue_set(s: &str) -> (ds::GlueRatio, common::GlueOrder) {
    let (neg, s) = if let Some(s) = s.strip_prefix("- ") {
        (true, s)
    } else {
        (false, s)
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
    let mut ratio = ds::GlueRatio::from_float_str(s).expect("glue set ratio is a float");
    if neg {
        ratio.num.0 *= -1;
    }
    (ratio, order)
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

    fn parse_hbox_lang(source: &str) -> ds::HBox {
        let mut list = boxworks::lang::parse_horizontal_list(source).unwrap();
        assert_eq!(list.len(), 1);
        match list.remove(0) {
            ds::Horizontal::HBox(hbox) => hbox,
            other => panic!("expected hbox, got {other:?}"),
        }
    }

    fn parse_vbox_lang(source: &str) -> ds::VBox {
        let mut list = boxworks::lang::parse_horizontal_list(source).unwrap();
        assert_eq!(list.len(), 1);
        match list.remove(0) {
            ds::Horizontal::VBox(mut vbox) => {
                // TODO: when vpack is implemented, remove this.
                vbox.width = common::Scaled::ZERO;
                vbox.height = common::Scaled::ZERO;
                vbox.depth = common::Scaled::ZERO;
                vbox.shift_amount = common::Scaled::ZERO;
                vbox
            }
            other => panic!("expected vbox, got {other:?}"),
        }
    }

    struct MockTexEngine(String);

    impl TexEngine for MockTexEngine {
        fn run(&self, _: &str, _: &HashMap<PathBuf, Vec<u8>>) -> String {
            self.0.clone()
        }
    }

    #[test]
    fn test_build_horizontal_lists() {
        let log = r#"This is TeX, Version 3.141592653 (TeX Live 2024) (preloaded format=tex 2025.1.20)  9 MAY 2026 22:47
**/var/folders/tk/q1zszl0n7c34crg980zs_0lw0000gn/T/tex-input.tex
(/var/folders/tk/q1zszl0n7c34crg980zs_0lw0000gn/T/tex-input.tex
@firstpass
\customFont Mint and me
@\par via @@0 b=0 p=-10000 d=100
@@1: line 1.2- t=100 -> @@0


Texcraft: begin
> \box0=
\vbox(18.94444+0.0)x469.75499
.\hbox(6.94444+0.0)x56.66678
..\customFont M
..\customFont i
..\customFont n
..\kern-0.27779
..\customFont t
..\glue 3.33333 plus 1.66666 minus 1.11111
..\customFont a
..\customFont n
..\customFont d
..\glue 3.33333 plus 1.66666 minus 1.11111
..\customFont m
..\customFont e
.\glue(\parskip) 0.0 plus 1.0
.\glue(\baselineskip) 5.05556
.\hbox(6.94444+0.0)x469.75499, glue set 413.08821fil
..\customFont M
..\customFont i
..\customFont n
..\kern-0.27779
..\customFont t
..\glue 3.33333 plus 1.66666 minus 1.11111
..\customFont a
..\customFont n
..\customFont d
..\glue 3.33333 plus 1.66666 minus 1.11111
..\customFont m
..\customFont e
..\penalty 10000
..\glue(\parfillskip) 0.0 plus 1.0fil
..\glue(\rightskip) 0.0

! OK.
\printBox ...Message {Texcraft: begin} \showbox 0
                                                    \par \fullLineMessage {Tex...
l.45 \printBox{Mint and me}



Texcraft: end
@firstpass
[]\customFont We add some text at the end.
@\par via @@0 b=0 p=-10000 d=100
@@1: line 1.2- t=100 -> @@0

[1] )
Output written on tex-input.dvi (1 page, 244 bytes).
        "#;

        let tex_engine = MockTexEngine(log.to_string());
        let (got_fonts, got_list) = build_horizontal_lists(
            &tex_engine,
            &Default::default(),
            &"",
            &mut vec!["".to_string()].iter(),
            false,
        );

        let want_list = parse_hbox_lang(
            r#"
            hbox(
                height=6.94444pt,
                width=56.66678pt,
                content=[
                    chars("Min")
                    kern(-0.27779pt)
                    chars("t")
                    glue(3.33333pt, 1.66666pt, 1.11111pt)
                    chars("and")
                    glue(3.33333pt, 1.66666pt, 1.11111pt)
                    chars("me")
                ]
            )
        "#,
        );
        let want_fonts = {
            let mut m = HashMap::new();
            m.insert("customFont".to_string(), 0);
            m
        };

        assert_eq!(got_list, vec![want_list]);
        assert_eq!(got_fonts, want_fonts);
    }

    #[test]
    fn test_discretionary_pre_and_post_break() {
        let input = r"> \box0=
\hbox(6.94444+0.0)x10.0
.\discretionary replacing 3
..\tenrm d
..\tenrm e
..\tenrm f
.|\tenrm g
.|\tenrm h
";
        let mut fonts = Default::default();
        let got = parse_hlist(&mut TexOutputIter::new(input), &mut fonts).unwrap();
        let want = parse_hbox_lang(
            r#"hbox(
                height=6.94444pt,
                width=10.0pt,
                content=[
                    disc(
                        pre_break=[chars("def", font=0)],
                        post_break=[chars("gh", font=0)],
                        replace_count=3,
                    )
                ]
            )"#,
        );
        assert_eq!(got, want);
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
            &[common::Scaled::ONE * 41],
            &boxworks_knuthplass::Params::default(),
            &mut vec!["".to_string()].iter(),
        );

        let want_list = parse_vbox_lang(
            r#"
            vbox(
                height=18.94444pt,
                width=41.0pt,
                content=[
                    hbox(
                        height=6.94444pt,
                        width=41.0pt,
                        glue_ratio="0.26662",
                        content=[
                            chars("Min")
                            kern(-0.27779pt)
                            chars("t")
                            glue(3.33333pt, 1.66666pt, 1.11111pt)
                            chars("and")
                            glue()
                        ]
                    )
                    penalty(300)
                    glue(7.69446pt)
                    hbox(
                        height=4.30554pt,
                        width=41.0pt,
                        glue_ratio="28.2222",
                        glue_order="fil",
                        content=[
                            chars("me")
                            penalty(10000)
                            glue(0.0pt, 1.0fil, 0.0pt)
                            glue()
                        ]
                    )
                ]
            )
        "#,
        );
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
            &[common::Scaled::ONE * 41],
            &boxworks_knuthplass::Params::default(),
            &mut vec!["".to_string()].iter(),
        );

        let want_list = parse_vbox_lang(
            r#"
            vbox(
                height=6.83331pt,
                width=41.0pt,
                content=[
                    hbox(
                        height=6.83331pt,
                        width=41.0pt,
                        content=[
                            chars("A")
                            hbox(
                                height=6.83331pt,
                                width=48.08336pt,
                                content=[
                                    chars("B")
                                    vbox(
                                        # todo
                                        # height=6.83331pt,
                                        # width=41.0pt,
                                        content=[
                                            hbox(
                                                height=6.83331pt,
                                                width=41.0pt,
                                                glue_ratio="6.13887",
                                                glue_order="fil",
                                                content=[
                                                    hbox(width=20.0pt)
                                                    chars("C")
                                                    hbox(
                                                        height=6.83331pt,
                                                        width=7.6389pt,
                                                        content=[chars("D")]
                                                    )
                                                    penalty(10000)
                                                    glue(0.0pt, 1.0fil, 0.0pt)
                                                    glue()
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
                            penalty(10000)
                            glue(0.0pt, 1.0fil, 0.0pt)
                            glue()
                            rule(height="running", width=5.0pt, depth="running")
                        ]
                    )
                ]
            )
        "#,
        );
        let want_fonts = {
            let mut m = HashMap::new();
            m.insert("tenrm".to_string(), 0);
            m
        };

        assert_eq!(got_fonts, want_fonts);
        assert_eq!(got_list, vec![want_list]);
    }

    macro_rules! test_parse_hlist_error {
        ($(($name:ident, $input:expr, $expected:expr)),* $(,)?) => [$(
            #[test]
            fn $name() {
                let err = parse_hlist(&mut TexOutputIter::new($input), &mut Default::default())
                    .unwrap_err();
                assert_eq!(err, $expected);
            }
        )*];
    }

    test_parse_hlist_error![
        (
            test_empty_hlist,
            r"> \box0=
",
            Error {
                kind: ErrorKind::EmptyHlist,
                line_number: 2
            }
        ),
        (
            test_missing_hbox_prefix,
            r"> \box0=
\vbox(6.0+0.0)x10.0
",
            Error {
                kind: ErrorKind::MissingHboxPrefix,
                line_number: 2
            }
        ),
        (
            test_missing_height_depth_separator,
            r"> \box0=
\hbox(6.94444 no plus here)x10.0
",
            Error {
                kind: ErrorKind::MissingHboxHeightDepthSeparator,
                line_number: 2
            }
        ),
        (
            test_missing_depth_width_separator,
            r"> \box0=
\hbox(6.94444+0.0 no depth width)
",
            Error {
                kind: ErrorKind::MissingHboxDepthWidthSeparator,
                line_number: 2
            }
        ),
        (
            test_kern_missing_width,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\kern
",
            Error {
                kind: ErrorKind::KernMissingWidth,
                line_number: 3
            }
        ),
        (
            test_invalid_penalty_value,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\penalty abc
",
            Error {
                kind: ErrorKind::InvalidPenaltyValue,
                line_number: 3
            }
        ),
        (
            test_rule_missing_width_separator,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\rule (*+*) 5.0
",
            Error {
                kind: ErrorKind::RuleMissingWidthSeparator,
                line_number: 3
            }
        ),
        (
            test_discretionary_expected_replacing_keyword,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\discretionary wrong 3
",
            Error {
                kind: ErrorKind::DiscretionaryExpectedReplacingKeyword,
                line_number: 3
            }
        ),
        (
            test_discretionary_missing_replace_count,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\discretionary replacing
",
            Error {
                kind: ErrorKind::DiscretionaryMissingReplaceCount,
                line_number: 3
            }
        ),
        (
            test_discretionary_invalid_replace_count,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\discretionary replacing abc
",
            Error {
                kind: ErrorKind::DiscretionaryInvalidReplaceCount,
                line_number: 3
            }
        ),
        (
            test_missing_char_after_font,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\tenrm
",
            Error {
                kind: ErrorKind::MissingCharAfterFont,
                line_number: 3
            }
        ),
        (
            test_ligature_missing_original_chars,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\tenrm f (ligature
",
            Error {
                kind: ErrorKind::LigatureMissingOriginalChars,
                line_number: 3
            }
        ),
        (
            test_ligature_missing_closing_paren,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
.\tenrm f (ligature fi
",
            Error {
                kind: ErrorKind::LigatureMissingClosingParen,
                line_number: 3
            }
        ),
    ];

    macro_rules! test_parse_vlist_error {
        ($(($name:ident, $input:expr, $expected:expr)),* $(,)?) => [$(
            #[test]
            fn $name() {
                let err = parse_vlist(&mut TexOutputIter::new($input), &mut Default::default())
                    .unwrap_err();
                assert_eq!(err, $expected);
            }
        )*];
    }

    test_parse_vlist_error![
        (
            test_empty_vlist,
            r"> \box0=
",
            Error {
                kind: ErrorKind::EmptyVlist,
                line_number: 2
            }
        ),
        (
            test_missing_vbox_prefix,
            r"> \box0=
\hbox(6.94444+0.0)x10.0
",
            Error {
                kind: ErrorKind::MissingVboxPrefix,
                line_number: 2
            }
        ),
        (
            test_missing_vbox_height_depth_separator,
            r"> \box0=
\vbox(6.94444 no plus here)x10.0
",
            Error {
                kind: ErrorKind::MissingVboxHeightDepthSeparator,
                line_number: 2
            }
        ),
        (
            test_missing_vbox_depth_width_separator,
            r"> \box0=
\vbox(6.94444+0.0 no depth width)
",
            Error {
                kind: ErrorKind::MissingVboxDepthWidthSeparator,
                line_number: 2
            }
        ),
        (
            test_vlist_invalid_penalty_value,
            r"> \box0=
\vbox(6.94444+0.0)x10.0
.\penalty abc
",
            Error {
                kind: ErrorKind::InvalidPenaltyValue,
                line_number: 3
            }
        ),
        (
            test_unknown_vlist_keyword,
            r"> \box0=
\vbox(6.94444+0.0)x10.0
.\unknown stuff
",
            Error {
                kind: ErrorKind::UnknownVlistKeyword,
                line_number: 3
            }
        ),
    ];
}
