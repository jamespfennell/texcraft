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
use std::collections::HashMap;

/// Implementations of this trait can run TeX source code and return stdout.
pub trait TexEngine {
    /// Run the provided TeX source code and return stdout.
    fn run_and_return_stdout(&self, tex_source_code: &str) -> String;
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
    fn run_and_return_stdout(&self, tex_source_code: &str) -> String {
        let mut input_path = std::env::temp_dir();
        input_path.push("tex-input");
        input_path.set_extension("tex");
        std::fs::write(&input_path, tex_source_code).expect("Unable to write file");

        let output = std::process::Command::new(&self.0)
            .arg(&input_path)
            .output()
            .expect("failed to run tex command");

        String::from_utf8(output.stdout).expect("stdout output of TeX is utf-8")
    }
}

/// Build a horizontal list from some text.
///
/// This function works by putting the text inside a TeX `\hbox{}`,
///     and then instructing TeX to describe the contents of the box.
///
/// This function is the inverse of TeX.2021.173 and onwards
/// (part 12 of TeX: displaying boxes).
pub fn build_horizontal_lists(
    tex_engine: &dyn TexEngine,
    contents: &mut dyn Iterator<Item = &String>,
) -> (HashMap<String, u32>, Vec<ds::HList>) {
    let mut fonts: HashMap<String, u32> = Default::default();
    let mut hlists = vec![];
    let macro_calls: Vec<String> = contents.map(|s| format!(r#"\printBox{{{s}}}"#)).collect();
    let tex_source_code = CONVERT_TEXT_TEMPLATE.replace("<print_calls>", &macro_calls.join("\n\n"));
    let output = tex_engine.run_and_return_stdout(&tex_source_code);

    enum Expect {
        Begin,
        Dimension,
        Content(ds::HList),
    }
    let mut expect = Expect::Begin;
    for line in output.lines() {
        expect = match expect {
            Expect::Begin => {
                if !line.starts_with("Texcraft: begin") {
                    Expect::Begin
                } else {
                    Expect::Dimension
                }
            }
            Expect::Dimension => {
                // We want \hbox(height+depth)xwidth
                if let Some(s) = line.strip_prefix(r"\hbox(") {
                    let i = s
                        .find('+')
                        .expect("hbox dimension spec has a + between height and depth");
                    let height = parse_scaled(&s[..i]);
                    let s = &s[i + 1..];
                    let i = s
                        .find(")x")
                        .expect("hbox dimension spec has a )x between depth and width");
                    let depth = parse_scaled(&s[..i]);
                    let width = parse_scaled(&s[i + 2..]);
                    Expect::Content(ds::HList {
                        height,
                        width,
                        depth,
                        ..Default::default()
                    })
                } else {
                    Expect::Dimension
                }
            }
            Expect::Content(mut hlist) => {
                if line.starts_with("Texcraft: end") {
                    hlists.push(hlist);
                    Expect::Begin
                } else if let Some(glue_spec) = line.strip_prefix(r".\glue") {
                    let mut words = glue_spec.split_ascii_whitespace();
                    let width = parse_scaled(words.next().expect("glue has 5 words"));
                    assert_eq!(words.next(), Some("plus"));
                    let stretch = parse_scaled(words.next().expect("glue has 5 words"));
                    assert_eq!(words.next(), Some("minus"));
                    let shrink = parse_scaled(words.next().expect("glue has 5 words"));
                    hlist.list.push(
                        ds::Glue {
                            kind: ds::GlueKind::Normal,
                            value: core::Glue {
                                width,
                                stretch,
                                shrink,
                                ..Default::default()
                            },
                        }
                        .into(),
                    );
                    Expect::Content(hlist)
                } else if let Some(kern_spec) = line.strip_prefix(r".\kern") {
                    let mut words = kern_spec.split_ascii_whitespace();
                    let width = parse_scaled(words.next().expect("glue has 1 word"));
                    hlist.list.push(
                        ds::Kern {
                            kind: ds::KernKind::Normal,
                            width,
                        }
                        .into(),
                    );
                    Expect::Content(hlist)
                } else if let Some(char_spec) = line.strip_prefix(r".\") {
                    let mut words = char_spec.split_ascii_whitespace();
                    let font_name = words.next().expect("char has 2 words");
                    use std::collections::hash_map::Entry;
                    let num_fonts: u32 = fonts.len().try_into().expect("no more than 2^32 fonts");
                    let font = match fonts.entry(font_name.to_string()) {
                        Entry::Occupied(occupied_entry) => *occupied_entry.get(),
                        Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(num_fonts);
                            num_fonts
                        }
                    };
                    let char = parse_char(words.next().expect("char has 2 words"));
                    if words.next() == Some("(ligature") {
                        let og_chars = words.next().expect("lig has 4 words");
                        let og_chars = og_chars.strip_suffix(")").expect("lig ends with ')'");
                        hlist.list.push(
                            ds::Ligature {
                                included_left_boundary: false,
                                included_right_boundary: false,
                                char,
                                font,
                                original_chars: og_chars.to_string(),
                            }
                            .into(),
                        );
                    } else {
                        hlist.list.push(ds::Char { char, font }.into());
                    }
                    Expect::Content(hlist)
                } else {
                    Expect::Content(hlist)
                }
            }
        };
    }
    (fonts, hlists)
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

fn parse_scaled(s: &str) -> core::Scaled {
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
    let f = core::Scaled::from_decimal_digits(&f);
    if i < 0 {
        -core::Scaled::new(-i, f, core::ScaledUnit::Point).expect("scaled is in bounds")
    } else {
        core::Scaled::new(i, f, core::ScaledUnit::Point).expect("scaled is in bounds")
    }
}

const CONVERT_TEXT_TEMPLATE: &str = r"
% After showing a box, TeX stops and waits for user input.
% The following command suppresses that behavior.
\nonstopmode

% Output the box description to the terminal, from which we'll read it.
\tracingonline=1

% Output up to 1 million nodes.
\showboxbreadth=1000000

% Prints the contents on its own line in the terminal.
\def\fullLineMessage#1{
    {
        \newlinechar=`@
        \message{@#1@}
    }
}

\def\printBox#1{
    % Put the content we want to see in box 0.
    \setbox0=\hbox{#1}
    % Add a start marker so we know where to begin in the log
    \fullLineMessage{Texcraft: begin}
    % Show the box!
    \showbox0

    % Add a start marker so we know where to end in the log
    \fullLineMessage{Texcraft: end}
}

<print_calls>

\end
";

#[cfg(test)]
mod tests {
    use super::*;

    struct MockTexEngine(String);

    impl TexEngine for MockTexEngine {
        fn run_and_return_stdout(&self, _: &str) -> String {
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
        let (got_fonts, got_list) =
            build_horizontal_lists(&tex_engine, &mut vec!["".to_string()].iter());

        let want_list = ds::HList {
            height: parse_scaled("6.94444"),
            width: parse_scaled("56.66678"),
            depth: core::Scaled::ZERO,
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
                    value: core::Glue {
                        width: parse_scaled("3.33333"),
                        stretch: parse_scaled("1.66666"),
                        stretch_order: core::GlueOrder::Normal,
                        shrink: parse_scaled("1.11111"),
                        shrink_order: core::GlueOrder::Normal,
                    },
                }
                .into(),
                ds::Char { char: 'a', font: 0 }.into(),
                ds::Char { char: 'n', font: 0 }.into(),
                ds::Char { char: 'd', font: 0 }.into(),
                ds::Glue {
                    kind: ds::GlueKind::Normal,
                    value: core::Glue {
                        width: parse_scaled("3.33333"),
                        stretch: parse_scaled("1.66666"),
                        stretch_order: core::GlueOrder::Normal,
                        shrink: parse_scaled("1.11111"),
                        shrink_order: core::GlueOrder::Normal,
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
}
