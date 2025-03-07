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

use std::collections::HashMap;

use crate::node;

/// Build a horizontal list from some text.
///
/// This function works by putting the text inside a TeX `\hbox{}`,
///     and then instructing TeX to describe the contents of the box.
///
/// This function is the inverse of TeX.2021.173 and onwards
/// (part 12 of TeX: displaying boxes).
pub fn build_horizontal_list(s: &str) -> (Vec<String>, Vec<node::Horizontal>) {
    let mut v = vec![];
    let mut fonts = vec![];
    let mut font_to_idx: HashMap<&str, usize> = Default::default();
    let tex = CONVERT_TEXT_TEMPLATE.replace("<content>", s);
    let output = run_in_tex(&tex);
    let mut found = false;
    for line in output.lines() {
        if line.starts_with("Texcraft: begin") {
            found = true;
            continue;
        }
        if !found {
            continue;
        }
        if line.starts_with("Texcraft: end") {
            break;
        }
        if let Some(glue_spec) = line.strip_prefix(r".\glue") {
            let mut words = glue_spec.split_ascii_whitespace();
            let width = parse_scaled(words.next().expect("glue has 5 words"));
            assert_eq!(words.next(), Some("plus"));
            let stretch = parse_scaled(words.next().expect("glue has 5 words"));
            assert_eq!(words.next(), Some("minus"));
            let shrink = parse_scaled(words.next().expect("glue has 5 words"));
            v.push(
                node::Glue {
                    kind: node::GlueKind::Normal,
                    value: core::Glue {
                        width,
                        stretch,
                        shrink,
                        ..Default::default()
                    },
                }
                .into(),
            );
            continue;
        }
        if let Some(kern_spec) = line.strip_prefix(r".\kern") {
            let mut words = kern_spec.split_ascii_whitespace();
            let width = parse_scaled(words.next().expect("glue has 1 word"));
            v.push(
                node::Kern {
                    kind: node::KernKind::Normal,
                    width,
                }
                .into(),
            );
            continue;
        }
        if let Some(char_spec) = line.strip_prefix(r".\") {
            let mut words = char_spec.split_ascii_whitespace();
            let font_name = words.next().expect("char has 2 words");
            use std::collections::hash_map::Entry;
            let font_idx = match font_to_idx.entry(font_name) {
                Entry::Occupied(occupied_entry) => *occupied_entry.get(),
                Entry::Vacant(vacant_entry) => {
                    let idx = fonts.len();
                    fonts.push(font_name.to_string());
                    vacant_entry.insert(idx);
                    idx
                }
            };
            let font_idx: u32 = font_idx.try_into().expect("no more than 2^32 fonts");
            let char = parse_char(words.next().expect("char has 2 words"));
            if words.next() == Some("(ligature") {
                let og_chars = words.next().expect("lig has 4 words");
                let og_chars = og_chars.strip_suffix(")").expect("lig ends with ')'");
                v.push(
                    node::Ligature {
                        included_left_boundary: false,
                        included_right_boundary: false,
                        char,
                        font: font_idx,
                        original_chars: og_chars.to_string(),
                    }
                    .into(),
                );
            } else {
                v.push(
                    node::Char {
                        char,
                        font: font_idx,
                    }
                    .into(),
                );
            }
            continue;
        }
    }
    (fonts, v)
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
    for (k, c) in parts.next().expect("scaled has a fractional part").chars().enumerate() {
        f[k] = c
            .to_digit(10)
            .expect("fractional part are digits")
            .try_into()
            .unwrap();
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

% Put the content we want to see in box 0.
\setbox0=\hbox{<content>}

% Add a start marker so we know where to begin in the log
\message{Texcraft: begin}
% Show the box!
\showbox0
% Add a start marker so we know where to end in the log
\message{Texcraft: end}

\end
";

fn run_in_tex(input: &str) -> String {
    let mut input_path = std::env::temp_dir();
    input_path.push("tex-input");
    input_path.set_extension("tex");
    std::fs::write(&input_path, input).expect("Unable to write file");

    let output = std::process::Command::new("tex")
        .arg(&input_path)
        .output()
        .expect("failed to run tex command");

    String::from_utf8(output.stdout).unwrap()
}
