//! # Boxworks text preprocessor
//!
//! This crate implements the logic that converts text (words and spaces)
//! into horizontal list elements.
//! It is implemented in the Chief Executive chapter in Knuth's
//! TeX (starting in TeX.2021.1029).

use boxworks::ds;
use common::font;

pub struct Params {
    pub space_factor_codes: SpaceFactorCodes,
    pub space_skip: common::Glue,
    pub extra_space_skip: common::Glue,
}

impl Params {
    /// Output the parameters in TeX format.
    pub fn tex(&self) -> String {
        let Params {
            space_factor_codes,
            space_skip,
            extra_space_skip,
        } = self;
        _ = space_factor_codes;
        format!(
            r"
            \spaceskip={space_skip}
            \xspaceskip={extra_space_skip}
        "
        )
    }
}

impl Default for Params {
    fn default() -> Self {
        Self::plain_tex_defaults()
    }
}

impl Params {
    pub fn plain_tex_defaults() -> Self {
        Self {
            space_factor_codes: SpaceFactorCodes::plain_tex_defaults(),
            space_skip: common::Glue::ZERO,
            extra_space_skip: common::Glue::ZERO,
        }
    }
}

pub struct TextPreprocessor {
    current_font: font::Id,
    space_factor: SpaceFactor,
    pub params: Params,
}

impl TextPreprocessor {
    pub fn new(params: Params) -> Self {
        Self {
            current_font: font::Id::NULL,
            space_factor: Default::default(),
            params,
        }
    }
}

pub struct SpaceFactorCodes(pub [i32; 256]);

impl Default for SpaceFactorCodes {
    fn default() -> Self {
        Self::plain_tex_defaults()
    }
}

impl SpaceFactorCodes {
    pub fn plain_tex_defaults() -> Self {
        let mut a = [1000_i32; 256];
        for (c, value) in [
            // From plain.tex
            (')', 0),
            ('\'', 0),
            (']', 0),
            // From \nonfrenchspacing in plain.tex
            ('.', 3000),
            ('?', 3000),
            ('!', 3000),
            (':', 2000),
            (';', 1500),
            (',', 1250),
        ] {
            a[c as usize] = value;
        }
        for c in 'A'..='Z' {
            // INITTEX
            a[c as usize] = 999;
        }
        Self(a)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SpaceFactor(pub i32);

impl Default for SpaceFactor {
    fn default() -> Self {
        Self(1000)
    }
}

impl SpaceFactor {
    fn adjust(&mut self, c: char, codes: &SpaceFactorCodes) {
        // TeX.2021.1034
        let new: i32 = codes.0.get(c as usize).copied().unwrap_or(1000);
        if new > 0 && new <= 1000 {
            self.0 = new;
        } else if new > 1000 {
            if self.0 < 1000 {
                self.0 = 1000
            } else {
                self.0 = new
            }
        }
    }
}

impl TextPreprocessor {
    pub fn activate_font(&mut self, font: font::Id) {
        self.current_font = font;
    }

    pub fn new_paragraph(&mut self) {
        self.space_factor = Default::default();
    }

    pub fn add_word<Font: font::TextBuilder>(
        &mut self,
        font_repo: &font::Repo<Font>,
        word: &str,
        list: &mut Vec<ds::Horizontal>,
    ) {
        // TeX.2021.1034
        let font = font_repo.get(self.current_font);
        for elem in font.build_text(word.chars(), Default::default()) {
            use font::TextItem::*;
            match elem {
                Char(c) => {
                    list.push(
                        ds::Char {
                            char: c,
                            font: self.current_font,
                        }
                        .into(),
                    );
                    // TeX.2021.1035
                    // TODO: \hyphenchar
                    if c == '-' {
                        list.push(ds::Discretionary::default().into());
                    }
                }
                Kern(kern) => {
                    list.push(
                        ds::Kern {
                            width: kern,
                            kind: ds::KernKind::Normal,
                        }
                        .into(),
                    );
                }
                Ligature {
                    c,
                    original,
                    includes_left_boundary,
                    includes_right_boundary,
                } => {
                    let ins_disc = original.as_ref().ends_with('-');
                    list.push(
                        ds::Ligature {
                            char: c,
                            font: self.current_font,
                            original_chars: original,
                            includes_left_boundary,
                            includes_right_boundary,
                        }
                        .into(),
                    );
                    // TeX.2021.1035
                    // TODO: \hyphenchar
                    if ins_disc {
                        list.push(ds::Discretionary::default().into());
                    }
                }
            }
        }
        // TODO: consider merging this loop with the loop in the lig/kern program.
        // We can change the run method to accept a callback that is invoked for
        // each character.
        for c in word.chars() {
            self.space_factor.adjust(c, &self.params.space_factor_codes);
        }
    }

    pub fn add_space<Font: font::Format>(
        &mut self,
        font_repo: &font::Repo<Font>,
        list: &mut Vec<ds::Horizontal>,
    ) {
        let g = if self.space_factor == SpaceFactor::default() {
            // TeX.2021.1041
            if !self.params.space_skip.is_zero() {
                self.params.space_skip
            } else {
                // TeX.2021.1042
                font_repo.get(self.current_font).default_space()
            }
        } else {
            // TeX.2021.1043
            if self.space_factor.0 >= 2000 && !self.params.extra_space_skip.is_zero() {
                self.params.extra_space_skip
            } else if !self.params.space_skip.is_zero() {
                self.params.space_skip
            } else {
                // TeX.2021.1042
                let mut g = font_repo.get(self.current_font).default_space();
                // TeX.2021.1044
                if self.space_factor.0 >= 2000 {
                    g.width += font_repo.get(self.current_font).extra_space();
                }
                g.stretch = g.stretch.xn_over_d(self.space_factor.0, 1000).unwrap().0;
                g.shrink = g.shrink.xn_over_d(1000, self.space_factor.0).unwrap().0;
                g
            }
        };
        list.push(ds::Horizontal::Glue(g.into()));
    }

    pub fn add_text<Font: font::Format + font::TextBuilder>(
        &mut self,
        font_repo: &font::Repo<Font>,
        text: &str,
        list: &mut Vec<ds::Horizontal>,
    ) {
        self.new_paragraph();
        let mut pending_space = text.chars().next().unwrap_or(' ').is_ascii_whitespace();
        for word in text.split_ascii_whitespace() {
            if pending_space {
                self.add_space(font_repo, list);
            }
            let word = word.trim_matches(' ');
            self.add_word(font_repo, word.trim_matches(' '), list);
            pending_space = true;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use boxworks_testing;
    use boxworks_testing::assert_box_eq;
    use boxworks_testing::assert_box_lossy_eq;

    macro_rules! preprocessor_tests {
        (
            $namespace: ident,
            $tfm: ident,
            $( (
                $name: ident,
                $input: expr,
                $want: expr,
                $( params: Params {
                    $( $param_name: ident: $param_value: expr, )+
                }, )?
            ), )+ ) => {
                mod $namespace {
                    use super::*;
                    $(
                        #[test]
                        fn $name() {
                            let tfm = super::$tfm;
                            let input = $input;
                            let want = $want;
                            let params = Params {
                                $( $(
                                    $param_name: $param_value,
                                )+ )?
                                .. Params::plain_tex_defaults()
                            };
                            run_preprocessor_test(tfm, params, input, want)
                        }
                    )+
                }
        };
    }

    const TFM_CMR10: &'static [u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");

    preprocessor_tests!(
        cmr10,
        TFM_CMR10,
        (
            basic,
            "second",
            r#"
                chars("second")
            "#,
        ),
        (
            basic_with_space,
            "sec ond",
            r#"
                chars("sec")
                glue(3.33333pt, 1.66666pt, 1.11111pt)
                chars("ond")
            "#,
        ),
        (
            kern_ao,
            "AO",
            r#"
                chars("A")
                kern(-0.27779pt)
                chars("O")
            "#,
        ),
        (
            kern_av,
            "AV",
            r#"
                chars("A")
                kern(-1.11113pt)
                chars("V")
            "#,
        ),
        (
            ligature_1,
            "ff",
            r#"
                lig("\u{b}", "ff")
            "#,
        ),
        (
            ligature_2,
            "ffi",
            r#"
                lig("\u{e}", "ffi")
            "#,
        ),
        (
            ragged_right,
            "a b. c",
            r##"
                chars("a")
                glue(3.33298pt, 0.0pt, 0.0pt)
                chars("b.")
                glue(5.0pt, 0.0pt, 0.0pt)
                chars("c")
            "##,
            params: Params {
                space_skip: common::Glue {
                    width: common::Scaled::parse_from_string("3.33298pt").unwrap(),
                    ..Default::default()
                },
                extra_space_skip: common::Glue {
                    width: common::Scaled::parse_from_string("5.0pt").unwrap(),
                    ..Default::default()
                },
            },
        ),
    );

    macro_rules! spacing_tests {
        ( $( ( $name: ident, $input: expr, $want: expr, ), )+ ) => {
            mod spacing {
                $(
                    #[test]
                    fn $name() {
                        let tfm = super::TFM_CMR10;
                        let input = format!["{} a", $input];
                        let want =  format![r#"
                            chars("{}")
                            {}
                            chars("a")
                        "#, $input, $want];
                        super::run_preprocessor_test(tfm, Default::default(), &input, &want)
                    }
                )+
            }
        };
    }

    spacing_tests!(
        // These tests are testing the default space factors in plain.tex.
        (default_1, "a;", "glue(3.33333pt, 2.49998pt, 0.74074pt)",),
        (default_2, "a,", "glue(3.33333pt, 2.08331pt, 0.88889pt)",),
        (default_3, "a.", "glue(4.44444pt, 4.99997pt, 0.37036pt)",),
        (default_4, "a:", "glue(4.44444pt, 3.33331pt, 0.55556pt)",),
        // The next tests are for the adjust_space_factor function.
        // The SF is adjusted based on both its current value and the SF
        // of the next character. We first test 16 possible cases where
        // current and next are in the following 4 classes: zero, small
        // (less than 1000), normal (1000), large (greater than 1000).
        (
            adjust_space_factor_zero_zero,
            "))",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_zero_small,
            ")A",
            "glue(3.33333pt, 1.66498pt, 1.11221pt)",
        ),
        (
            adjust_space_factor_zero_normal,
            ")a",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_zero_large,
            ").",
            "glue(4.44444pt, 4.99997pt, 0.37036pt)",
        ),
        (
            adjust_space_factor_small_zero,
            "A)",
            "glue(3.33333pt, 1.66498pt, 1.11221pt)",
        ),
        (
            adjust_space_factor_small_small,
            "AA",
            "glue(3.33333pt, 1.66498pt, 1.11221pt)",
        ),
        (
            adjust_space_factor_small_normal,
            "Aa",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_small_large,
            "A.",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_normal_zero,
            "a)",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_normal_small,
            "aA",
            "glue(3.33333pt, 1.66498pt, 1.11221pt)",
        ),
        (
            adjust_space_factor_normal_normal,
            "aa",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_normal_large,
            "a.",
            "glue(4.44444pt, 4.99997pt, 0.37036pt)",
        ),
        (
            adjust_space_factor_large_zero,
            ".)",
            "glue(4.44444pt, 4.99997pt, 0.37036pt)",
        ),
        (
            adjust_space_factor_large_small,
            ".A",
            "glue(3.33333pt, 1.66498pt, 1.11221pt)",
        ),
        (
            adjust_space_factor_large_normal,
            ".a",
            "glue(3.33333pt, 1.66666pt, 1.11111pt)",
        ),
        (
            adjust_space_factor_large_large,
            "..",
            "glue(4.44444pt, 4.99997pt, 0.37036pt)",
        ),
    );

    const TFM_SMFEBSL: &'static [u8] = include_bytes!("../../tfm/corpus/ctan/smfebsl10-3.tfm");

    preprocessor_tests!(
        smfebsl,
        TFM_SMFEBSL,
        (
            basic_with_space,
            "sec ond",
            r#"
                chars("sec")
                glue(4.78204pt, 2.39102pt, 1.19551pt)
                chars("on")
                kern(-0.49814pt)
                chars("d")
            "#,
        ),
        (
            numbers_start_of_word,
            "123B",
            r##"
                lig("$", "", includes_left_boundary="true")
                chars("123")
                lig("#", "")
                chars("B")
            "##,
        ),
        (
            numbers_mid_word,
            "A123B",
            r##"
                chars("A")
                lig("$", "")
                chars("123")
                lig("#", "")
                chars("B")
            "##,
        ),
        (
            numbers_end_of_word,
            "A123",
            r##"
                chars("A")
                lig("$", "")
                chars("123")
                lig("#", "", includes_right_boundary="true")
            "##,
        ),
    );

    fn run_preprocessor_test(tfm_bytes: &[u8], params: Params, input: &str, want: &str) {
        if std::env::var("TEXCRAFT_VERIFY").unwrap_or_default() == "tex" {
            use std::path::PathBuf;
            let mut auxiliary_files: HashMap<PathBuf, Vec<u8>> = Default::default();
            auxiliary_files.insert("customFont.tfm".into(), tfm_bytes.to_vec());
            let preamble = format!(
                r"
                {}
                \font \customFont customFont
                \customFont
                ",
                params.tex(),
            );
            let mut tex_engine = boxworks::tex::new_tex_engine_binary("tex".to_string()).unwrap();
            let (_, mut tex_got) = boxworks::tex::build_horizontal_lists(
                tex_engine.as_mut(),
                &auxiliary_files,
                &preamble,
                &mut [input.to_string()].iter(),
                /*hyphenate=*/ false,
            );
            let tex_got = tex_got.remove(0).list;
            // The lossy comparison is used because TeX's box dumps represent
            // the boundary character in a ligature's original characters with
            // a `|` marker rather than as separate fields.
            assert_box_lossy_eq!(want, tex_got);
            return;
        }

        let tfm_font = tfm::Font::build_from_bytes(tfm_bytes)
            .expect("tfm file is valid")
            .0;
        let mut font_repo: font::Repo<tfm::Font> = Default::default();
        let font_id = font_repo.register(tfm_font);

        let mut tp = TextPreprocessor::new(params);
        tp.activate_font(font_id);
        let mut got = vec![];
        for word in input.split_inclusive(' ') {
            tp.add_word(&font_repo, word.trim_matches(' '), &mut got);
            if word.ends_with(" ") {
                tp.add_space(&font_repo, &mut got);
            }
        }

        assert_box_eq!(got, want);
    }
}
