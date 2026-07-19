//! # Boxworks text preprocessor
//!
//! This crate implements the logic that converts text (words and spaces)
//! into horizontal list elements.
//! It is implemented in the Chief Executive chapter in Knuth's
//! TeX (starting in TeX.2021.1029).

use boxworks::ds;
use std::collections::HashMap;
use tfm::ligkern;

#[derive(Debug)]
struct Font {
    default_space: common::Glue,
    extra_space: common::Scaled,
    lig_kern_program: tfm::ligkern::CompiledProgram,
}

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
pub struct TextPreprocessorImpl {
    fonts: Vec<Font>,
    // TODO: should be initialized to the null font
    // TODO: should current_font be some kind of specific font identifier type.
    current_font: u32,
    space_factor: SpaceFactor,
    pub params: Params,
}

impl TextPreprocessorImpl {
    pub fn new(params: Params) -> Self {
        Self {
            fonts: vec![],
            current_font: 0,
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

impl TextPreprocessorImpl {
    pub fn activate_font(&mut self, font: u32) {
        self.current_font = font;
    }
}

impl boxworks::TextPreprocessor for TextPreprocessorImpl {
    fn new_paragraph(&mut self) {
        self.space_factor = Default::default();
    }

    fn add_word(&mut self, word: &str, list: &mut Vec<ds::Horizontal>) {
        let font = &self.fonts[self.current_font as usize];
        for elem in font.lig_kern_program.run(word, None) {
            use ligkern::RunItem::*;
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
                Ligature(ligature) => {
                    let ins_disc = ligature.original.as_ref().ends_with('-');
                    list.push(
                        ds::Ligature {
                            char: ligature.c,
                            font: self.current_font,
                            original_chars: ligature.original,
                            includes_left_boundary: ligature.includes_left_boundary,
                            includes_right_boundary: ligature.includes_right_boundary,
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

    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>) {
        let g = if self.space_factor == SpaceFactor::default() {
            // TeX.2021.1041
            if !self.params.space_skip.is_zero() {
                self.params.space_skip
            } else {
                // TeX.2021.1042
                self.fonts[self.current_font as usize].default_space
            }
        } else {
            // TeX.2021.1043
            if self.space_factor.0 >= 2000 && !self.params.extra_space_skip.is_zero() {
                self.params.extra_space_skip
            } else if !self.params.space_skip.is_zero() {
                self.params.space_skip
            } else {
                // TeX.2021.1042
                let mut g = self.fonts[self.current_font as usize].default_space;
                // TeX.2021.1044
                if self.space_factor.0 >= 2000 {
                    g.width += self.fonts[self.current_font as usize].extra_space;
                }
                g.stretch = g.stretch.xn_over_d(self.space_factor.0, 1000).unwrap().0;
                g.shrink = g.shrink.xn_over_d(1000, self.space_factor.0).unwrap().0;
                g
            }
        };
        list.push(ds::Horizontal::Glue(g.into()));
    }
}

impl TextPreprocessorImpl {
    pub fn register_font(
        &mut self,
        id: u32,
        tfm_file: &tfm::File,
        lig_kern_program: tfm::ligkern::CompiledProgram,
    ) {
        assert_eq!(id as usize, self.fonts.len());
        self.fonts.push(Font {
            default_space: common::Glue {
                width: tfm_file
                    .named_param_scaled(tfm::NamedParameter::Space)
                    .unwrap(),
                stretch: tfm_file
                    .named_param_scaled(tfm::NamedParameter::Stretch)
                    .unwrap(),
                stretch_order: common::GlueOrder::Normal,
                shrink: tfm_file
                    .named_param_scaled(tfm::NamedParameter::Shrink)
                    .unwrap(),
                shrink_order: common::GlueOrder::Normal,
            },
            extra_space: tfm_file
                .named_param_scaled(tfm::NamedParameter::ExtraSpace)
                .unwrap(),
            lig_kern_program,
        });
    }
}

#[derive(Debug, Default)]
pub struct TfmFontRepo {
    fonts: HashMap<u32, tfm::File>,
}

impl TfmFontRepo {
    pub fn register_font(&mut self, id: u32, tfm_file: tfm::File) {
        assert_eq!(id as usize, self.fonts.len());
        self.fonts.insert(id, tfm_file);
    }
}

impl boxworks::FontRepo for TfmFontRepo {
    fn width(&self, c: char, font: u32) -> Option<common::Scaled> {
        self.fonts[&font].width_utf8(c)
    }
    fn height(&self, c: char, font: u32) -> Option<common::Scaled> {
        self.fonts[&font].height_utf8(c)
    }
    fn depth(&self, c: char, font: u32) -> Option<common::Scaled> {
        self.fonts[&font].depth_utf8(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use boxworks::TextPreprocessor;
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

        let mut tfm_file = tfm::File::deserialize(tfm_bytes).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;

        let mut tp = TextPreprocessorImpl::new(params);
        tp.register_font(0, &tfm_file, lig_kern_program);
        tp.activate_font(0);
        let mut got = vec![];
        for word in input.split_inclusive(' ') {
            tp.add_word(word.trim_matches(' '), &mut got);
            if word.ends_with(" ") {
                tp.add_space(&mut got);
            }
        }

        assert_box_eq!(got, want);
    }
}
