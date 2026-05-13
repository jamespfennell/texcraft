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

#[derive(Default)]
pub struct TextPreprocessorImpl {
    fonts: Vec<Font>,
    // TODO: should be initialized to the null font
    // TODO: should current_font be some kind of specific font identifier type.
    current_font: u32,
    space_factor: SpaceFactor,
    pub space_factor_codes: SpaceFactorCodes,
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

        struct Emitter<'a>(&'a mut Vec<ds::Horizontal>, u32);
        impl<'a> ligkern::Emitter for Emitter<'a> {
            fn emit_character(&mut self, c: char) {
                self.0.push(
                    ds::Char {
                        char: c,
                        font: self.1,
                    }
                    .into(),
                );
                // TeX.2021.1035
                // TODO: \hyphenchar
                if c == '-' {
                    self.0.push(ds::Discretionary::default().into());
                }
            }
            fn emit_kern(&mut self, kern: common::Scaled) {
                self.0.push(
                    ds::Kern {
                        width: kern,
                        kind: ds::KernKind::Normal,
                    }
                    .into(),
                );
            }
            fn emit_ligature(&mut self, ligature: ligkern::Ligature) {
                let ins_disc = ligature.original.as_ref().ends_with('-');
                self.0.push(
                    ds::Ligature {
                        included_left_boundary: false,
                        included_right_boundary: false,
                        char: ligature.c,
                        font: self.1,
                        original_chars: ligature.original,
                    }
                    .into(),
                );
                // TeX.2021.1035
                // TODO: \hyphenchar
                if ins_disc {
                    self.0.push(ds::Discretionary::default().into());
                }
            }
        }

        let mut e = Emitter(list, self.current_font);
        font.lig_kern_program.run(word, &mut e);
        // TODO: consider merging this loop with the loop in the lig/kern program.
        // We can change the run method to accept a callback that is invoked for
        // each character.
        for c in word.chars() {
            self.space_factor.adjust(c, &self.space_factor_codes);
        }
    }

    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>) {
        let mut g = self.fonts[self.current_font as usize].default_space;
        let g = if self.space_factor == SpaceFactor::default() {
            // TeX.2021.1041
            // TODO: implement \spaceskip.
            g
        } else {
            // TeX.2021.1043
            // TODO: implement "xspace skip" and \spaceskip
            if self.space_factor.0 >= 2000 {
                g.width += self.fonts[self.current_font as usize].extra_space;
            }
            g.stretch = g.stretch.xn_over_d(self.space_factor.0, 1000).unwrap().0;
            g.shrink = g.shrink.xn_over_d(1000, self.space_factor.0).unwrap().0;
            g
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
    use boxworks::lang as bwl;
    use boxworks::TextPreprocessor;

    macro_rules! preprocessor_tests {
        ( $namespace: ident, $tfm: ident, $( ( $name: ident, $input: expr, $want: expr, ), )+ ) => {
            mod $namespace {
                $(
                    #[test]
                    fn $name() {
                        let tfm = super::$tfm;
                        let input = $input;
                        let want = $want;
                        super::run_preprocessor_test(tfm, input, want)
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
                chars("second", font=0)
            "#,
        ),
        (
            basic_with_space,
            "sec ond",
            r#"
                chars("sec", font=0)
                glue(3.33333pt, 1.66666pt, 1.11111pt)
                chars("ond", font=0)
            "#,
        ),
        (
            kern_ao,
            "AO",
            r#"
                chars("A", font=0)
                kern(-0.27779pt)
                chars("O", font=0)
            "#,
        ),
        (
            kern_av,
            "AV",
            r#"
                chars("A", font=0)
                kern(-1.11113pt)
                chars("V", font=0)
            "#,
        ),
        (
            ligature_1,
            "ff",
            r#"
                lig("\u{b}", "ff", font=0)
            "#,
        ),
        (
            ligature_2,
            "ffi",
            r#"
                lig("\u{e}", "ffi", font=0)
            "#,
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
                            chars("{}", font=0)
                            {}
                            chars("a", font=0)
                        "#, $input, $want];
                        super::run_preprocessor_test(tfm, &input, &want)
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
                chars("sec", font=0)
                glue(4.78204pt, 2.39102pt, 1.19551pt)
                chars("on", font=0)
                kern(-0.49814pt)
                chars("d", font=0)
            "#,
        ),
        (
            numbers_start_of_word,
            "123B",
            r##"
                lig("$", "|", font=0)
                chars("123", font=0)
                lig("#", "", font=0)
                chars("B", font=0)
            "##,
        ),
        (
            numbers_mid_word,
            "A123B",
            r##"
                chars("A", font=0)
                lig("$", "", font=0)
                chars("123", font=0)
                lig("#", "", font=0)
                chars("B", font=0)
            "##,
        ),
        /*
        TODO: right boundary char
        (
            numbers_end_of_word,
            "A123",
            r##"
                chars("A", font=0)
                lig("$", "", font=0)
                chars("123", font=0)
                lig("#", "|", font=0)
            "##,
        ),
        */
    );

    fn run_preprocessor_test(tfm_bytes: &[u8], input: &str, want: &str) {
        let mut tfm_file = tfm::File::deserialize(tfm_bytes).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;

        let want = bwl::parse_horizontal_list(want).unwrap();

        let mut tp: TextPreprocessorImpl = Default::default();
        tp.register_font(0, &tfm_file, lig_kern_program);
        tp.activate_font(0);
        let mut got = vec![];
        for word in input.split_inclusive(' ') {
            tp.add_word(word.trim_matches(' '), &mut got);
            if word.ends_with(" ") {
                tp.add_space(&mut got);
            }
        }

        assert_eq!(got, want);
    }
}
