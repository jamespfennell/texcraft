//! # Boxworks text preprocessor
//!
//! This crate implements the logic that converts text (words and spaces)
//! into horizontal list elements.
//! It is implemented in the Chief Executive chapter in Knuth's
//! TeX (starting in TeX.2021.1029).

use std::rc::Rc;

use boxworks::ds;
use tfm::ligkern;

#[derive(Debug)]
struct Font {
    default_space: core::Glue,
    extra_space: core::Scaled,
    lig_kern_program: tfm::ligkern::CompiledProgram,
}

#[derive(Default)]
pub struct TextPreprocessorImpl {
    fonts: Vec<Font>,
    // TODO: should be initialized to the null font
    // TODO: should current_font be some kind of specific font identifier type.
    current_font: u32,
    space_factor: SpaceFactor,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SpaceFactor(pub u16);

impl Default for SpaceFactor {
    fn default() -> Self {
        Self(1000)
    }
}

impl SpaceFactor {
    fn adjust(&mut self, c: char) {
        // TeX.2021.1034
        // TODO: implement \sfcode and make this mapping configurable.
        let new: u16 = match c {
            // From plain.tex
            ')' | '\'' | ']' => 0,
            // From \nonfrenchspacing in plain.tex
            '.' | '?' | '!' => 3000,
            ':' => 2000,
            ';' => 1500,
            ',' => 1250,
            // INITTEX
            'A'..='Z' => 999,
            // Default for other chars.
            _ => 1000,
        };
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

impl boxworks::TextPreprocessor for TextPreprocessorImpl {
    fn new_paragraph(&mut self) {
        self.space_factor = Default::default();
    }

    fn add_word(&mut self, word: &str, list: &mut Vec<ds::Horizontal>) {
        let font = &self.fonts[self.current_font as usize];

        struct Emitter<'a>(&'a mut Vec<ds::Horizontal>, u32);
        impl<'a> ligkern::Emitter for Emitter<'a> {
            fn emit_character(&mut self, c: char) {
                self.0.push(ds::Horizontal::Char(ds::Char {
                    char: c,
                    font: self.1,
                }));
            }
            fn emit_kern(&mut self, kern: core::Scaled) {
                self.0.push(ds::Horizontal::Kern(ds::Kern {
                    width: kern,
                    kind: ds::KernKind::Normal,
                }));
            }
            fn emit_ligature(&mut self, c: char, original: Rc<str>) {
                self.0.push(ds::Horizontal::Ligature(ds::Ligature {
                    included_left_boundary: false,
                    included_right_boundary: false,
                    char: c,
                    font: self.1,
                    original_chars: original,
                }));
            }
        }

        let mut e = Emitter(list, self.current_font);
        font.lig_kern_program.run(word, &mut e);
        // TODO: consider merging this loop with the loop in the lig/kern program.
        // We can change the run method to accept a callback that is invoked for
        // each character.
        for c in word.chars() {
            self.space_factor.adjust(c);
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
            g.stretch = g
                .stretch
                .xn_over_d(self.space_factor.0.into(), 1000)
                .unwrap()
                .0;
            g.shrink = g
                .shrink
                .xn_over_d(1000, self.space_factor.0.into())
                .unwrap()
                .0;
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
            default_space: core::Glue {
                width: tfm_file
                    .named_param_scaled(tfm::NamedParameter::Space)
                    .unwrap(),
                stretch: tfm_file
                    .named_param_scaled(tfm::NamedParameter::Stretch)
                    .unwrap(),
                stretch_order: core::GlueOrder::Normal,
                shrink: tfm_file
                    .named_param_scaled(tfm::NamedParameter::Shrink)
                    .unwrap(),
                shrink_order: core::GlueOrder::Normal,
            },
            extra_space: tfm_file
                .named_param_scaled(tfm::NamedParameter::ExtraSpace)
                .unwrap(),
            lig_kern_program,
        });
    }
    pub fn activate_font(&mut self, id: u32) {
        self.current_font = id;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use boxworks::TextPreprocessor;
    use boxworks_lang as bwl;

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
                text("second", font=0)
            "#,
        ),
        (
            basic_with_space,
            "sec ond",
            r#"
                text("sec", font=0)
                glue(3.33333pt, 1.66666pt, 1.11111pt)
                text("ond", font=0)
            "#,
        ),
        (
            kern_ao,
            "AO",
            r#"
                text("A", font=0)
                kern(-0.27779pt)
                text("O", font=0)
            "#,
        ),
        (
            kern_av,
            "AV",
            r#"
                text("A", font=0)
                kern(-1.11113pt)
                text("V", font=0)
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
                            text("{}", font=0)
                            {}
                            text("a", font=0)
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
                text("sec", font=0)
                glue(4.78204pt, 2.39102pt, 1.19551pt)
                text("on", font=0)
                kern(-0.49814pt)
                text("d", font=0)
            "#,
        ),
        (
            numbers_start_of_word,
            "123B",
            r##"
                lig("$", "|", font=0)
                text("123", font=0)
                lig("#", "", font=0)
                text("B", font=0)
            "##,
        ),
        (
            numbers_mid_word,
            "A123B",
            r##"
                text("A", font=0)
                lig("$", "", font=0)
                text("123", font=0)
                lig("#", "", font=0)
                text("B", font=0)
            "##,
        ),
        /*
        TODO: right boundary char
        (
            numbers_end_of_word,
            "A123",
            r##"
                text("A", font=0)
                lig("$", "", font=0)
                text("123", font=0)
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
