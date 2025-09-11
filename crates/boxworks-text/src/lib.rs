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
pub struct Font {
    pub default_space: core::Glue,
    pub lig_kern_program: tfm::ligkern::CompiledProgram,
}

#[derive(Default)]
pub struct TextPreprocessorImpl {
    fonts: Vec<Font>,
    // TODO: should be initialized to the null font
    // TODO: should current_font be some kind of specific font identifier type.
    current_font: u32,
}

impl boxworks::TextPreprocessor for TextPreprocessorImpl {
    fn add_text(&mut self, text: &str, list: &mut Vec<ds::Horizontal>) {
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
        font.lig_kern_program.run(text, &mut e);
    }

    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>) {
        // TeX.2021.1041
        // TODO: space factor, etc.
        list.push(ds::Horizontal::Glue(
            self.fonts[self.current_font as usize].default_space.into(),
        ));
    }
}

impl TextPreprocessorImpl {
    pub fn register_font(&mut self, id: u32, font: Font) {
        assert_eq!(id as usize, self.fonts.len());
        self.fonts.push(font);
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
        ( $( ( $name: ident, $input: expr, $want: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let want = $want;
                    run_preprocessor_test(input, want)
                }
            )+
        };
    }

    preprocessor_tests!(
        (
            cmr10_basic,
            "second",
            r#"
                text("second", font=0)
            "#,
        ),
        (
            cmr10_basic_with_space,
            "sec ond",
            r#"
                text("sec", font=0)
                glue(3.33333pt, 1.66666pt, 1.11111pt)
                text("ond", font=0)
            "#,
        ),
        (
            cmr10_kern_ao,
            "AO",
            r#"
                text("A", font=0)
                kern(-0.27779pt)
                text("O", font=0)
            "#,
        ),
        (
            cmr10_kern_av,
            "AV",
            r#"
                text("A", font=0)
                kern(-1.11113pt)
                text("V", font=0)
            "#,
        ),
        (
            cmr10_ligature,
            "ff",
            r#"
                lig("\u{b}", "ff", font=0)
            "#,
        ),
    );

    fn run_preprocessor_test(input: &str, want: &str) {
        let mut tfm_file = {
            let raw = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");
            tfm::File::deserialize(raw).0.unwrap()
        };

        let want = bwl::parse_horizontal_list(want).unwrap();

        let mut tp: TextPreprocessorImpl = Default::default();
        tp.register_font(
            0,
            Font {
                default_space: core::Glue {
                    // TODO: read these from the tfm file params
                    // We need to convert FixWord to Scaled
                    // and then adjust by the design size...
                    // So the following line is not good enough - it's off
                    // by the design size of ~10
                    // width: tfm_file.params[1].to_scaled(),
                    width: core::Scaled::ONE * 10 / 3,
                    stretch: core::Scaled::ONE * 5 / 3,
                    stretch_order: core::GlueOrder::Normal,
                    shrink: core::Scaled::ONE * 10 / 9 + core::Scaled(1),
                    shrink_order: core::GlueOrder::Normal,
                },
                lig_kern_program: tfm::ligkern::CompiledProgram::compile_from_tfm_file(
                    &mut tfm_file,
                )
                .0,
            },
        );
        tp.activate_font(0);
        let mut got = vec![];
        for word in input.split_inclusive(' ') {
            tp.add_text(word.trim_matches(' '), &mut got);
            if word.ends_with(" ") {
                tp.add_space(&mut got);
            }
        }

        assert_eq!(got, want);
    }
}
