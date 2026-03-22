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
    lig_kern_program: tfm::ligkern::CompiledProgram,
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
        ( $namespace: ident, $tfm_path: expr, $( ( $name: ident, $input: expr, $want: expr, ), )+ ) => {
            mod $namespace {
                const TFM: &'static [u8] = include_bytes!($tfm_path);
                $(
                    #[test]
                    fn $name() {
                        let input = $input;
                        let want = $want;
                        super::run_preprocessor_test(TFM, input, want)
                    }
                )+
            }
        };
    }

    preprocessor_tests!(
        cmr10,
        "../../tfm/corpus/computer-modern/cmr10.tfm",
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

    preprocessor_tests!(
        smfebsl,
        "../../tfm/corpus/ctan/smfebsl10-3.tfm",
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
        let c: char = 65_u32.try_into().unwrap();
        println!("----> \"{c}\"");
        let mut tfm_file = tfm::File::deserialize(tfm_bytes).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;

        let want = bwl::parse_horizontal_list(want).unwrap();

        let mut tp: TextPreprocessorImpl = Default::default();
        tp.register_font(0, &tfm_file, lig_kern_program);
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
