//! # Boxworks text preprocessor
//!
//! This crate implements the logic that converts text (words and spaces)
//! into horizontal list elements.
//! It is implemented in the Chief Executive chapter in Knuth's
//! TeX (starting in TeX.2021.1029).

use boxworks::ds;

#[derive(Default)]
pub struct TextPreprocessorImpl {
    fonts: Vec<usize>,
    // TODO: should be initialized to the null font
    // TODO: should current_font be some kind of specific font identifier type.
    current_font: u32,
}

impl boxworks::TextPreprocessor for TextPreprocessorImpl {
    fn add_text(&mut self, text: &str, list: &mut Vec<ds::Horizontal>) {
        // TODO: kerns
        for c in text.chars() {
            list.push(ds::Horizontal::Char(ds::Char {
                char: c,
                font: self.current_font,
            }));
        }
    }

    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>) {
        // TODO: glues, space factor, etc.
        _ = list;
        todo!()
    }
}

impl TextPreprocessorImpl {
    pub fn register_font(&mut self, id: u32) {
        assert_eq!(id as usize, self.fonts.len());
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
    #[test]
    fn test_basic() {
        let input = "second";
        let want = r#"
            text("second", font=0)
        "#;
        let want = bwl::parse_horizontal_list(want).unwrap();

        let mut tp: TextPreprocessorImpl = Default::default();
        tp.register_font(0);
        tp.activate_font(0);
        let mut got = vec![];
        tp.add_text(input, &mut got);

        assert_eq!(got, want);
    }
}
