//! # Boxworks
//!
//! Boxworks is an in-progress implementation of the typesetting engine inside TeX.
//! It is independent of the TeX language.
//! One of the main goals of Boxworks is to support creating new typesetting
//! languages that use this engine to perform the actual typesetting.

pub mod ds;
pub mod tex;

pub trait TextPreprocessor {
    fn new_paragraph(&mut self);

    fn add_word(&mut self, word: &str, list: &mut Vec<ds::Horizontal>);

    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>);

    fn add_text(&mut self, text: &str, list: &mut Vec<ds::Horizontal>) {
        self.new_paragraph();
        let mut pending_space = text.chars().next().unwrap_or(' ').is_ascii_whitespace();
        for word in text.split_ascii_whitespace() {
            if pending_space {
                self.add_space(list);
            }
            let word = word.trim_matches(' ');
            self.add_word(word.trim_matches(' '), list);
            pending_space = true;
        }
    }
}
