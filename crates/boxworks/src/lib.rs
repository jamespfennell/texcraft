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

#[derive(Default, Debug)]
pub struct SimpleTextPreprocessor {
    font: u32,
}

impl TextPreprocessor for SimpleTextPreprocessor {
    fn new_paragraph(&mut self) {}

    fn add_word(&mut self, word: &str, list: &mut Vec<ds::Horizontal>) {
        for char in word.chars() {
            list.push(
                ds::Char {
                    char,
                    font: self.font,
                }
                .into(),
            );
        }
    }
    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>) {
        list.push(
            ds::Glue {
                kind: ds::GlueKind::Normal,
                value: common::Glue {
                    width: common::Scaled::ONE * 10,
                    stretch: common::Scaled::ONE * 4,
                    shrink: common::Scaled::ONE * 4,
                    ..Default::default()
                },
            }
            .into(),
        );
    }
}

pub trait FontRepo {
    fn width(&self, c: char, font: u32) -> Option<common::Scaled>;
}

pub trait LineBreaker {
    fn break_line<F: FontRepo>(
        self,
        font_repo: &F,
        v_list: &mut Vec<ds::Vertical>,
        h_list: &mut Vec<ds::Horizontal>,
    );
}
