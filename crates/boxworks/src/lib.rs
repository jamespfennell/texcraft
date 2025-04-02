//! # Boxworks
//!
//! Boxworks is an in-progress implementation of the typesetting engine inside TeX.
//! It is independent of the TeX language.
//! One of the main goals of Boxworks is to support creating new typesetting
//! languages that use this engine to perform the actual typesetting.

pub mod ds;
pub mod tex;

pub trait TextPreprocessor {
    fn add_text(&mut self, text: &str, list: &mut Vec<ds::Horizontal>);
    fn add_space(&mut self, list: &mut Vec<ds::Horizontal>);
}
