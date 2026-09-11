//! # Boxworks
//!
//! Boxworks is an in-progress implementation of the typesetting engine inside TeX.
//! It is independent of the TeX language.
//! One of the main goals of Boxworks is to support creating new typesetting
//! languages that use this engine to perform the actual typesetting.

pub mod ds;
pub mod lang;
pub mod tex;

pub trait LineBreaker {
    fn break_line(self, v_list: &mut Vec<ds::Vertical>, h_list: &mut Vec<ds::Horizontal>);
}

pub trait Hyphenator {
    fn hyphenate(&self, list: &mut Vec<ds::Horizontal>);
}
