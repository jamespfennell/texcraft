use hyphenate::{AsciiLowerCaser, Hyphenator};
use std::sync::OnceLock;
use wasm_bindgen::prelude::*;

static HYPHENATOR: OnceLock<Hyphenator> = OnceLock::new();

#[wasm_bindgen]
pub fn hyphenate(word: &str) -> String {
    let h = HYPHENATOR.get_or_init(Hyphenator::plain_tex_en_us);
    let mut result = String::new();
    h.hypthenate(&AsciiLowerCaser::default(), word, &mut result);
    result
}
