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

#[wasm_bindgen]
pub fn explain(word: &str) -> String {
    let h = HYPHENATOR.get_or_init(Hyphenator::plain_tex_en_us);
    let exp = h.calculate_explanation(&AsciiLowerCaser::default(), word);
    let mut j = String::from("{\"word\":\"");
    push_json_str(&mut j, &exp.lower_cased);
    j.push_str("\",\"patterns\":[");
    for (pi, p) in exp.patterns.iter().enumerate() {
        if pi > 0 {
            j.push(',');
        }
        j.push_str("{\"offset\":");
        j.push_str(&p.offset.to_string());
        j.push_str(",\"chars\":\"");
        push_json_str(&mut j, &p.chars);
        j.push_str("\",\"scores\":[");
        for (si, s) in p.scores.iter().enumerate() {
            if si > 0 {
                j.push(',');
            }
            j.push_str(&s.to_string());
        }
        j.push_str("],\"start_of_word\":");
        j.push_str(if p.start_of_word { "true" } else { "false" });
        j.push_str(",\"end_of_word\":");
        j.push_str(if p.end_of_word { "true" } else { "false" });
        j.push('}');
    }
    j.push_str("],\"aggregate_scores\":[");
    for (si, s) in exp.aggregate_scores.iter().enumerate() {
        if si > 0 {
            j.push(',');
        }
        j.push_str(&s.to_string());
    }
    j.push_str("]}");
    j
}

fn push_json_str(out: &mut String, s: &str) {
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(c),
        }
    }
}
