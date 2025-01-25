//! # Typesetting
//!
//! This crate implements the Knuth/TeX typesetting engine.

use tfm::Number;

pub struct Engine {
    font: tfm::Font,
    char_buffer: Option<char>,
    suppress_next_boundary_ligature: bool,
}

// TODO: figure out the char vs Char situation
impl Engine {
    pub fn char_sequence_start(&mut self, c: char) {
        self.char_buffer = Some(c);
    }
    pub fn char_sequence_extend(&mut self, c: char) {
        let left_char = self
            .char_buffer
            .expect("add_first_char() has already been called");
        let replacement = self
            .font
            .lig_kern_program
            .get_replacement(left_char.try_into().unwrap(), c.try_into().unwrap());
        match replacement.left_char_operation {
            tfm::ligkern::LeftCharOperation::Retain => {
                // self.do_char(left_char);
            }
            tfm::ligkern::LeftCharOperation::Delete => {}
            tfm::ligkern::LeftCharOperation::AppendKern(_kern) => {
                // self.do_char(left_char);
                // self.do_kern(kern);
            }
        }
        for (_c, kern) in replacement.middle_chars {
            // self.do_char((*c).into());
            if *kern != Number::ZERO {
                // self.do_kern(*kern);
            }
        }
        self.char_buffer = Some(replacement.last_char.into());
    }
    pub fn char_sequence_finish(&mut self) {
        let last_char = self
            .char_buffer
            .expect("add_first_char() has already been called");
        self.do_char(last_char);
        self.suppress_next_boundary_ligature = false;
    }
    pub fn suppress_next_boundary_ligature(&mut self) {
        self.suppress_next_boundary_ligature = true;
    }
}

impl Engine {
    fn do_char(&mut self, c: char) {
        _ = c;
    }
}
