//! Texcraft primitive library.
//!
//! This module contains implementations of TeX primtives for Texcraft.

pub mod catcodecmd;
pub mod conditional;
pub mod def;
#[macro_use]
pub mod registers;
pub mod execwhitespace;
pub mod texcraft;
pub mod the;
pub mod time;
pub mod variableops;

/// A state struct that is compatible with every primitive in the Texcraft commands library.
///
/// This is temporary state struct until we create states corresponding to actualy TeX engines.
pub struct WholeLibraryState {
    registers: registers::Component,
    time: time::Component,
}

impl WholeLibraryState {
    pub fn new() -> WholeLibraryState {
        WholeLibraryState {
            registers: registers::Component::new(),
            time: time::Component::new(),
        }
    }
}

implement_has_registers![WholeLibraryState, registers];

impl time::HasTime for WholeLibraryState {
    fn get_time(&self) -> &time::Component {
        &self.time
    }
    fn get_time_mut(&mut self) -> &mut time::Component {
        &mut self.time
    }
}
