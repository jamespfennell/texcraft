//! # Texlang: a TeX language interpreter.
//!
//! This crate implements a general purpose TeX language interpreter called Texlang.
//!
//! Most of the high level documentation is provided in the [Texcraft website](https://texcraft.dev),
//! including guided introductions to using Texlang to build TeX interpreters.
//! The documentation here is mostly for reference.

extern crate texcraft_stdext;

pub mod command;
pub mod error;
pub mod parse;
pub mod texmacro;
pub mod token;
pub mod variable;
pub mod vm;

/// Module that re-exports all of the crate's traits.
///
/// This is useful for getting all of the traits in scope in a Rust module:
/// ```
/// use texlang_core::traits::*;
/// ```
pub mod traits {
    pub use super::vm::ExpandedStream;
    pub use super::vm::HasComponent;
    pub use super::vm::TokenStream;
}
