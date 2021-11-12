//! # Texlang: the TeX language interpreter.
//!
//! This crate implements a general purpose TeX language interpreter called Texlang.
//! Throughout this crate the Rust module documentation is intentionally brief.
//! Full documentation is provided in the Texlang book.
//!
//! It is important to understand that Texlang is only concerned with
//!   the pure language features of TeX.
//! These, by themselves, have almost nothing to do with typesetting.
//! In Texlang's view, TeX is merely a scripting language that provides a certain notion of
//!   "command", along with a macro expansion system and dynamic lexing rules.
//! In TeX distributions, the commands usually do something related to typesetting,
//!   but that has nothing to do with the language as such.
//! It is possible to use Texlang to create, say, a sort of "TeX bash" system containing commands
//!   like `\ls` and `\mkdir` which read TeX tokens and perform the appropriate system operation.
//! (This is probably not a good idea!)
//!
//! Texlang is designed to be a building block for TeX engines.
//! It has clear, modular APIs for things like state, commands and variables.
//! These are systematically discussed in the book.
//!
//! The sibling crate texlang_stdlib implements a standard library for Texlang.
//! This library consists of general purpose execution and expansion commands like `\def` and `\the`.
//! These commands, again, have nothing to do with typesetting.

extern crate texcraft_stdext;

pub mod command;
pub mod error;
pub mod parse;
pub mod prelude;
pub mod runtime;
pub mod texmacro;
pub mod token;
pub mod variable;
