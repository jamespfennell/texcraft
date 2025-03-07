//! # Boxworks
//!
//! Boxworks is an in-progress implementation of the typesetting engine inside TeX.
//! It is independent of the TeX language.
//! One of the main goals of Boxworks is to support creating new typesetting
//! languages that use this engine to perform the actual typesetting.

pub mod node;
pub mod tex;
