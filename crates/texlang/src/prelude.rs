//! Texlang prelude.
//!
//! This is just an idea.
//! We may get rid of the prelude and just make all of the public
//! aliases at the crate level.

/// Result type in Texlang.
pub type Result<T> = std::result::Result<T, crate::vm::ShutdownSignal>;
