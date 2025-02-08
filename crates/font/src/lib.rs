//! Font abstractions and types
//!
//!

/// Trait satisfied by font formats (like .tfm files).
pub trait Format: Sized {
    const DEFAULT_FILE_EXTENSION: &'static str;
    type Error: std::error::Error + 'static;

    /// Parse binary data into a font.
    fn parse(b: &[u8]) -> Result<Self, Self::Error>;
}
