//! Fonts and font repositories.

use crate::Scaled;

/// Identifier for a font.
///
/// The zero value is the null font; real fonts start at 1.
#[derive(PartialEq, Eq, Debug, Copy, Clone, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Id(pub u32);

impl Id {
    /// The null font.
    pub const NULL: Id = Id(0);

    /// The first non-null font ID.
    pub const ONE: Id = Id(1);
}

impl Default for Id {
    /// The default font ID is the null font,
    /// matching TeX's behavior for font variables that have not been set
    /// (TeX.2021.222).
    fn default() -> Self {
        Id::NULL
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A repository (repo) of fonts of a given type.
pub struct Repo<Font> {
    fonts: Vec<Font>,
    next_id: Id,
}

impl<F: Format> Default for Repo<F> {
    fn default() -> Self {
        Self {
            fonts: vec![F::null()],
            next_id: Id(1),
        }
    }
}

impl<FontType> Repo<FontType> {
    /// Register a new font in the repo.
    pub fn register(&mut self, font: FontType) -> Id {
        self.fonts.push(font);
        let id = self.next_id;
        self.next_id = Id(id.0.checked_add(1).expect("no more than 2^32 fonts"));
        id
    }

    /// Get a font from the repository.
    ///
    /// Panics if the font has not been registered.
    pub fn get(&self, id: Id) -> &FontType {
        self.fonts
            .get(id.0 as usize)
            .expect("font has been registered")
    }
}

/// Implementations of this trait provide the minimal set of features
/// required by a font format.
pub trait Format {
    /// Return the null font in this format.
    fn null() -> Self;

    /// Get the width of the character, or [`None`] if the character is not defined in the font.
    fn width(&self, c: char) -> Option<Scaled>;
    /// Get the height of the character, or [`None`] if the character is not defined in the font.
    fn height(&self, c: char) -> Option<Scaled>;
    /// Get the depth of the character, or [`None`] if the character is not defined in the font.
    fn depth(&self, c: char) -> Option<Scaled>;

    /// Get the width, height and depth of the character, or [`None`] if the character is not
    /// defined in the font.
    fn width_height_depth(&self, c: char) -> Option<[Scaled; 3]> {
        Some([
            self.width(c)?,
            self.height(c).unwrap_or(Scaled::ZERO),
            self.depth(c).unwrap_or(Scaled::ZERO),
        ])
    }
}

#[derive(PartialEq, Debug)]
pub enum TextItem {
    Char(char),
    Kern(Scaled),
    Ligature {
        c: char,
        original: std::rc::Rc<str>,
        includes_left_boundary: bool,
        includes_right_boundary: bool,
    },
}

pub trait TextIter: Iterator<Item = TextItem> {
    fn is_separation_point(&self) -> bool;
}

pub trait TextBuilder {
    type TextIter<'a, Word: Iterator<Item = char>>: TextIter
    where
        Self: 'a;
    fn build_text<'a, Word: Iterator<Item = char>>(
        &'a self,
        word: Word,
        options: BuildTextOptions,
    ) -> Self::TextIter<'a, Word>;

    fn has_replacement(&self, left: Option<char>, right: Option<char>) -> bool;
}

pub struct BuildTextOptions {
    pub disable_left_boundary: bool,
    pub right_boundary_override: Option<char>,
}

pub struct DefaultTextIter<Word> {
    word: Word,
}

impl<Word> DefaultTextIter<Word> {
    pub fn new(word: Word) -> Self {
        Self { word }
    }
}

impl<Word: Iterator<Item = char>> Iterator for DefaultTextIter<Word> {
    type Item = TextItem;

    fn next(&mut self) -> Option<Self::Item> {
        self.word.next().map(TextItem::Char)
    }
}

impl<Word: Iterator<Item = char>> TextIter for DefaultTextIter<Word> {
    fn is_separation_point(&self) -> bool {
        true
    }
}

/*
TODO:
- Add a boxworks-tfm crate that wraps TFM and implements all the traits here
- This will have null_font and cmr10 factory methods
- Migrate boxworks-knuthplass -text and -hyphenate to use the new mechanisms
 */
