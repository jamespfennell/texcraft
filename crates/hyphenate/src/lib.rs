//! Implementation of TeX's hyphenation algorithm.
//!
//! The main entry point is [`Hyphenator`], which can be constructed with
//! plain TeX's built-in English patterns using [`Hyphenator::plain_tex_en_us`].

/// Hyphenates words using TeX's pattern-matching algorithm (Knuth-Liang).
///
/// Construct with [`Hyphenator::plain_tex_en_us`] for the standard plain TeX
/// English patterns, or load custom patterns with [`Hyphenator::load_patterns`].
#[derive(Default)]
pub struct Hyphenator {
    data: Vec<u8>,
    patterns: trie::Trie,
}

/// Implementations of this trait can get the lower case character of a character.
pub trait LowerCaser {
    /// Return the lower case character of the provided character, of [`None`] if the character
    /// does not have a lower case character.
    fn to_lower_case(&self, c: char) -> Option<char>;
}

/// The ASCII implementation of [`LowerCaser`] returns the lower case letter for all ASCII
/// alphabetic characters, and [`None`] for all other characters.
#[derive(Default)]
pub struct AsciiLowerCaser {}

impl LowerCaser for AsciiLowerCaser {
    fn to_lower_case(&self, c: char) -> Option<char> {
        if c.is_ascii_alphabetic() {
            Some(c.to_ascii_lowercase())
        } else {
            None
        }
    }
}

impl Hyphenator {
    /// Construct a hyphenator loaded with plain TeX's English (US) patterns and exceptions.
    pub fn plain_tex_en_us() -> Self {
        let patterns = include_str!("plain_tex_patterns.txt");
        let exceptions = include_str!("plain_tex_exceptions.txt");
        let mut h: Self = Default::default();
        h.load_patterns(patterns);
        h.insert_exceptions(exceptions);
        h
    }
    /// Load hyphenation patterns from a whitespace-separated string in TeX pattern format.
    pub fn load_patterns(&mut self, patterns: &str) {
        // TeX.2021.961 and onwards.
        let mut empty_value: Option<trie::Value> = None;
        for pattern in patterns.split_whitespace() {
            let mut vertex = self.patterns.root();
            let mut value = &mut empty_value;
            if pattern.starts_with('.') {
                (vertex, value) = self.patterns.next(vertex, trie::Edge::StartOfWord);
            }
            let data_start = self.data.len();
            enum State {
                // The payload is the number of characters before this character
                // that were not assigned a score. If we write out a score for this
                // character, we will first write out zero scores for the other
                // characters. This is a serialization optimization.
                AfterChar(usize),
                AfterScore,
            }
            let mut state = State::AfterChar(0);
            for c in pattern.chars() {
                match c {
                    '0'..='9' => {
                        match state {
                            State::AfterChar(mut n) => {
                                while n > 0 {
                                    let code = (n + 10).try_into().unwrap_or(u8::MAX);
                                    self.data.push(code);
                                    n -= (code - 10) as usize;
                                }
                            }
                            State::AfterScore => {
                                // In the case of two consecutive scores (e.g. a123b)
                                // it seems the last one wins. See. TeX.2021.962.
                                self.data.pop();
                            }
                        }
                        self.data
                            .push((c as u32 - '0' as u32).try_into().expect("digits are <= 9"));
                        state = State::AfterScore;
                    }
                    '.' => {
                        // Already handled above.
                        continue;
                    }
                    _ => {
                        // TODO: we should error if it's not a valid pattern like
                        // // the `help1` in TeX.2021.962.
                        (vertex, value) = self.patterns.next(vertex, trie::Edge::Char(c));
                        state = State::AfterChar(match state {
                            State::AfterChar(n) => n + 1,
                            State::AfterScore => 0,
                        });
                    }
                }
            }
            if pattern.ends_with('.') {
                (vertex, value) = self.patterns.next(vertex, trie::Edge::EndOfWord);
            }
            self.data.push(10);
            *value = Some(trie::Value(data_start));
        }
    }
    /// Add multiple hyphenation exceptions. These are separate words separated by whitespace, with
    /// each word satisfying the format in [`Self::insert_exception`].
    pub fn insert_exceptions(&mut self, hyphenated_words: &str) {
        hyphenated_words
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
            .for_each(|l| {
                self.insert_exception(l);
            });
    }
    /// Add a hyphenation exception. The word is given with hyphens marking the allowed break points,
    /// e.g. `"hy-phen-ation"`.
    pub fn insert_exception(&mut self, hyphenated_word: &str) {
        let mut vertex = self.patterns.root();
        vertex = self.patterns.next(vertex, trie::Edge::StartOfWord).0;
        let data_start = self.data.len();
        let mut word = String::new();
        let mut indices = vec![0];
        self.data.push(6);
        for c in hyphenated_word.chars() {
            if c == '-' {
                indices.pop();
                indices.push(7);
                self.data.pop();
                self.data.push(7);
            } else {
                vertex = self.patterns.next(vertex, trie::Edge::Char(c)).0;
                word.push(c);
                indices.push(6);
                self.data.push(6);
            }
        }
        self.data.push(10);
        let value = self.patterns.next(vertex, trie::Edge::EndOfWord).1;
        *value = Some(trie::Value(data_start));
    }
    /// Hyphenate a word, returning it with `-` inserted at each valid break point.
    pub fn hypthenate<L: LowerCaser>(&self, lower_caser: &L, word: &str, target: &mut String) {
        let mut indices = self.calculate_indices(lower_caser, word);
        let mut next = indices.next();
        for (i, c) in word.chars().enumerate() {
            if next == Some(i) {
                target.push('-');
                next = indices.next();
            }
            target.push(c);
        }
    }
    /// Return the set of character indices before which a hyphen may be inserted.
    pub fn calculate_indices<L: LowerCaser>(
        &self,
        lower_caser: &L,
        word: &str,
    ) -> impl Iterator<Item = usize> {
        let mut total_scores = vec![0_u8; word.len() + 1];

        let mut process = |mut vertex: trie::Vertex, lower: usize| {
            let mut chars = word[lower..].chars().map(|c| lower_caser.to_lower_case(c));
            loop {
                let edge = match chars.next() {
                    None => trie::Edge::EndOfWord,
                    Some(None) => {
                        // Some(None) occurs when the words contains a non-letter character.
                        // In this case we stop trying to hyphenate.
                        return;
                    }
                    Some(Some(c)) => trie::Edge::Char(c),
                };
                let pattern;
                (vertex, pattern) = match self.patterns.next_or(vertex, edge) {
                    None => return,
                    Some(entry) => entry,
                };
                let Some(pattern) = pattern else {
                    continue;
                };
                let mut k = 0;
                for op in self.data[pattern.0..].iter() {
                    match op {
                        score @ ..10 => {
                            if total_scores[lower + k] < *score {
                                total_scores[lower + k] = *score;
                            }
                            k += 1;
                        }
                        10 => {
                            break;
                        }
                        _ => {
                            k += (op - 10) as usize;
                        }
                    }
                }
            }
        };
        if let Some((vertex, _)) = self
            .patterns
            .next_or(self.patterns.root(), trie::Edge::StartOfWord)
        {
            process(vertex, 0);
        }
        let mut num_chars: usize = 0;
        let mut lower = 0;
        while let Some(c) = word[lower..].chars().next() {
            if lower_caser.to_lower_case(c).is_none() {
                break;
            }
            process(self.patterns.root(), lower);
            lower += c.len_utf8();
            num_chars += 1;
        }
        total_scores[0] = 0;
        total_scores[1] = 0;
        total_scores.truncate(num_chars.saturating_sub(2_usize));
        total_scores
            .into_iter()
            .enumerate()
            .filter(|(_, score)| *score % 2 != 0)
            .map(|(i, _)| i)
    }
}

/// Remove all `-` characters from a word.
pub fn strip_hyphens(word: &str) -> String {
    word.chars().filter(|c| *c != '-').collect()
}

mod trie {
    use std::collections::HashMap;

    #[derive(Debug, Default)]
    pub struct Trie {
        m: HashMap<(Vertex, Edge), (Vertex, Option<Value>)>,
        next_vertex: Vertex,
    }

    impl Trie {
        pub fn root(&self) -> Vertex {
            Vertex(u32::MAX)
        }
        pub fn next_or(&self, current: Vertex, edge: Edge) -> Option<(Vertex, Option<Value>)> {
            self.m.get(&(current, edge)).copied()
        }
        pub fn next(&mut self, current: Vertex, edge: Edge) -> (Vertex, &mut Option<Value>) {
            let (a, b) = self.m.entry((current, edge)).or_insert_with(|| {
                let next = self.next_vertex;
                self.next_vertex = Vertex(self.next_vertex.0 + 1);
                (next, None)
            });
            (*a, b)
        }
    }

    #[derive(Debug, PartialEq, Eq, Hash, Default, Clone, Copy)]
    pub struct Vertex(u32);

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    pub enum Edge {
        StartOfWord,
        Char(char),
        EndOfWord,
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Value(pub usize);
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! hyphenation_tests {
        ( $( $name:ident => $expected:literal, )* ) => {
            $(
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    let hyphenator = Hyphenator::plain_tex_en_us();
                    let mut got = String::new();
                    let lower_caser: AsciiLowerCaser = Default::default();
                    hyphenator.hypthenate( &lower_caser, stringify!($name), &mut got);
                    assert_eq!(got, $expected);
                }
            )*
        };
    }

    hyphenation_tests!(
        // From the TeXBook
        record => "record",
        hyphenation => "hy-phen-ation",
        concatenation => "con-cate-na-tion",
        supercalifragilisticexpialidocious => "su-per-cal-ifrag-ilis-tic-ex-pi-ali-do-cious",
        bachelor => "bach-e-lor",
        echelon => "ech-e-lon",
        toothaches => "toothaches",
        campfire => "camp-fire",
        biorhythm => "biorhythm",
        algorithm => "al-go-rithm",
        pneumonoultramicroscopicsilicovolcanoconiosis => "pneu-monoul-tra-mi-cro-scop-ic-sil-i-co-vol-canoco-nio-sis",
        project => "project",
        present => "present",
        table => "ta-ble",
        Table => "Ta-ble",
        // From running the hyphenator over /usr/share/dict/words on Mac
        ach => "ach",
        Aaronic => "Aa-ronic",
        Abelia => "Abelia",
        William => "William",
        chaffless => "chaf-f-less",
    );
}
