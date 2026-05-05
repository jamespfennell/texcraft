//! Implementation of TeX's hyphenation algorithm.
//!
//! The main entry point is [`Hyphenator`], which can be constructed with
//! plain TeX's built-in English patterns using [`Hyphenator::plain_tex_en_us`].

use std::collections::HashMap;

/// Hyphenates words using TeX's pattern-matching algorithm (Knuth-Liang).
///
/// Construct with [`Hyphenator::plain_tex_en_us`] for the standard plain TeX
/// English patterns, or load custom patterns with [`Hyphenator::load_patterns`].
#[derive(Default)]
pub struct Hyphenator {
    data: Vec<u8>,
    patterns: Trie,
    exceptions: HashMap<String, Vec<u8>>,
}

#[derive(Debug, Default)]
struct Trie {
    m: HashMap<(TrieVertex, TrieEdge), (TrieVertex, Option<TrieValue>)>,
    r: HashMap<TrieVertex, (TrieVertex, TrieEdge)>,
    next_vertex: TrieVertex,
}

impl Trie {
    fn root(&self) -> TrieVertex {
        TrieVertex(u32::MAX)
    }
    fn next_or(
        &self,
        current: TrieVertex,
        edge: TrieEdge,
    ) -> Option<(TrieVertex, Option<TrieValue>)> {
        self.m.get(&(current, edge)).copied()
    }
    fn next(&mut self, current: TrieVertex, edge: TrieEdge) -> TrieVertex {
        self.m
            .entry((current, edge))
            .or_insert_with(|| {
                let next = self.next_vertex;
                self.next_vertex = TrieVertex(self.next_vertex.0 + 1);
                self.r.insert(next, (current, edge));
                (next, None)
            })
            .0
    }
    fn set_value(&mut self, vertex: TrieVertex, value: TrieValue) {
        self.m.get_mut(&self.r[&vertex]).unwrap().1 = Some(value);
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone, Copy)]
struct TrieVertex(u32);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum TrieEdge {
    StartOfWord,
    Char(char),
    EndOfWord,
}

#[derive(Debug, Clone, Copy)]
struct TrieValue(usize);

impl Hyphenator {
    /// Construct a hyphenator loaded with plain TeX's English (US) patterns and exceptions.
    pub fn plain_tex_en_us() -> Self {
        let patterns = include_str!("plain_tex_patterns.txt");
        let exceptions = include_str!("plain_tex_exceptions.txt");
        let mut h: Self = Default::default();
        h.load_patterns(patterns);
        exceptions
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
            .for_each(|l| {
                h.insert_exception(l);
            });
        h
    }
    /// Load hyphenation patterns from a whitespace-separated string in TeX pattern format.
    pub fn load_patterns(&mut self, patterns: &str) {
        // TeX.2021.961 and onwards.
        // (bool, String, bool) -> Vec<u8>
        for pattern in patterns.split_whitespace() {
            let mut vertex = self.patterns.root();
            let start_of_word = pattern.starts_with('.');
            if start_of_word {
                vertex = self.patterns.next(vertex, TrieEdge::StartOfWord);
            }
            let end_of_word = pattern.ends_with('.');
            let mut word = String::new();
            let mut num_chars = 0_usize;
            let mut scores = Vec::<u8>::new();
            let mut num_scores = 0;
            for c in pattern.chars() {
                match c {
                    '0'..='9' => {
                        while let Some(missing) = num_chars.checked_sub(num_scores) {
                            if missing == 0 {
                                break;
                            }
                            num_scores += missing;
                            scores.push((missing + 10).try_into().unwrap_or(u8::MAX));
                        }
                        // TODO: set digit_sensed=true and hit the error path when the char is invalid
                        scores.push((c as u32 - '0' as u32).try_into().expect("digits are <= 9"));
                        num_scores += 1;
                    }
                    '.' => {
                        // Already handled above.
                        continue;
                    }
                    _ => {
                        // TODO: set to lower case and check for non-chars like #
                        // This is all done using the lower case infra
                        vertex = self.patterns.next(vertex, TrieEdge::Char(c));
                        word.push(c);
                        num_chars += 1;
                    }
                }
            }
            if end_of_word {
                vertex = self.patterns.next(vertex, TrieEdge::EndOfWord);
            }
            let index = self.data.len();
            self.data.extend_from_slice(&scores);
            self.data.push(10);
            self.patterns.set_value(vertex, TrieValue(index));
        }
    }
    /// Add a hyphenation exception. The word is given with hyphens marking the allowed break points,
    /// e.g. `"hy-phen-ation"`.
    pub fn insert_exception(&mut self, hyphenated_word: &str) {
        let mut word = String::new();
        let mut indices = vec![0];
        for c in hyphenated_word.chars() {
            if c == '-' {
                indices.pop();
                indices.push(1);
            } else {
                word.push(c);
                indices.push(0);
            }
        }
        self.exceptions.insert(word, indices);
    }
    /// Hyphenate a word, returning it with `-` inserted at each valid break point.
    pub fn hypthenate(&self, word: &str, target: &mut String) {
        let mut indices = self.calculate_indices(word);
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
    pub fn calculate_indices(&self, word: &str) -> impl Iterator<Item = usize> {
        let s: String;
        let word = if word.chars().any(|c| c.is_ascii_uppercase()) {
            s = word.chars().map(|c| c.to_ascii_lowercase()).collect();
            &s
        } else {
            word
        };
        let total_scores = match self.exceptions.get(word) {
            Some(s) => s.clone(),
            None => {
                let mut total_scores = vec![0_u8; word.len() + 1];

                let mut process = |mut vertex: TrieVertex, lower: usize| {
                    let mut chars = word[lower..].chars();
                    loop {
                        let c = chars.next();
                        let edge = match c {
                            None => TrieEdge::EndOfWord,
                            Some(c) => TrieEdge::Char(c),
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
                    .next_or(self.patterns.root(), TrieEdge::StartOfWord)
                {
                    process(vertex, 0);
                }
                let mut lower = 0;
                while let Some(c) = word[lower..].chars().next() {
                    process(self.patterns.root(), lower);
                    lower += c.len_utf8();
                }
                total_scores[0] = 0;
                total_scores[1] = 0;
                for j in 1..=3 {
                    if let Some(i) = total_scores.len().checked_sub(j) {
                        if let Some(total_score) = total_scores.get_mut(i) {
                            *total_score = 0;
                        };
                    }
                }
                total_scores
            }
        };
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
                    hyphenator.hypthenate(stringify!($name), &mut got);
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
