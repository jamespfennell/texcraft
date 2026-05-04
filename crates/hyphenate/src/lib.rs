//! Implementation of TeX's hyphenation algorithm.
//!
//! The main entry point is [`Hyphenator`], which can be constructed with
//! plain TeX's built-in English patterns using [`Hyphenator::plain_tex_en_us`].

use std::collections::{HashMap, HashSet};

/// Hyphenates words using TeX's pattern-matching algorithm (Knuth-Liang).
///
/// Construct with [`Hyphenator::plain_tex_en_us`] for the standard plain TeX
/// English patterns, or load custom patterns with [`Hyphenator::load_patterns`].
#[derive(Default)]
pub struct Hyphenator {
    patterns: HashMap<(bool, String, bool), (Vec<u8>, String)>,
    exceptions: HashMap<String, HashSet<usize>>,
}

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
            let start_of_word = pattern.starts_with('.');
            let end_of_word = pattern.ends_with('.');
            let mut word = String::new();
            let mut num_chars = 0_usize;
            let mut scores = Vec::<u8>::new();
            for c in pattern.chars() {
                match c {
                    '0'..='9' => {
                        while scores.len() < num_chars {
                            scores.push(0);
                        }
                        // TODO: set digit_sensed=true and hit the error path when the char is invalid
                        scores.push((c as u32 - '0' as u32).try_into().expect("digits are <= 9"));
                    }
                    '.' => {
                        // Already handled above.
                        continue;
                    }
                    _ => {
                        // TODO: set to lower case and check for non-chars like #
                        // This is all done using the lower case infra
                        word.push(c);
                        num_chars += 1;
                    }
                }
            }
            self.patterns
                .insert((start_of_word, word, end_of_word), (scores, pattern.into()));
        }
    }
    /// Add a hyphenation exception. The word is given with hyphens marking the allowed break points,
    /// e.g. `"hy-phen-ation"`.
    pub fn insert_exception(&mut self, hyphenated_word: &str) {
        let mut word = String::new();
        let mut indices = HashSet::new();
        let mut n = 0_usize;
        for c in hyphenated_word.chars() {
            if c == '-' {
                indices.insert(n);
            } else {
                word.push(c);
                n += 1;
            }
        }
        self.exceptions.insert(word, indices);
    }
    /// Hyphenate a word, returning it with `-` inserted at each valid break point.
    pub fn hypthenate(&self, word: &str) -> String {
        let indices = self.calculate_indices(word);
        let mut got = String::new();
        for (i, c) in word.chars().enumerate() {
            if indices.contains(&i) {
                got.push('-');
            }
            got.push(c);
        }
        got
    }
    /// Return the set of character indices before which a hyphen may be inserted.
    pub fn calculate_indices(&self, word: &str) -> HashSet<usize> {
        if let Some(s) = self.exceptions.get(word) {
            return s.clone();
        }
        let chars: Vec<char> = word.chars().map(|c| c.to_ascii_lowercase()).collect();
        let mut total_scores = vec![0_u8; chars.len() + 1];
        for i in 1..=chars.len() {
            for j in 0..=(chars.len() - i) {
                // considering substrings of length i starting at index j
                let s: String = chars[j..j + i].iter().collect();
                let start_of_words = if j == 0 {
                    vec![true, false]
                } else {
                    vec![false]
                };
                let end_of_words = if j + i == chars.len() {
                    vec![true, false]
                } else {
                    vec![false]
                };
                for start_of_word in start_of_words.clone() {
                    for end_of_word in end_of_words.clone() {
                        let Some(pattern) =
                            self.patterns.get(&(start_of_word, s.clone(), end_of_word))
                        else {
                            continue;
                        };
                        for (k, score) in pattern.0.iter().copied().enumerate() {
                            if total_scores[j + k] < score {
                                total_scores[j + k] = score;
                            }
                        }
                    }
                }
            }
        }
        total_scores
            .iter()
            .enumerate()
            .filter(|(_, score)| *score % 2 != 0)
            .map(|(i, _)| i)
            .filter(|i| *i > 1 && i + 2 < chars.len())
            .collect()
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
                    let got = hyphenator.hypthenate(stringify!($name));
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
        // From running the hyphenator over /usr/share/dict/words on Mac
        ach => "ach",
        Aaronic => "Aa-ronic",
        Abelia => "Abelia",
        William => "William",
        chaffless => "chaf-f-less",
    );
}
