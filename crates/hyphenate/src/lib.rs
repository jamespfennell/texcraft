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
    // The data u8s have the following meaning.
    // The bottom four bits (u8 % 16) contain the op code. For 0..=9, this says
    // emit this score. 10 and above means: terminate.
    // The top four bits (u8 / 16) is the number of zero scores to emit before
    // performing the operation above.
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
                        let num_zeros: u8 = match state {
                            State::AfterChar(mut n) => {
                                while let Some(m) = n.checked_sub(16) {
                                    // This outputs 15 in the high bits and 0 in the low bits.
                                    // We will then output 15 0 scores from the high bits and 1 0 score from the
                                    // low bits, so 16 0s in total.
                                    self.data.push(15 * 16);
                                    n = m;
                                }
                                n.try_into().expect("n<16 which fits in u8")
                            }
                            State::AfterScore => {
                                // In the case of two consecutive scores (e.g. a123b)
                                // it seems the last one wins. See. TeX.2021.962.
                                self.data.pop();
                                0
                            }
                        };
                        let op: u8 = (c as u32 - '0' as u32).try_into().expect("digits are <= 9");
                        self.data.push(op + num_zeros * 16);
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
            let terminal_op = if pattern.ends_with('.') {
                (vertex, value) = self.patterns.next(vertex, trie::Edge::EndOfWord);
                11
            } else {
                10
            };
            let num_zeros: u8 = match state {
                State::AfterChar(mut n) => {
                    while let Some(m) = n.checked_sub(16) {
                        // This outputs 15 in the high bits and 0 in the low bits.
                        // We will then output 15 0 scores from the high bits and 1 0 score from the
                        // low bits, so 16 0s in total.
                        self.data.push(15 * 16);
                        n = m;
                    }
                    n.try_into().expect("n<16 which fits in u8")
                }
                State::AfterScore => 0,
            };
            self.data.push(terminal_op + num_zeros * 16);
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
        self.calculate_aggregate_scores(lower_caser, word)
            .into_iter()
            .enumerate()
            .filter(|(_, score)| *score % 2 != 0)
            .map(|(i, _)| i)
    }
    fn calculate_aggregate_scores<L: LowerCaser>(&self, lower_caser: &L, word: &str) -> Vec<u8> {
        let mut scores = vec![0_u8; word.len() + 1];
        self.for_each_pattern(lower_caser, word, |p| {
            let mut k = 0;
            for op in p.data {
                let num_zeros = op / 16;
                k += (num_zeros) as usize;
                let op = op % 16;
                match op {
                    score @ ..10 => {
                        if scores[p.offset + k] < score {
                            scores[p.offset + k] = score;
                        }
                        k += 1;
                    }
                    _ => {
                        break;
                    }
                }
            }
        });
        let num_chars = word.chars().count();
        // Never hyphenate before the word.
        scores[0] = 0;
        // Never hyphenate after the word.
        scores.truncate(num_chars);
        scores
    }

    /// Return the set of character indices before which a hyphen may be inserted.
    pub fn calculate_explanation<L: LowerCaser>(&self, lower_caser: &L, word: &str) -> Explanation {
        let lower_cased: String = word
            .chars()
            .map(|c| lower_caser.to_lower_case(c).unwrap_or(c))
            .collect();
        // let mut total_scores = vec![0_u8; word.len() + 1];
        let mut patterns: Vec<MatchedPattern> = vec![];
        self.for_each_pattern(lower_caser, word, |p| {
            let mut scores: Vec<u8> = vec![];
            let mut it = p.data.iter();
            let end_of_word = loop {
                let Some(op) = it.next() else { break false };
                let num_zeros = op / 16;
                scores.resize(scores.len() + num_zeros as usize, 0_u8);
                let op = op % 16;
                match op {
                    score @ ..10 => {
                        scores.push(score);
                    }
                    _ => {
                        break op == 11;
                    }
                }
            };
            patterns.push(MatchedPattern {
                start_of_word: p.start_of_word,
                offset: p.offset,
                chars: lower_cased
                    .chars()
                    .skip(p.offset)
                    .take(p.num_chars)
                    .collect(),
                end_of_word,
                scores,
            });
        });
        Explanation {
            lower_cased,
            patterns,
            aggregate_scores: self.calculate_aggregate_scores(lower_caser, word),
        }
    }

    /// Runs the provided closure for every matching pattern.
    ///
    /// The arguments to the cluster are: the operations for the pattern, whether the
    /// pattern is for the start of the word only, and the offset within the word
    /// that the pattern starts.
    fn for_each_pattern<L: LowerCaser, F: FnMut(Pattern)>(
        &self,
        lower_caser: &L,
        word: &str,
        mut for_each: F,
    ) {
        let mut process =
            |mut vertex: trie::Vertex, start_of_word: bool, lower: usize, lower_chars: usize| {
                let mut chars = word[lower..].chars().map(|c| lower_caser.to_lower_case(c));
                let mut num_chars = 0_usize;
                loop {
                    num_chars += 1;
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
                    for_each(Pattern {
                        start_of_word,
                        offset: lower_chars,
                        num_chars,
                        data: &self.data[pattern.0..],
                    });
                }
            };
        if let Some((vertex, _)) = self
            .patterns
            .next_or(self.patterns.root(), trie::Edge::StartOfWord)
        {
            process(vertex, true, 0, 0);
        }
        let mut lower_chars: usize = 0;
        let mut lower = 0;
        while let Some(c) = word[lower..].chars().next() {
            if lower_caser.to_lower_case(c).is_none() {
                break;
            }
            process(self.patterns.root(), false, lower, lower_chars);
            lower += c.len_utf8();
            lower_chars += 1;
        }
    }
}

struct Pattern<'a> {
    start_of_word: bool,
    offset: usize,
    num_chars: usize,
    data: &'a [u8],
}

/// Remove all `-` characters from a word.
pub fn strip_hyphens(word: &str) -> String {
    word.chars().filter(|c| *c != '-').collect()
}

/// Explanation of why a word is hyphenated the way it is.
#[derive(Debug, PartialEq)]
pub struct Explanation {
    /// Lower cased word.
    pub lower_cased: String,
    /// All patterns that matched the word.
    pub patterns: Vec<MatchedPattern>,
    /// Aggregate scores among all scores for matching patterns.
    pub aggregate_scores: Vec<u8>,
}

impl std::fmt::Display for Explanation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let n = self.lower_cased.chars().count();
        write!(f, ".")?;
        for c in self.lower_cased.chars() {
            write!(f, " {}", c)?;
        }
        writeln!(f, " .")?;
        for _ in 0..n {
            write!(f, "--")?;
        }
        writeln!(f, "---")?;
        for pattern in &self.patterns {
            writeln!(f, "{pattern}")?;
        }
        for _ in 0..n {
            write!(f, "--")?;
        }
        writeln!(f, "---")?;
        for score in &self.aggregate_scores {
            write!(f, " {score}")?;
        }
        writeln!(f)?;
        for score in &self.aggregate_scores {
            write!(f, " {}", if score % 2 == 0 { ' ' } else { '-' })?;
        }
        writeln!(f)?;
        write!(f, ".")?;
        for (i, c) in self.lower_cased.chars().enumerate() {
            write!(
                f,
                "{}{}",
                if self.aggregate_scores.get(i).unwrap_or(&0) % 2 == 0 {
                    ' '
                } else {
                    '-'
                },
                c
            )?;
        }
        writeln!(f, " .")?;
        Ok(())
    }
}

/// Pattern matched when performing hyphenation.
#[derive(Debug, PartialEq)]
pub struct MatchedPattern {
    /// Whether this pattern only matches starts of words.
    pub start_of_word: bool,
    /// Offset in the word, in characters,that the pattern starts.
    pub offset: usize,
    /// Characters in the pattern.
    pub chars: String,
    /// Whether this pattern only matches ends of words.
    pub end_of_word: bool,
    /// Scores for this pattern.
    pub scores: Vec<u8>,
}

impl std::fmt::Display for MatchedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.start_of_word { '.' } else { ' ' })?;
        for _ in 0..self.offset {
            write!(f, "  ")?;
        }
        let mut chars = self.chars.chars();
        for score in &self.scores {
            if *score == 0 {
                write!(f, " {}", chars.next().unwrap_or(' '))?;
            } else {
                write!(f, "{}{}", score, chars.next().unwrap_or(' '))?;
            };
        }
        if self.end_of_word {
            write!(f, " .")?;
        }
        Ok(())
    }
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
    use pretty_assertions::assert_eq;

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
        // From the TeXBook.
        // But the TeXBook assumes that \leftminhyphen=\rightminhyphen=3 so the results are a little different.
        record => "record",
        hyphenation => "hy-phen-ation",
        concatenation => "con-cate-na-tion",
        supercalifragilisticexpialidocious => "su-per-cal-ifrag-ilis-tic-ex-pi-ali-do-cious",
        bachelor => "bach-e-lor",
        echelon => "ech-e-lon",
        toothaches => "toothaches",
        campfire => "camp-fire",
        biorhythm => "biorhyth-m",
        algorithm => "al-go-rith-m",
        pneumonoultramicroscopicsilicovolcanoconiosis => "p-neu-monoul-tra-mi-cro-scop-ic-sil-i-co-vol-canoco-nio-sis",
        project => "project",
        present => "present",
        table => "ta-ble",
        Table => "Ta-ble",
        // From running the hyphenator over /usr/share/dict/words on Mac
        ach => "ach",
        Aaronic => "Aa-ron-ic",
        Abelia => "A-beli-a",
        William => "William",
        chaffless => "chaf-f-less",
    );

    macro_rules! explanation_tests {
        ( $( $name:ident => ($expected:expr, $want_explain: expr,), )* ) => {
            $(
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    let hyphenator = Hyphenator::plain_tex_en_us();
                    let lower_caser: AsciiLowerCaser = Default::default();
                    let got = hyphenator.calculate_explanation( &lower_caser, stringify!($name));
                    assert_eq!(got, $expected);
                    assert_eq!(format!["{}", got], $want_explain);
                }
            )*
        };
    }

    explanation_tests!(
            DifFicult => (Explanation{
                lower_cased: "difficult".into(),
                patterns: vec![
                    // d1if
                    MatchedPattern{
                        start_of_word: false,
                        offset: 0,
                        chars: "dif".into(),
                        end_of_word: false,
                        scores: vec![0, 1, 0],
                   },
                   // 4f1f
                   MatchedPattern{
                       start_of_word: false,
                       offset: 2,
                       chars: "ff".into(),
                       end_of_word: false,
                       scores: vec![4, 1],
                  },
                   // 1fi
                   MatchedPattern{
                       start_of_word: false,
                       offset: 3,
                       chars: "fi".into(),
                       end_of_word: false,
                       scores: vec![1, 0],
                  },
                   // fi3cu
                   MatchedPattern{
                       start_of_word: false,
                       offset: 3,
                       chars: "ficu".into(),
                       end_of_word: false,
                       scores: vec![0, 0, 3, 0],
                  },
                   // 4lt
                   MatchedPattern{
                       start_of_word: false,
                       offset: 7,
                       chars: "lt".into(),
                       end_of_word: false,
                       scores: vec![4, 0],
                  },
                ],
                aggregate_scores: vec![0, 1, 4, 1, 0, 3, 0, 4, 0],
            },
". d i f f i c u l t .
---------------------
  d1i f
     4f1f
       1f i
        f i3c u
               4l t
---------------------
 0 1 4 1 0 3 0 4 0
   -   -   -      
. d-i f-f i-c u l t .
",
            ),
            cove => (Explanation{
                lower_cased: "cove".into(),
                patterns: vec![
                    // 1co
                    MatchedPattern{
                        start_of_word: false,
                        offset: 0,
                        chars: "co".into(),
                        end_of_word: false,
                        scores: vec![1, 0],
                    },
                    // cov1
                    MatchedPattern{
                        start_of_word: false,
                        offset: 0,
                        chars: "cov".into(),
                        end_of_word: false,
                        scores: vec![0, 0, 0, 1],
                    },
                    // cove4
                    MatchedPattern{
                        start_of_word: false,
                        offset: 0,
                        chars: "cove".into(),
                        end_of_word: false,
                        scores: vec![0, 0, 0, 0, 4],
                    },
                    // 4ve.
                    MatchedPattern{
                        start_of_word: false,
                        offset: 2,
                        chars: "ve".into(),
                        end_of_word: true,
                        scores: vec![4, 0],
                    },
                ],
                aggregate_scores: vec![0, 0, 4, 1],
            },
". c o v e .
-----------
 1c o
  c o v1 
  c o v e4 
     4v e .
-----------
 0 0 4 1
       -
. c o v-e .
",
            ),
            antce => (Explanation{
                lower_cased: "antce".into(),
                patterns: vec![
                    // .ant4
                    MatchedPattern{
                        start_of_word: true,
                        offset: 0,
                        chars: "ant".into(),
                        end_of_word: false,
                        scores: vec![0, 0, 0, 4],
                    },
                    // a2n
                    MatchedPattern{
                        start_of_word: false,
                        offset: 0,
                        chars: "an".into(),
                        end_of_word: false,
                        scores: vec![0, 2],
                    },
                    // n1t
                    MatchedPattern{
                        start_of_word: false,
                        offset: 1,
                        chars: "nt".into(),
                        end_of_word: false,
                        scores: vec![0, 1],
                    },
                    // 4tc
                    MatchedPattern{
                        start_of_word: false,
                        offset: 2,
                        chars: "tc".into(),
                        end_of_word: false,
                        scores: vec![4, 0],
                    },
                    // 2ce.
                    MatchedPattern{
                        start_of_word: false,
                        offset: 3,
                        chars: "ce".into(),
                        end_of_word: true,
                        scores: vec![2, 0],
                    },
                ],
                aggregate_scores: vec![0, 2, 4, 4, 0],
            },
". a n t c e .
-------------
. a n t4 
  a2n
    n1t
     4t c
       2c e .
-------------
 0 2 4 4 0
          
. a n t c e .
",
    ),
    );
}
