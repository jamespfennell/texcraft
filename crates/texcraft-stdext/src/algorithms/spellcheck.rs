//! Spell checking using Levenshtein distance
//!
//! This module contains a simple spell check facilty in the form a [find_close_words]
//! function. This function accepts a word and a dictionary, which is a list of valid words.
//! It find the words in the dictionary that are closest to the original world, where "closest"
//! is defined using
//! [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance).
//!
//! The result of the search is a list of [WordDiffs](WordDiff).
//! This data structure describes how the initial word can be changed to the dictionary word using a combination
//! of keep, add, subtract and modify [DiffOps](DiffOp).
//!
//! ## Implementation notes
//!
//! The Levenshtein distance calculation is implemented using dynamic programming and has the smallest possible
//! space and time complexity (both O(n^2)). Note the space complexity could be O(n) if we only
//! cared about the minimal distance between the strings, but we want to return the full [WordDiff].
//! We use only O(n) space during the calculation, but each [WordDiff] is also O(n), so we end up with O(n^2).
//!
//! To see why we can only use O(n) space,
//! let `n = a.len()` and `m = b.len()` and consider the `n x m `matrix `X` where `X[i][j]` is the comparison
//! between `a[:i]` and `b[:j]`.
//! The notation `a[:i]` means all characters in `a` between 0 and `i` inclusive.
//! The solution is `X[n][m]`. The recursive relation is:
//!
//! ```text
//! X[i][j] = {
//!     X[i-1][j-1] if a[i] == b[j]  // append the (keep the character a[i]) op
//!                                  //  to the best WordDiff for (a[:i-1], b[:j-1])
//!     1 + min (
//!         X[i-1][j],    // append the (subtract the character a[i]) op
//!                       //  to the best WordDiff for (a[:i-1], b[j])
//!         X[i][j-1],    // append the (add the character b[j]) op
//!                       //  to the best WordDiff for (a[:i], b[j-1])
//!         X[i-1][j-1],  // append the (modify the character a[i] to b[j]) op
//!                       //  to the best WordDiff for (a[:i-1], b[:j-1])
//!     ) otherwise
//! }
//! ```
//!
//! We calculate the matrix by iterating over the `j` variable first and then the `i` variable:
//! ```
//! # let a = Vec::<i64>::new();
//! # let b = Vec::<i64>::new();
//! for j in 0..(b.len() + 1) {
//!     for i in 0..(a.len() + 1) {
//!         // calculate X[i][j] using X[i-1][j], X[i][j-1] and X[i-1][j-1]
//!     }
//! }
//! ```
//!
//! With this calculation order, we observe that `X[i][j]` only depends on the last `m+2` elements of `X` that have
//! been calculated. Specifically,
//!
//! - `X[i-1][j]` was calculated in the previous iteration of the `i` loop, so `1` iteration before.
//! - `X[i][j-1]` was calculated in the previous iteration of the `j` loop with the same `i` index, so `m+1` iterations before
//!        because the `i` variable takes `m+1` values.
//! - `X[i-1][j-1]` was calculated `m+2` iterations before.
//!
//! So, we don't need to store the full `X` matrix at all: we just need to store the last `m+2` elements that were calculated.
//! We use a circular buffer of size `m+2` to do this.

use crate::collections::circularbuffer::CircularBuffer;
use std::ops::Index;

/// Find words in the provided dictionary that are close to the search word.
///
/// The return value is an ordered list corresponding to every word in the dictionary, with
/// the closest matches first.
pub fn find_close_words(dictionary: &[&str], word: &str) -> Vec<WordDiff> {
    // TODO: accept a generic iterator
    let size_hint = dictionary.len();
    //size_hint() {
    //   (s, None) => s,
    //   (_, Some(s)) => s,
    //};
    let mut comparisons = Vec::with_capacity(size_hint);
    for valid_word in dictionary {
        let comparison = levenshtein_distance(word, valid_word); // word);
        comparisons.push(comparison);
    }
    comparisons.sort_by(|a, b| a.distance.cmp(&b.distance));
    comparisons
}

fn levenshtein_distance(a: &str, b: &str) -> WordDiff {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();

    let m = b.len();
    let mut c = CircularBuffer::new(m + 2);
    let idx_modify = m + 1;
    let idx_subtract = m;
    let idx_add = 0;

    // This is comparing two empty strings - i.e., a[:0] and b[:0]
    c.push(WordDiff {
        distance: 0,
        ops: Vec::with_capacity(std::cmp::max(a.len(), b.len())),
    });

    for b_j in &b {
        // Here we are comparing an empty a string (i.e., a[:0]) with b[:j+1].
        // There is only one possible action: append (add b[j]) to the diff for b[:j]
        let cmp = c.clone_to_front(idx_add);
        cmp.ops.push(DiffOp::Add(*b_j));
        cmp.distance += 1;
    }

    for a_i in &a {
        // Here we are comparing a[:i+1] with an empty b string (i.e., b[:0])
        // There is only one possible action: append (subtract a[i]) to the diff for a[:i]
        //let i_subtract = (idx_to_set + 1) % c.len();
        let cmp = c.clone_to_front(idx_subtract);
        cmp.ops.push(DiffOp::Subtract(*a_i));
        cmp.distance += 1;

        for b_j in &b {
            // Here we are comparing a[:i+1] with a b[:j+1]
            let (idx_to_clone, diff, distance_delta) = if a_i == b_j {
                (idx_modify, DiffOp::Keep(*a_i), 0)
            } else {
                let cost_modify = c.index(idx_modify).distance;
                let cost_add = c.index(idx_add).distance;
                let cost_subtract = c.index(idx_subtract).distance;
                if cost_modify <= std::cmp::min(cost_subtract, cost_add) {
                    (idx_modify, DiffOp::Modify(*a_i, *b_j), 1)
                } else if cost_subtract <= std::cmp::min(cost_modify, cost_add) {
                    (idx_subtract, DiffOp::Subtract(*a_i), 1)
                } else {
                    (idx_add, DiffOp::Add(*b_j), 1)
                }
            };

            let cmp = c.clone_to_front(idx_to_clone);
            cmp.ops.push(diff);
            cmp.distance += distance_delta;
        }
    }
    c.index(0).clone()
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum DiffOp {
    Keep(char),
    Add(char),
    Subtract(char),
    Modify(char, char),
}

impl DiffOp {
    #[cfg(test)]
    fn invert(&self) -> DiffOp {
        match self {
            DiffOp::Keep(c) => DiffOp::Keep(*c),
            DiffOp::Add(c) => DiffOp::Subtract(*c),
            DiffOp::Subtract(c) => DiffOp::Add(*c),
            DiffOp::Modify(a, b) => DiffOp::Modify(*b, *a),
        }
    }

    #[cfg(test)]
    fn distance(&self) -> usize {
        match self {
            DiffOp::Keep(_) => 0,
            DiffOp::Add(_) => 1,
            DiffOp::Subtract(_) => 1,
            DiffOp::Modify(_, _) => 1,
        }
    }

    fn left(&self) -> Option<char> {
        match self {
            DiffOp::Keep(c) => Some(*c),
            DiffOp::Add(_) => None,
            DiffOp::Subtract(c) => Some(*c),
            DiffOp::Modify(c, _) => Some(*c),
        }
    }

    fn right(&self) -> Option<char> {
        match self {
            DiffOp::Keep(c) => Some(*c),
            DiffOp::Add(c) => Some(*c),
            DiffOp::Subtract(_) => None,
            DiffOp::Modify(_, c) => Some(*c),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct WordDiff {
    distance: usize,
    ops: Vec<DiffOp>,
}

impl WordDiff {
    pub fn left(&self) -> String {
        self.ops.iter().filter_map(DiffOp::left).collect()
    }

    pub fn right(&self) -> String {
        self.ops.iter().filter_map(DiffOp::right).collect()
    }
}

impl Clone for WordDiff {
    fn clone(&self) -> Self {
        let mut cmp = WordDiff {
            distance: self.distance,
            ops: Vec::with_capacity(self.ops.capacity()),
        };
        cmp.ops.clone_from(&self.ops);
        cmp
    }

    fn clone_from(&mut self, source: &Self) {
        self.distance = source.distance;
        self.ops.clear();
        for diff in source.ops.iter() {
            self.ops.push(*diff);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! levenshtein_tests {
        ($( ($name: ident, $a: expr, $b: expr, $i: expr),)+) => {
            $(
            #[test]
            fn $name() {
                let mut cmp = WordDiff {
                    distance: 0,
                    ops: $i,
                };
                for diff in cmp.ops.iter() {
                    cmp.distance += diff.distance();
                }
                assert_eq![levenshtein_distance($a, $b), cmp];

                let mut inverse_diffs = Vec::new();
                for diff in cmp.ops.iter() {
                    inverse_diffs.push(diff.invert());
                }
                let cmp = WordDiff {
                    distance: cmp.distance,
                    ops: inverse_diffs,
                };
                assert_eq![levenshtein_distance($b, $a), cmp];
            }
            )+
        };
    }

    levenshtein_tests![
        (case_1, "", "", Vec::<DiffOp>::new()),
        (case_2, "a", "", vec![DiffOp::Subtract('a')]),
        (case_3, "a", "a", vec![DiffOp::Keep('a')]),
        (case_4, "a", "b", vec![DiffOp::Modify('a', 'b')]),
        (
            case_5,
            "aa",
            "a",
            vec![DiffOp::Subtract('a'), DiffOp::Keep('a')]
        ),
        (
            case_6,
            "aa",
            "ab",
            vec![DiffOp::Keep('a'), DiffOp::Modify('a', 'b')]
        ),
        (
            case_7,
            "abb",
            "acbb",
            vec![
                DiffOp::Keep('a'),
                DiffOp::Add('c'),
                DiffOp::Keep('b'),
                DiffOp::Keep('b'),
            ]
        ),
        (
            case_8,
            "aabb",
            "abb",
            vec![
                DiffOp::Subtract('a'),
                DiffOp::Keep('a'),
                DiffOp::Keep('b'),
                DiffOp::Keep('b'),
            ]
        ),
        (
            case_9,
            "james",
            "laura",
            vec![
                DiffOp::Modify('j', 'l'),
                DiffOp::Keep('a'),
                DiffOp::Modify('m', 'u'),
                DiffOp::Modify('e', 'r'),
                DiffOp::Modify('s', 'a'),
            ]
        ),
        (
            case_10,
            "ab12345e",
            "a12345de",
            vec![
                DiffOp::Keep('a'),
                DiffOp::Subtract('b'),
                DiffOp::Keep('1'),
                DiffOp::Keep('2'),
                DiffOp::Keep('3'),
                DiffOp::Keep('4'),
                DiffOp::Keep('5'),
                DiffOp::Add('d'),
                DiffOp::Keep('e'),
            ]
        ),
    ];

    #[test]
    fn find_close_words_test() {
        let dictionary = vec!["james", "laura", "mint"];
        let word = "janes";
        let result = find_close_words(&dictionary, &word);

        assert_eq![result[0].right(), "james"];
        assert_eq![result[1].right(), "laura"];
        assert_eq![result[2].right(), "mint"];
        assert_eq![result.len(), 3];
    }
}
