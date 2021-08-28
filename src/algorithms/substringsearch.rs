//! Knuth–Morris–Pratt substring search algorithm
//!
//! This module contains an optimal in time algorithm for finding a substring
//! in a string. By 'string' and 'substring' we mean a vector of elements of the same type.
//! The algorithm is due to
//! [Knuth, Morris and Pratt](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm).
//!
//! The API for this module is based on two assumptions:
//!
//! - There may be multiple searches for the same substring in different strings.
//!
//! - The elements of the string may be generated on the demand as the search progresses.
//!    That is, the full string is not necessarily known at the start.
//!
//! ## Example
//!
//! To use the algorithm, a matcher factory is first created.
//! This factory takes ownership of the substring.
//! On initialization the factory computes a number of internal
//! quantities which make the subsequent matching fast.
//! These quantites depend on the substring, so mutating the substring
//! after it has been passed to the factory is statically prevented.
//!
//! To match a string, a new matcher instance is created. Elements
//! of the string are passed in one at a time to the `next` method
//! of the matcher.
//! If the substring has length `m` and matches the last `m` elements that
//! have been passed in, the `next` method returns `true`.
//! Otherwise it returns `false`.
//! The matcher may be used to find multiple instances of the substring
//! in the same string.
//!
//! ```
//! # use texcraft::algorithms::substringsearch::{KMPMatcherFactory};
//! use texcraft::datastructures::nevec::Nevec;
//! use texcraft::nevec;
//!
//! let substring = nevec![2, 3, 2];
//! let factory = KMPMatcherFactory::new(substring);
//! let mut matcher = factory.matcher();
//! assert_eq![matcher.next(&1), false];
//! assert_eq![matcher.next(&2), false];
//! assert_eq![matcher.next(&3), false];
//! assert_eq![matcher.next(&2), true];
//! assert_eq![matcher.next(&3), false];
//! assert_eq![matcher.next(&2), true];
//! ```
//!
use crate::datastructures::nevec::Nevec;

/// Factory for creating KMP matchers for a specific substring.
pub struct KMPMatcherFactory<T: PartialEq> {
    substring: Nevec<T>,
    prefix_fn: Nevec<usize>,
}

impl<T: PartialEq> KMPMatcherFactory<T> {
    /// Create a new KMP matcher factory.
    pub fn new(substring: Nevec<T>) -> KMPMatcherFactory<T> {
        let mut prefix_fn = Nevec::with_capacity(0, substring.len());
        let mut k = 0;
        for i in 1..substring.len() {
            while k > 0 && substring[k] != substring[i] {
                k = prefix_fn[k - 1];
            }
            if substring[k] == substring[i] {
                k += 1;
            }
            prefix_fn.push(k);
        }

        KMPMatcherFactory {
            substring,
            prefix_fn,
        }
    }

    /// Create a new KMP matcher from the factory.
    pub fn matcher(&self) -> KMPMatcher<T> {
        KMPMatcher {
            factory: self,
            q: 0,
        }
    }

    /// Get an immutable reference to the underlying substring.
    //
    // Obtaining a mutable reference is not supported as internal details of
    // the matcher factory rely on the substring remaining constant.
    pub fn substring(&self) -> &Nevec<T> {
        &self.substring
    }

    /// Retake ownership of the underlying substring.
    pub fn take_substring(self) -> Nevec<T> {
        self.substring
    }
}

/// Data structure for matching a substring in a string.
pub struct KMPMatcher<'a, T: PartialEq> {
    factory: &'a KMPMatcherFactory<T>,
    q: usize,
}

impl<'a, T: PartialEq> KMPMatcher<'a, T> {
    /// Provide the next element of the string to the matcher.
    /// This returns true if the last `m` elements of the string match the substring, where
    /// `m` is the length of the substring.
    pub fn next(&mut self, tail: &T) -> bool {
        while self.q > 0 && &self.factory.substring[self.q] != tail {
            self.q = self.factory.prefix_fn[self.q - 1];
        }
        if &self.factory.substring[self.q] == tail {
            self.q += 1;
        }
        if self.q == self.factory.substring.len() {
            self.q = self.factory.prefix_fn[self.q - 1];
            return true;
        }
        false
    }
}
