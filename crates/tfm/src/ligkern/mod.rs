//! Ligature and kern data
//!
//! The TFM file format can provide information about ligatures and kerns.
//! A [ligature](https://en.wikipedia.org/wiki/Ligature_(writing))
//!     is a special character that can replace two or more adjacent characters.
//! For example, the pair of characters ae can be replaced by the æ ligature which is a single character.
//! A [kern](https://en.wikipedia.org/wiki/Kerning) is special space inserted between
//!     two adjacent characters to align them better.
//! For example, a kern can be inserted between A and V to compensate for the large
//!     amount of space created by the specific combination of these two characters.
//!
//! ## The lig/kern programming language
//!
//! TFM provides ligature and kern data in the form of
//!     "instructions in a simple programming language that explains what to do for special letter pairs"
//!     (quoting TFtoPL.2014.13).
//! This lig/kern programming language can be used to specify instructions like
//!     "replace the pair (a,e) by æ" and
//!     "insert a kern of width -0.1pt between the pair (A,V)".
//! But it can also specify more complex behaviors.
//! For example, a lig/kern program can specify "replace the pair (x,y) by the pair (z,y)".
//!
//! In general for any pair of characters (x,y) the program specifies zero or one lig/kern instructions.
//! After this instruction is executed, there may be a new
//!     pair of characters remaining, as in the (x,y) to (z,y) instruction.
//! The lig/kern instruction for this pair is then executed, if it exists.
//! This process continues until there are no more instructions left to run.
//!
//! Lig/kern instructions are represented in this module by the [`lang::Instruction`] type.
//!
//! ## Related code by Knuth
//!
//! The TFtoPL and PLtoTF programs don't contain any code for running lig/kern programs.
//! They only contain logic for translating between the `.tfm` and `.pl`
//!     formats for lig/kern programs, and for doing some validation as described below.
//! Lig/kern programs are actually executed in TeX; see KnuthTeX.2021.1032-1040.
//!
//! One of the challenges with lig/kern programs is that they can contain infinite loops.
//! Here is a simple example of a lig/kern program with two instruction and an infinite loop:
//!
//! - Replace (x,y) with (z,y) (in property list format, `(LABEL C x)(LIG/ C y C z)`)
//! - Replace (z,y) with (x,y) (in property list format, `(LABEL C z)(LIG/ C y C x)`)
//!
//! When this program runs (x,y) will be swapped with (z,y) ad infinitum.
//! See TFtoPL.2014.88 for more examples.
//!
//! Both TFtoPL and PLtoTF contain code that checks that a lig/kern program
//!     does not contain infinite loops (TFtoPL.2014.88-95 and PLtoTF.2014.116-125).
//! The algorithm for detecting infinite loops is a topological sorting algorithm
//!     over a graph where each node is a pair of characters.
//! However it's a bit complicated because the full graph cannot be constructed without
//!     running the lig/kern program.
//!
//! TeX does not check for infinite loops, presumably under the assumption that any `.tfm` file will have
//!     been generated by PLtoTF and thus already validated.
//! However TeX does check for interrupts when executing lig/kern programs so that
//!     at least a user can terminate TeX if an infinite loop is hit.
//! (See the `check_interrupt` line in KnuthTeX.2021.1040.)
//!
//! ## Functionality in this module
//!
//! This module handles lig/kern programs in a different way,
//!     inspired by the ["parse don't validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
//!     philosophy.
//! This module is able to represent raw lig/kern programs as a vector of [`lang::Instruction`] values.
//! But can also _compile_ lig/kern programs (into a [`CompiledProgram`]).
//! This compilation process essentially executes the lig/kern program for every possible character pair.
//! The result is a map from each character pair to the full list of
//!     replacement characters and kerns for that pair.
//! If there is an infinite loop in the program this compilation will naturally fail.
//! The compiled program is thus a "parsed" version of the lig/kern program
//!     and it is impossible for infinite loops to appear in it.
//!
//! An advantage of this model is that the lig/kern program does not need to be repeatedly
//!     executed in the main hot loop of TeX.
//! This may make TeX faster.
//! However the compiled lig/kern program does have a larger memory footprint than the raw program,
//!     and so it may be slower if TeX is memory bound.

mod compiler;
use crate::Char;
use crate::Number;
use std::collections::HashMap;
pub mod lang;

/// A compiled lig/kern program.
#[derive(Debug)]
pub struct CompiledProgram {
    left_to_pairs: HashMap<Char, (u16, u16)>,
    pairs: Vec<(Char, RawReplacement)>,
    middle_chars: Vec<(Char, Number)>,
}

#[derive(Debug, Clone)]
struct RawReplacement {
    left_char_operation: LeftCharOperation,
    middle_char_bounds: std::ops::Range<u16>,
    last_char: Char,
}

impl CompiledProgram {
    /// Compile a lig/kern program.
    pub fn compile(
        instructions: &[lang::Instruction],
        kerns: &[Number],
        entrypoints: HashMap<Char, u16>,
    ) -> Result<(CompiledProgram, Vec<CompilationWarning>), InfiniteLoopError> {
        compiler::compile(instructions, kerns, &entrypoints)
    }

    /// Get an iterator over the full lig/kern replacement for a pair of characters.
    pub fn get_replacement_iter(&self, left_char: Char, right_char: Char) -> ReplacementIter {
        self.get_replacement(left_char, right_char)
            .into_iter(left_char)
    }

    /// Get the full lig/kern replacement for a pair of characters.
    pub fn get_replacement(&self, left_char: Char, right_char: Char) -> Replacement {
        if let Some((lower, upper)) = self.left_to_pairs.get(&left_char) {
            for (candidate_right_char, replacement) in
                &self.pairs[(*lower as usize)..(*upper as usize)]
            {
                if *candidate_right_char != right_char {
                    continue;
                }
                return if replacement.middle_char_bounds.end == 0 {
                    Replacement {
                        left_char_operation: replacement.left_char_operation,
                        middle_chars: &[],
                        last_char: replacement.last_char,
                    }
                } else {
                    Replacement {
                        left_char_operation: replacement.left_char_operation,
                        middle_chars: &self.middle_chars[replacement.middle_char_bounds.start
                            as usize
                            ..replacement.middle_char_bounds.end as usize],
                        last_char: replacement.last_char,
                    }
                };
            }
        }
        Replacement::no_op(right_char)
    }

    /// Returns an iterator over all pairs `(char,char)` that have a replacement
    ///     specified in the lig/kern program.
    pub fn all_pairs_having_replacement(&self) -> impl '_ + Iterator<Item = (Char, Char)> {
        PairsIter {
            current_left: Char(0),
            left_iter: self.left_to_pairs.iter(),
            right_chars: vec![],
            program: self,
        }
    }
}

/// An error returned from lig/kern compilation.
#[derive(Debug, PartialEq, Eq)]
pub struct InfiniteLoopError {
    /// The pair of characters the starts the infinite loop.
    pub starting_pair: (Char, Char),
    /// A sequence of steps forming the infinite loop.
    ///
    /// At the end of these steps, the next pair to replace will be the `starting_pair` again.
    pub infinite_loop: Vec<InfiniteLoopStep>,
}

impl InfiniteLoopError {
    pub fn pltotf_message(&self) -> String {
        // PLtoTF.2014.125
        format!(
            "Infinite ligature loop starting with '{:o} and '{:o}!",
            self.starting_pair.0 .0, self.starting_pair.1 .0
        )
    }
}

/// One step in a lig/kern infinite loop.
///
/// A vector of these steps is returned in a [`InfiniteLoopError`].
#[derive(Debug, PartialEq, Eq)]
pub struct InfiniteLoopStep {
    /// The index of the instruction to apply in this step.
    pub instruction_index: usize,
    /// The replacement text after applying this step.
    pub post_replacement: Vec<Char>,
    /// The position of the cursor after applying this step.
    pub post_cursor_position: usize,
}

/// A warning returned from lig/kern compilation.
#[derive(Debug)]
pub enum CompilationWarning {
    InvalidNextInstruction,
    DuplicateRule,
    OrphanRule,
}

/// Data structure describing the replacement of a character pair in a lig/kern program.
pub struct Replacement<'a> {
    /// Operation to perform on the left character.
    pub left_char_operation: LeftCharOperation,
    /// Slice of characters and kerns to insert after the left character.
    pub middle_chars: &'a [(Char, Number)],
    /// Last character to insert.
    pub last_char: Char,
}

impl<'a> Replacement<'a> {
    fn no_op(right_char: Char) -> Replacement<'a> {
        Replacement {
            left_char_operation: LeftCharOperation::Retain,
            middle_chars: &[],
            last_char: right_char,
        }
    }
}

impl<'a> Replacement<'a> {
    pub fn into_iter(self, left_char: Char) -> ReplacementIter<'a> {
        ReplacementIter {
            left_char,
            full_operation: self,
            state: IterState::LeftChar,
        }
    }
}

/// Operation to perform on the left character of a lig/kern pair.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum LeftCharOperation {
    /// Retain the left character and do not add a kern.
    Retain,
    /// Delete the left character.
    Delete,
    /// Retain the left character and append the specified kern.
    AppendKern(Number),
}

/// Iterator over the replacement of a character pair in a lig/kern program.
pub struct ReplacementIter<'a> {
    left_char: Char,
    full_operation: Replacement<'a>,
    state: IterState,
}

enum IterState {
    LeftChar,
    MiddleChar(usize),
    LastChar,
    Exhausted,
}

impl<'a> ReplacementIter<'a> {
    fn i(&self) -> (IterState, Option<(Char, Number)>) {
        match self.state {
            IterState::LeftChar => (
                IterState::MiddleChar(0),
                match self.full_operation.left_char_operation {
                    LeftCharOperation::Retain => Some((self.left_char, Number::ZERO)),
                    LeftCharOperation::Delete => None,
                    LeftCharOperation::AppendKern(kern) => Some((self.left_char, kern)),
                },
            ),
            IterState::MiddleChar(i) => match self.full_operation.middle_chars.get(i).copied() {
                None => (IterState::LastChar, None),
                Some(t) => (IterState::MiddleChar(i + 1), Some(t)),
            },
            IterState::LastChar => (
                IterState::Exhausted,
                Some((self.full_operation.last_char, Number::ZERO)),
            ),
            IterState::Exhausted => (IterState::Exhausted, None),
        }
    }
}

impl<'a> Iterator for ReplacementIter<'a> {
    type Item = (Char, Number);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (state, r) = self.i();
            self.state = state;
            match (&self.state, r) {
                (_, Some(t)) => return Some(t),
                (IterState::Exhausted, _) => return None,
                (_, _) => {}
            }
        }
    }
}

/// An iterator over all pairs of characters that have a lig/kern replacement in a program.
struct PairsIter<'a, L> {
    current_left: Char,
    left_iter: L,
    right_chars: Vec<Char>,
    program: &'a CompiledProgram,
}

impl<'a, L: 'a + Iterator<Item = (&'a Char, &'a (u16, u16))>> Iterator for PairsIter<'a, L> {
    type Item = (Char, Char);
    fn next(&mut self) -> Option<Self::Item> {
        match self.right_chars.pop() {
            Some(right_char) => Some((self.current_left, right_char)),
            None => match self.left_iter.next() {
                None => None,
                Some((&new_left, (lower, upper))) => {
                    self.current_left = new_left;
                    self.right_chars = self.program.pairs[*lower as usize..*upper as usize]
                        .iter()
                        .map(|t| t.0)
                        .collect();
                    Some((new_left, self.right_chars.pop().unwrap()))
                }
            },
        }
    }
}
