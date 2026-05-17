//! Types corresponding to the "lig/kern programming language".
//!
//! See the documentation on the [`super`] module for information about this programming language.
//!
//! The types here are put in a separate module because users of this crate are generally not expected to use them.
//! Instead, users will work with compiled lig/kern programs.

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::Char;
use crate::FixWord;

/// A lig/kern program.
///
/// In theory the program also includes entrypoints.
/// However because these are provided in different ways in .tfm and .pl files,
/// it's easier to exclude them on this type and pass them in when needed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    pub instructions: Vec<Instruction>,
    pub left_boundary_char_entrypoint: Option<u16>,
    pub right_boundary_char: Option<Char>,
    pub passthrough: HashSet<u16>,
}

/// A single instruction in a lig/kern program.
///
/// In TFM files, instructions are serialized to 32 bit integers.
///
/// In property list files, instructions are specified using a `(LIG _ _)` or `(KERN _ _)` element,
///     and optionally a `(STOP)` or `(SKIP _)` element directly after.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    /// Specifies the next instruction to run if this instruction is not applicable -
    ///     e.g., if the right character of the pair is not `right_char`.
    /// If the payload is present, that number of lig/kern instructions in the list of all instructions are skipped to
    ///     find the next instruction.
    /// Otherwise this is the final instruction and there are no more instructions to consider.
    pub next_instruction: Option<u8>,
    /// This instruction is run if the right character in the pair is this character.
    /// Otherwise the next lig kern instruction for the current character is considered,
    ///     using the `next_instruction` field.
    ///
    /// After this operation is performed,
    ///     no more operations need to be performed on this pair.
    /// However the result of the operation may result in a new pair being created
    ///     and the lig/kern program will run for that pair.
    /// See the documentation on [`PostLigOperation`] for information on that.
    pub right_char: Char,
    /// The operation to perform for this instruction.
    pub operation: Operation,
}

/// A lig/kern operation to perform.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Operation {
    /// Insert a kern between the current character and the next character.
    ///
    /// The variant payload is the size of the kern.
    Kern(FixWord),
    /// Insert a kern between the current character and the next character.
    ///
    /// The variant payload is the index of the kern in the kerns array.
    KernAtIndex(u16),
    /// Perform a ligature step.
    /// This inserts `char_to_insert` between the left and right characters,
    ///     and then performs the post-lig operation.
    Ligature {
        /// Character to insert.
        char_to_insert: Char,
        /// What to do after inserting the character.
        post_lig_operation: PostLigOperation,
        /// If the tag in the .tfm file was invalid, this will be true.
        /// In this case the post_lig_operation will be [`PostLigOperation::RetainNeitherMoveToInserted`].
        ///
        /// This field is used in the .tfm validation function to generate a warning for this case.
        /// We could also generate a warning in the deserialization code itself but then the
        /// ordering of the warning would be incorrect with respect to other validation warnings.
        post_lig_tag_invalid: bool,
    },
    /// If the entrypoint for a character is this operation, go to the instruction indexed by the payload.
    ///
    /// This redirect mechanism exists because in .tfm files entrypoints are [`u8`]s but lig/kern
    ///     programs can contain more than 256 instructions.
    ///
    /// If this operation is encountered in another situation, it is an unconditional stop.
    ///
    /// The boolean value in the payload is true if the boundary character should be
    ///     serialized inside the instruction.
    EntrypointRedirect(u16, bool),
}

/// Errors returned by the [`Operation::parse_compact`] method.
#[derive(Debug, PartialEq)]
pub enum ParseCompactOperationError {
    NotThreeWords,
    MiddleWordIsNotAnArrow,
    FirstWordIsWrongSize,
    LeftCharInvalid,
    RightCharInvalid,
    ReplacementCharInvalid,
    InvalidDigit,
    InvalidKern,
    InvalidLigature,
    MissingCursor,
    CursorNotAfterChar,
    MultipleCursors,
    MissingReplacementChar,
    MissingLeftChar,
    MissingRightChar,
}

impl Operation {
    /// Parses a compact representation of an operation.
    ///
    /// The representation is of the form `<left><right> -> <operation>` 
    /// where `<left>` and `<right>` are the pair of characters this operation applies to,
    /// and `<operation>` describes the [`Operation`].
    ///
    /// ## Kerns
    ///
    /// For kerns, the operation is `<left>[<n>]<right>` where `<n>` is a decimal integer
    /// (possibly negative) giving the kern size as a [`FixWord`]:
    ///
    /// ```
    /// use tfm::ligkern::lang::*;
    /// use tfm::FixWord;
    /// assert_eq![
    ///     Operation::parse_compact("ab -> a[13]b"),
    ///     Ok(('a', 'b', Operation::Kern(FixWord(13))))
    /// ];
    /// assert_eq![
    ///     Operation::parse_compact("ab -> a[-4]b"),
    ///     Ok(('a', 'b', Operation::Kern(FixWord(-4))))
    /// ];
    /// ```
    ///
    /// ## Ligatures
    ///
    /// For ligatures, the operation consists of four characters.
    /// The following three characters come in order:
    /// 1.  the left character if it is retained, or `_` if it is not.
    /// 1.  the replacement character.
    /// 1.  the right character if it is retained, or `_` if it is not.
    ///
    /// There is also a caret `^` indicating where the cursor should be after
    /// the replacement. The caret can come after any character that is not `_`.
    /// Thus:
    ///
    /// ```
    /// use tfm::ligkern::lang::*;
    /// assert_eq![
    ///     Operation::parse_compact("ab -> ax^b"),
    ///     Ok(('a', 'b', Operation::Ligature{
    ///         char_to_insert: 'x'.try_into().unwrap(),
    ///         post_lig_operation: PostLigOperation::RetainBothMoveToInserted,
    ///         post_lig_tag_invalid: false,
    ///     }))
    /// ];
    /// assert_eq![
    ///     Operation::parse_compact("ab -> _xb^"),
    ///     Ok(('a', 'b', Operation::Ligature{
    ///         char_to_insert: 'x'.try_into().unwrap(),
    ///         post_lig_operation: PostLigOperation::RetainRightMoveToRight,
    ///         post_lig_tag_invalid: false,
    ///     }))
    /// ];
    /// ```
    pub fn parse_compact(s: &str) -> Result<(char, char, Self), ParseCompactOperationError> {
        use ParseCompactOperationError::*;
        let words: Option<[&str; 3]> = (|| {
            let mut raw = s.split_ascii_whitespace();
            let words = [raw.next()?, raw.next()?, raw.next()?];
            if raw.next().is_some() {
                None
            } else {
                Some(words)
            }
        })();
        let Some([first, second, third]) = words else {
            return Err(NotThreeWords);
        };
        if second != "->" {
            return Err(MiddleWordIsNotAnArrow);
        }
        let mut c = first.chars();
        let (l, r) = match (c.next(), c.next(), c.next()) {
            (Some(l), Some(r), None) => (l, r),
            _ => {
                return Err(FirstWordIsWrongSize);
            }
        };
        if l == '_' || l == '^' {
            return Err(LeftCharInvalid);
        }
        if r == '_' || r == '^' {
            return Err(RightCharInvalid);
        }

        if third.contains('[') {
            let mut chars = third.chars();
            let lc = chars.next().ok_or(MissingLeftChar)?;
            if chars.next() != Some('[') {
                return Err(InvalidKern);
            }
            let n_start = lc.len_utf8() + '['.len_utf8();
            let mut n_end = n_start;
            loop {
                match chars.next() {
                    None => return Err(InvalidKern),
                    Some(']') => break,
                    Some(c) => {
                        n_end += c.len_utf8();
                    }
                }
            }
            let rc = chars.next().ok_or(MissingRightChar)?;
            if chars.next().is_some() {
                return Err(InvalidKern);
            }
            if lc != l {
                return Err(MissingLeftChar);
            }
            if rc != r {
                return Err(MissingRightChar);
            }
            let n: i32 = third[n_start..n_end].parse().map_err(|_| InvalidKern)?;
            return Ok((l, r, Operation::Kern(FixWord(n))));
        }

        let mut c = third.chars();
        let c = match (c.next(), c.next(), c.next(), c.next(), c.next()) {
            (Some(a), Some(b), Some(c), Some(d), None) => (a, b, c, d),
            _ => {
                return Err(InvalidLigature);
            }
        };
        enum CaretPos {
            Left,
            Middle,
            Right,
        }
        let (caret_pos, a, b, c) = match c {
            ('^', _, _, _) => {
                return Err(CursorNotAfterChar);
            }
            (a, '^', b, c) => (CaretPos::Left, a, b, c),
            (a, b, '^', c) => (CaretPos::Middle, a, b, c),
            (a, b, c, '^') => (CaretPos::Right, a, b, c),
            _ => {
                return Err(MissingCursor);
            }
        };
        let retain_l = if a == l {
            true
        } else if a == '_' {
            false
        } else {
            return Err(MissingLeftChar);
        };
        if b == '_' {
            return Err(ReplacementCharInvalid);
        }
        let Ok(b) = b.try_into() else {
            return Err(ReplacementCharInvalid);
        };
        let retain_r = if c == r {
            true
        } else if c == '_' {
            false
        } else {
            return Err(MissingRightChar);
        };
        let post_lig_operation = match (retain_l, retain_r, caret_pos) {
            (true, true, CaretPos::Left) => PostLigOperation::RetainBothMoveNowhere,
            (true, true, CaretPos::Middle) => PostLigOperation::RetainBothMoveToInserted,
            (true, true, CaretPos::Right) => PostLigOperation::RetainBothMoveToRight,
            (true, false, CaretPos::Left) => PostLigOperation::RetainLeftMoveNowhere,
            (true, false, CaretPos::Middle) => PostLigOperation::RetainLeftMoveToInserted,
            (false, true, CaretPos::Middle) => PostLigOperation::RetainRightMoveToInserted,
            (false, true, CaretPos::Right) => PostLigOperation::RetainRightMoveToRight,
            (false, false, CaretPos::Middle) => PostLigOperation::RetainNeitherMoveToInserted,
            (true, false, CaretPos::Right)
            | (false, true, CaretPos::Left)
            | (false, false, CaretPos::Left)
            | (false, false, CaretPos::Right) => return Err(CursorNotAfterChar),
        };
        Ok((
            l,
            r,
            Operation::Ligature {
                char_to_insert: b,
                post_lig_operation,
                post_lig_tag_invalid: false,
            },
        ))
    }

    /// Display the compact representation of a operation. The format is described in [`Operation::parse_compact`].
    pub fn display_compact<'a>(&'a self, l: char, r: char) -> impl std::fmt::Display + 'a {
        struct D<'a> {
            s: &'a Operation,
            l: char,
            r: char,
        }
        impl<'a> std::fmt::Display for D<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{} -> ", self.l, self.r)?;
                match self.s {
                    Operation::Kern(kern) => {
                        write!(f, "{}[{}]{}", self.l, kern.0, self.r)
                    }
                    Operation::KernAtIndex(_) | Operation::EntrypointRedirect(_, _) => {
                        write!(f, "<unknown>")
                    }
                    Operation::Ligature {
                        char_to_insert,
                        post_lig_operation,
                        post_lig_tag_invalid: _,
                    } => {
                        use PostLigOperation::*;
                        match post_lig_operation {
                            RetainBothMoveNowhere => {
                                write!(f, "{}^{}{}", self.l, char_to_insert, self.r)
                            }
                            RetainBothMoveToInserted => {
                                write!(f, "{}{}^{}", self.l, char_to_insert, self.r)
                            }
                            RetainBothMoveToRight => {
                                write!(f, "{}{}{}^", self.l, char_to_insert, self.r)
                            }
                            RetainRightMoveToInserted => {
                                write!(f, "_{}^{}", char_to_insert, self.r)
                            }
                            RetainRightMoveToRight => {
                                write!(f, "_{}{}^", char_to_insert, self.r)
                            }
                            RetainLeftMoveNowhere => {
                                write!(f, "{}^{}_", self.l, char_to_insert)
                            }
                            RetainLeftMoveToInserted => {
                                write!(f, "{}{}^_", self.l, char_to_insert)
                            }
                            RetainNeitherMoveToInserted => {
                                write!(f, "_{}^_", char_to_insert)
                            }
                        }
                    }
                }
            }
        }
        D { s: self, l, r }
    }

    pub(crate) fn lig_kern_operation_from_bytes(op_byte: u8, remainder: u8) -> Self {
        match op_byte.checked_sub(128) {
            Some(r) => Self::KernAtIndex(u16::from_be_bytes([r, remainder])),
            None => {
                // TFtoPL.2014.77
                let delete_next_char = op_byte.is_multiple_of(2);
                let op_byte = op_byte / 2;
                let delete_current_char = op_byte.is_multiple_of(2);
                let skip = op_byte / 2;
                use PostLigOperation::*;
                let (post_lig_operation, post_lig_tag_invalid) =
                    match (delete_current_char, delete_next_char, skip) {
                        (false, false, 0) => (RetainBothMoveNowhere, false),
                        (false, false, 1) => (RetainBothMoveToInserted, false),
                        (false, false, 2) => (RetainBothMoveToRight, false),
                        (false, true, 0) => (RetainLeftMoveNowhere, false),
                        (false, true, 1) => (RetainLeftMoveToInserted, false),
                        (true, false, 0) => (RetainRightMoveToInserted, false),
                        (true, false, 1) => (RetainRightMoveToRight, false),
                        (true, true, 0) => (RetainNeitherMoveToInserted, false),
                        _ => (RetainNeitherMoveToInserted, true),
                    };
                Self::Ligature {
                    char_to_insert: Char(remainder),
                    post_lig_operation,
                    post_lig_tag_invalid,
                }
            }
        }
    }
}

/// A post-lig operation to perform after performing a ligature operation ([`Operation::Ligature`]).
///
/// A lig operation starts with a pair of characters (x,y) and a "cursor" on x.
/// The operation then inserts another character to get, say, (x,z,y).
/// At this point the cursor is still on x.
/// The post-lig operation does two things:
///
/// 1. First, it potentially deletes x or y or both.
/// 1. Second, it potentially moves the cursor forward.
///
/// After this, if the cursor is not at the end of the list of characters,
///     the lig-kern program is run for the new pair starting at the cursor.
///
/// For example, the post-lig operation [`PostLigOperation::RetainLeftMoveNowhere`] retains
///     x and deletes y, leaving (x,z).
/// It then moves the cursor nowhere, leaving it on x.
/// As a result, the lig kern program for the pair (x,z) will run.
///
/// On the other hand, if the post-lig operation [`PostLigOperation::RetainLeftMoveToInserted`]
///     runs, y is still deleted but the cursor moves to z.
/// This is the last character in this list and there no more pairs of characters to consider.
/// The lig/kern program thus terminates.
///
/// In general all of the post-lig operations are of the form `RetainXMoveY` where `X`
///     specifies the characters to retain and `Y` specifies where the cursor should move.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum PostLigOperation {
    /// Corresponds to the `/LIG/` property list element.
    RetainBothMoveNowhere,
    /// Corresponds to the `/LIG/>` property list element.
    RetainBothMoveToInserted,
    /// Corresponds to the `/LIG/>>` property list element.
    RetainBothMoveToRight,
    /// Corresponds to the `LIG/` property list element.
    RetainRightMoveToInserted,
    /// Corresponds to the `LIG/>` property list element.
    RetainRightMoveToRight,
    /// Corresponds to the `/LIG` property list element.
    RetainLeftMoveNowhere,
    /// Corresponds to the `/LIG>` property list element.
    RetainLeftMoveToInserted,
    /// Corresponds to the `LIG` property list element.
    RetainNeitherMoveToInserted,
}

#[derive(Clone, Debug)]
pub enum InvalidEntrypointError {
    Direct { entrypoint: u8 },
    Indirect { packed: u8, unpacked: u16 },
}

#[derive(PartialEq, Debug)]
pub enum ParseCompactProgramError {
    InvalidOperation(ParseCompactOperationError),
    InvalidLeftChar,
    InvalidRightChar,
    TooManyOperations,
}

impl From<ParseCompactOperationError> for ParseCompactProgramError {
    fn from(value: ParseCompactOperationError) -> Self {
        Self::InvalidOperation(value)
    }
}

impl Program {
    pub fn parse_compact(s: &str) -> Result<(Self, HashMap<Char, u16>), ParseCompactProgramError> {
        use ParseCompactProgramError::*;
        let mut operations: BTreeMap<Char, BTreeMap<Char, Operation>> = Default::default();
        for line in s.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            let (l, r, op) = Operation::parse_compact(line)?;
            let Ok(l) = l.try_into() else {
                return Err(InvalidLeftChar);
            };
            let m = operations.entry(l).or_default();
            let Ok(r) = r.try_into() else {
                return Err(InvalidRightChar);
            };
            m.insert(r, op);
        }
        let mut entrypoints: HashMap<Char, u16> = Default::default();
        let mut instructions: Vec<Instruction> = vec![];
        for (left_char, m) in operations {
            let i: u16 = instructions
                .len()
                .try_into()
                .map_err(|_| TooManyOperations)?;
            entrypoints.insert(left_char, i);
            let mut has_operations = false;
            for (right_char, operation) in m {
                instructions.push(Instruction {
                    next_instruction: Some(0),
                    right_char,
                    operation,
                });
                has_operations = true;
            }
            if has_operations {
                instructions
                    .last_mut()
                    .expect("we wrote at least one instruction")
                    .next_instruction = None;
            }
        }
        Ok((
            Self {
                instructions,
                left_boundary_char_entrypoint: None,
                right_boundary_char: None,
                passthrough: Default::default(),
            },
            entrypoints,
        ))
    }

    pub fn unpack_entrypoint(&mut self, entrypoint: u8) -> Result<u16, InvalidEntrypointError> {
        match self.instructions.get(entrypoint as usize) {
            None => Err(InvalidEntrypointError::Direct { entrypoint }),
            Some(instruction) => match instruction.operation {
                Operation::EntrypointRedirect(u_big, _) => {
                    if (u_big as usize) < self.instructions.len() {
                        self.passthrough.insert(entrypoint as u16);
                        Ok(u_big)
                    } else {
                        Err(InvalidEntrypointError::Indirect {
                            packed: entrypoint,
                            unpacked: u_big,
                        })
                    }
                }
                _ => Ok(entrypoint as u16),
            },
        }
    }

    pub fn pack_entrypoints(&mut self, entrypoints: HashMap<Char, u16>) -> HashMap<Char, u8> {
        let instructions = &mut self.instructions;
        let ordered_entrypoints = {
            let mut m: HashMap<u16, Vec<Char>> = Default::default();
            for (c, u) in entrypoints {
                m.entry(u).or_default().push(c);
            }
            let mut v: Vec<(u16, Vec<Char>)> = m.into_iter().collect();
            v.sort_by_key(|(u, _)| *u);
            v
        };
        let mut offset: u8 = if self.right_boundary_char.is_some() {
            // In .tfm files the boundary char is transmitted in each entrypoint redirect instruction.
            // If there is a boundary char, we need at least one entrypoint redirect to exist so
            // that the boundary char is there.
            instructions.push(Instruction {
                next_instruction: None,
                right_char: self.right_boundary_char.unwrap_or(Char(0)),
                operation: Operation::EntrypointRedirect(0, true),
            });
            1
        } else {
            0
        };
        let mut new_entrypoints: HashMap<Char, u8> = Default::default();
        let mut redirects: Vec<u16> = vec![];
        for (i, (u16_entrypoint, chars)) in ordered_entrypoints.into_iter().rev().enumerate() {
            let u: u8 = match (u16_entrypoint + offset as u16).try_into() {
                Ok(u) => u,
                Err(_) => {
                    redirects.push(u16_entrypoint);
                    if i == 0 && self.right_boundary_char.is_some() {
                        // This implements the "optimization" "location 0 can do double duty" in PLtoTF.2014.141
                        instructions.pop();
                        offset = 0;
                    }
                    let u = offset;
                    offset = offset.checked_add(1).expect(
                        "offset is incremented at most once per 8-bit-char and so cannot exceed 256",
                    );
                    u
                }
            };
            for c in chars {
                new_entrypoints.insert(c, u);
            }
        }
        for redirect in redirects {
            instructions.push(Instruction {
            next_instruction: None,
            right_char: self.right_boundary_char.unwrap_or(Char(0)),
            operation: Operation::EntrypointRedirect(
                redirect.checked_add(offset as u16).expect("the inputted lig/kern instructions vector doesn't have enough space for new instructions"),
            true,
        ),
        });
        }
        instructions.rotate_right(offset as usize);
        if let Some(boundary_char_entrypoint) = self.left_boundary_char_entrypoint {
            instructions.push(Instruction {
                next_instruction: None,
                right_char: Char(0),
                operation: Operation::EntrypointRedirect(
                    boundary_char_entrypoint + offset as u16,
                    false,
                ),
            })
        }
        new_entrypoints
    }

    pub fn unpack_kerns(&mut self) -> Vec<FixWord> {
        let mut kerns = vec![];
        let mut kerns_dedup = HashMap::<FixWord, usize>::new();
        for instruction in &mut self.instructions {
            if let Operation::Kern(kern) = instruction.operation {
                use std::collections::hash_map::Entry;
                let index = match kerns_dedup.entry(kern) {
                    Entry::Occupied(o) => *o.get(),
                    Entry::Vacant(v) => {
                        let l = kerns.len();
                        v.insert(l);
                        kerns.push(kern);
                        l
                    }
                };
                instruction.operation = Operation::KernAtIndex(index.try_into().unwrap());
            }
        }
        kerns
    }

    pub fn pack_kerns(&mut self, kerns: &[FixWord]) {
        for i in &mut self.instructions {
            if let Operation::KernAtIndex(index) = &i.operation {
                // TODO: log a warning if the index is not in the kerns array as
                // in TFtoPL.2014.76
                i.operation =
                    Operation::Kern(kerns.get(*index as usize).copied().unwrap_or_default())
            }
        }
    }

    pub fn reachable_iter<I: Iterator<Item = (Char, u16)>>(
        &self,
        entrypoints: I,
    ) -> ReachableIter<'_> {
        ReachableIter {
            next: 0,
            reachable: self.reachable_array(entrypoints),
            program: self,
        }
    }

    /// Iterate over the lig/kern instructions for a specific entrypoint.
    pub fn instructions_for_entrypoint(
        &self,
        entrypoint: u16,
    ) -> InstructionsForEntrypointIter<'_> {
        InstructionsForEntrypointIter {
            next: entrypoint as usize,
            instructions: &self.instructions,
        }
    }

    pub fn is_seven_bit_safe(&self, entrypoints: HashMap<Char, u16>) -> bool {
        entrypoints
            .into_iter()
            .filter(|(c, _)| c.is_seven_bit())
            .flat_map(|(_, e)| self.instructions_for_entrypoint(e))
            .filter(|(_, instruction)| instruction.right_char.is_seven_bit())
            .filter_map(|(_, instruction)| match instruction.operation {
                Operation::Ligature { char_to_insert, .. } => Some(char_to_insert),
                _ => None,
            })
            .all(|c| c.is_seven_bit())
    }

    pub fn validate_and_fix<I, T>(
        &mut self,
        smallest_char: Char,
        entrypoints: I,
        char_exists: T,
        kerns: &[FixWord],
    ) -> Vec<ValidationWarning>
    where
        I: Iterator<Item = (Char, u8)>,
        T: Fn(Char) -> bool,
    {
        let mut warnings = vec![];
        // TFtoPL.2014.68
        if let Some(entrypoint) = self.left_boundary_char_entrypoint {
            if self.instructions.len() <= entrypoint as usize {
                self.left_boundary_char_entrypoint = None;
                warnings.push(ValidationWarning::InvalidBoundaryCharEntrypoint);
            }
        }
        // TFtoPL.2014.69
        let unpacked_entrypoints: Vec<(Char, u16)> = entrypoints
            .into_iter()
            .filter_map(|(c, e)| match self.unpack_entrypoint(e) {
                Ok(e) => Some((c, e)),
                Err(_) => {
                    warnings.push(ValidationWarning::InvalidEntrypoint(c));
                    None
                }
            })
            .collect();
        let reachable = self.reachable_array(unpacked_entrypoints.iter().cloned());
        let n = self.instructions.len();
        // TFtoPL.2014.70
        self.instructions
            .iter_mut()
            .zip(reachable.iter())
            .enumerate()
            .filter(|(_, (_, reachable))| **reachable)
            .filter_map(|(i, (instruction, _))| {
                instruction
                    .next_instruction
                    .map(|inc| (i, inc, instruction))
            })
            .filter(|(i, inc, _)| *i + (*inc as usize) + 1 >= n)
            .for_each(|(i, _, instruction)| {
                instruction.next_instruction = None;
                warnings.push(ValidationWarning::SkipTooLarge(i));
            });

        for (i, (instruction, reachable)) in self.instructions.iter_mut().zip(reachable).enumerate()
        {
            let is_kern_step = match instruction.operation {
                Operation::Kern(_) | Operation::KernAtIndex(_) => true,
                Operation::Ligature { .. } => false,
                Operation::EntrypointRedirect(r, _) => {
                    if let Ok(u) = i.try_into() {
                        // If it's a passthrough instruction, don't issue a warning.
                        if !reachable && self.passthrough.contains(&u) {
                            continue;
                        }
                    }
                    if r as usize >= n {
                        warnings.push(ValidationWarning::EntrypointRedirectTooBig(i));
                    }
                    continue;
                }
            };
            if !char_exists(instruction.right_char)
                && Some(instruction.right_char) != self.right_boundary_char
            {
                warnings.push(if is_kern_step {
                    ValidationWarning::KernStepForNonExistentCharacter {
                        instruction_index: i,
                        right_char: instruction.right_char,
                        new_right_char: smallest_char,
                    }
                } else {
                    ValidationWarning::LigatureStepForNonExistentCharacter {
                        instruction_index: i,
                        right_char: instruction.right_char,
                        new_right_char: smallest_char,
                    }
                });
                instruction.right_char = smallest_char;
            }
            match &mut instruction.operation {
                Operation::Kern(_) => {}
                Operation::KernAtIndex(k) => {
                    if *k as usize >= kerns.len() {
                        warnings.push(ValidationWarning::KernIndexTooBig(i));
                        instruction.operation = Operation::Kern(FixWord::ZERO);
                    }
                }
                Operation::Ligature {
                    char_to_insert,
                    post_lig_tag_invalid,
                    ..
                } => {
                    if !char_exists(*char_to_insert) {
                        warnings.push(
                            ValidationWarning::LigatureStepProducesNonExistentCharacter {
                                instruction_index: i,
                                replacement_char: *char_to_insert,
                                new_replacement_char: smallest_char,
                            },
                        );
                        *char_to_insert = smallest_char;
                    }
                    if *post_lig_tag_invalid {
                        warnings.push(ValidationWarning::InvalidLigTag(i));
                        *post_lig_tag_invalid = false;
                    }
                }
                Operation::EntrypointRedirect(_, _) => {}
            }
        }

        let entrypoints: HashMap<Char, u16> = unpacked_entrypoints.into_iter().collect();
        // We are only running the compiler to find errors and are disregarding the actual
        // result. Thus the true design size is not needed. Using this dummy avoids having
        // to plumb in the real design size into this function.
        let dummy_design_size = FixWord::ONE * 10;
        let (_, errors) =
            super::CompiledProgram::compile(self, dummy_design_size, kerns, entrypoints);
        for err in errors {
            warnings.push(ValidationWarning::InfiniteLoop(err));
        }
        warnings
    }

    fn reachable_array<I: Iterator<Item = (Char, u16)>>(&self, entrypoints: I) -> Vec<bool> {
        let mut reachable = vec![false; self.instructions.len()];
        // TFtoPL.2014.68
        for (_, entrypoint) in entrypoints {
            if let Some(slot) = reachable.get_mut(entrypoint as usize) {
                *slot = true;
            }
        }
        // TFtoPL.2014.69
        if let Some(entrypoint) = self.left_boundary_char_entrypoint {
            // There is a bug (?) in Knuth's TFtoPL when the entrypoint for the boundary char
            // points at the last instruction - i.e., the instruction containing the
            // boundary char entrypoint. In this case the last instruction is marked as passthrough
            // and not accessible.
            if entrypoint as usize != self.instructions.len() - 1 {
                if let Some(slot) = reachable.get_mut(entrypoint as usize) {
                    *slot = true;
                }
            }
        }
        // TFtoPL.2014.70
        for i in 0..reachable.len() {
            if !reachable[i] {
                continue;
            }
            if let Some(inc) = self.instructions[i].next_instruction {
                if let Some(slot) = reachable.get_mut(i + inc as usize + 1) {
                    *slot = true;
                }
            }
        }
        reachable
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValidationWarning {
    SkipTooLarge(usize),
    LigatureStepForNonExistentCharacter {
        // Index of the buggy lig instruction in the program.
        instruction_index: usize,
        // Right character that is non existent.
        right_char: Char,
        // New right character after the buggy instruction is fixed.
        //
        // This is set to bc by tftopl. Note there is no guarantee that bc
        // itself exists, so the instruction may still be buggy.
        new_right_char: Char,
    },
    KernStepForNonExistentCharacter {
        // Index of the buggy kern instruction in the program.
        instruction_index: usize,
        // Right character that is non existent.
        right_char: Char,
        // New right character after the buggy instruction is fixed.
        //
        // This is set to bc by tftopl. Note there is no guarantee that bc
        // itself exists, so the instruction may still be buggy.
        new_right_char: Char,
    },
    LigatureStepProducesNonExistentCharacter {
        // Index of the buggy kern instruction in the program.
        instruction_index: usize,
        // Replacement character that is non existent.
        replacement_char: Char,
        // New replacement character after the buggy instruction is fixed.
        //
        // This is set to bc by tftopl. Note there is no guarantee that bc
        // itself exists, so the instruction may not be fully fixed.
        new_replacement_char: Char,
    },
    KernIndexTooBig(usize),
    InvalidLigTag(usize),
    EntrypointRedirectTooBig(usize),
    InvalidEntrypoint(Char),
    InvalidBoundaryCharEntrypoint,
    InfiniteLoop(super::InfiniteLoopError),
}

impl ValidationWarning {
    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tftopl_message(&self) -> String {
        use ValidationWarning::*;
        match self {
            SkipTooLarge(i) => {
                format!["Bad TFM file: Ligature/kern step {i} skips too far;\nI made it stop."]
            }
            LigatureStepForNonExistentCharacter { right_char, .. } => format![
                "Bad TFM file: Ligature step for nonexistent character '{:03o}.",
                right_char.0
            ],
            KernStepForNonExistentCharacter { right_char, .. } => format![
                "Bad TFM file: Kern step for nonexistent character '{:03o}.",
                right_char.0
            ],
            LigatureStepProducesNonExistentCharacter {
                replacement_char, ..
            } => format![
                "Bad TFM file: Ligature step produces the nonexistent character '{:03o}.",
                replacement_char.0
            ],
            KernIndexTooBig(_) => "Bad TFM file: Kern index too large.".to_string(),
            InvalidLigTag(_) => "Ligature step with nonstandard code changed to LIG".to_string(),
            EntrypointRedirectTooBig(_) => {
                "Bad TFM file: Ligature unconditional stop command address is too big.".to_string()
            }
            InvalidEntrypoint(c) => {
                format![" \nLigature/kern starting index for character '{:03o} is too large;\nso I removed it.", c.0]
            }
            InvalidBoundaryCharEntrypoint => {
                " \nLigature/kern starting index for boundarychar is too large;so I removed it."
                    .to_string()
            }
            InfiniteLoop(err) => err.pltotf_message(),
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> u8 {
        use ValidationWarning::*;
        match self {
            SkipTooLarge(_) => 70,
            LigatureStepForNonExistentCharacter { .. }
            | LigatureStepProducesNonExistentCharacter { .. }
            | InvalidLigTag(_) => 77,
            KernStepForNonExistentCharacter { .. } | KernIndexTooBig(_) => 76,
            EntrypointRedirectTooBig(_) => 74,
            InvalidEntrypoint(_) => 67,
            InvalidBoundaryCharEntrypoint => 69,
            InfiniteLoop(_) => 90,
        }
    }
}

pub struct ReachableIter<'a> {
    next: u16,
    reachable: Vec<bool>,
    program: &'a Program,
}

#[derive(Debug)]
pub enum ReachableIterItem {
    Reachable { adjusted_skip: Option<u8> },
    Unreachable,
    Passthrough,
}

impl<'a> Iterator for ReachableIter<'a> {
    type Item = ReachableIterItem;

    fn next(&mut self) -> Option<Self::Item> {
        let this = self.next;
        let instruction = self.program.instructions.get(this as usize)?;
        self.next += 1;
        Some(if self.reachable[this as usize] {
            let adjusted_skip = match instruction.next_instruction {
                None | Some(0) => None,
                Some(inc) => {
                    match self
                        .reachable
                        .get(this as usize + 1..this as usize + 1 + inc as usize)
                    {
                        None => None,
                        Some(n) => {
                            let reachable_skipped: u8 =
                                n.iter()
                                .filter(|reachable| **reachable)
                                .count()
                                .try_into()
                                .expect("iterating over at most u8::MAX elements, so the count will be at most u8::MAX");
                            Some(reachable_skipped)
                        }
                    }
                }
            };
            ReachableIterItem::Reachable { adjusted_skip }
        } else if self.program.passthrough.contains(&this) {
            ReachableIterItem::Passthrough
        } else {
            ReachableIterItem::Unreachable
        })
    }
}

/// Iterator over the lig/kern instructions for a specific entrypoint.
///
/// Create using [`Program::instructions_for_entrypoint`].
pub struct InstructionsForEntrypointIter<'a> {
    next: usize,
    instructions: &'a [Instruction],
}

impl<'a> Iterator for InstructionsForEntrypointIter<'a> {
    type Item = (usize, &'a Instruction);

    fn next(&mut self) -> Option<Self::Item> {
        self.instructions.get(self.next).map(|i| {
            let this = self.next;
            self.next = match i.next_instruction {
                None => usize::MAX,
                Some(inc) => self.next + inc as usize + 1,
            };
            (this, i)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! parse_compact_program_tests {
        ( $( ($name: ident, $input: expr, $want: expr, ),)+ ) => {
            mod parse_compact_program {
                use super::*;
                use ParseCompactProgramError::*;
            $(
                #[test]
                fn $name() {
                    let want: Result<(Program, HashMap<Char, u16>), ParseCompactProgramError> = $want;
                    assert_eq![
                        Program::parse_compact($input),
                        want,
                    ];
                }
            )+
            }
        };
    }

    parse_compact_program_tests!(
        (
            ok,
            "
                ac -> _y^_
                ab -> axb^
                bc -> b[1]c
            ",
            Ok((
                Program {
                    instructions: vec![
                        Instruction {
                            next_instruction: Some(0),
                            right_char: 'b'.try_into().unwrap(),
                            operation: Operation::Ligature {
                                char_to_insert: 'x'.try_into().unwrap(),
                                post_lig_operation: PostLigOperation::RetainBothMoveToRight,
                                post_lig_tag_invalid: false,
                            }
                        },
                        Instruction {
                            next_instruction: None,
                            right_char: 'c'.try_into().unwrap(),
                            operation: Operation::Ligature {
                                char_to_insert: 'y'.try_into().unwrap(),
                                post_lig_operation: PostLigOperation::RetainNeitherMoveToInserted,
                                post_lig_tag_invalid: false,
                            }
                        },
                        Instruction {
                            next_instruction: None,
                            right_char: 'c'.try_into().unwrap(),
                            operation: Operation::Kern(FixWord(1)),
                        },
                    ],
                    left_boundary_char_entrypoint: None,
                    right_boundary_char: None,
                    passthrough: Default::default(),
                },
                HashMap::from([('a'.try_into().unwrap(), 0), ('b'.try_into().unwrap(), 2),])
            )),
        ),
        (invalid_left_char, "αb -> _c^_", Err(InvalidLeftChar),),
        (invalid_right_char, "bα -> _c^_", Err(InvalidRightChar),),
    );

    macro_rules! parse_compact_operation_tests {
        ( $( ($name: ident, $input: expr, $want: expr, ),)+ ) => {
            mod parse_compact_operation {
                use super::*;
                use ParseCompactOperationError::*;
            $(
                #[test]
                fn $name() {
                    let want: Result<(char, char, Operation), ParseCompactOperationError> = $want;
                    assert_eq![
                        Operation::parse_compact($input),
                        want,
                    ];
                    if let Ok((l, r, op)) = want {
                        let s = format!["{}", op.display_compact(l, r)];
                        assert_eq![s, $input];
                    }
                }
            )+
            }
        };
    }

    parse_compact_operation_tests!(
        (one_word, "ab", Err(NotThreeWords),),
        (four_words, "ab -> axb^ four", Err(NotThreeWords),),
        (not_an_arrow, "ab %% axb^", Err(MiddleWordIsNotAnArrow),),
        (first_too_small, "a -> axb^", Err(FirstWordIsWrongSize),),
        (first_too_big, "abc -> axb^", Err(FirstWordIsWrongSize),),
        (left_char_invalid, "_b -> _xb^", Err(LeftCharInvalid),),
        (right_char_invalid, "a_ -> ax_^", Err(RightCharInvalid),),
        (
            cursor_not_after_char_1,
            "ab -> ^_x_",
            Err(CursorNotAfterChar),
        ),
        (
            cursor_not_after_char_2,
            "ab -> _^x_",
            Err(CursorNotAfterChar),
        ),
        (
            cursor_not_after_char_3,
            "ab -> _x_^",
            Err(CursorNotAfterChar),
        ),
        (
            cursor_not_after_char_4,
            "ab -> _^xb",
            Err(CursorNotAfterChar),
        ),
        (
            cursor_not_after_char_5,
            "ab -> ax_^",
            Err(CursorNotAfterChar),
        ),
        (
            lig_1,
            "ab -> a^xb",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainBothMoveNowhere,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_2,
            "ab -> ax^b",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainBothMoveToInserted,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_3,
            "ab -> axb^",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainBothMoveToRight,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_4,
            "ab -> _x^b",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainRightMoveToInserted,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_5,
            "ab -> _xb^",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainRightMoveToRight,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_6,
            "ab -> a^x_",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainLeftMoveNowhere,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_7,
            "ab -> ax^_",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainLeftMoveToInserted,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            lig_8,
            "ab -> _x^_",
            Ok((
                'a',
                'b',
                Operation::Ligature {
                    char_to_insert: 'x'.try_into().unwrap(),
                    post_lig_operation: PostLigOperation::RetainNeitherMoveToInserted,
                    post_lig_tag_invalid: false,
                }
            )),
        ),
        (
            kern_1,
            "ab -> a[13]b",
            Ok(('a', 'b', Operation::Kern(FixWord(13)),)),
        ),
        (
            kern_2,
            "ab -> a[-789]b",
            Ok(('a', 'b', Operation::Kern(FixWord(-789)),)),
        ),
        (kern_char_before_bracket, "ab -> ax[1]b", Err(InvalidKern),),
        (kern_no_close_bracket, "ab -> a[13", Err(InvalidKern),),
        (kern_no_right_char, "ab -> a[13]", Err(MissingRightChar),),
        (kern_trailing_chars, "ab -> a[13]bx", Err(InvalidKern),),
        (kern_wrong_left_char, "ab -> c[13]b", Err(MissingLeftChar),),
        (kern_wrong_right_char, "ab -> a[13]c", Err(MissingRightChar),),
        (kern_invalid_number, "ab -> a[xyz]b", Err(InvalidKern),),
        (kern_empty_number, "ab -> a[]b", Err(InvalidKern),),
        (third_too_small, "ab -> axb", Err(InvalidLigature),),
        (third_too_big, "ab -> axbc^", Err(InvalidLigature),),
        (missing_cursor, "ab -> axb_", Err(MissingCursor),),
        (
            replacement_char_is_underscore,
            "ab -> a^_b",
            Err(ReplacementCharInvalid),
        ),
        (
            replacement_char_out_of_range,
            "ab -> a^αb",
            Err(ReplacementCharInvalid),
        ),
        (missing_left_char, "ab -> cxb^", Err(MissingLeftChar),),
        (missing_right_char, "ab -> axc^", Err(MissingRightChar),),
        (left_char_is_caret, "^b -> ^xb^", Err(LeftCharInvalid),),
        (right_char_is_caret, "a^ -> ax^^", Err(RightCharInvalid),),
    );
}
