//! Types corresponding to the "lig/kern programming language".
//!
//! See the documentation on the [`super`] module for information about this programming language.
//!
//! The types here are put in a separate module because users of this crate are generally not expected to use them.
//! Instead, users will work with compiled lig/kern programs.

use std::collections::HashMap;
use std::collections::HashSet;

use crate::Char;
use crate::Number;

/// A lig/kern program.
///
/// In theory the program also requires entrypoints.
/// However because these are provided in different ways in .tfm and .pl files,
/// it's easier to exclude them on this type and pass them in when needed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    pub instructions: Vec<Instruction>,
    pub boundary_char: Option<Char>,
    pub boundary_char_entrypoint: Option<u16>,
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
    Kern(Number),
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

impl Program {
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

    fn unpack_entrypoints<'a, I: Iterator<Item = (Char, u8)> + 'a>(
        &'a mut self,
        entrypoints: I,
    ) -> impl Iterator<Item = (Char, Result<u16, InvalidEntrypointError>)> + 'a {
        entrypoints.map(|(c, u)| (c, self.unpack_entrypoint(u)))
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
        let mut offset: u8 = if self.boundary_char.is_some() {
            // In .tfm files the boundary char is transmitted in each entrypoint redirect instruction.
            // If there is a boundary char, we need at least one entrypoint redirect to exist so
            // that the boundary char is there.
            instructions.push(Instruction {
                next_instruction: None,
                right_char: self.boundary_char.unwrap_or(Char(0)),
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
                    if i == 0 && self.boundary_char.is_some() {
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
            right_char: self.boundary_char.unwrap_or(Char(0)),
            operation: Operation::EntrypointRedirect(
                redirect.checked_add(offset as u16).expect("the inputted lig/kern instructions vector doesn't have enough space for new instructions"),
            true,
        ),
        });
        }
        instructions.rotate_right(offset as usize);
        if let Some(boundary_char_entrypoint) = self.boundary_char_entrypoint {
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

    pub fn unpack_kerns(&mut self) -> Vec<Number> {
        let mut kerns = vec![];
        let mut kerns_dedup = HashMap::<Number, usize>::new();
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

    pub fn pack_kerns(&mut self, kerns: &[Number]) {
        for i in &mut self.instructions {
            if let Operation::KernAtIndex(index) = &i.operation {
                // TODO: log a warning if the index is not in the kerns array as
                // in TFtoPL.2014.76
                i.operation =
                    Operation::Kern(kerns.get(*index as usize).copied().unwrap_or_default())
            }
        }
    }

    pub fn reachable_iter<I: Iterator<Item = (Char, u16)>>(&self, entrypoints: I) -> ReachableIter {
        let (reachable, _) = self.reachable_array(entrypoints.map(|(c, e)| (c, Ok(e))));
        ReachableIter {
            next: 0,
            reachable,
            program: self,
        }
    }

    /// Iterate over the lig/kern instructions for a specific entrypoint.
    pub fn instructions_for_entrypoint(&self, entrypoint: u16) -> InstructionsForEntrypointIter {
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
        kerns: &[Number],
    ) -> (Vec<ValidationWarning>, Option<super::CompiledProgram>)
    where
        I: Iterator<Item = (Char, u8)>,
        T: Fn(Char) -> bool,
    {
        let unpacked_entrypoints: Vec<(Char, Result<u16, InvalidEntrypointError>)> =
            self.unpack_entrypoints(entrypoints).collect();
        let (reachable, mut warnings) =
            { self.reachable_array(unpacked_entrypoints.clone().into_iter()) };
        warnings
            .iter()
            .filter_map(|w| match w {
                ValidationWarning::SkipTooLarge(u) => Some(u),
                _ => None,
            })
            .for_each(|u| self.instructions[*u].next_instruction = None);

        let n = self.instructions.len();
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
                && Some(instruction.right_char) != self.boundary_char
            {
                warnings.push(if is_kern_step {
                    ValidationWarning::KernStepForNonExistentCharacter(i, instruction.right_char)
                } else {
                    ValidationWarning::LigatureStepForNonExistentCharacter(
                        i,
                        instruction.right_char,
                    )
                });
                instruction.right_char = smallest_char;
            }
            match &mut instruction.operation {
                Operation::Kern(_) => {}
                Operation::KernAtIndex(k) => {
                    if *k as usize >= kerns.len() {
                        warnings.push(ValidationWarning::KernIndexTooBig(i));
                        instruction.operation = Operation::Kern(Number::ZERO);
                    }
                }
                Operation::Ligature {
                    char_to_insert,
                    post_lig_tag_invalid,
                    ..
                } => {
                    if !char_exists(*char_to_insert) {
                        warnings.push(ValidationWarning::LigatureStepProducesNonExistentCharacter(
                            i,
                            *char_to_insert,
                        ));
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

        let entrypoints: HashMap<Char, u16> = unpacked_entrypoints
            .into_iter()
            .filter_map(|(c, e_or)| e_or.ok().map(|e| (c, e)))
            .collect();
        let program_or =
            match super::CompiledProgram::compile(&self.instructions, kerns, entrypoints) {
                Ok(program) => Some(program),
                Err(err) => {
                    warnings.push(ValidationWarning::InfiniteLoop(err));
                    None
                }
            };
        (warnings, program_or)
    }

    fn reachable_array<I: Iterator<Item = (Char, Result<u16, InvalidEntrypointError>)>>(
        &self,
        entrypoints: I,
    ) -> (Vec<bool>, Vec<ValidationWarning>) {
        let mut reachable = vec![false; self.instructions.len()];
        let mut warnings = vec![];
        // TFtoPL.2014.68
        for (c, entrypoint_or_err) in entrypoints {
            match entrypoint_or_err {
                Ok(entrypoint) => {
                    *reachable
                        .get_mut(entrypoint as usize)
                        .expect("entrypoint is valid so indexes into the reachable array") = true;
                }
                Err(_) => {
                    // TODO: move this warning into the unpacking code?
                    warnings.push(ValidationWarning::InvalidEntrypoint(c));
                }
            }
        }
        if let Some(entrypoint) = self.boundary_char_entrypoint {
            // TODO: warning
            if let Some(slot) = reachable.get_mut(entrypoint as usize) {
                *slot = true;
            }
        }
        // TFtoPL.2014.70
        for i in 0..reachable.len() {
            if !reachable[i] {
                continue;
            }
            if let Some(inc) = self.instructions[i].next_instruction {
                match reachable.get_mut(i + inc as usize + 1) {
                    None => warnings.push(ValidationWarning::SkipTooLarge(i)),
                    Some(slot) => *slot = true,
                }
            }
        }
        (reachable, warnings)
    }
}

#[derive(Clone, Debug)]
pub enum ValidationWarning {
    SkipTooLarge(usize),
    LigatureStepForNonExistentCharacter(usize, Char),
    KernStepForNonExistentCharacter(usize, Char),
    LigatureStepProducesNonExistentCharacter(usize, Char),
    KernIndexTooBig(usize),
    InvalidLigTag(usize),
    EntrypointRedirectTooBig(usize),
    InvalidEntrypoint(Char),
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
            LigatureStepForNonExistentCharacter(_, c) => format![
                "Bad TFM file: Ligature step for nonexistent character '{:03o}.",
                c.0
            ],
            KernStepForNonExistentCharacter(_, c) => format![
                "Bad TFM file: Kern step for nonexistent character '{:03o}.",
                c.0
            ],
            LigatureStepProducesNonExistentCharacter(_, c) => format![
                "Bad TFM file: Ligature step produces the nonexistent character '{:03o}.",
                c.0
            ],
            KernIndexTooBig(_) => "Bad TFM file: Kern index too large.".to_string(),
            InvalidLigTag(_) => "Ligature step with nonstandard code changed to LIG".to_string(),
            EntrypointRedirectTooBig(_) => {
                "Bad TFM file: Ligature unconditional stop command address is too big.".to_string()
            }
            InvalidEntrypoint(c) => {
                format![" \nLigature/kern starting index for character '{:03o} is too large;\nso I removed it.", c.0]
            }
            InfiniteLoop(err) => {
                // TODO: the error can involve a boundary char
                format![
                    "Infinite ligature loop starting with '{:03o} and '{:03o}!",
                    err.starting_pair.0 .0, err.starting_pair.1 .0
                ]
            }
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> u8 {
        use ValidationWarning::*;
        match self {
            SkipTooLarge(_) => 70,
            LigatureStepForNonExistentCharacter(_, _)
            | LigatureStepProducesNonExistentCharacter(_, _)
            | InvalidLigTag(_) => 77,
            KernStepForNonExistentCharacter(_, _) | KernIndexTooBig(_) => 76,
            EntrypointRedirectTooBig(_) => 74,
            InvalidEntrypoint(_) => 67,
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
        let instruction = match self.program.instructions.get(this as usize) {
            None => return None,
            Some(instruction) => instruction,
        };
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
