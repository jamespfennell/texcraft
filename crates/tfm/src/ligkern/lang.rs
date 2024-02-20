//! Types corresponding to the "lig/kern programming language".
//!
//! See the documentation on the [`super`] module for information about this programming language.
//!
//! The types here are put in a separate module because users of this crate are generally not expected to use them.
//! Instead, users will work with compiled lig/kern programs.

use std::collections::HashMap;

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

impl Program {
    pub fn unpack_entrypoint(&self, entrypoint: u8) -> u16 {
        match self.instructions[entrypoint as usize].operation {
            Operation::EntrypointRedirect(u_big, _) => u_big,
            _ => entrypoint as u16,
        }
    }

    pub fn unpack_entrypoints<I: Iterator<Item = (Char, u8)>>(
        &self,
        entrypoints: I,
    ) -> HashMap<Char, u16> {
        entrypoints
            .map(|(c, u)| (c, self.unpack_entrypoint(u)))
            .collect()
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
                    let u = offset;
                    redirects.push(u16_entrypoint);
                    if i == 0 && self.boundary_char.is_some() {
                        // This implements the "optimization" "location 0 can do double duty" in PLtoTF.2014.141
                        instructions.pop();
                        offset = 0;
                    }
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

    pub fn reachable_iter<I: Iterator<Item = u16>>(&self, entrypoints: I) -> ReachableIter {
        ReachableIter {
            next: 0,
            reachable: self.reachable_array(entrypoints),
            instructions: &self.instructions,
        }
    }

    /// Iterate over the lig/kern instructions for a specific entrypoint.
    pub fn instructions_for_entrypoint(&self, entrypoint: u16) -> InstructionsForEntrypointIter {
        InstructionsForEntrypointIter {
            next: entrypoint as usize,
            instructions: &self.instructions,
        }
    }

    pub fn validate_and_fix<T>(
        &mut self,
        smallest_char: Option<Char>,
        char_exists: T,
        num_kerns: usize,
    ) -> Vec<ValidationWarning>
    where
        T: Fn(Char) -> bool,
    {
        let mut warnings: Vec<ValidationWarning> = vec![];

        let n = self.instructions.len();
        for (i, instruction) in self.instructions.iter_mut().enumerate() {
            let is_kern_step = match instruction.operation {
                Operation::Kern(_) => true,
                Operation::KernAtIndex(_) => true,
                Operation::Ligature { .. } => false,
                Operation::EntrypointRedirect(_, _) => continue,
            };
            if let Operation::EntrypointRedirect(_, _) = instruction.operation {
                continue;
            }
            if let Some(inc) = instruction.next_instruction {
                if i + (inc as usize) + 1 >= n {
                    warnings.push(ValidationWarning::SkipTooLarge(i));
                    instruction.next_instruction = None;
                }
            }
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
                instruction.right_char = smallest_char.unwrap_or(Char(0));
            }
            match &mut instruction.operation {
                Operation::Kern(_) => {}
                Operation::KernAtIndex(k) => {
                    if *k as usize >= num_kerns {
                        warnings.push(ValidationWarning::KernIndexTooBig(i));
                        instruction.operation = Operation::Kern(Number::ZERO);
                    }
                }
                Operation::Ligature { char_to_insert, .. } => {
                    if !char_exists(*char_to_insert) {
                        warnings.push(ValidationWarning::LigatureStepProducesNonExistentCharacter(
                            i,
                            *char_to_insert,
                        ));
                        *char_to_insert = smallest_char.unwrap_or(Char(0));
                    }
                }
                Operation::EntrypointRedirect(_, _) => {}
            }
        }

        warnings
    }

    pub fn reachable_array<I: Iterator<Item = u16>>(&self, entrypoints: I) -> Vec<bool> {
        let mut reachable = vec![false; self.instructions.len()];
        // TFtoPL.2014.68
        for entrypoint in entrypoints {
            *reachable
                .get_mut(entrypoint as usize)
                .expect("each entrypoint is a valid index for the instructions vector") = true;
        }
        if let Some(entrypoint) = self.boundary_char_entrypoint {
            *reachable
                .get_mut(entrypoint as usize)
                .expect("boundary_char_entrypoint is a valid index for the instructions vector") =
                true;
        }
        // TFtoPL.2014.70
        for i in 0..reachable.len() {
            if !reachable[i] {
                continue;
            }
            if let Some(inc) = self.instructions[i].next_instruction {
                if let Some(slot) = reachable.get_mut(i + inc as usize + 1) {
                    *slot = true
                }
            }
        }
        reachable
    }
}

pub enum ValidationWarning {
    SkipTooLarge(usize),
    LigatureStepForNonExistentCharacter(usize, Char),
    KernStepForNonExistentCharacter(usize, Char),
    LigatureStepProducesNonExistentCharacter(usize, Char),
    KernIndexTooBig(usize),
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
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> usize {
        use ValidationWarning::*;
        match self {
            SkipTooLarge(_) => 70,
            LigatureStepForNonExistentCharacter(_, _)
            | LigatureStepProducesNonExistentCharacter(_, _) => 77,
            KernStepForNonExistentCharacter(_, _) | KernIndexTooBig(_) => 76,
        }
    }
}

pub struct ReachableIter<'a> {
    next: u16,
    reachable: Vec<bool>,
    instructions: &'a [Instruction],
}

pub enum ReachableIterItem<'a> {
    Reachable {
        instruction: &'a Instruction,
        index: u16,
        skip_override: Option<u8>,
    },
    Unreachable(&'a [Instruction]),
}

impl<'a> Iterator for ReachableIter<'a> {
    type Item = ReachableIterItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let this = self.next;
            let instruction = match self.instructions.get(this as usize) {
                None => return None,
                Some(instruction) => instruction,
            };
            self.next += 1;
            if let Operation::EntrypointRedirect(_, _) = instruction.operation {
                continue;
            }
            return Some(if self.reachable[this as usize] {
                let skip_override = match instruction.next_instruction {
                    None | Some(0) => None,
                    Some(inc) => {
                        let reachable_skipped: u8 = self.reachable
                            .get(this as usize + 1..this as usize+ 1+ inc as usize)
                            .expect("lig/kern skip produces an index within bounds")
                            .iter()
                            .filter(|reachable| **reachable)
                            .count()
                            .try_into()
                            .expect("iterating over at most u8::MAX elements, so the count will be at most u8::MAX");
                        Some(reachable_skipped)
                    }
                };
                ReachableIterItem::Reachable {
                    instruction,
                    index: this,
                    skip_override,
                }
            } else {
                let mut upper_bound = this + 1;
                loop {
                    if let Some(instruction) = self.instructions.get(upper_bound as usize) {
                        if let Operation::EntrypointRedirect(_, _) = instruction.operation {
                            break;
                        }
                    }
                    if self.reachable.get(upper_bound as usize).copied() != Some(false) {
                        break;
                    }
                    upper_bound += 1;
                }
                self.next = upper_bound;
                ReachableIterItem::Unreachable(
                    &self.instructions[this as usize..upper_bound as usize],
                )
            });
        }
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
        self.instructions.get(self.next).and_then(|i| {
            if let Operation::EntrypointRedirect(_, _) = i.operation {
                return None;
            }
            let this = self.next;
            self.next = match i.next_instruction {
                None => usize::MAX,
                Some(inc) => self.next + inc as usize + 1,
            };
            Some((this, i))
        })
    }
}
