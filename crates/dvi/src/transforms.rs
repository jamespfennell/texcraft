//! Transforms on DVI data.
//!
//! The DVI format can represent the same document
//! in multiple different ways.
//! For example this list of operations:
//! ```
//! vec![
//!     dvi::Op::SetVar(dvi::Var::X, 3),
//!     dvi::Op::Move(dvi::Var::X),
//!     dvi::Op::TypesetChar{char: 'D' as u32, move_h: false},
//! ];
//! ```
//! describes the same document as this list of operations:
//! ```
//! vec![
//!     dvi::Op::Right(6),
//!     dvi::Op::TypesetChar{char: 'D' as u32, move_h: false},
//! ];
//! ```
//! In both cases, the result of the DVI operations is a document
//! with the single character D typeset at the coordinate (6,0).
//!
//! This module contains _transforms_ that change the DVI representation
//! of documents without changing the actual meaning of the document.
//! You can think of transforms as being like optimization passes in
//! an optimizing compiler: the output program behaves the same,
//!     but its code is different.
//!
//! There are at least two reasons why one would want to perform a DVI transform:
//!
//! 1. To **optimize** the DVI in some way; for example, to reduce
//!     the size of the DVI file.
//!     Knuth performs an optmization of this type in TeX.2021.604-615.
//!
//! 2. To **normalize** the DVI file so that a DVI produced by one program
//!     will match the DVI produced by another program.
use super::*;

/// Transform that removes uses of the [`Var`] variables.
///
/// This transform removes all [`Op::Move`] and [`Op::SetVar`]
/// operations and thus removes all uses of the _w_, _x_, _y_ and
/// _z_ variables.
/// All [`Op::Move`] and [`Op::SetVar`] are replaced by [`Op::Right`]
/// and [`Op::Down`] operations, with the correct payload.
///
/// This transform undoes the optimization that Knuth performs in
/// TeX.2021.604-615.
///
/// ```
/// use dvi::transforms::VarRemover;
/// let ops_1 = vec![
///     dvi::Op::SetVar(dvi::Var::X, 3),
///     dvi::Op::Push,
///     dvi::Op::SetVar(dvi::Var::X, 5),
///     dvi::Op::Move(dvi::Var::X),
///     dvi::Op::Pop,
///     dvi::Op::Move(dvi::Var::X),
/// ];
/// let ops_2: Vec<dvi::Op> = VarRemover::new(ops_1).collect();
/// assert_eq![
///     ops_2,
///     vec![
///         dvi::Op::Right(3),
///         dvi::Op::Push,
///         dvi::Op::Right(5),
///         dvi::Op::Right(5),
///         dvi::Op::Pop,
///         dvi::Op::Right(3),
///     ],
/// ];
/// ```
pub struct VarRemover<I> {
    iter: I,
    values: Values,
}

impl<I: Iterator<Item = Op>> VarRemover<I> {
    pub fn new<J: IntoIterator<IntoIter = I>>(iter: J) -> Self {
        Self {
            iter: iter.into_iter(),
            values: Default::default(),
        }
    }
}

impl<I: Iterator<Item = Op>> Iterator for VarRemover<I> {
    type Item = Op;

    fn next(&mut self) -> Option<Self::Item> {
        let op = self.iter.next()?;
        self.values.update(&op);
        Some(match op {
            Op::Move(var) | Op::SetVar(var, _) => {
                let value = self.values.var(var);
                match var {
                    Var::W | Var::X => Op::Right(value),
                    Var::Y | Var::Z => Op::Down(value),
                }
            }
            _ => op,
        })
    }
}
