//! # tfm: TeX font metric data
//!
//! This is a crate for working with TeX font metric data.
//! It includes:
//!
//! - Functions to read and write TeX font metric (.tfm) files
//!     to and from a value of type [`File`]
//!     ([`deserialize`](File::deserialize), [`serialize`](File::serialize)).
//!
//! - Functions to read and write property list (.pl) files
//!     to and from a value of type [`pl::File`]
//!     ([`from_pl_source_code`](pl::File::from_pl_source_code), [`display`](pl::File::display)).
//!
//! - Converters from .tfm to .pl files and vice-versa
//!     (using Rust's [`From`] trait to go between [`File`] and [`pl::File`]).
//!
//! ## Background
//!
//! Probably the most famous part of the implementation of TeX is the Knuth-Plass line breaking algorithm.
//! This algorithm determines the "optimal" places to add line breaks when typesetting a paragraph of text.
//! In order to run the algorithm one needs to provide the dimensions of all characters in the current font.
//! These dimensions are used to size the boxes in the Knuth-Plass box and glue model.
//!
//! In TeX, character dimensions are provided using TeX font metric files.
//! These are binary files.
//! By convention they have a .tfm file extension.
//! Unlike more modern file formats like TrueType, .tfm files only contain the font dimensions;
//!     they don't contains the glyphs.
//! In general,
//!     .tfm files are produced by other software like Metafont,
//!     placed in some well-known directory in the TeX distribution,
//!     and then read into memory when TeX is running.
//!
//! Because .tfm files are binary files, it's hard to debug or tweak them.
//! To remedy this, Knuth and his team developed another file format called a property list file
//!     (extension .pl or .plst)
//!     that contains the same information but in a modifiable text format.
//! They then wrote two programs:
//!     `tftopl` to convert a .tfm file to a .pl file,
//!     and `pltotf` to convert a .pl file to a .tfm file.
//!
//! The general goal of this crate to fully re-implement all of the TeX font metric
//!     code written by Knuth and others.
//! This includes `tftopl`, `pltotf`, and also the parts of TeX itself that contain logic
//!     for reading and interpreting .tfm files.
//! However, unlike these monolithic software programs,
//!     this re-implementation is in the form of a modular library in which
//!     individual pieces of logic and be used and re-used.
//!
//! ## Basic example
//!
//! ```
//! // Include the .tfm file for Computer Modern in size 10pt.
//! let tfm_bytes = include_bytes!["../corpus/computer-modern/cmr10.tfm"];
//!
//! // Deserialize the .tfm file.
//! let (tfm_file_or_error, deserialization_warnings) = tfm::File::deserialize(tfm_bytes);
//! let mut tfm_file = tfm_file_or_error.expect("cmr10.tfm is a valid .tfm file");
//! assert_eq![deserialization_warnings, vec![], "the data in cmr10.tfm is 100% valid, so there are no deserialization warnings"];
//! // TODO assert_eq![tfm_file.header.design_size, tfm::FixWord::UNITY * 10]; make it 11 to be more interesting
//! // TODO query some data
//!
//! // Validate the .tfm file.
//! let validation_warnings = tfm_file.validate_and_fix();
//! assert_eq![validation_warnings, vec![], "the data in cmr10.tfm is 100% valid, so there are no validation warnings"];
//!
//! // Convert the .tfm file to a .pl file and print it.
//! let pl_file: tfm::pl::File = tfm_file.clone().into();
//! // TODO query some data
//! println!["cmr10.pl:\n{}", pl_file.display(/*indent=*/2, tfm::pl::CharDisplayFormat::Default)];
//! ```
//!
//!
//! ## Advanced functionality
//!
//! In addition to supporting the basic use cases of querying font metric data
//!     and converting between different formats,
//!     this crate has advanced functionality for performing additional tasks on font metric data.
//! The full set of functionality can be understood by navigating through the crate documentation.
//! But here are 3 highlights we think are interesting:
//!
//! - **Language analysis of .pl files**:
//!     In `pltotf`, Knuth parses .pl files in a single pass.
//!     This crate takes a common approach nowadays of parsing in multiple passes:
//!     first constructing a [concrete syntax tree](pl::cst::Cst) (or parse tree),
//!     next constructing a [fully typed and checked abstract syntax tree](pl::ast::Ast),
//!     and finally building the [`pl::File`] itself.
//!     Each of the passes is exposed, so you can e.g. just build the AST for the .pl file and
//!         do some analysis on it.
//!
//! - **Debug output for .tfm files**:
//!     
//! - **Compilation of lig/kern programs**:
//!
//!
//! ## Binaries
//!
//! The Texcraft project produces 3 binaries based on this crate:
//!
//! - `tftopl` and `pltotf`: re-implementations of Knuth's programs.
//! - `tfmtools`: a new binary that has a bunch of different tools
//!         for working with TeX font metric data.
//!         Run `tfmtools help` to list all of the available tools.
//!
//! In the root of [the Texcraft repository](https://github.com/jamespfennell/texcraft)
//!     these tools can be run with `cargo run --bin $NAME`
//!     and built with `cargo build --bin $NAME`.
//!
//!
//! ## Correctness
//!
//! As part of the development of this crate significant effort has been spent
//!     ensuring it exactly replicates the work of Knuth.
//! This correctness checking is largely based around diff testing the binaries
//!     `tftopl` and `pltotf`.
//! We verify that the Texcraft and Knuth implementations have the same output
//!     and generate the same error messages.
//!
//! This diff testing has been performed in a few different ways:
//!
//! - We have run diff tests over all ~100,000 .tfm files in CTAN.
//!     These tests verify that `tftopl` gives the correct result,
//!     and that running `pltotf` on the output .pl file gives the correct result too.
//!     Unfortunately running `pltotf` on the .pl files in CTAN is infeasible
//!     because most of these files are Perl scripts, not property list files.
//!
//! - We have developed a fuzz testing harness (so far just for `tftopl`)
//!     that generates highly heterogenous .tfm files and verifies that `tftopl` gives the correct result.
//!     This fuzz testing has uncovered many issues in the Texcraft implementation,
//!     and has even identified [a 30-year old bug](https://tug.org/pipermail/tex-k/2024-March/004031.html)
//!     in Knuth's implementation of `tftopl`.
//!
//! Any .tfm or .pl file that exposes a bug in this library is added to
//!     [our automated testing corpus](https://github.com/jamespfennell/texcraft/tree/main/crates/tfm/bin/tests/data).
//! Running `cargo t` validates that Texcraft's binaries give the same result as Knuth's binaries
//!     (the output of Knuth's binaries is in source control).
//! This ensures there are no regressions.
//!
//! If you discover a .tfm or .pl file such that the Texcraft and Knuth implementations
//!     diverge, this indicates there is a bug in this library.
//! Please create an issue on the Texcraft GitHub repo.
//! We will fix the bug and add your files to the testing corpus.

pub mod algorithms;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    num::NonZeroU8,
};
pub mod ligkern;
pub mod pl;

use std::collections::BTreeMap;

mod debug;
mod deserialize;
mod serialize;
mod validate;

pub use deserialize::DeserializationError;
pub use deserialize::DeserializationWarning;
pub use deserialize::RawFile;
pub use deserialize::SubFileSizes;
pub use validate::ValidationWarning;

/// Complete contents of a TeX font metric (.tfm) file.
///
/// The struct contain multiple vectors.
/// In TeX and TFtoPL there is an optimization in which all of data in the vectors
/// is stored in one large vector of 32-bit integers.
/// The conversion from [u32] to the specific types like [FixWord] are then done when the
/// data is needed.
/// This makes the memory footprint of this type much more compact,
///     and such a change may be considered in the future.
///
/// In fact in TeX the font data for all fonts is stored in one contiguous piece of memory
///     (`font_info`, defined in TeX82.2021.549).
/// This is a little too unsafe to pull off though.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct File {
    /// Header.
    pub header: Header,

    /// Smallest character in the .tfm.
    pub smallest_char: Char,

    /// Character dimensions.
    pub char_dimens: BTreeMap<Char, CharDimensions>,

    /// Character tags.
    ///
    /// Note there is no correlation between a character having
    ///     a tag and a character having a dimension.
    /// All four combinations of (has or hasn't dimensions) and (has or hasn't a tag)
    ///     are possible.
    pub char_tags: BTreeMap<Char, CharTag>,

    /// Tags that have been unset, but whose discriminant is still written to a .tfm file by PLtoTF.
    pub unset_char_tags: BTreeMap<Char, u8>,

    /// Character widths
    pub widths: Vec<FixWord>,

    /// Character heights
    pub heights: Vec<FixWord>,

    /// Character depths
    pub depths: Vec<FixWord>,

    /// Character italic corrections
    pub italic_corrections: Vec<FixWord>,

    /// Lig kern program.
    pub lig_kern_program: ligkern::lang::Program,

    /// Kerns. These are referenced from inside the lig kern commands.
    pub kerns: Vec<FixWord>,

    /// Extensible characters.
    pub extensible_chars: Vec<ExtensibleRecipe>,

    /// Font parameters.
    pub params: Vec<FixWord>,
}

/// Data about one character in a .tfm file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharDimensions {
    /// Index of the width of this character in the widths array.
    ///
    /// In valid TFM files, this index will always be non-zero.
    /// This is because if the width index is zero it means there is no data for the character in the file.
    /// In this case the other indices (height, etc.) are necessarily zero.
    /// See TFtoPL.2014.? (where blocks of data with a zero width index are skipped)
    ///     and PLtoTF.2014.? (where missing characters are written with all indices 0).
    ///
    /// There is one edge case where this index can be zero.
    /// This is if the width index is invalid.
    /// In this case tftopl essentially sets the width index to 0.
    ///
    /// Note that even if a character doesn't have dimensions, it can still have a tag.
    pub width_index: WidthIndex,
    /// Index of the height of this character in the height array.
    pub height_index: u8,
    pub depth_index: u8,
    pub italic_index: u8,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum WidthIndex {
    Invalid,
    Valid(NonZeroU8),
}

impl WidthIndex {
    pub fn get(&self) -> u8 {
        match self {
            WidthIndex::Invalid => 0,
            WidthIndex::Valid(n) => n.get(),
        }
    }
}

/// Tag of a character in a .tfm file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CharTag {
    Ligature(u8),
    List(Char),
    Extension(u8),
}

impl CharTag {
    pub fn ligature(&self) -> Option<u8> {
        match self {
            CharTag::Ligature(l) => Some(*l),
            CharTag::List(_) | CharTag::Extension(_) => None,
        }
    }
    pub fn list(&self) -> Option<Char> {
        match self {
            CharTag::List(c) => Some(*c),
            CharTag::Ligature(_) | CharTag::Extension(_) => None,
        }
    }
    pub fn extension(&self) -> Option<u8> {
        match self {
            CharTag::Extension(u) => Some(*u),
            CharTag::Ligature(_) | CharTag::List(_) => None,
        }
    }
}

/// Extensible recipe instruction in a .tfm file.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ExtensibleRecipe {
    pub top: Option<Char>,
    pub middle: Option<Char>,
    pub bottom: Option<Char>,
    pub rep: Char,
}

impl ExtensibleRecipe {
    pub fn is_seven_bit(&self) -> bool {
        self.chars().all(|c| c.is_seven_bit())
    }

    pub fn chars(&self) -> impl Iterator<Item = Char> {
        [self.top, self.middle, self.bottom, Some(self.rep)]
            .into_iter()
            .flatten()
    }
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Default::default(),
            smallest_char: Char(1),
            char_dimens: Default::default(),
            char_tags: Default::default(),
            unset_char_tags: Default::default(),
            widths: vec![FixWord::ZERO],
            heights: vec![FixWord::ZERO],
            depths: vec![FixWord::ZERO],
            italic_corrections: vec![FixWord::ZERO],
            lig_kern_program: Default::default(),
            kerns: vec![],
            extensible_chars: vec![],
            params: Default::default(),
        }
    }
}

/// Print debugging information about a .tfm file.
pub fn debug<'a>(
    path: Option<&'a str>,
    sub_file_sizes: SubFileSizes,
    tfm_file: Option<&'a File>,
    raw_file: Option<&'a RawFile<'a>>,
    sections: Vec<Section>,
) -> impl std::fmt::Display + 'a {
    debug::Debug {
        sub_file_sizes,
        path,
        raw_file,
        tfm_file,
        sections,
    }
}

impl File {
    pub fn from_raw_file(raw_file: &RawFile) -> Self {
        deserialize::from_raw_file(raw_file)
    }

    pub fn validate_and_fix(&mut self) -> Vec<ValidationWarning> {
        validate::validate_and_fix(self)
    }

    pub fn deserialize(
        b: &[u8],
    ) -> (
        Result<File, DeserializationError>,
        Vec<DeserializationWarning>,
    ) {
        deserialize::deserialize(b)
    }

    pub fn serialize(&self) -> Vec<u8> {
        serialize::serialize(self)
    }

    /// Return a map from characters to the lig/kern entrypoint for that character.
    /// TODO: can probably return impl Iterator<Item=(Char, u8)>
    pub fn lig_kern_entrypoints(&self) -> HashMap<Char, u8> {
        self.char_tags
            .iter()
            .filter_map(|(c, d)| match *d {
                CharTag::Ligature(l) => Some((*c, l)),
                _ => None,
            })
            .collect()
    }

    fn char_info_bounds(&self) -> Option<(Char, Char)> {
        let mut r: Option<(Char, Char)> = None;
        for c in self.char_dimens.keys().copied() {
            r = Some(match r {
                None => (c, c),
                Some((lower, upper)) => (
                    if c.0 < lower.0 { c } else { lower },
                    if c.0 < upper.0 { upper } else { c },
                ),
            })
        }
        r
    }

    /// Calculate the checksum of this .tfm file.
    pub fn checksum(&self) -> u32 {
        // This checksum algorithm is in PLtoTF.2014.134.
        let (bc, ec) = self.char_info_bounds().unwrap_or((Char(1), Char(0)));
        let mut b = [bc.0, ec.0, bc.0, ec.0];
        for c in bc.0..=ec.0 {
            let char_dimens = match self.char_dimens.get(&Char(c)) {
                None => continue,
                Some(char_dimens) => char_dimens,
            };
            let width = self.widths[char_dimens.width_index.get() as usize].0;
            // TODO: adjust based on the design units
            let width = width + (c as i32 + 4) * 0o20_000_000;
            let add = |b: u8, m: u8| -> u8 {
                (((b as i32) + (b as i32) + width) % (m as i32))
                    .try_into()
                    .expect("(i32 % u8) is always a u8")
            };
            b = [
                add(b[0], 255),
                add(b[1], 253),
                add(b[2], 251),
                add(b[3], 247),
            ]
        }
        u32::from_be_bytes(b)
    }
}

impl From<crate::pl::File> for File {
    fn from(pl_file: crate::pl::File) -> Self {
        let mut char_bounds: Option<(Char, Char)> = None;

        let lig_kern_entrypoints = pl_file.lig_kern_entrypoints(true);
        let mut lig_kern_program = pl_file.lig_kern_program;
        let kerns = lig_kern_program.unpack_kerns();
        let lig_kern_entrypoints = lig_kern_program.pack_entrypoints(lig_kern_entrypoints);

        let mut widths = pl_file.additional_widths;
        let mut heights = pl_file.additional_heights;
        let mut depths = pl_file.additional_depths;
        let mut italic_corrections = pl_file.additional_italics;
        for (char, char_dimens) in &pl_file.char_dimens {
            widths.push(char_dimens.width.unwrap_or_default());
            match char_dimens.height {
                None | Some(FixWord::ZERO) => {}
                Some(height) => heights.push(height),
            }
            match char_dimens.depth {
                None | Some(FixWord::ZERO) => {}
                Some(depth) => depths.push(depth),
            }
            match char_dimens.italic_correction {
                None | Some(FixWord::ZERO) => {}
                Some(italic_correction) => italic_corrections.push(italic_correction),
            }
            char_bounds = Some(match char_bounds {
                None => (*char, *char),
                Some((lower, upper)) => (
                    if *char < lower { *char } else { lower },
                    if *char > upper { *char } else { upper },
                ),
            })
        }
        let (widths, width_to_index) = compress(&widths, 255);
        let (heights, height_to_index) = compress(&heights, 15);
        let (depths, depth_to_index) = compress(&depths, 15);
        let (italic_corrections, italic_correction_to_index) = compress(&italic_corrections, 63);
        let mut extensible_chars = vec![];
        let char_dimens = match char_bounds {
            None => Default::default(),
            Some((lower, upper)) => {
                let mut m: BTreeMap<Char, CharDimensions> = Default::default();
                for c in lower.0..=upper.0 {
                    let pl_data = match pl_file.char_dimens.get(&Char(c)) {
                        Some(pl_data) => pl_data,
                        None => continue,
                    };
                    let width = pl_data.width.unwrap_or_default();
                    let width_index = *width_to_index.get(&width).expect(
                        "the map returned from compress(_,_) contains every input as a key",
                    );
                    m.insert(
                        Char(c),
                        CharDimensions {
                            width_index: WidthIndex::Valid(width_index),
                            height_index: match pl_data.height {
                                None => 0,
                                Some(height) => {
                                    // If the height data is missing from the height_to_index map, it's because
                                    // the height is 0. Similar with depths and italic corrections.
                                    height_to_index
                                        .get(&height)
                                        .copied()
                                        .map(NonZeroU8::get)
                                        .unwrap_or(0)
                                }
                            },
                            depth_index: match pl_data.depth {
                                None => 0,
                                Some(depth) => depth_to_index
                                    .get(&depth)
                                    .copied()
                                    .map(NonZeroU8::get)
                                    .unwrap_or(0),
                            },
                            italic_index: match pl_data.italic_correction {
                                None => 0,
                                Some(italic_correction) => italic_correction_to_index
                                    .get(&italic_correction)
                                    .copied()
                                    .map(NonZeroU8::get)
                                    .unwrap_or(0),
                            },
                        },
                    );
                }
                m
            }
        };
        let ordered_chars = {
            let mut m: Vec<Char> = pl_file.char_tags.keys().copied().collect();
            m.sort();
            m
        };
        let char_tags = ordered_chars.into_iter().map(|c| {
            (c,
                match pl_file.char_tags.get(&c).unwrap() {
                    pl::CharTag::Ligature(_) => {
                        let entrypoint = *lig_kern_entrypoints.get(&c).expect("the map returned by crate::ligkern::lang::compress_entrypoints has a key for all chars with a lig tag");
                        CharTag::Ligature(entrypoint)
                    }
                    pl::CharTag::List(c) => CharTag::List(*c),
                    pl::CharTag::Extension(e) => {
                        let index: u8 = extensible_chars.len().try_into().unwrap();
                        extensible_chars.push(e.clone());
                        CharTag::Extension(index)
                    }
                },
            )
        }).collect();

        let mut file = Self {
            header: pl_file.header,
            smallest_char: char_bounds.map(|t| t.0).unwrap_or(Char(1)),
            char_dimens,
            char_tags,
            unset_char_tags: pl_file.unset_char_tags.clone(),
            widths,
            heights,
            depths,
            italic_corrections,
            lig_kern_program,
            kerns,
            extensible_chars,
            params: pl_file.params,
        };
        if file.header.checksum.is_none() {
            file.header.checksum = Some(file.checksum());
        }
        file
    }
}

/// Lossy compression of numbers for TFM files.
///
/// The TFM file format can only store up to 15 heights, 15 depths and 63 italic corrections.
/// If a property list file contains, e.g., more than 15 distinct heights, something has to give.
/// PLtoTF contains a lossy compression algorithm that takes a list of values and returns another
/// bounded list of values which approximates the original list.
/// This is implemented in PLtoTF.2014.75-80 and re-implemented here.
///
/// ## How the algorithm works.
///
/// For a given delta, the algorithm partitions the ordered list of values such that within
/// each partition the maximum distance between two elements is delta. All of the values within
/// the partition are then approximated by `(interval_max+interval_min)/2`.
/// As such, each value may be increased or decreased by up to `delta/2`.
/// After this procedure, the list of values is replaced by the list of approximations.
/// This compresses the list of values into a list whose size is the number of intervals.
///
/// Given this subroutine, the algorithm finds the smallest delta such that the number of intervals
/// is less than the required maximum (e.g., 15 for heights).
///
/// There are some important features of this algorithm to note:
///
/// - For a given delta value there may be multiple possible partitions.
///     The algorithm uses a greedy approach in which it maximizes the size of the first partition,
///     then the size of the second partition, and so on.
///
/// - Distinct delta values can yield the same partition.
///     For example, if the initial values are `[1, 4, 5]` then any delta in the range `[1, 3)`
///     gives the same result (`[[1], [4, 5]]`)
///     Whenever we check a delta, we are really checking the _interval of deltas_  that gives the same result.
///     Both Knuth's and our implementations use this fact to speed up the search by reducing the search space.
///     E.g. in the example above, after checking `delta=1` we _don't_ check `delta=2`.
///
/// ## This re-implementation
///
/// The re-implementation here follows PLtoTF closely enough, but with one modification.
/// To find the optimal delta, Knuth first calculates the minimal possible delta.
/// This is the minimum distance between adjacent elements in the ordered list of values.
/// In general this will not be a valid solution because the number of intervals
///     it generates will be large.
/// So, he next finds the smallest `k` such that `2^k * min_delta` is a valid solution.
/// Te then does a upwards linear search within the interval `[min_delta * 2^{k-1}, min_delta * 2^k]` to find
///     the optimal delta.
///
/// Checking a particular delta is `O(n)`.
/// The worst-case running time of Knuth's algorithm is then `O(n^3)` because the interval
///     `[2^{k-1}, 2^k]` can contain `O(n^2)` distinct deltas to check in the worst-case [Note 1].
///
/// In the re-implementation here we realize that the problem of finding the smallest possible delta
///     is a classic binary search problem.
/// This is because if delta is a valid solution, any larger delta also is;
///     and if delta is not a valid solution, any smaller delta is also not a valid solution.
/// The re-implementation using binary search is `O(n log n)`.
/// Moreover, the constant factors are about the same.
///
/// [Note 1] In the worst-case there are O(n^2) distinct deltas because each pair of elements yields a delta.
/// Let m be the smallest delta and M the largest delta.
/// In the initial `2^k`-based ranging scheme, the largest `K` satisfies `m 2^{K-1} < M <= m 2^K`,
/// or `K-1 <= log_2(M/m)`. Thus there are `K=O(1)` ranges in this initial scheme.
/// By the pigeon-hole principle, there exists a `k` such that the range `[m * 2^{k-1}, m * 2^k]`
///     contains O(n^2) elements.
/// In the worst-case, the solution is the maximum element of this range.
pub fn compress(values: &[FixWord], max_size: u8) -> (Vec<FixWord>, HashMap<FixWord, NonZeroU8>) {
    let max_size = max_size as usize;
    let dedup_values = {
        let s: HashSet<FixWord> = values.iter().copied().collect();
        // remove the zero value for non-widths
        // and then add it back in at the start
        let mut v: Vec<FixWord> = s.into_iter().collect();
        v.sort();
        v
    };
    // After deduplication, it is possible we don't need to compress at all so we can exit early.
    // This also handles the case when the values slice is empty.
    if dedup_values.len() <= max_size {
        let m: HashMap<FixWord, NonZeroU8> = dedup_values
            .iter()
            .enumerate()
            .map(|(i, &w)| {
                let i: u8 = i.try_into().expect("`dedup_values` has at most `max_size` elements, so the index is at most `max_size-1`");
                let i: NonZeroU8 = (i+1).try_into().expect("`i<=max_size-1<=u8::MAX`, so `i+1<=u8::MAX`");
                (w, i) })
            .collect();
        let mut dedup_values = dedup_values;
        dedup_values.push(FixWord::ZERO);
        dedup_values.rotate_right(1);
        return (dedup_values, m);
    }

    // For the binary search we maintain lower and upper indices as usual.
    // The optimal delta is in the interval [lower, upper].
    //
    // Invariant: delta<lower is never a solution.
    // Because delta must be non-negative, we initialize it to zero.
    let mut lower = FixWord::ZERO;
    // Invariant: delta=upper is always solution.
    // To initialize upper and begin the search we construct a solution that always works: a single
    // interval encompassing the entire slice and the largest delta possible.
    let max_delta = *dedup_values.last().unwrap() - *dedup_values.first().unwrap();
    let mut upper = max_delta;
    let mut solution = vec![dedup_values.len()];

    let mut buffer = vec![];
    while lower < upper {
        // After the following line delta is potentially equal to lower. This is what we want as
        // we know upper is a solution so to advance the search when upper=lower+1
        // we need to check lower+1.
        let delta = lower + (upper - lower) / 2;

        let mut interval_start = *dedup_values.first().unwrap();
        // The smallest delta such that the candidate solution will be the same.
        // This is the maximum of all gaps that don't start a new interval.
        let mut delta_lower = FixWord::ZERO;
        // The largest delta such that the candidate solution will be different.
        // This is the minimum of all gaps that start a new interval.
        let mut delta_upper = max_delta;
        for (i, &v) in dedup_values.iter().enumerate() {
            let gap = v - interval_start;
            if gap > delta {
                // We need to start a new interval
                if gap < delta_upper {
                    delta_upper = gap;
                }
                buffer.push(i);
                // If the candidate solution is already too big, we can exit early.
                if buffer.len() >= max_size {
                    break;
                }
                interval_start = v;
            } else {
                // We need to extend the current interval
                // For any delta in the range [gap, delta] we would have made the same choice here.
                if gap > delta_lower {
                    delta_lower = gap;
                }
            }
        }
        buffer.push(dedup_values.len());

        if buffer.len() <= max_size {
            // solution
            std::mem::swap(&mut buffer, &mut solution);
            upper = delta_lower;
        } else {
            // not a solution
            lower = delta_upper;
        }
        buffer.clear();
    }

    let mut value_to_index = HashMap::<FixWord, NonZeroU8>::new();
    let mut result = vec![FixWord::ZERO];
    let mut previous = 0_usize;
    for i in solution {
        let interval = &dedup_values[previous..i];
        previous = i;
        for &v in interval {
            let index: u8 = result.len().try_into().expect("the `result` array contains at most `1+max_size` elements, so the index it at most `max_size` which is a u8");
            let index: NonZeroU8 = index
                .try_into()
                .expect("the `result` array contains at least 1 element so this is never 0");
            value_to_index.insert(v, index);
        }
        let replacement = (*interval.last().unwrap() + *interval.first().unwrap()) / 2;
        result.push(replacement);
    }

    (result, value_to_index)
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Section {
    /// Sub-file sizes
    SubFileSizes,
    /// Header
    Header,
    /// Character data
    CharInfos,
    /// Widths array
    Widths,
    /// Heights array
    Heights,
    /// Depths array
    Depths,
    /// Italic corrections array
    ItalicCorrections,
    /// Lig/kern instructions
    LigKern,
    /// Kerns array
    Kerns,
    /// Extensible recipes
    ExtensibleRecipes,
    /// Params array
    Params,
}

impl Section {
    pub const NAMES: [&'static str; 11] = [
        "sub-file-sizes",
        "header",
        "char-infos",
        "widths",
        "heights",
        "depths",
        "italic-corrections",
        "lig-kern",
        "kerns",
        "extensible-recipes",
        "params",
    ];
    pub const ALL_SECTIONS: [Section; 11] = [
        Section::SubFileSizes,
        Section::Header,
        Section::CharInfos,
        Section::Widths,
        Section::Heights,
        Section::Depths,
        Section::ItalicCorrections,
        Section::LigKern,
        Section::Kerns,
        Section::ExtensibleRecipes,
        Section::Params,
    ];
}

impl TryFrom<&str> for Section {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        for i in 0..Section::NAMES.len() {
            if Section::NAMES[i] == value {
                return Ok(Section::ALL_SECTIONS[i]);
            }
        }
        Err(())
    }
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Section::NAMES[*self as usize])
    }
}

/// The TFM header, which contains metadata about the file.
///
/// This is defined in TFtoPL.2014.10.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Header {
    /// The font checksum, if specified.
    ///
    /// In .tfm files checksums are always specified because the format has no
    ///     way to omit a checksum.
    ///
    /// In .pl files checksums are specified if the `CHECKSUM` node appears.
    /// If no checksum is specified in a .pl file, pltotf calculates the
    ///     correct value and writes that.
    ///
    /// In TeX82, this is stored in the `font_check` array (TeX82.2021.549).
    pub checksum: Option<u32>,
    /// In TeX82, this is stored in the `font_dsize` array (TeX82.2021.549).
    pub design_size: FixWord,
    pub design_size_valid: bool,
    pub character_coding_scheme: Option<String>,
    pub font_family: Option<String>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    /// The TFM format allows the header to contain arbitrary additional data.
    pub additional_data: Vec<u32>,
}

impl Header {
    /// Returns the default header when parsing property list files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn pl_default() -> Header {
        Header {
            checksum: None,
            design_size: FixWord::ONE * 10,
            design_size_valid: true,
            character_coding_scheme: Some("UNSPECIFIED".into()),
            font_family: Some("UNSPECIFIED".into()),
            seven_bit_safe: None,
            face: Some(0.into()),
            additional_data: vec![],
        }
    }

    /// Returns the default header when parsing .tfm files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn tfm_default() -> Header {
        Header {
            checksum: Some(0),
            design_size: FixWord::ZERO,
            design_size_valid: true,
            character_coding_scheme: None,
            font_family: None,
            seven_bit_safe: None,
            face: None,
            additional_data: vec![],
        }
    }
}

/// A character in a TFM file.
///
/// TFM and PL files only support 1-byte characters.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Char(pub u8);

impl std::fmt::Display for Char {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if (self.0 as char).is_ascii_graphic() {
            write!(f, "{}", self.0 as char)
        } else {
            write!(f, "0x{:02x}", self.0)
        }
    }
}

impl From<u8> for Char {
    fn from(value: u8) -> Self {
        Char(value)
    }
}

impl TryFrom<char> for Char {
    type Error = std::char::TryFromCharError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let u: u8 = value.try_into()?;
        Ok(Char(u))
    }
}

impl From<Char> for char {
    fn from(value: Char) -> Self {
        value.0 as char
    }
}

macro_rules! const_chars {
    ( $( ($name: ident, $value: expr), )+ ) => {
        $(
            pub const $name: Char = Char($value);
        )+
    };
}

impl Char {
    const_chars![
        (A, b'A'),
        (B, b'B'),
        (C, b'C'),
        (D, b'D'),
        (X, b'X'),
        (Y, b'Y'),
        (Z, b'Z'),
    ];

    pub fn is_seven_bit(&self) -> bool {
        self.0 <= 127
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum FaceWeight {
    Light,
    Medium,
    Bold,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum FaceSlope {
    Roman,
    Italic,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum FaceExpansion {
    Regular,
    Condensed,
    Extended,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Face {
    Valid(FaceWeight, FaceSlope, FaceExpansion),
    Other(u8),
}

impl From<u8> for Face {
    fn from(value: u8) -> Self {
        if value >= 18 {
            return Face::Other(value);
        }
        let a = match (value % 6) / 2 {
            0 => FaceWeight::Medium,
            1 => FaceWeight::Bold,
            2 => FaceWeight::Light,
            _ => unreachable!(),
        };
        let b = match value % 2 {
            0 => FaceSlope::Roman,
            1 => FaceSlope::Italic,
            _ => unreachable!(),
        };
        let c = match value / 6 {
            0 => FaceExpansion::Regular,
            1 => FaceExpansion::Condensed,
            2 => FaceExpansion::Extended,
            _ => unreachable!(),
        };
        Face::Valid(a, b, c)
    }
}

impl From<Face> for u8 {
    fn from(value: Face) -> Self {
        match value {
            Face::Valid(w, s, c) => {
                let a: u8 = match w {
                    FaceWeight::Medium => 0,
                    FaceWeight::Bold => 1,
                    FaceWeight::Light => 2,
                };
                let b: u8 = match s {
                    FaceSlope::Roman => 0,
                    FaceSlope::Italic => 1,
                };
                let c: u8 = match c {
                    FaceExpansion::Regular => 0,
                    FaceExpansion::Condensed => 1,
                    FaceExpansion::Extended => 2,
                };
                c * 6 + a * 2 + b
            }
            Face::Other(b) => b,
        }
    }
}

/// A named TeX font metric parameter.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum NamedParameter {
    Slant,
    Space,
    Stretch,
    Shrink,
    XHeight,
    Quad,
    ExtraSpace,
    Num1,
    Num2,
    Num3,
    Denom1,
    Denom2,
    Sup1,
    Sup2,
    Sup3,
    Sub1,
    Sub2,
    SupDrop,
    SubDrop,
    Delim1,
    Delim2,
    AxisHeight,
    DefaultRuleThickness,
    BigOpSpacing1,
    BigOpSpacing2,
    BigOpSpacing3,
    BigOpSpacing4,
    BigOpSpacing5,
}

impl NamedParameter {
    pub fn number(&self) -> u8 {
        match self {
            NamedParameter::Slant => 1,
            NamedParameter::Space => 2,
            NamedParameter::Stretch => 3,
            NamedParameter::Shrink => 4,
            NamedParameter::XHeight => 5,
            NamedParameter::Quad => 6,
            NamedParameter::ExtraSpace => 7,
            NamedParameter::Num1 => 8,
            NamedParameter::Num2 => 9,
            NamedParameter::Num3 => 10,
            NamedParameter::Denom1 => 11,
            NamedParameter::Denom2 => 12,
            NamedParameter::Sup1 => 13,
            NamedParameter::Sup2 => 14,
            NamedParameter::Sup3 => 15,
            NamedParameter::Sub1 => 16,
            NamedParameter::Sub2 => 17,
            NamedParameter::SupDrop => 18,
            NamedParameter::SubDrop => 19,
            NamedParameter::Delim1 => 20,
            NamedParameter::Delim2 => 21,
            NamedParameter::AxisHeight => 22,
            NamedParameter::DefaultRuleThickness => 8,
            NamedParameter::BigOpSpacing1 => 9,
            NamedParameter::BigOpSpacing2 => 10,
            NamedParameter::BigOpSpacing3 => 11,
            NamedParameter::BigOpSpacing4 => 12,
            NamedParameter::BigOpSpacing5 => 13,
        }
    }
}

/// Warning from the compilation of "next larger character" instructions.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NextLargerProgramWarning {
    NonExistentCharacter { original: Char, next_larger: Char },
    InfiniteLoop { original: Char, next_larger: Char },
}

impl NextLargerProgramWarning {
    pub fn bad_char(&self) -> Char {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original,
                next_larger: _,
            } => *original,
            NextLargerProgramWarning::InfiniteLoop { original, .. } => *original,
        }
    }

    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tftopl_message(&self) -> String {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original: _,
                next_larger,
            } => {
                format![
                    "Bad TFM file: Character list link to nonexistent character '{:03o}.",
                    next_larger.0
                ]
            }
            NextLargerProgramWarning::InfiniteLoop { original, .. } => {
                format!["Bad TFM file: Cycle in a character list!\nCharacter '{:03o} now ends the list.", original.0]
            }
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> u8 {
        84
    }

    /// Returns the section in Knuth's PLtoTF (version 2014) in which this warning occurs.
    pub fn pltotf_section(&self) -> u8 {
        match self {
            NextLargerProgramWarning::NonExistentCharacter { .. } => 111,
            NextLargerProgramWarning::InfiniteLoop { .. } => 113,
        }
    }

    /// Returns the warning message the PLtoTF program prints for this kind of error.
    pub fn pltotf_message(&self) -> String {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original,
                next_larger: _,
            } => {
                format![
                    "The character NEXTLARGER than '{:03o} had no CHARACTER spec.",
                    original.0
                ]
            }
            NextLargerProgramWarning::InfiniteLoop { original, .. } => {
                format![
                    "A cycle of NEXTLARGER characters has been broken at '{:03o}.",
                    original.0
                ]
            }
        }
    }
}
/// Compiled program of "next larger character" instructions
///
/// The .tfm file format can associate a "next larger" character to any character in a font.
/// Next larger characters form sequences: i.e. B can be the next larger character for A,
///     and C can be the next larger character for B,
///     leading to the sequences A-B-C.
/// These next larger characters are used at certain points in TeX.
/// TeX occasionally traverses the entire sequence for a given starting character (e.g. A).
///
/// As with ligatures, next larger specifications can contain infinite loops -
///     e.g, if X is the next larger character for Y
///      and Y is the next larger character for X.
/// These loops are invalid and removed by TFtoPL and PLtoTF.
///
/// Motivated by the idea of "parse don't validate", this type represents
///     a compiled version of the next larger specifications in which infinite loops
///     are statically guaranteed not to exist.
///
/// The basic use of a valid program looks like this:
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::A, Char::B),
///     (Char::B, Char::C),
/// ];
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
///
/// assert_eq![warnings, vec![]];
///
/// let sequence_A: Vec<Char> = next_larger_program.get(Char::A).collect();
/// assert_eq!(sequence_A, vec![Char::B, Char::C]);
///
/// let sequence_B: Vec<Char> = next_larger_program.get(Char::B).collect();
/// assert_eq!(sequence_B, vec![Char::C]);
///
/// let sequence_C: Vec<Char> = next_larger_program.get(Char::C).collect();
/// assert_eq!(sequence_C, vec![]);
///
/// // Character that is not in the program.
/// let sequence_D: Vec<Char> = next_larger_program.get(Char::D).collect();
/// assert_eq!(sequence_D, vec![]);
/// ```
///
/// ## Warnings
///
/// There are two types of error that can occur when constructing the next
///     larger program.
/// Both of these errors are handled gracefully, so we officially refer to them as warnings.
/// The constructor returns them as values of type [`NextLargerProgramWarning`].
///
/// ### Infinite loops
///
/// The first error is that next larger programs can contain infinite loops -
///     e.g, if X is the next larger character for Y
///      and Y is the next larger character for X.
/// In this case the loop is broken by removing the next larger program for the
///     character with the largest 8-bit code, in this case Y.
/// A [`NextLargerProgramWarning::InfiniteLoop`] warning is returned
///     from the program constructor.
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
///     (Char::Y, Char::X),
/// ];
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::InfiniteLoop{
///     original: Char::Y,
///     next_larger: Char::X,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![Char::Y]);
///
/// let sequence_Y: Vec<Char> = next_larger_program.get(Char::Y).collect();
/// assert_eq!(sequence_Y, vec![]);
/// ```
///
/// ### Non-existent characters
///
/// The second error is that characters referred to in the next larger program
///     may not be defined in the .tfm or .pl file.
/// For example, a .pl file may contain the snippet `(CHARACTER C X (NEXTLARGER C Y))`
///     without defining the character Y.
/// The constructor [`NextLargerProgram::new`] accepts a function for checking if a
///     character exists.
/// In all cases a [`NextLargerProgramWarning::NonExistentCharacter`] warning is returned
///     if a non-existent character is encountered.
///
/// The behavior of the resulting next larger program is configured using the
///     `drop_non_existent_characters` argument.
/// If this is false, then the behavior is the same as PLtoTF and the program still
///     contains the character.
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
/// ];
/// let character_exists = |c| {
///     if c == Char::Y {
///         false
///     } else {
///         true
///     }
/// };
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), character_exists, false);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::NonExistentCharacter{
///     original: Char::X,
///     next_larger: Char::Y,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![Char::Y]);
/// ```
///
/// If `drop_non_existent_characters` is true, next larger instructions pointing at non-existent
///     characters are dropped.
/// This is how TFtoPL behaves.
///
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
/// ];
/// let character_exists = |c| {
///     if c == Char::Y {
///         false
///     } else {
///         true
///     }
/// };
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), character_exists, true);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::NonExistentCharacter{
///     original: Char::X,
///     next_larger: Char::Y,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![]);
/// ```
///
#[derive(Clone, Debug)]
pub struct NextLargerProgram {
    entrypoints: HashMap<Char, u8>,
    next_larger: Vec<(Char, NonZeroU8)>,
}

impl NextLargerProgram {
    /// Build a new next larger program from an iterator over edges.
    pub fn new<I: Iterator<Item = (Char, Char)>, F: Fn(Char) -> bool>(
        edges: I,
        character_exists: F,
        drop_non_existent_characters: bool,
    ) -> (Self, Vec<NextLargerProgramWarning>) {
        // This function implements functionality in TFtoPL.2014.84 and PLtoTF.2014.{110,111,113}.
        let mut warnings: Vec<NextLargerProgramWarning> = vec![];

        let mut node_to_larger = HashMap::<Char, Char>::new();
        let mut node_to_num_smaller = HashMap::<Char, usize>::new();
        for (smaller, larger) in edges {
            if !character_exists(larger) {
                warnings.push(NextLargerProgramWarning::NonExistentCharacter {
                    original: smaller,
                    next_larger: larger,
                });
                if drop_non_existent_characters {
                    continue;
                }
            }
            node_to_larger.insert(smaller, larger);
            node_to_num_smaller.entry(smaller).or_default();
            *node_to_num_smaller.entry(larger).or_default() += 1;
        }

        let mut leaves: Vec<Char> = vec![];
        let mut non_leaves: BTreeSet<Char> = Default::default();
        for (node, num_smaller) in &node_to_num_smaller {
            if *num_smaller == 0 {
                leaves.push(*node);
            } else {
                non_leaves.insert(*node);
            }
        }

        let mut sorted_chars = vec![];
        let mut infinite_loop_warnings = vec![];
        loop {
            while let Some(smaller) = leaves.pop() {
                if let Some(larger) = node_to_larger.get(&smaller).copied() {
                    let num_smaller = node_to_num_smaller
                        .get_mut(&larger)
                        .expect("`node_to_num_smaller` contains all nodes");
                    *num_smaller = num_smaller
                        .checked_sub(1)
                        .expect("the larger of a smaller must have at least one smaller");
                    if *num_smaller == 0 {
                        leaves.push(larger);
                        non_leaves.remove(&larger);
                    }
                }
                sorted_chars.push(smaller);
            }

            // There could be pending nodes left because of a cycle.
            // We break the cycle at the largest node.
            let smaller = match non_leaves.last() {
                None => break,
                Some(child) => *child,
            };
            let larger = node_to_larger.remove(&smaller).expect(
                "General graph fact: sum_(node)#in_edges(node)=sum_(node)#out_edges(node).
                Fact about the next larger graph: #out_edges(node)<=1, because each char has at most one next larger char.
                If #out_edge(child)=0 then sum_(node)#in_edges(node)=sum_(node)#out_edges(node) < #nodes.
                Thus there exists another node with #in_edges(node)=0 and that node is a leaf.
                But `leaves.len()=0` at this line of code",
            );
            infinite_loop_warnings.push(NextLargerProgramWarning::InfiniteLoop {
                original: smaller,
                next_larger: larger,
            });
            leaves.push(larger);
            non_leaves.remove(&larger);
        }
        warnings.extend(infinite_loop_warnings.into_iter().rev());

        let next_larger = {
            let parents: HashSet<Char> = node_to_larger.values().copied().collect();
            let mut node_to_position = HashMap::<Char, u8>::new();
            let mut next_larger: Vec<(Char, NonZeroU8)> = vec![];

            for c in sorted_chars.iter().rev() {
                if !parents.contains(c) {
                    continue;
                }
                // The present character is the parent of at least one child, aka it is the next larger
                // character for another character. So it needs to be in the next_larger array.
                let child_position: u8 = next_larger
                    .len()
                    .try_into()
                    .expect("there are at most u8::MAX chars in the `next_larger` array");
                let offset = match node_to_larger.get(c) {
                    // The next_larger array contains at most 256 elements: one for each char.
                    // (Actually it contains at most 255 because one character necessarily does not
                    // have a child node and this character does not appear in the array.)
                    // Anyway, an offset of 256 sends the index outside the array bound, and so
                    // subsequent calls to iterator return None.
                    None => NonZeroU8::MAX,
                    Some(parent) => {
                        let parent_position = *node_to_position
                            .get(parent)
                            .expect("parent has already been inserted");
                        child_position
                            .checked_sub(parent_position)
                            .expect("parent inserted before so its position it is strictly smaller")
                            .try_into()
                            .expect("parent inserted before so its position it is strictly smaller")
                    }
                };
                next_larger.push((*c, offset));
                node_to_position.insert(*c, child_position);
            }
            next_larger.reverse();
            next_larger
        };

        let entrypoints = {
            let node_to_position: HashMap<Char, u8> = next_larger
                .iter()
                .enumerate()
                .map(|(i, (c, _))| {
                    let u: u8 = i
                        .try_into()
                        .expect("there are at most u8::MAX chars in the `next_larger` array");
                    (*c, u)
                })
                .collect();
            let mut entrypoints = HashMap::<Char, u8>::new();
            for c in sorted_chars.iter().rev() {
                if let Some(parent) = node_to_larger.get(c) {
                    entrypoints.insert(
                        *c,
                        *node_to_position
                            .get(parent)
                            .expect("parent has already been inserted"),
                    );
                }
            }
            entrypoints
        };

        (
            Self {
                entrypoints,
                next_larger,
            },
            warnings,
        )
    }

    /// Get the next larger sequence for a character
    pub fn get(&self, c: Char) -> impl Iterator<Item = Char> + '_ {
        NextLargerProgramIter {
            current: self.entrypoints.get(&c).copied(),
            program: self,
        }
    }

    /// Returns whether this program is seven-bit safe.
    ///
    /// A next larger program is seven-bit safe if the next larger sequences for
    ///     seven-bit characters only contain seven-bit characters.
    /// Conversely a program is seven-bit unsafe if there is a seven-bit
    ///     character whose next larger sequence contains a non-seven-bit character.
    ///
    /// ```
    /// # use tfm::*;
    /// let edges = vec![
    ///     (Char(250), Char(125)),
    ///     (Char(125), Char(126)),
    /// ];
    /// let (next_larger_program, _) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
    /// assert_eq!(true, next_larger_program.is_seven_bit_safe());
    ///
    /// let edges = vec![
    ///     (Char(125), Char(250)),
    /// ];
    /// let (next_larger_program, _) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
    /// assert_eq!(false, next_larger_program.is_seven_bit_safe());
    /// ```
    pub fn is_seven_bit_safe(&self) -> bool {
        // For each c, we only need to check the first element in c's next larger sequence.
        // If there is a subsequent element d of the sequence that is seven-bit unsafe,
        // we will find it when considering one of d's children.
        // This optimization makes this function O(n), rather than worst case O(n^2).
        self.entrypoints
            .keys()
            .copied()
            .filter(Char::is_seven_bit)
            .filter_map(|c| self.get(c).next())
            .all(|c| c.is_seven_bit())
    }
}

#[derive(Clone, Debug)]
struct NextLargerProgramIter<'a> {
    current: Option<u8>,
    program: &'a NextLargerProgram,
}

impl<'a> Iterator for NextLargerProgramIter<'a> {
    type Item = Char;

    fn next(&mut self) -> Option<Self::Item> {
        // Note that the iterator is statically guaranteed to terminate!
        // If it returns None, it has already terminated.
        // If it returns Some, then self.current will be incremented by a strictly
        //  positive number.
        // Incrementing like this can only happen a finite number of times before overflow
        //  occurs, and then self.current is None and the iterator is terminated.
        match self.current {
            None => None,
            Some(current) => match self.program.next_larger.get(current as usize) {
                None => None,
                Some((c, inc)) => {
                    self.current = current.checked_add(inc.get());
                    Some(*c)
                }
            },
        }
    }
}

impl core::FontFormat for File {
    const DEFAULT_FILE_EXTENSION: &'static str = "tfm";
    type Error = DeserializationError;

    fn parse(b: &[u8]) -> Result<Self, Self::Error> {
        File::deserialize(b).0
    }
}

/// Fixed-width numeric type used in TFM files.
///
/// This numeric type has 11 bits for the integer part,
/// 20 bits for the fractional part, and a single signed bit.
/// The inner value is the number multiplied by 2^20.
/// It is called a `fix_word` in TFtoPL.
///
/// In property list files, this type is represented as a decimal number
///   with up to 6 digits after the decimal point.
/// This is a non-lossy representation
///   because 10^(-6) is larger than 2^(-20).
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct FixWord(pub i32);

impl FixWord {
    /// Representation of the number 0 as a [FixWord].
    pub const ZERO: FixWord = FixWord(0);

    /// Representation of the number 1 as a [FixWord].
    pub const ONE: FixWord = FixWord(1 << 20);

    /// Returns true if the number is less than 16.0 in magnitude according to Knuth.
    ///
    /// The number +16.0 is not allowed.
    /// This is covered in the E2E tests.
    /// See `check_fix` in TFtoPL.2014.60.
    pub fn is_abs_less_than_16(&self) -> bool {
        *self >= FixWord::ONE * -16 && *self < FixWord::ONE * 16
    }
}

impl std::fmt::Display for FixWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TFtoPL.2014.40-43
        if self.0 < 0 {
            write!(f, "-")?;
        }
        let integer_part = (self.0 / FixWord::ONE.0).abs();
        write!(f, "{integer_part}.")?;
        let mut fp = (self.0 % FixWord::ONE.0).abs();
        fp = 10 * fp + 5;
        let mut delta = 10;
        loop {
            if delta > 0o4_000_000 {
                fp = fp + 0o2_000_000 - delta / 2;
            }
            write!(f, "{}", fp / 0o4_000_000)?;
            fp = 10 * (fp % 0o4_000_000);
            delta *= 10;
            if fp <= delta {
                break;
            }
        }
        Ok(())
    }
}

impl std::ops::Add<FixWord> for FixWord {
    type Output = FixWord;
    fn add(self, rhs: FixWord) -> Self::Output {
        FixWord(self.0 + rhs.0)
    }
}
impl std::ops::Sub<FixWord> for FixWord {
    type Output = FixWord;
    fn sub(self, rhs: FixWord) -> Self::Output {
        FixWord(self.0 - rhs.0)
    }
}

impl std::ops::Mul<i32> for FixWord {
    type Output = FixWord;

    fn mul(self, rhs: i32) -> Self::Output {
        FixWord(self.0 * rhs)
    }
}

impl std::ops::Div<i32> for FixWord {
    type Output = FixWord;

    fn div(self, rhs: i32) -> Self::Output {
        FixWord(self.0 / rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_compress_test(
        values: Vec<FixWord>,
        max_size: u8,
        want: Vec<FixWord>,
        want_map: Vec<u8>,
    ) {
        let (got, got_map) = compress(&values, max_size);
        assert_eq!(got, want);
        let want_map: HashMap<FixWord, NonZeroU8> = want_map
            .into_iter()
            .enumerate()
            .map(|(i, t)| (values[i], t.try_into().unwrap()))
            .collect();
        assert_eq!(got_map, want_map);
    }

    macro_rules! compress_tests {
        ( $( ($name: ident, $values: expr, $max_size: expr, $want: expr, $want_map: expr, ), )+ ) => {
            $(
                #[test]
                fn $name () {
                    let values = $values;
                    let max_size = $max_size;
                    let want = $want;
                    let want_map = $want_map;
                    run_compress_test(values, max_size, want, want_map);
                }
            )+
        };
    }

    compress_tests!(
        (no_op_0, vec![], 1, vec![FixWord(0)], vec![],),
        (
            no_op_2,
            vec![FixWord::ONE * 2, FixWord::ONE],
            2,
            vec![FixWord(0), FixWord::ONE, FixWord::ONE * 2],
            vec![2, 1],
        ),
        (
            just_deduplication,
            vec![FixWord::ONE, FixWord::ONE],
            1,
            vec![FixWord(0), FixWord::ONE],
            vec![1, 1],
        ),
        (
            simple_compression_case,
            vec![FixWord::ONE, FixWord::ONE * 2],
            1,
            vec![FixWord(0), FixWord::ONE * 3 / 2],
            vec![1, 1],
        ),
        (
            simple_compression_case_2,
            vec![
                FixWord::ONE,
                FixWord::ONE * 2,
                FixWord::ONE * 200,
                FixWord::ONE * 201
            ],
            2,
            vec![FixWord(0), FixWord::ONE * 3 / 2, FixWord::ONE * 401 / 2],
            vec![1, 1, 2, 2],
        ),
        (
            lower_upper_close_edge_case_1,
            vec![FixWord(1), FixWord(3)],
            1,
            vec![FixWord(0), FixWord(2)],
            vec![1, 1],
        ),
        (
            lower_upper_close_edge_case_2,
            vec![FixWord(0), FixWord(2)],
            1,
            vec![FixWord(0), FixWord(1)],
            vec![1, 1],
        ),
        (
            lower_upper_close_edge_case_3,
            vec![FixWord(1), FixWord(4)],
            1,
            vec![FixWord(0), FixWord(2)],
            vec![1, 1],
        ),
        (
            lower_upper_close_edge_case_4,
            vec![FixWord(1), FixWord(2)],
            1,
            vec![FixWord(0), FixWord(1)],
            vec![1, 1],
        ),
    );

    mod next_larger_tests {
        use super::*;

        fn run(
            edges: Vec<(Char, Char)>,
            want_sequences: HashMap<Char, Vec<Char>>,
            want_warnings: Vec<NextLargerProgramWarning>,
        ) {
            let (program, got_warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
            assert_eq!(got_warnings, want_warnings);
            for u in 0..=u8::MAX {
                let want_sequence = want_sequences
                    .get(&Char(u))
                    .map(Vec::as_slice)
                    .unwrap_or_default();
                let got_sequence: Vec<Char> = program.get(Char(u)).collect();
                assert_eq!(
                    got_sequence, want_sequence,
                    "got/want sequences for {u} do not match"
                );
            }
        }

        fn big_infinite_loop_edges() -> Vec<(Char, Char)> {
            (0..=u8::MAX)
                .into_iter()
                .map(|u| (Char(u), Char(u.wrapping_add(1))))
                .collect()
        }

        fn big_infinite_loop_sequences() -> HashMap<Char, Vec<Char>> {
            (0..=u8::MAX)
                .into_iter()
                .map(|u| {
                    let v: Vec<Char> = match u.checked_add(1) {
                        None => vec![],
                        Some(w) => (w..=u8::MAX).into_iter().map(Char).collect(),
                    };
                    (Char(u), v)
                })
                .collect()
        }

        macro_rules! next_larger_tests {
            ( $( ($name: ident, $edges: expr, $want_sequences: expr, $want_warnings: expr, ), )+ ) => {
                $(
                    #[test]
                    fn $name () {
                        run($edges, $want_sequences, $want_warnings);
                    }
                )+
            };
        }

        next_larger_tests!(
            (
                same_node_loop,
                vec![(Char::A, Char::A)],
                HashMap::from([(Char::A, vec![])]),
                vec![NextLargerProgramWarning::InfiniteLoop {
                    original: Char::A,
                    next_larger: Char::A,
                }],
            ),
            (
                two_loops,
                vec![
                    (Char::A, Char::B),
                    (Char::B, Char::C),
                    (Char::C, Char::B),
                    (Char::X, Char::Y),
                    (Char::Y, Char::Z),
                    (Char::Z, Char::X),
                ],
                HashMap::from([
                    (Char::A, vec![Char::B, Char::C]),
                    (Char::B, vec![Char::C]),
                    (Char::C, vec![]),
                    (Char::X, vec![Char::Y, Char::Z]),
                    (Char::Y, vec![Char::Z]),
                    (Char::Z, vec![]),
                ]),
                vec![
                    NextLargerProgramWarning::InfiniteLoop {
                        original: Char::C,
                        next_larger: Char::B,
                    },
                    NextLargerProgramWarning::InfiniteLoop {
                        original: Char::Z,
                        next_larger: Char::X,
                    },
                ],
            ),
            (
                path_leading_to_loop,
                vec![(Char::A, Char::B), (Char::B, Char::C), (Char::C, Char::B),],
                HashMap::from([
                    (Char::A, vec![Char::B, Char::C]),
                    (Char::B, vec![Char::C]),
                    (Char::C, vec![]),
                ]),
                vec![NextLargerProgramWarning::InfiniteLoop {
                    original: Char::C,
                    next_larger: Char::B,
                }],
            ),
            (
                big_infinite_loop,
                big_infinite_loop_edges(),
                big_infinite_loop_sequences(),
                vec![NextLargerProgramWarning::InfiniteLoop {
                    original: Char(u8::MAX),
                    next_larger: Char(0),
                }],
            ),
        );
    }
}
