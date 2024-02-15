//! The TeX font metric (.tfm) file format
use std::collections::HashMap;
use std::collections::HashSet;
use std::num::NonZeroU8;

mod deserialize;
mod serialize;

use super::*;

pub use deserialize::Error as DeserializeError;
pub use deserialize::RawFile;
pub use deserialize::SubFileSizes;
pub use deserialize::Warning as DeserializeWarning;

/// Complete contents of a TeX font metric (.tfm) file.
///
/// The struct contain multiple vectors.
/// In TeX and TFtoPL there is an optimization in which all of data in the vectors
/// is stored in one large vector of 32-bit integers.
/// The conversion from [u32] to the specific types like [Number] are then done when the
/// data is needed.
/// This makes the memory footprint of this type much more compact,
///     and such a change may be considered in the future.
///
/// In fact in TeX the font data for all fonts is stored in one contiguous piece of memory
///     (`font_info`, defined in TeX82.2021.549).
/// This is a little too unsafe to pull off though.
#[derive(Debug, PartialEq, Eq)]
pub struct File {
    /// Header.
    pub header: Header,

    /// Character infos.
    ///
    /// The char infos mostly contain indices for other vectors in this struct.
    pub char_infos: HashMap<Char, CharInfo>,

    /// Character widths
    pub widths: Vec<Number>,

    /// Character heights
    pub heights: Vec<Number>,

    /// Character depths
    pub depths: Vec<Number>,

    /// Character italic corrections
    pub italic_corrections: Vec<Number>,

    /// Lig kern program.
    pub lig_kern_program: ligkern::lang::Program,

    /// Kerns. These are referenced from inside the lig kern commands.
    pub kerns: Vec<Number>,

    /// Extensible characters.
    pub extensible_chars: Vec<ExtensibleRecipe>,

    /// Font parameters.
    pub params: Params,
}

/// Data about one character in a .tfm file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharInfo {
    /// Index of the width of this character in the widths array.
    ///
    /// In TFM files, if the width index is zero it means there is no data for the character in the file.
    /// In this case the other indices are necessarily zero and the tag is [CharTag::None].
    /// See TFtoPL.2014.? (where blocks of data with a zero width index are skipped)
    ///     and PLtoTF.2014.? (where missing characters are written with all indices 0)
    ///
    /// In this crate we represent the TFM data for each character as [`Option<CharInfo>`].
    /// If the width index is zero, the data is [None].
    /// Otherwise the data is a [CharInfo] value with an index that is statically guaranteed to be non-zero.
    /// This makes the illegal state of a zero width index and non-zero height index un-representable.
    pub width_index: NonZeroU8,
    /// Index of the height of this character in the height array.
    pub height_index: u8,
    pub depth_index: u8,
    pub italic_index: u8,
    pub tag: CharTag,
}

/// Tag of a character in a .tfm file.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum CharTag {
    #[default]
    None,
    Ligature(u8),
    List(Char),
    Extension(u8),
}

/// Extensible recipe instruction in a .tfm file.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ExtensibleRecipe {
    pub top: Option<Char>,
    pub middle: Option<Char>,
    pub bottom: Option<Char>,
    pub rep: Char,
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Default::default(),
            char_infos: Default::default(),
            widths: vec![Number::ZERO],
            heights: vec![Number::ZERO],
            depths: vec![Number::ZERO],
            italic_corrections: vec![Number::ZERO],
            lig_kern_program: Default::default(),
            kerns: vec![],
            extensible_chars: vec![],
            params: Default::default(),
        }
    }
}

impl File {
    pub fn deserialize(b: &[u8]) -> (Result<File, DeserializeError>, Vec<DeserializeWarning>) {
        deserialize::deserialize(b)
    }

    pub fn from_raw_file(raw_file: &RawFile, warnings: &mut Vec<DeserializeWarning>) -> Self {
        deserialize::from_raw_file(raw_file, warnings)
    }

    pub fn serialize(&self) -> Vec<u8> {
        serialize::serialize(self)
    }

    /// Return a map from characters to the lig/kern entrypoint for that character.
    pub fn lig_kern_entrypoints(&self) -> HashMap<Char, u8> {
        self.char_infos
            .iter()
            .filter_map(|(c, d)| match d.tag {
                CharTag::Ligature(l) => Some((*c, l)),
                _ => None,
            })
            .collect()
    }

    fn char_info_bounds(&self) -> Option<(Char, Char)> {
        let mut r: Option<(Char, Char)> = None;
        for c in self.char_infos.keys().copied() {
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
        // This checksum algorithm is in PLtoTF.2014.13.
        let (bc, ec) = self.char_info_bounds().unwrap_or((Char(1), Char(0)));
        let mut b = [bc.0, ec.0, bc.0, ec.0];
        for c in bc.0..=ec.0 {
            let char_info = match self.char_infos.get(&Char(c)) {
                None => continue,
                Some(char_info) => char_info,
            };
            let width = self.widths[char_info.width_index.get() as usize].0;
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

    pub fn from_pl_file(pl_file: &crate::pl::File) -> Self {
        let mut char_bounds: Option<(Char, Char)> = None;

        let mut lig_kern_program = pl_file.lig_kern_program.clone();
        let kerns = lig_kern_program.unpack_kerns();
        let lig_kern_entrypoints =
            lig_kern_program.pack_entrypoints(pl_file.lig_kern_entrypoints());

        let mut widths = vec![];
        let mut heights = vec![];
        let mut depths = vec![];
        let mut italic_corrections = vec![];
        for (char, char_data) in &pl_file.char_data {
            widths.push(char_data.width);
            match char_data.height {
                None | Some(Number::ZERO) => {}
                Some(height) => heights.push(height),
            }
            match char_data.depth {
                None | Some(Number::ZERO) => {}
                Some(depth) => depths.push(depth),
            }
            match char_data.italic_correction {
                None | Some(Number::ZERO) => {}
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
        let char_infos = match char_bounds {
            None => Default::default(),
            Some((lower, upper)) => {
                let mut m: HashMap<Char, CharInfo> = Default::default();
                for c in lower.0..=upper.0 {
                    let pl_data = match pl_file.char_data.get(&Char(c)) {
                        Some(pl_data) => pl_data,
                        None => continue,
                    };
                    let width_index = *width_to_index.get(&pl_data.width).expect(
                        "the map returned from compress(_,_) contains every input as a key",
                    );
                    m.insert(Char(c), CharInfo {
                                width_index,
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
                                    Some(depth) => {
                                        depth_to_index
                                        .get(&depth)
                                        .copied()
                                        .map(NonZeroU8::get)
                                        .unwrap_or(0)
                                    }
                                },
                                italic_index: match pl_data.italic_correction {
                                    None => 0,
                                    Some(italic_correction) => {
                                        italic_correction_to_index
                                        .get(&italic_correction)
                                        .copied()
                                        .map(NonZeroU8::get)
                                        .unwrap_or(0)
                                    }
                                },
                                tag: match &pl_data.tag {
                                    pl::CharTag::None => CharTag::None,
                                    pl::CharTag::Ligature(_) => {
                                        let entrypoint = *lig_kern_entrypoints.get(&Char(c)).expect("the map returned by crate::ligkern::lang::compress_entrypoints has a key for all chars with a lig tag");
                                        CharTag::Ligature(entrypoint)
                                    }
                                    pl::CharTag::List(c) => CharTag::List(*c),
                                    pl::CharTag::Extension(e) => {
                                        let index: u8 = extensible_chars.len().try_into().unwrap();
                                        extensible_chars.push(e.clone());
                                        CharTag::Extension(index)
                                    }
                                },
                            });
                }
                m
            }
        };

        let mut file = Self {
            header: pl_file.header.clone(),
            char_infos,
            widths,
            heights,
            depths,
            italic_corrections,
            lig_kern_program,
            kerns,
            extensible_chars,
            params: pl_file.params.clone(),
        };
        if file.header.checksum == 0 {
            file.header.checksum = file.checksum();
        }
        file.header.seven_bit_safe = Some(true); // TODO: calculate this
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
///     `[2^{k-1}, 2^k]` can contain `O(n^2)` distinct deltas to check in the worst-case.*
///
/// In the re-implementation here we realize that the problem of finding the smallest possible delta
///     is a classic binary search problem.
/// This is because if delta is a valid solution, any larger delta also is;
///     and if delta is not a valid solution, any smaller delta is also not a valid solution.
/// The re-implementation using binary search is `O(n log n)`.
/// Moreover, the constant factors are about the same.
///
/// * In the worst-case there are O(n^2) distinct deltas because each pair of elements yields a delta.
/// Let m be the smallest delta and M the largest delta.
/// In the initial `2^k`-based ranging scheme, the largest `K` satisfies `m 2^{K-1} < M <= m 2^K`,
/// or `K-1 <= log_2(M/m)`. Thus there are `K=O(1)` ranges in this initial scheme.
/// By the pigeon-hole principle, there exists a `k` such that the range `[m * 2^{k-1}, m * 2^k]`
///     contains O(n^2) elements.
/// In the worst-case, the solution is the maximum element of this range.
pub fn compress(values: &[Number], max_size: u8) -> (Vec<Number>, HashMap<Number, NonZeroU8>) {
    let max_size = max_size as usize;
    let dedup_values = {
        let s: HashSet<Number> = values.iter().copied().collect();
        // remove the zero value for non-widths
        // and then add it back in at the start
        let mut v: Vec<Number> = s.into_iter().collect();
        v.sort();
        v
    };
    // After deduplication, it is possible we don't need to compress at all so we can exit early.
    // This also handles the case when the values slice is empty.
    if dedup_values.len() <= max_size {
        let m: HashMap<Number, NonZeroU8> = dedup_values
            .iter()
            .enumerate()
            .map(|(i, &w)| {
                let i: u8 = i.try_into().expect("`dedup_values` has at most `max_size` elements, so the index is at most `max_size-1`");
                let i: NonZeroU8 = (i+1).try_into().expect("`i<=max_size-1<=u8::MAX`, so `i+1<=u8::MAX`");
                (w, i) })
            .collect();
        let mut dedup_values = dedup_values;
        dedup_values.push(Number::ZERO);
        dedup_values.rotate_right(1);
        return (dedup_values, m);
    }

    // For the binary search we maintain lower and upper indices as usual.
    // The optimal delta is in the interval [lower, upper].
    //
    // Invariant: delta<lower is never a solution.
    // Because delta must be non-negative, we initialize it to zero.
    let mut lower = Number::ZERO;
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
        let mut delta_lower = Number::ZERO;
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

    let mut value_to_index = HashMap::<Number, NonZeroU8>::new();
    let mut result = vec![Number::ZERO];
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

#[cfg(test)]
mod tests {
    use super::*;

    fn run_compress_test(values: Vec<Number>, max_size: u8, want: Vec<Number>, want_map: Vec<u8>) {
        let (got, got_map) = compress(&values, max_size);
        assert_eq!(got, want);
        let want_map: HashMap<Number, NonZeroU8> = want_map
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
        (no_op_0, vec![], 1, vec![Number(0)], vec![],),
        (
            no_op_2,
            vec![Number::UNITY * 2, Number::UNITY],
            2,
            vec![Number(0), Number::UNITY, Number::UNITY * 2],
            vec![2, 1],
        ),
        (
            just_deduplication,
            vec![Number::UNITY, Number::UNITY],
            1,
            vec![Number(0), Number::UNITY],
            vec![1, 1],
        ),
        (
            simple_compression_case,
            vec![Number::UNITY, Number::UNITY * 2],
            1,
            vec![Number(0), Number::UNITY * 3 / 2],
            vec![1, 1],
        ),
        (
            simple_compression_case_2,
            vec![
                Number::UNITY,
                Number::UNITY * 2,
                Number::UNITY * 200,
                Number::UNITY * 201
            ],
            2,
            vec![Number(0), Number::UNITY * 3 / 2, Number::UNITY * 401 / 2],
            vec![1, 1, 2, 2],
        ),
        (
            lower_upper_close_edge_case_1,
            vec![Number(1), Number(3)],
            1,
            vec![Number(0), Number(2)],
            vec![1, 1],
        ),
        (
            lower_upper_close_edge_case_2,
            vec![Number(0), Number(2)],
            1,
            vec![Number(0), Number(1)],
            vec![1, 1],
        ),
        (
            lower_upper_close_edge_case_3,
            vec![Number(1), Number(4)],
            1,
            vec![Number(0), Number(2)],
            vec![1, 1],
        ),
        (
            lower_upper_close_edge_case_4,
            vec![Number(1), Number(2)],
            1,
            vec![Number(0), Number(1)],
            vec![1, 1],
        ),
    );
}
