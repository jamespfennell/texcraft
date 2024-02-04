use std::collections::HashMap;
use std::collections::HashSet;

use super::*;
use crate::Number;

pub fn serialize(file: &File) -> Vec<u8> {
    let mut b = vec![0_u8; 24];

    let lh = serialize_section(&file.header, &mut b);

    let (bc, ec) = if file.char_infos.is_empty() {
        (1, 0)
    } else {
        let bc = file.smallest_char_code.0 as u16;
        let diff = serialize_section(&file.char_infos, &mut b);
        (bc, bc + diff - 1)
    };

    let nw = serialize_section(&file.widths, &mut b);

    let nh = serialize_section(&file.heights, &mut b);

    let nd = serialize_section(&file.depths, &mut b);

    let ni = serialize_section(&file.italic_corrections, &mut b);

    let nl = serialize_section(&file.lig_kern_instructions, &mut b);
    let nk = serialize_section(&file.kern, &mut b);
    let ne = serialize_section(&file.extensible_chars, &mut b);

    let np = serialize_section(&file.params, &mut b);

    let lf = 6 + lh + (ec + 1 - bc) + nw + nh + nd + ni + nl + nk + ne + np;
    for (u, v) in [lf, lh, bc, ec, nw, nh, nd, ni, nl, nk, ne, np]
        .into_iter()
        .enumerate()
    {
        [b[2 * u], b[2 * u + 1]] = v.to_be_bytes();
    }
    b
}

fn serialize_section<T: Serializable>(t: &T, b: &mut Vec<u8>) -> u16 {
    let start = b.len();
    t.serialize(b);
    ((b.len() - start) / 4).try_into().unwrap()
}

trait Serializable: Sized {
    fn serialize(&self, b: &mut Vec<u8>);
}

impl Serializable for u32 {
    fn serialize(&self, b: &mut Vec<u8>) {
        b.extend(self.to_be_bytes())
    }
}

impl Serializable for CharInfo {
    fn serialize(&self, b: &mut Vec<u8>) {
        b.push(self.width_index);
        b.push(
            self.height_index
                .wrapping_mul(16)
                .wrapping_add(self.depth_index),
        );
        let (discriminant, payload) = match self.tag {
            CharTag::None => (0_u8, 0_u8),
            CharTag::Ligature(p) => (1, p),
            CharTag::List(p) => (2, p.0),
            CharTag::Extension(p) => (3, p),
        };
        b.push(self.italic_index.wrapping_mul(4).wrapping_add(discriminant));
        b.push(payload);
    }
}

impl<T: Serializable> Serializable for Vec<T> {
    fn serialize(&self, b: &mut Vec<u8>) {
        for element in self {
            element.serialize(b)
        }
    }
}

impl Serializable for Number {
    fn serialize(&self, b: &mut Vec<u8>) {
        (self.0 as u32).serialize(b)
    }
}

impl Serializable for ligkern::lang::Instruction {
    fn serialize(&self, b: &mut Vec<u8>) {
        b.push(self.next_instruction.unwrap_or(128));
        b.push(self.right_char.0);
        match self.operation {
            ligkern::lang::Operation::Kern(Number(index)) => {
                let index: u16 = index.try_into().unwrap();
                let [hi, lo] = index.to_be_bytes();
                b.push(hi + 128);
                b.push(lo);
            }
            ligkern::lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
            } => {
                use ligkern::lang::PostLigOperation::*;
                b.push(match post_lig_operation {
                    RetainBothMoveNowhere => 3,
                    RetainBothMoveToInserted => 3 + 4,
                    RetainBothMoveToRight => 3 + 8,
                    RetainRightMoveToInserted => 1,
                    RetainRightMoveToRight => 1 + 4,
                    RetainLeftMoveNowhere => 2,
                    RetainLeftMoveToInserted => 2 + 4,
                    RetainNeitherMoveToInserted => 0,
                });
                b.push(char_to_insert.0);
            }
        }
    }
}

impl Serializable for ExtensibleRecipe {
    fn serialize(&self, b: &mut Vec<u8>) {
        b.push(self.top.unwrap_or(Char(0)).0);
        b.push(self.middle.unwrap_or(Char(0)).0);
        b.push(self.bottom.unwrap_or(Char(0)).0);
        b.push(self.rep.0);
    }
}

impl Serializable for Params {
    fn serialize(&self, b: &mut Vec<u8>) {
        self.0.serialize(b)
    }
}

fn serialize_string(s: &str, size: u8, b: &mut Vec<u8>) {
    // TODO: issue a warning as in PLtoTF.2014.87 if the string doesn't fit
    let len_padding_or = match s.len().try_into() {
        Ok(len) => size.checked_sub(len).map(|padding| (len, padding)),
        Err(_) => None,
    };
    match len_padding_or {
        None => {
            b.push(size);
            b.extend(s[0..size as usize].as_bytes())
        }
        Some((len, padding)) => {
            b.push(len);
            b.extend(s.as_bytes());
            b.extend(vec![0; padding as usize]);
        }
    }
}

impl Serializable for Header {
    fn serialize(&self, b: &mut Vec<u8>) {
        self.checksum.serialize(b);
        self.design_size.serialize(b);
        serialize_string(&self.character_coding_scheme, 39, b);
        serialize_string(&self.font_family, 19, b);
        if self.seven_bit_safe == Some(true) {
            // Any value >=128 is interpreted as true, but PLtoTF.2014.133 uses 128 exactly...
            b.push(128);
        } else {
            // ...and 0 for false.
            b.push(0);
        }
        b.push(0);
        b.push(0);
        b.push(self.face.unwrap_or(0_u8.into()).into());
        self.additional_data.serialize(b);
    }
}

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
fn _compress(
    values: &[Number],
    max_size: std::num::NonZeroUsize,
) -> (Vec<Number>, HashMap<Number, usize>) {
    let dedup_values = {
        let s: HashSet<Number> = values.iter().copied().collect();
        let mut v: Vec<Number> = s.into_iter().collect();
        v.sort();
        v
    };
    // After deduplication, it is possible we don't need to compress at all so we can exit early.
    // This also handles the case when the values slice is empty.
    if dedup_values.len() <= max_size.get() {
        let m: HashMap<Number, usize> = dedup_values
            .iter()
            .enumerate()
            .map(|(i, &w)| (w, i))
            .collect();
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
                if buffer.len() >= max_size.get() {
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

        if buffer.len() <= max_size.get() {
            // solution
            std::mem::swap(&mut buffer, &mut solution);
            upper = delta_lower;
        } else {
            // not a solution
            lower = delta_upper;
        }
        buffer.clear();
    }

    let mut value_to_index = HashMap::<Number, usize>::new();
    let mut result = vec![];
    let mut previous = 0_usize;
    for i in solution {
        let interval = &dedup_values[previous..i];
        previous = i;
        for &v in interval {
            value_to_index.insert(v, result.len());
        }
        let replacement = (*interval.last().unwrap() + *interval.first().unwrap()) / 2;
        result.push(replacement);
    }

    (result, value_to_index)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_compress_test(values: Vec<Number>, max_size: std::num::NonZeroUsize, want: Vec<Number>) {
        let (got, _) = _compress(&values, max_size);
        assert_eq!(got, want);
    }

    macro_rules! compress_tests {
        ( $( ($name: ident, $values: expr, $max_size: expr, $want: expr, ), )+ ) => {
            $(
                #[test]
                fn $name () {
                    let values = $values;
                    let max_size: std::num::NonZeroUsize = $max_size.try_into().unwrap();
                    let want = $want;
                    run_compress_test(values, max_size, want);
                }
            )+
        };
    }

    compress_tests!(
        (no_op_0, vec![], 1, vec![],),
        (
            no_op_2,
            vec![Number::UNITY * 2, Number::UNITY],
            2,
            vec![Number::UNITY, Number::UNITY * 2],
        ),
        (
            just_deduplication,
            vec![Number::UNITY, Number::UNITY],
            1,
            vec![Number::UNITY],
        ),
        (
            simple_compression_case,
            vec![Number::UNITY, Number::UNITY * 2],
            1,
            vec![Number::UNITY * 3 / 2],
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
            vec![Number::UNITY * 3 / 2, Number::UNITY * 401 / 2],
        ),
        (
            lower_upper_close_edge_case_1,
            vec![Number(1), Number(3)],
            1,
            vec![Number(2)],
        ),
        (
            lower_upper_close_edge_case_2,
            vec![Number(0), Number(2)],
            1,
            vec![Number(1)],
        ),
        (
            lower_upper_close_edge_case_3,
            vec![Number(1), Number(4)],
            1,
            vec![Number(2)],
        ),
        (
            lower_upper_close_edge_case_4,
            vec![Number(1), Number(2)],
            1,
            vec![Number(1)],
        ),
    );
}
