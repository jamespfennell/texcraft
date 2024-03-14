#![no_main]

use libfuzzer_sys::fuzz_target;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::process::Command;
use tfm::format::SubFileSizes;

fuzz_target!(|input: Input| {
    let tfm_bytes = input.tfm_bytes();

    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(&tfm_bytes).unwrap();

    let knuth_output = Command::new("tftopl")
        .arg(&tfm_file_path)
        .output()
        .expect("failed to execute pltotf");
    let knuth_stdout = std::str::from_utf8(&knuth_output.stdout).unwrap();
    let knuth_stderr = std::str::from_utf8(&knuth_output.stderr).unwrap();

    let texcraft_output = tfm::algorithms::tfm_to_pl(&tfm_bytes, &|_| Default::default()).unwrap();

    if texcraft_output.success
        && knuth_stdout == texcraft_output.pl_data
        && knuth_stderr == texcraft_output.error_messages
    {
        return;
    }

    let base_path = format![
        "crates/tfm/bin/tests/data/fuzz/fuzz_tftopl_{:x}",
        input.calculate_hash()
    ];
    std::fs::copy(tfm_file_path, format!["{base_path}.tfm"]).unwrap();
    std::fs::write(format!["{base_path}.pl"], knuth_stdout).unwrap();
    if !knuth_stderr.is_empty() {
        std::fs::write(format!["{base_path}.stderr.txt"], knuth_stderr).unwrap();
    }

    assert!(
        texcraft_output.success,
        "failed to run Texcraft tftopl: {}",
        texcraft_output.error_messages
    );
    similar_asserts::assert_eq!(texcraft: texcraft_output.pl_data, knuth: knuth_stdout);
    similar_asserts::assert_eq!(texcraft: texcraft_output.error_messages, knuth: knuth_stderr);
});

#[derive(Clone, Debug, arbitrary::Arbitrary)]
pub struct Input {
    pub header: Vec<u32>,
    pub char_infos: Vec<u32>,
    pub widths: Vec<u32>,
    pub heights: Vec<u32>,
    pub depths: Vec<u32>,
    pub italic_corrections: Vec<u32>,
    pub lig_kern_instructions: Vec<u32>,
    pub kerns: Vec<u32>,
    pub extensible_recipes: Vec<u32>,
    pub params: Vec<u32>,
}

impl Input {
    fn tfm_bytes(&self) -> Vec<u8> {
        let mut b = vec![0_u8; 24];
        let mut sub_file_sizes = SubFileSizes {
            lf: 0, // correct value populated later
            lh: append_bytes(&mut b, &self.header, 2, i16::MAX),
            bc: if self.char_infos.is_empty() { 1 } else { 0 },
            ec: if self.char_infos.is_empty() {
                0
            } else {
                append_bytes(&mut b, &self.char_infos, 0, 256) - 1
            },
            nw: append_bytes(&mut b, &self.widths, 1, i16::MAX),
            nh: append_bytes(&mut b, &self.heights, 1, i16::MAX),
            nd: append_bytes(&mut b, &self.depths, 1, i16::MAX),
            ni: append_bytes(&mut b, &self.italic_corrections, 1, i16::MAX),
            nl: append_bytes(&mut b, &self.lig_kern_instructions, 0, i16::MAX),
            nk: append_bytes(&mut b, &self.kerns, 0, i16::MAX),
            ne: append_bytes(&mut b, &self.extensible_recipes, 0, i16::MAX),
            np: append_bytes(&mut b, &self.params, 0, i16::MAX),
        };
        sub_file_sizes.lf = sub_file_sizes.valid_lf();
        let sfs_b: [u8; 24] = sub_file_sizes.into();
        for (i, byte) in sfs_b.into_iter().enumerate() {
            b[i] = byte;
        }
        b
    }

    fn calculate_hash(&self) -> u64 {
        let debug = format!("{self:?}");
        let mut s = std::collections::hash_map::DefaultHasher::new();
        debug.hash(&mut s);
        s.finish()
    }
}

fn append_bytes(b: &mut Vec<u8>, mut raw: &[u32], min: i16, max: i16) -> i16 {
    if let Some(new_raw) = raw.get(..max as usize) {
        raw = new_raw;
    }
    b.extend(raw.iter().flat_map(|u| u.to_be_bytes()));
    match (min as usize).checked_sub(raw.len()) {
        None => raw.len().try_into().unwrap(),
        Some(diff) => {
            for _ in 0..diff {
                b.extend([0_u8; 4]);
            }
            min
        }
    }
}
