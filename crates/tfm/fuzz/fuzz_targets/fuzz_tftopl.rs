#![no_main]

use libfuzzer_sys::fuzz_target;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::process::Command;
use tfm::SubFileSizes;

fuzz_target!(|input: Input| {
    let tfm_bytes = input.tfm_bytes();

    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(&tfm_bytes).unwrap();

    let knuth_output = Command::new("tftopl")
        .arg(&tfm_file_path)
        .output()
        .expect("failed to execute tftopl");
    let knuth_stdout = std::str::from_utf8(&knuth_output.stdout).unwrap();
    let knuth_stderr = std::str::from_utf8(&knuth_output.stderr).unwrap();

    if output_always() {
        write_input_and_correct_output(&input, &tfm_file_path, knuth_stdout, knuth_stderr);
    }

    let texcraft_output =
        tfm::algorithms::tfm_to_pl(&tfm_bytes, 3, &|_| Default::default()).unwrap();
    let texcraft_stderr = {
        use std::fmt::Write;
        let mut s = String::new();
        for error_message in texcraft_output.error_messages {
            writeln!(&mut s, "{}", error_message.tftopl_message()).unwrap();
        }
        if let Err(err) = &texcraft_output.pl_data {
            writeln!(&mut s, "{}", err.tftopl_message()).unwrap();
        }
        s
    };
    if let Ok(pl_data) = &texcraft_output.pl_data {
        if knuth_stdout == pl_data && knuth_stderr == texcraft_stderr {
            return;
        }
    }

    write_input_and_correct_output(&input, &tfm_file_path, knuth_stdout, knuth_stderr);

    assert!(
        texcraft_output.pl_data.is_ok(),
        "failed to run Texcraft tftopl: {}",
        texcraft_stderr,
    );
    similar_asserts::assert_eq!(texcraft: texcraft_output.pl_data.unwrap(), knuth: knuth_stdout);
    similar_asserts::assert_eq!(texcraft: texcraft_stderr, knuth: knuth_stderr);
});

fn output_always() -> bool {
    std::env::var("TEXCRAFT_FUZZ_OUTPUT_ALWAYS").is_ok()
}

fn write_input_and_correct_output(
    input: &Input,
    tfm_file_path: &std::path::Path,
    knuth_stdout: &str,
    knuth_stderr: &str,
) {
    let base_path = format![
        "crates/tfm/corpus/fuzz/fuzz_tftopl_{:x}",
        input.calculate_hash()
    ];
    std::fs::copy(tfm_file_path, format!["{base_path}.tfm"]).unwrap();
    std::fs::write(format!["{base_path}.plst"], knuth_stdout).unwrap();
    if !knuth_stderr.is_empty() {
        std::fs::write(format!["{base_path}.stderr.txt"], knuth_stderr).unwrap();
    }
}

#[derive(Clone, Debug, arbitrary::Arbitrary)]
pub struct Input {
    pub header: Vec<u32>,
    pub bc: u8,
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
            lf: 0, // the correct value is calculated later
            lh: append_bytes(&mut b, &self.header, 2, i16::MAX),
            bc: if self.char_infos.is_empty() {
                1
            } else {
                self.bc.into()
            },
            ec: if self.char_infos.is_empty() {
                0
            } else {
                let bc: i16 = self.bc.into();
                let max_chars = 256_i16 - bc;
                let num_chars = append_bytes(&mut b, &self.char_infos, 0, max_chars);
                bc + num_chars - 1
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
