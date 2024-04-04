#![no_main]

use libfuzzer_sys::fuzz_target;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::process::Command;

fuzz_target!(|ast: tfm::pl::ast::Ast| {
    let cst = ast.lower(Default::default());
    let pl_input = format!["{}", cst.display(3)];

    let dir = tempfile::TempDir::new().unwrap();
    let pl_file_path = dir.path().join("input.pl");
    let mut pl_file = std::fs::File::create(&pl_file_path).unwrap();
    pl_file.write_all(pl_input.as_bytes()).unwrap();

    let tfm_file_path = dir.path().join("out.tfm");

    let knuth_output = Command::new("pltotf")
        .arg(&pl_file_path)
        .arg(&tfm_file_path)
        .output()
        .expect("failed to execute pltotf");
    let knuth_stdout = std::fs::read(&tfm_file_path).unwrap();
    let knuth_stderr = std::str::from_utf8(&knuth_output.stderr).unwrap();

    if output_always() {
        write_input_and_correct_output(&pl_input, &pl_file_path, &tfm_file_path, knuth_stderr);
    }

    // TODO: move this to the algorithms module?
    let (pl_file, warnings) = tfm::pl::File::from_pl_source_code(&pl_input);
    let texcraft_stderr = {
        use std::fmt::Write;
        let mut s = String::new();
        for warning in warnings {
            writeln!(&mut s, "{}", warning.pltotf_message(&pl_input)).unwrap();
        }
        s
    };
    let tfm_file: tfm::format::File = pl_file.into();
    let texcraft_output: Vec<u8> = tfm_file.serialize();

    if knuth_stdout == texcraft_output && knuth_stderr == texcraft_stderr {
        return;
    }

    write_input_and_correct_output(&pl_input, &pl_file_path, &tfm_file_path, knuth_stderr);

    similar_asserts::assert_eq!(texcraft: texcraft_output, knuth: knuth_stdout);
    similar_asserts::assert_eq!(texcraft: texcraft_stderr, knuth: knuth_stderr);
});

fn output_always() -> bool {
    std::env::var("TEXCRAFT_FUZZ_OUTPUT_ALWAYS").is_ok()
}

fn write_input_and_correct_output(
    input: &str,
    pl_file_path: &std::path::Path,
    tfm_file_path: &std::path::Path,
    knuth_stderr: &str,
) {
    let base_path = format![
        "crates/tfm/corpus/fuzz/fuzz_pltotf_{:x}",
        calculate_hash(input)
    ];
    std::fs::copy(pl_file_path, format!["{base_path}.plst"]).unwrap();
    std::fs::copy(tfm_file_path, format!["{base_path}.tfm"]).unwrap();
    if !knuth_stderr.is_empty() {
        std::fs::write(format!["{base_path}.stderr.txt"], knuth_stderr).unwrap();
    }
}

fn calculate_hash(input: &str) -> u64 {
    let mut s = std::collections::hash_map::DefaultHasher::new();
    input.hash(&mut s);
    s.finish()
}
