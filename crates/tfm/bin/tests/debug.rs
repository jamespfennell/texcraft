use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

fn run_debug_undebug_roundtrip_test(tfm: &[u8], additional_args: Vec<&str>) {
    let dir = tempfile::TempDir::new().unwrap();

    let tfm_file_path = dir.path().join("output.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    let mut args = vec!["debug", tfm_file_path.to_str().unwrap()];
    args.extend_from_slice(&additional_args);
    cmd.args(args);

    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );
    let debug_output = String::from_utf8(output.stdout).unwrap();

    let debug_file_path = dir.path().join("output.txt");
    let mut debug_file = std::fs::File::create(&debug_file_path).unwrap();
    debug_file.write_all(debug_output.as_bytes()).unwrap();

    let undebug_file_path = dir.path().join("undebug.tfm");

    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    let args = vec![
        "undebug",
        debug_file_path.to_str().unwrap(),
        undebug_file_path.to_str().unwrap(),
    ];
    cmd.args(args);
    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );

    let got = std::fs::read(&undebug_file_path).unwrap();
    if got == tfm {
        return;
    }
    similar_asserts::assert_eq!(texcraft: debug_tfm(&tfm_file_path), knuth: debug_tfm(&undebug_file_path));
    // It's possible that there is a bug in `tfmtools debug` such that different files have
    // the same debug output. For this case, we assert the bytes are equal too.
    assert_eq!(got, tfm);
}

fn debug_tfm(tfm_file_path: &std::path::PathBuf) -> String {
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(vec![
        "debug",
        "--omit-tfm-path",
        tfm_file_path.to_str().unwrap(),
    ]);
    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );
    String::from_utf8(output.stdout).unwrap()
}

macro_rules! debug_undebug_roundtrip_tests {
    ( $( ($name: ident, $tfm_path: expr , $tftopl_args: expr $(,)? ), )+ ) => {
        $(
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes!($tfm_path);

                #[test]
                fn debug_undebug_roundtrip() {
                    let v = $tftopl_args;
                    run_debug_undebug_roundtrip_test(TFM, v);
                }

            }
        )+
    };
}

debug_undebug_roundtrip_tests!(
    (cmr10, "data/computer-modern/cmr10.tfm", vec![]),
    (
        cmr10_calculate_sub_file_sizes,
        "data/computer-modern/cmr10.tfm",
        vec![
            "-s",
            "header",
            "-s",
            "char-infos",
            "-s",
            "widths",
            "-s",
            "heights",
            "-s",
            "depths",
            "-s",
            "italic-corrections",
            "-s",
            "lig-kern",
            "-s",
            "kerns",
            "-s",
            "extensible-recipes",
            "-s",
            "params",
        ],
    ),
    // cmex10 contains extensible recipes
    (cmex10, "data/computer-modern/cmex10.tfm", vec![]),
    // rashii2 has bc=16
    (rashii2, "data/ctan/rashii2.tfm", vec![]),
);
