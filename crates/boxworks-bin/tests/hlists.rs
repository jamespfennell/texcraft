use assert_cmd::prelude::*;
use std::process::Command;

fn run_hlists(texts_file: &str) -> String {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let texts_file_path = std::path::Path::new(&manifest_dir)
        .join("tests")
        .join(texts_file);

    let mut cmd = Command::cargo_bin("box").unwrap();
    cmd.arg("hbox");
    cmd.arg(format!("--texts-file={}", texts_file_path.display()));
    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run box command: {}",
        stderr
    );
    String::from_utf8(output.stdout).unwrap()
}

macro_rules! hlists_tests {
    ( $( ($name:ident, $texts_file:expr, $golden_file:expr), )+ ) => {
        $(
            #[test]
            fn $name() {
                const GOLDEN: &str = include_str!($golden_file);
                let box_output = run_hlists($texts_file);
                similar_asserts::assert_eq!(got: box_output, want: GOLDEN);
            }
        )+
    };
}

hlists_tests!((
    alice,
    "alice_in_wonderland.txt",
    "alice_in_wonderland_hlists.txt"
),);
