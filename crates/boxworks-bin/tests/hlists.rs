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

fn filter_width_lines(s: &str) -> String {
    let s = s.trim();
    s.lines()
        .filter(|line| !line.contains("width"))
        .collect::<Vec<_>>()
        .join("\n")
}

macro_rules! hlists_tests {
    ( $( ($name:ident, $texts_file:expr, $golden_file:expr), )+ ) => {
        $(
            #[test]
            fn $name() {
                const GOLDEN: &str = include_str!($golden_file);
                let box_output = run_hlists($texts_file);
                let box_filtered = filter_width_lines(&box_output);
                let golden_filtered = filter_width_lines(GOLDEN);
                similar_asserts::assert_eq!(got: box_filtered, want: golden_filtered);
            }
        )+
    };
}

hlists_tests!((
    alice,
    "alice_in_wonderland.txt",
    "alice_in_wonderland_hlists.txt"
),);
