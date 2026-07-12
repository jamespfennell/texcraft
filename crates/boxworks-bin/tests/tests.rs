use assert_cmd::prelude::*;
use std::process::Command;

fn run_box(args: &[&str], texts_file: &str) -> String {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let texts_file_path = std::path::Path::new(&manifest_dir)
        .join("tests")
        .join(texts_file);

    let mut cmd = Command::cargo_bin("box").unwrap();
    for arg in args {
        cmd.arg(arg);
    }
    cmd.arg(format!("--texts-file={}", texts_file_path.display()));
    if std::env::var("TEXCRAFT_VERIFY").unwrap_or("".to_string()) == "tex" {
        cmd.arg("--tex-engine=tex");
    }
    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run box command: {}",
        stderr
    );
    eprintln!("{stderr}");
    assert!(!output.stdout.is_empty(), "box output is empty");
    String::from_utf8(output.stdout).unwrap()
}

macro_rules! tests {
    ( $( (
        $name:ident,
        $golden_file:expr,
        $texts_file:expr,
        [
            $( $arg: expr ),+
        ],
   ), )+ ) => {
        $(
            #[test]
            fn $name() {
                const GOLDEN: &str = include_str!($golden_file);
                let args = [ $( $arg ),+ ];
                let box_output = run_box(&args, $texts_file);
                similar_asserts::assert_eq!(got: normalize(&box_output), want: normalize(GOLDEN));
            }
        )+
    };
}

fn normalize(s: &str) -> String {
    s.lines().collect::<Vec<_>>().join("\n")
}

tests!(
    (
        alice_hbox_unhyphenated,
        "alice_in_wonderland_hlists.txt",
        "alice_in_wonderland.txt",
        ["hbox"],
    ),
    (
        alice_hbox_hyphenated,
        "alice_in_wonderland_hlists_hyphenated.txt",
        "alice_in_wonderland.txt",
        ["hbox", "--hyphenate"],
    ),
    (
        alice_linebreak,
        "alice_in_wonderland_linebreak.txt",
        "alice_in_wonderland.txt",
        ["linebreak", "--widths=10in"],
    ),
);
