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
                if overwrite_golden() {
                    if normalize(&box_output) != normalize(GOLDEN) {
                        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
                        let golden_path = std::path::Path::new(&manifest_dir)
                            .join("tests")
                            .join($golden_file);
                        std::fs::write(golden_path, &box_output).unwrap();
                    }
                    return;
                }
                use boxworks_testing::assert_box_eq;
                assert_box_eq!(box_output, GOLDEN);
            }
        )+
    };
}

fn normalize(s: &str) -> String {
    s.lines().collect::<Vec<_>>().join("\n")
}

/// Returns true if the golden files should be regenerated instead of asserted
/// against. The `TEXCRAFT_VERIFY=tex` requirement ensures the golden files
/// only ever contain TeX's output.
fn overwrite_golden() -> bool {
    std::env::var("TEXCRAFT_VERIFY").unwrap_or_default() == "tex"
        && std::env::var("TEXCRAFT_VERIFY_OVERWRITE").unwrap_or_default() == "true"
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
    (
        wolf_hall_linebreak_line_penalty,
        "wolf_hall_linebreak_line_penalty.txt",
        "wolf_hall.txt",
        ["linebreak", "--width=3in", "--line-penalty=100"],
    ),
    (
        wolf_hall_linebreak_right_skip,
        "wolf_hall_linebreak_right_skip.txt",
        "wolf_hall.txt",
        [
            "linebreak",
            "--width=3in",
            "--right-skip=0pt plus 20.00003pt"
        ],
    ),
    (
        wolf_hall_linebreak_vlist_penalties,
        "wolf_hall_linebreak_vlist_penalties.txt",
        "wolf_hall.txt",
        [
            "linebreak",
            "--width=3in",
            "--broken-penalty=250",
            "--club-penalty=1000",
            "--inter-line-penalty=100",
            "--final-widow-penalty=500"
        ],
    ),
    // This test contains two identical paragraphs to verify that looseness
    // is applied to every paragraph, not just the first one.
    (
        farewell_linebreak_looseness,
        "farewell_to_arms_linebreak_looseness.txt",
        "farewell_to_arms.txt",
        ["linebreak", "--width=3in", "--looseness=1"],
    ),
);
