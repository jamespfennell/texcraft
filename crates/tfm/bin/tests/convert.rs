use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

type TfmToPlArgsFn = for<'a> fn(tfm_file_path: &'a str) -> Vec<&'a str>;

fn run_tfm_to_pl_test(tfm: &[u8], pl: &str, command: &str, args_fn: TfmToPlArgsFn) {
    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    let mut cmd = Command::cargo_bin(command).unwrap();
    let args = args_fn(tfm_file_path.to_str().unwrap());
    cmd.args(args);

    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );
    let got = String::from_utf8(output.stdout).unwrap();
    similar_asserts::assert_eq!(texcraft: got, knuth: pl.replace("\r\n", "\n"));
    similar_asserts::assert_eq!(texcraft: stderr, knuth: "");
}

type PlToTfmArgsFn = for<'a> fn(pl_file_path: &'a str, tfm_file_path: &'a str) -> Vec<&'a str>;

fn run_pl_to_tfm_test(tfm: &[u8], pl: &str, command: &str, args_fn: PlToTfmArgsFn) {
    let dir = tempfile::TempDir::new().unwrap();
    let pl_file_path = dir.path().join("input.pl");
    let mut pl_file = std::fs::File::create(&pl_file_path).unwrap();
    pl_file.write_all(pl.as_bytes()).unwrap();
    let tfm_file_path = dir.path().join("output.tfm");

    let mut cmd = Command::cargo_bin(command).unwrap();
    let args = args_fn(
        pl_file_path.to_str().unwrap(),
        tfm_file_path.to_str().unwrap(),
    );
    cmd.args(args);

    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    similar_asserts::assert_eq!(texcraft: stdout, knuth: "");
    similar_asserts::assert_eq!(texcraft: stderr, knuth: "");

    let got = std::fs::read(&tfm_file_path).unwrap();
    if got == tfm {
        return;
    }
    let want_tfm_file_path = dir.path().join("want.tfm");
    let mut want_tfm_file = std::fs::File::create(&want_tfm_file_path).unwrap();
    want_tfm_file.write_all(tfm).unwrap();

    similar_asserts::assert_eq!(texcraft: debug(&tfm_file_path), knuth: debug(&want_tfm_file_path));
    // It's possible that there is a bug in `tfmtools debug` such that different files have
    // the same debug output. For this case, we assert the bytes are equal too.
    assert_eq!(got, tfm);
}

fn debug(tfm_file_path: &std::path::PathBuf) -> String {
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(vec!["debug", tfm_file_path.to_str().unwrap()]);
    let output = cmd.output().unwrap();
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap()
}

macro_rules! convert_tests {
    ( $( ($name: ident, $tfm_path: expr, $pl_path: expr $(, $tftopl_args: expr )? $(,)? ), )+ ) => {
        $(
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes!($tfm_path);
                const PL: &'static str = include_str!($pl_path);

                #[test]
                fn tfmtools_convert_tfm_to_pl() {
                    run_tfm_to_pl_test(TFM, PL, "tfmtools", |tfm_file_path| {
                        let mut v: Vec<&str> = vec![];
                        v.extend(["convert", tfm_file_path]);
                        $(
                            v.extend( $tftopl_args );
                        )?
                        v
                    });
                }

                #[test]
                fn tftopl() {
                    run_tfm_to_pl_test(TFM, PL, "tftopl", |tfm_file_path|{
                        let mut v: Vec<&str> = vec![];
                        v.extend([tfm_file_path]);
                        $(
                            v.extend( $tftopl_args );
                        )?
                        v
                    });
                }

                #[test]
                fn tfmtools_convert_pl_to_tfm() {
                    run_pl_to_tfm_test(TFM, PL, "tfmtools", |pl_file_path, tfm_file_path| {
                        vec!["convert", "-o", tfm_file_path, pl_file_path]
                    });
                }

                #[test]
                fn pltotf() {
                    run_pl_to_tfm_test(TFM, PL, "pltotf", |pl_file_path, tfm_file_path|{
                        vec![pl_file_path, tfm_file_path]
                    });
                }
            }
        )+
    };
}

convert_tests!(
    (cmr10, "data/cmr10.tfm", "data/cmr10.pl"),
    (
        cmr10_ascii,
        "data/cmr10.tfm",
        "data/cmr10_ascii.pl",
        vec!["--charcode-format", "ascii"]
    ),
    (
        cmr10_octal,
        "data/cmr10.tfm",
        "data/cmr10_octal.pl",
        vec!["--charcode-format", "octal"]
    ),
    (cmss8, "data/cmss8.tfm", "data/cmss8.pl"),
    (cmex10, "data/cmex10.tfm", "data/cmex10.pl"),
    (cminch, "data/cminch.tfm", "data/cminch.pl"),
    (cmsy7, "data/cmsy7.tfm", "data/cmsy7.pl"),
);
