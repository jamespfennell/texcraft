use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

fn run_convert_test(tfm: &[u8], pl: &str, command: &str, args: &dyn Fn(&str) -> Vec<&str>) {
    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    let mut cmd = Command::cargo_bin(command).unwrap();
    cmd.args(args(tfm_file_path.to_str().unwrap()));
    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );
    let got = String::from_utf8(output.stdout).unwrap();
    similar_asserts::assert_eq!(texcraft: got, knuth: pl.replace("\r\n", "\n"));
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
                    run_convert_test(TFM, PL, "tfmtools", &|tfm_file_path| {
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
                    run_convert_test(TFM, PL, "tftopl", &|tfm_file_path|{
                        let mut v: Vec<&str> = vec![];
                        v.extend([tfm_file_path]);
                        $(
                            v.extend( $tftopl_args );
                        )?
                        v
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
