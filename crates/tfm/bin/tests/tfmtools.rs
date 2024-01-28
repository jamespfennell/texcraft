use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::io::Write;
use std::process::Command;

#[test]
fn incorrect_extension() {
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(["convert", "incorrect_extension.rs"]);
    cmd.assert().failure().stderr(predicate::str::contains(
        "the file extension must be .pl or .tfm but it is .rs",
    ));
}

// TODO: get the test working for windows.
// The problem is that the new line chars in the file is different than stdout.
#[cfg(not(target_os = "windows"))]
#[test]
fn tftopl() {
    let tfm = include_bytes!("data/cmr10.tfm");
    let pl = include_str!("data/cmr10.pl");

    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    // TODO: also have a test for tftopl
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(["convert", tfm_file_path.to_str().unwrap()]);
    cmd.assert().success().stdout(predicate::eq(pl).normalize());
}

#[cfg(not(target_os = "windows"))]
#[test]
fn tftopl_2() {
    let tfm = include_bytes!("data/cmss8.tfm");
    let pl = include_str!("data/cmss8.pl");

    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(["convert", tfm_file_path.to_str().unwrap()]);
    cmd.assert().success().stdout(predicate::eq(pl).normalize());
}
