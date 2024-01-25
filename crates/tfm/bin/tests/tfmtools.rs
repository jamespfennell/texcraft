use assert_cmd::prelude::*;
use std::process::Command;
use predicates::prelude::*;

#[test]
fn t() {
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(["convert", "incorrect_extension.rs"]);
    cmd.assert()
    .failure()
    .stderr(predicate::str::contains("the file extension must be .pl or .tfm but it is .rs"));

}
