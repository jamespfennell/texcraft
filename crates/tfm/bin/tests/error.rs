use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn incorrect_extension() {
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(["convert", "incorrect_extension.rs"]);
    // TODO: stop using predicates lib
    cmd.assert().failure().stderr(predicate::str::contains(
        "the file extension must be .pl or .plst or .tfm but it is .rs",
    ));
}
