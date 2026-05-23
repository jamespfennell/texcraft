use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

const CMR10: &[u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");

#[test]
fn ligkern_run_and_replace() {
    let dir = tempfile::TempDir::new().unwrap();
    let tfm_path = dir.path().join("cmr10.tfm");
    std::fs::File::create(&tfm_path)
        .unwrap()
        .write_all(CMR10)
        .unwrap();

    // Step 1: run the lig/kern program on "difficult" with the original cmr10.
    Command::cargo_bin("tfmtools")
        .unwrap()
        .args(["ligkern", "run", tfm_path.to_str().unwrap(), "difficult"])
        .assert()
        .success()
        .stdout("di\u{e}cult\n");

    // Step 2: replace the lig/kern program with one that only has `ff -> _a^_`.
    let program_path = dir.path().join("program.txt");
    std::fs::write(&program_path, PROGRAM).unwrap();

    Command::cargo_bin("tfmtools")
        .unwrap()
        .args([
            "ligkern",
            "replace",
            tfm_path.to_str().unwrap(),
            "--program",
            program_path.to_str().unwrap(),
            "--in-place",
        ])
        .assert()
        .success();

    // Step 3: run the lig/kern program again; ff is now replaced by 'fabcdef'.
    Command::cargo_bin("tfmtools")
        .unwrap()
        .args(["ligkern", "run", tfm_path.to_str().unwrap(), "difficult"])
        .assert()
        .success()
        .stdout("difabcdeficult\n");
    Command::cargo_bin("tfmtools")
        .unwrap()
        .args(["ligkern", "run", tfm_path.to_str().unwrap(), "ABC"])
        .assert()
        .success()
        .stdout("XABCY\n");

    // Step 4: remove the lig/kern program entirely.
    Command::cargo_bin("tfmtools")
        .unwrap()
        .args([
            "ligkern",
            "replace",
            tfm_path.to_str().unwrap(),
            "--remove",
            "--in-place",
        ])
        .assert()
        .success();

    // Step 5: with no lig/kern program, the word is returned unchanged.
    Command::cargo_bin("tfmtools")
        .unwrap()
        .args(["ligkern", "run", tfm_path.to_str().unwrap(), "difficult"])
        .assert()
        .success()
        .stdout("difficult\n");
}

const PROGRAM: &str = "
fb -> f^ab
fc -> f^bc
fd -> f^cd
fe -> f^de
ff -> f^ef
|A -> |XA^
C| -> CY^|
";
