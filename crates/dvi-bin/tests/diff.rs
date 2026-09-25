use assert_cmd::prelude::*;
use std::process::Command;

/// Write the provided ops to a DVI file in the directory and return its path.
fn write_dvi_file(dir: &tempfile::TempDir, name: &str, ops: Vec<dvi::Op>) -> std::path::PathBuf {
    let path = dir.path().join(name);
    std::fs::write(&path, dvi::serialize(ops)).unwrap();
    path
}

/// Run `dvitools diff` on the two lists of ops and return the exit code
/// and the output printed to stdout.
fn run_diff(left: Vec<dvi::Op>, right: Vec<dvi::Op>, args: &[&str]) -> (i32, String) {
    let dir = tempfile::TempDir::new().unwrap();
    let left = write_dvi_file(&dir, "left.dvi", left);
    let right = write_dvi_file(&dir, "right.dvi", right);

    let mut cmd = Command::cargo_bin("dvitools").unwrap();
    cmd.arg("diff").arg(&left).arg(&right).args(args);
    let output = cmd.output().unwrap();
    // The paths of the temporary files appear in the diff header. Replace
    // them so that the output can be compared against a fixed string.
    let stdout = String::from_utf8(output.stdout)
        .unwrap()
        .replace(left.to_str().unwrap(), "left.dvi")
        .replace(right.to_str().unwrap(), "right.dvi");
    (output.status.code().unwrap(), stdout)
}

fn char(c: char) -> dvi::Op {
    dvi::Op::Char {
        char: c as u32,
        move_h: true,
    }
}

#[test]
fn identical_files() {
    let ops = vec![char('a'), char('b')];
    let (code, stdout) = run_diff(ops.clone(), ops, &[]);
    assert_eq!(code, 0);
    assert_eq!(stdout, "");
}

#[test]
fn empty_files() {
    let (code, stdout) = run_diff(vec![], vec![], &[]);
    assert_eq!(code, 0);
    assert_eq!(stdout, "");
}

#[test]
fn different_files() {
    let (code, stdout) = run_diff(
        vec![char('a'), char('b'), char('c')],
        vec![char('a'), char('z'), char('c')],
        &[],
    );
    assert_eq!(code, 1);
    assert_eq!(
        stdout,
        concat!(
            "--- left.dvi\n",
            "+++ right.dvi\n",
            "@@ -1,3 +1,3 @@\n",
            " char('a')\n",
            "-char('b')\n",
            "+char('z')\n",
            " char('c')\n",
        ),
    );
}

#[test]
fn context_option() {
    let ops = |middle: char| {
        vec![
            char('1'),
            char('2'),
            char('3'),
            char(middle),
            char('4'),
            char('5'),
            char('6'),
        ]
    };
    let (code, stdout) = run_diff(ops('a'), ops('b'), &["--context", "1"]);
    assert_eq!(code, 1);
    assert_eq!(
        stdout,
        concat!(
            "--- left.dvi\n",
            "+++ right.dvi\n",
            "@@ -3,3 +3,3 @@\n",
            " char('3')\n",
            "-char('a')\n",
            "+char('b')\n",
            " char('4')\n",
        ),
    );
}

/// Two files whose push and pop operations are paired up differently
/// have a large diff when indentation is enabled, because the indentation
/// of every operation between the pushes differs.
#[test]
fn indentation_inflates_the_diff() {
    let left = vec![dvi::Op::Push, char('a'), char('b'), dvi::Op::Pop];
    let right = vec![char('a'), char('b')];

    let (code, stdout) = run_diff(left.clone(), right.clone(), &[]);
    assert_eq!(code, 1);
    assert_eq!(
        stdout,
        concat!(
            "--- left.dvi\n",
            "+++ right.dvi\n",
            "@@ -1,4 +1,2 @@\n",
            "-push\n",
            "-  char('a')\n",
            "-  char('b')\n",
            "-pop\n",
            "+char('a')\n",
            "+char('b')\n",
        ),
    );

    // With indentation disabled the diff contains only the push and pop.
    let (code, stdout) = run_diff(left, right, &["--no-indentation"]);
    assert_eq!(code, 1);
    assert_eq!(
        stdout,
        concat!(
            "--- left.dvi\n",
            "+++ right.dvi\n",
            "@@ -1,4 +1,2 @@\n",
            "-push\n",
            " char('a')\n",
            " char('b')\n",
            "-pop\n",
        ),
    );
}

#[test]
fn missing_file() {
    let dir = tempfile::TempDir::new().unwrap();
    let left = write_dvi_file(&dir, "left.dvi", vec![char('a')]);
    let mut cmd = Command::cargo_bin("dvitools").unwrap();
    cmd.arg("diff").arg(&left).arg(dir.path().join("right.dvi"));
    let output = cmd.output().unwrap();
    assert_eq!(output.status.code().unwrap(), 1);
    assert!(String::from_utf8(output.stderr)
        .unwrap()
        .contains("failed to read"));
}

#[test]
fn invalid_dvi_data() {
    let dir = tempfile::TempDir::new().unwrap();
    let left = write_dvi_file(&dir, "left.dvi", vec![char('a')]);
    let right = dir.path().join("right.dvi");
    // 254 is not a valid DVI op code.
    std::fs::write(&right, [254]).unwrap();

    let mut cmd = Command::cargo_bin("dvitools").unwrap();
    cmd.arg("diff").arg(&left).arg(&right);
    let output = cmd.output().unwrap();
    assert_eq!(output.status.code().unwrap(), 1);
    assert!(String::from_utf8(output.stderr)
        .unwrap()
        .contains("invalid op code 254"));
}
