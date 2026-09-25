use assert_cmd::prelude::*;
use std::process::Command;

/// Write the provided ops to a DVI file in the directory.
fn write_dvi_file(dir: &tempfile::TempDir, name: &str, ops: Vec<dvi::Op>) {
    std::fs::write(dir.path().join(name), dvi::serialize(ops)).unwrap();
}

/// Build a `dvitools` command that runs inside the provided directory.
///
/// The command is run inside the directory so that the file paths passed to
/// it are just file names, with no directory separators in them.
/// The paths appear in the output of the diff subcommand, and if they
/// contained separators the output would be different on Windows.
/// (It would in fact be doubly different, because the diff format escapes
/// the backslash separators and wraps the escaped path in quotes.)
fn command(dir: &tempfile::TempDir) -> Command {
    let mut cmd = Command::cargo_bin("dvitools").unwrap();
    cmd.current_dir(dir.path());
    cmd
}

/// Convert output of the command to a string.
///
/// Line endings are normalized because they are different on Windows.
fn output_to_string(b: Vec<u8>) -> String {
    String::from_utf8(b).unwrap().replace("\r\n", "\n")
}

/// Run `dvitools diff` on the two lists of ops and return the exit code
/// and the output printed to stdout.
fn run_diff(left: Vec<dvi::Op>, right: Vec<dvi::Op>, args: &[&str]) -> (i32, String) {
    let dir = tempfile::TempDir::new().unwrap();
    write_dvi_file(&dir, "left.dvi", left);
    write_dvi_file(&dir, "right.dvi", right);

    let mut cmd = command(&dir);
    cmd.args(["diff", "left.dvi", "right.dvi"]).args(args);
    let output = cmd.output().unwrap();
    (
        output.status.code().unwrap(),
        output_to_string(output.stdout),
    )
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
    write_dvi_file(&dir, "left.dvi", vec![char('a')]);

    let output = command(&dir)
        .args(["diff", "left.dvi", "right.dvi"])
        .output()
        .unwrap();
    assert_eq!(output.status.code().unwrap(), 1);
    assert!(output_to_string(output.stderr).contains("failed to read"));
}

#[test]
fn invalid_dvi_data() {
    let dir = tempfile::TempDir::new().unwrap();
    write_dvi_file(&dir, "left.dvi", vec![char('a')]);
    // 254 is not a valid DVI op code.
    std::fs::write(dir.path().join("right.dvi"), [254]).unwrap();

    let output = command(&dir)
        .args(["diff", "left.dvi", "right.dvi"])
        .output()
        .unwrap();
    assert_eq!(output.status.code().unwrap(), 1);
    assert!(output_to_string(output.stderr).contains("invalid op code 254"));
}
