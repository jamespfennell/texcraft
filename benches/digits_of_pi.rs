use criterion::{criterion_group, criterion_main, Criterion};
use std::io::Write;
use std::process::Command;
use std::process::Stdio;
use texcraft::tex::command::library::execwhitespace;
use texcraft::tex::command::library::WholeLibraryState;
use texcraft::tex::prelude::*;

const DIGITS_OF_PI_TEX: &'static str = include_str!("digits_of_pi.tex");

fn digits_of_pi(tex_input: &str) -> () {
    let mut state = WholeLibraryState::new();
    state.set_command("par", execwhitespace::get_par());
    state.set_command("end", execwhitespace::get_newline());
    let mut input = input::Unit::new();
    input.push_new_str(tex_input);
    driver::exec(&mut state, &mut input, true).unwrap();
}

pub fn digits_of_pi_bench(c: &mut Criterion) {
    let n = match std::env::var("DIGITS_OF_PI_N") {
        Ok(val) => match val.parse::<usize>() {
            Ok(val) => val,
            Err(_) => panic!["Failed to parse env var DIGITS_OF_PI={} as an integer", val],
        },
        Err(_) => 100,
    };
    let tex_input = str::replace(DIGITS_OF_PI_TEX, r"\n = 100", &format![r"\n = {}", n]);

    let mut group = c.benchmark_group("digits-of-pi");

    group.bench_function("digits_of_pi_texcraft", |b| {
        b.iter(|| digits_of_pi(&tex_input))
    });

    let host_has_pdftex = Command::new("which")
        .arg("pdftex")
        .spawn()
        .expect("`which pdftex` command failed to start")
        .wait()
        .expect("failed to run `which pdftex`")
        .success();
    if host_has_pdftex {
        group.bench_function("digits_of_pi_pdftex", |b| {
            b.iter(|| {
                let mut child = Command::new("pdftex")
                    .stdin(Stdio::piped())
                    .stdout(Stdio::null())
                    .spawn()
                    .expect("pdftex command failed to start");
                let child_stdin = child.stdin.as_mut().unwrap();
                child_stdin
                    .write_all(tex_input.as_bytes())
                    .expect("failed to write to pdfTeX");
                child.wait().expect("Failed to run pdfTeX");
            })
        });
    } else {
        println!("Skipping pdfTeX benchmark as pdfTeX is not installed (`which pdftex` failed).");
    }
}

criterion_group!(benches, digits_of_pi_bench);
criterion_main!(benches);
