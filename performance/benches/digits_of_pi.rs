use criterion::{criterion_group, criterion_main, Criterion};

const DIGITS_OF_PI_TEX: &'static str = include_str!("digits_of_pi.tex");

pub fn digits_of_pi_bench(c: &mut Criterion) {
    let n = match std::env::var("DIGITS_OF_PI_N") {
        Ok(val) => match val.parse::<usize>() {
            Ok(val) => val,
            Err(_) => panic![
                "Failed to parse env var DIGITS_OF_PI_N={} as an integer",
                val
            ],
        },
        Err(_) => 175,
    };
    let tex_input = str::replace(DIGITS_OF_PI_TEX, r"\n = 100", &format![r"\n = {}", n]);

    let mut group = c.benchmark_group("digits-of-pi");

    group.bench_function("digits_of_pi_texcraft", |b| {
        b.iter(|| performance::run_in_texcraft(&tex_input))
    });

    if performance::host_has_pdftex() {
        group.bench_function("digits_of_pi_pdftex", |b| {
            b.iter(|| {
                performance::run_in_pdftex(&tex_input);
            })
        });
    } else {
        println!("Skipping pdfTeX benchmark as pdfTeX is not installed (`which pdftex` failed).");
    }
}

criterion_group!(benches, digits_of_pi_bench);
criterion_main!(benches);
