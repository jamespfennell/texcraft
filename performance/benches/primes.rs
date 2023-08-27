use criterion::{criterion_group, criterion_main, Criterion};

const PRIMES_TEX: &'static str = include_str!("primes.tex");

pub fn digits_of_pi_bench(c: &mut Criterion) {
    let n = match std::env::var("PRIMES_N") {
        Ok(val) => match val.parse::<usize>() {
            Ok(val) => val,
            Err(_) => panic!["Failed to parse env var PRIMES_N={} as an integer", val],
        },
        Err(_) => 130,
    };
    let tex_input = str::replace(PRIMES_TEX, r"\o < 130", &format![r"\o < {}", n]);

    let mut group = c.benchmark_group("primes");

    group.bench_function("primes_texcraft", |b| {
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
