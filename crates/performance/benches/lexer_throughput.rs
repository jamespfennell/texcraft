use criterion::{criterion_group, criterion_main, Criterion};
use rand::SeedableRng;
use texlang_core::prelude::*;
use texlang_stdlib::execwhitespace;
use texlang_stdlib::StdLibState;

fn digits_of_pi(tex_input: &str) -> () {
    let mut state = StdLibState::new();
    state.set_command("par", execwhitespace::get_par());
    state.set_command("end", execwhitespace::get_newline());
    let mut execution_input = driver::ExecutionInput::new_with_str(state, tex_input);
    driver::exec(&mut execution_input, true).unwrap();
}

pub fn lexer_throughput_bench(c: &mut Criterion) {
    let weights = Default::default();
    let mut rng = rand::prelude::StdRng::seed_from_u64(43);
    let tex_input =
        performance::generate_random_tex_document(&mut rng, 200000, (20, 50), (80, 100), &weights);

    let mut group = c.benchmark_group("lexer-throughput");

    group.bench_function("lexer_throughput_texcraft", |b| {
        b.iter(|| digits_of_pi(&tex_input))
    });

    if performance::host_has_pdftex() {
        group.bench_function("lexer_throughput_pdftex", |b| {
            b.iter(|| {
                performance::run_in_pdftex(&tex_input);
            })
        });
    } else {
        println!("Skipping pdfTeX benchmark as pdfTeX is not installed (`which pdftex` failed).");
    }
}

criterion_group!(benches, lexer_throughput_bench);
criterion_main!(benches);
