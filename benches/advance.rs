use core::time::Duration;
use criterion::{criterion_group, criterion_main, Criterion};
use texcraft::tex::command::library::execwhitespace;
use texcraft::tex::command::library::WholeLibraryState;
use texcraft::tex::prelude::*;

pub fn advance_bench(c: &mut Criterion) {
    let mut state = WholeLibraryState::new();
    state.set_command("par", execwhitespace::get_par());
    state.set_command("newline", execwhitespace::get_newline());
    let mut input = input::Unit::new();
    input.push_new_str(r"\countdef\k 0 \def\a{\advance\k by 1}");
    driver::exec(&mut state, &mut input, true).unwrap();

    let mut group = c.benchmark_group("advance");
    group.bench_function("advance", |b| {
        b.iter(|| {
            // TODO: replace with input.push_singleton(\a token)
            input.push_new_str(r"\a");
            driver::exec(&mut state, &mut input, true).unwrap();
        })
    });
}

criterion_group!(benches, advance_bench);
criterion_main!(benches);
