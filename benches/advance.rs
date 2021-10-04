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
    group.sample_size(10000);
    let a_cs = Token::new_control_sequence('\\', state.cs_names.get_or_intern("a"));
    let expansion = vec![a_cs; 1000];
    group.bench_function("advance", |b| {
        b.iter(|| {
            input.push_expansion(&expansion);
            driver::exec(&mut state, &mut input, true).unwrap();
        })
    });
}

criterion_group!(benches, advance_bench);
criterion_main!(benches);
