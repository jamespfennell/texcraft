use criterion::{criterion_group, criterion_main, Criterion};
use texlang_core::prelude::*;
use texlang_stdlib::execwhitespace;
use texlang_stdlib::StdLibState;

pub fn advance_bench(c: &mut Criterion) {
    let mut state = StdLibState::new();
    state.set_command("par", execwhitespace::get_par());
    state.set_command("newline", execwhitespace::get_newline());
    state
        .push_source(r"\countdef\k 0 \def\a{\advance\k by 1}".to_string())
        .unwrap();
    let mut execution_input = runtime::ExecutionInput::new(state);
    execwhitespace::exec(&mut execution_input, true).unwrap();
    let a_cs = Token::new_control_sequence(
        execution_input
            .env()
            .cs_name_interner()
            .get("a")
            .expect("a should have been interned already"),
        0,
    );

    let mut group = c.benchmark_group("advance");
    group.sample_size(10000);
    let expansion = vec![a_cs; 1000];
    group.bench_function("advance", |b| {
        b.iter(|| {
            execution_input
                .regular()
                .unexpanded_stream()
                .push_expansion(&expansion);
            execwhitespace::exec(&mut execution_input, true).unwrap();
        })
    });
}

criterion_group!(benches, advance_bench);
criterion_main!(benches);
