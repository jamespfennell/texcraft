use criterion::{criterion_group, criterion_main, Criterion};
use texlang_core::prelude::*;
use texlang_core::runtime::ExpandedInput;
use texlang_stdlib::execwhitespace;
use texlang_stdlib::StdLibState;

pub fn advance_bench(c: &mut Criterion) {
    let mut env = StdLibState::new();
    env.set_command("par", execwhitespace::get_par());
    env.set_command("newline", execwhitespace::get_newline());
    env.push_source(r"\countdef\k 0 \def\a{\advance\k by 1}".to_string())
        .unwrap();
    execwhitespace::exec(&mut env, true).unwrap();
    let a_cs = Token::new_control_sequence(
        env.cs_name_interner()
            .get("a")
            .expect("a should have been interned already"),
        0,
    );

    let mut group = c.benchmark_group("advance");
    group.sample_size(10000);
    let expansion = vec![a_cs; 1000];
    group.bench_function("advance", |b| {
        b.iter(|| {
            ExpandedInput::new(&mut env).push_expansion(&expansion);
            execwhitespace::exec(&mut env, true).unwrap();
        })
    });
}

criterion_group!(benches, advance_bench);
criterion_main!(benches);
