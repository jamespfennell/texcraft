use criterion::{criterion_group, criterion_main, Criterion};
use texlang_core::prelude::*;
use texlang_core::vm::Env;
use texlang_core::vm::ExpansionInput;
use texlang_core::token::catcode;
use texlang_stdlib::script;
use texlang_stdlib::StdLibState;

pub fn advance_bench(c: &mut Criterion) {
    let mut initial_built_ins = StdLibState::all_initial_built_ins();
    initial_built_ins.insert("par", script::get_par());
    initial_built_ins.insert("end", script::get_newline());
    let mut env = Env::<StdLibState>::new(
        catcode::CatCodeMap::new_with_tex_defaults(),
        initial_built_ins,
        Default::default(),
    );
    env.push_source(
        "".to_string(),
        r"\countdef\k 0 \def\a{\advance\k by 1}".to_string(),
    )
    .unwrap();
    script::run(&mut env, true).unwrap();
    let a_cs = Token::new_control_sequence(
        env.cs_name_interner()
            .get("a")
            .expect("a should have been interned already"),
        trace::Key::dummy(),
    );

    let mut group = c.benchmark_group("advance");
    group.sample_size(10000);
    let expansion = vec![a_cs; 1000];
    group.bench_function("advance", |b| {
        b.iter(|| {
            ExpansionInput::new(&mut env).push_expansion(&expansion);
            script::run(&mut env, true).unwrap();
        })
    });
}

criterion_group!(benches, advance_bench);
criterion_main!(benches);
