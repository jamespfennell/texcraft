//! Utilities for writing unit tests

use std::collections::HashMap;

use crate::prefix;
use crate::script;
use anyhow::Result;
use texlang_core::command;
use texlang_core::runtime;
use texlang_core::runtime::implement_has_component;
use texlang_core::runtime::Env;
use texlang_core::runtime::HasComponent;
use texlang_core::token;
use texlang_core::token::catcode;

#[derive(Default)]
pub struct State {
    exec: script::Component,
    prefix: prefix::Component,
}

implement_has_component![
    State,
    (script::Component, exec),
    (prefix::Component, prefix),
];

#[macro_export]
macro_rules! expansion_test {
    ($name: ident, $lhs: expr, $rhs: expr) => {
        expansion_test![$name, $lhs, $rhs, setup_expansion_test, None];
    };
    ($name: ident, $lhs: expr, $rhs: expr, $setup_fn: ident, $a: expr) => {
        #[test]
        fn $name() {
            crate::testutil::run_expansion_test($setup_fn, $a, $lhs.to_string(), $rhs.to_string());
        }
    };
}

pub use expansion_test;

#[macro_export]
macro_rules! expansion_failure_test {
    ($name: ident, $input: expr) => {
        #[test]
        fn $name() {
            crate::testutil::run_expansion_failure_test(setup_expansion_test, $input.to_string());
        }
    };
}

pub use expansion_failure_test;

pub fn run_expansion_test<S: Default + HasComponent<script::Component>>(
    setup_fn: fn() -> HashMap<&'static str, command::Command<S>>,
    post_setup_env_fn: Option<fn(&mut Env<S>)>,
    lhs: String,
    rhs: String,
) {
    use ::texlang_core::token::Value::ControlSequence;

    let (output_1, env_1) = crate::testutil::run(setup_fn, post_setup_env_fn, lhs);
    let (output_2, env_2) = crate::testutil::run(setup_fn, post_setup_env_fn, rhs);

    let output_1 = output_1.unwrap();
    let output_2 = output_2.unwrap();

    println!("{:?}", output_1);
    let equal = match output_1.len() == output_2.len() {
        false => {
            println!(
                "output lengths do not match: {} != {}",
                output_1.len(),
                output_2.len()
            );
            false
        }
        true => {
            let mut equal = true;
            for (token_1, token_2) in output_1.iter().zip(output_2.iter()) {
                let token_equal = match (&token_1.value(), &token_2.value()) {
                    (ControlSequence(cs_name_1), ControlSequence(cs_name_2)) => {
                        let name_1 = env_1.cs_name_interner().resolve(cs_name_1).unwrap();
                        let name_2 = env_2.cs_name_interner().resolve(cs_name_2).unwrap();
                        name_1 == name_2
                    }
                    _ => token_1 == token_2,
                };
                if !token_equal {
                    equal = false;
                    break;
                }
            }
            equal
        }
    };

    if !equal {
        println!("Expansion output is different:");
        println!("------[lhs]------");
        println!(
            "'{}'",
            ::texlang_core::token::write_tokens(&output_1, &env_1.cs_name_interner())
        );
        println!("------[rhs]------");
        println!(
            "'{}'",
            ::texlang_core::token::write_tokens(&output_2, &env_2.cs_name_interner())
        );
        println!("-----------------");
        panic!("Expansion test failed");
    }
}

pub fn run_expansion_failure_test<S: Default + HasComponent<script::Component>>(
    setup_fn: fn() -> HashMap<&'static str, command::Command<S>>,
    input: String,
) {
    let (result, env) = run(setup_fn, None, input);
    if let Ok(output) = result {
        println!("Expansion succeeded:");
        println!(
            "{}",
            ::texlang_core::token::write_tokens(&output, &env.cs_name_interner())
        );
        panic!("Expansion failure test did not pass: expansion successful");
    }
}

fn run<S: Default + HasComponent<script::Component>>(
    setup_fn: fn() -> HashMap<&'static str, command::Command<S>>,
    post_setup_env_fn: Option<fn(&mut Env<S>)>,
    source: String,
) -> (Result<Vec<token::Token>>, Env<S>) {
    let mut env = Env::<S>::new(
        catcode::CatCodeMap::new_with_tex_defaults(),
        setup_fn(),
        Default::default(),
    );
    if let Some(post_set_env_fn) = post_setup_env_fn {
        post_set_env_fn(&mut env)
    }
    env.push_source("testutil.tex".to_string(), source).unwrap();
    let output = script::run(&mut env, false);
    (output, env)
}

/// In-memory filesystem for testing.
#[derive(Default)]
pub struct FileSystemOps {
    files: HashMap<std::path::PathBuf, String>,
}

impl runtime::FileSystemOps for FileSystemOps {
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
        match self.files.get(path) {
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "not found",
            )),
            Some(content) => Ok(content.clone()),
        }
    }
}

impl FileSystemOps {
    /// Add a file to the in-memory file system
    pub fn add_file(&mut self, path: std::path::PathBuf, content: &str) {
        self.files.insert(path, content.to_string());
    }
}

/// Gets an expansion command which does noting and always succeeds.
pub fn get_noop_expansion_cmd<S>() -> texlang_core::command::Command<S> {
    fn noop_expansion_cmd_fn<S>(
        _: token::Token,
        _: &mut runtime::ExpansionInput<S>,
    ) -> anyhow::Result<Vec<token::Token>> {
        Ok(Vec::new())
    }
    texlang_core::command::Command::new_expansion(noop_expansion_cmd_fn)
}

/// Gets an execution command which does noting and always succeeds.
pub fn get_noop_execution_cmd<S>() -> texlang_core::command::Command<S> {
    fn noop_execution_cmd_fn<S>(
        _: token::Token,
        _: &mut runtime::ExecutionInput<S>,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    texlang_core::command::Command::new_execution(noop_execution_cmd_fn)
}
