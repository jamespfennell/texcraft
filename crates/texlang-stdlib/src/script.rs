/// TeX as a scripting language
///
/// This module enables using TeX as a scripting language.
/// TeX files are processed using the usual TeX semantics, but instead
/// of typesetting the result and outputing it to PDF (say), the output is returned as a list of tokens.
/// These can be easily converted to a string using [texlang_core::token::write_tokens].
use texlang_core::prelude::*;

#[derive(Default)]
pub struct Component {
    exec_output: Vec<Token>,
    num_trailing_newlines: usize,
}

/// Get the `\newline` command.
///
/// This adds a newline to the output.
pub fn get_newline<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_execution(newline_primitive_fn)
}

fn newline_primitive_fn<S: HasComponent<Component>>(
    t: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let c = input.state_mut().component_mut();
    let newline_token = Token::new_space('\n', t.trace_key());
    c.exec_output.push(newline_token);
    c.num_trailing_newlines += 1;
    Ok(())
}

/// Get the `\par` command.
///
/// The `\par` command adds two newlines to the output.
/// Consecutive `\par` commands are treated as one.
pub fn get_par<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_execution(par_primitive_fn)
}

fn par_primitive_fn<S: HasComponent<Component>>(
    t: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let c = input.state_mut().component_mut();
    if c.exec_output.is_empty() {
        return Ok(());
    }
    let par_token = Token::new_space('\n', t.trace_key());
    match c.num_trailing_newlines {
        0 => {
            c.exec_output.push(par_token);
            c.exec_output.push(par_token);
            c.num_trailing_newlines += 2;
        }
        1 => {
            c.exec_output.push(par_token);
            c.num_trailing_newlines += 1;
        }
        _ => {}
    }
    Ok(())
}

/// Run the Texlang interpreter for the provided environment and return the result as list of tokens.
pub fn run<S: HasComponent<Component>>(
    env: &mut runtime::Env<S>,
    err_for_undefined_cs: bool,
) -> anyhow::Result<Vec<Token>> {
    let undefined_cs_handler = match err_for_undefined_cs {
        true => runtime::default_undefined_cs_handler,
        false => handle_character,
    };
    runtime::run(env, handle_character, undefined_cs_handler)?;
    let mut result = Vec::new();
    std::mem::swap(
        &mut result,
        &mut env.custom_state.component_mut().exec_output,
    );
    Ok(result)
}

fn handle_character<S: HasComponent<Component>>(
    mut token: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let c = input.state_mut().component_mut();
    if let Some('\n') = token.char() {
        token = Token::new_space(' ', token.trace_key());
    }
    c.exec_output.push(token);
    c.num_trailing_newlines = 0;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::def;
    use crate::testutil::*;
    use texlang_core::runtime;
    use texlang_core::token;
    use texlang_core::token::catcode;

    fn setup_expansion_test() -> HashMap<&'static str, command::Command<State>> {
        HashMap::from([
            ("par", get_par()),
            ("def", def::get_def()),
            ("newline", get_newline()),
        ])
    }

    macro_rules! script_test {
        ($name: ident, $input: expr, $want: expr) => {
            #[test]
            fn $name() {
                let mut env = runtime::Env::<State>::new(
                    catcode::CatCodeMap::new_with_tex_defaults(),
                    setup_expansion_test(),
                    Default::default(),
                );
                env.push_source("testutil.tex".to_string(), $input.to_string())
                    .unwrap();
                let tokens = run(&mut env, false).unwrap();
                let got = token::write_tokens(&tokens, env.cs_name_interner());
                let want = $want.to_string();

                if got != want {
                    println!("Output is different:");
                    println!("------[got]-------");
                    println!("{}", got);
                    println!("------[want]------");
                    println!("{}", want);
                    println!("-----------------");
                    panic!("write_tokens test failed");
                }
            }
        };
    }

    script_test!(char_newline_1, "H\nW", "H W");
    script_test!(newline_1, "H\\newline W", "H\nW");
    script_test!(newline_2, "H\\newline \\newline W", "H\n\nW");
    script_test!(newline_3, "H\\newline \\newline \\newline W", "H\n\n\nW");
    script_test!(par_1, "H\n\n\nW", "H\n\nW");
    script_test!(par_2, "H\n\n\n\n\nW", "H\n\nW");
}
