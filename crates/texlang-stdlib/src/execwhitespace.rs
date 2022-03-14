/// Commands for printing whitespace in exec mode
use texlang_core::prelude::*;
use texlang_core::runtime::HasComponent;

#[derive(Default)]
pub struct Component {
    exec_output: Vec<Token>,
    num_trailing_newlines: usize,
}

/// Get the `\newline` command.
pub fn get_newline<S: HasComponent<Component>>() -> command::ExecutionFn<S> {
    newline_primitive_fn
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
pub fn get_par<S: HasComponent<Component>>() -> command::ExecutionFn<S> {
    par_primitive_fn
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

pub fn exec<S: HasComponent<Component>>(
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
    use super::*;
    use crate::def;
    use crate::testutil::*;

    fn setup_expansion_test(s: &mut runtime::Env<State>) {
        s.set_command("par", get_par());
        s.set_command("def", def::get_def());
        s.set_command("newline", get_newline());
    }

    macro_rules! whitespace_test {
        ($name: ident, $lhs: expr, $rhs: expr) => {
            #[test]
            fn $name() {
                let (output_1, env_1) =
                    crate::testutil::run(setup_expansion_test, $lhs.to_string());
                let output_1 = output_1.unwrap();

                let lhs_output =
                    ::texlang_core::token::write_tokens(&output_1, &env_1.cs_name_interner());
                let rhs_output = $rhs.to_string();

                if lhs_output != rhs_output {
                    println!("Output is different:");
                    println!("------[lhs]------");
                    println!("{}", lhs_output);
                    println!("------[rhs]------");
                    println!("{}", rhs_output);
                    println!("--[lhs-verbose]--");
                    for token in &output_1 {
                        println!("{:?}", token);
                    }
                    println!("-----------------");
                    panic!("Whitespace test failed");
                }
            }
        };
    }

    whitespace_test![
        trim_whitespace_from_start_1,
        r"\def\A{}
\A

Hello",
        r"Hello"
    ];

    whitespace_test![
        trim_whitespace_from_start_2,
        r"Hello

\def\world{World}
\world
",
        r"Hello

World"
    ];
}
