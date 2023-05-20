//! TeX macro debugging

use colored::*;
use texlang_core::{command, texmacro, token::write_tokens, traits::*, variable, vm};

/// Component for storing state related to macro tracing.
#[derive(Default)]
pub struct Component {
    tracing_macros: i32,
}

/// Get the `\tracingmacros` command.
pub fn get_tracingmacros<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _: variable::Index| -> &i32 { &state.component().tracing_macros },
        |state: &mut S, _: variable::Index| -> &mut i32 {
            &mut state.component_mut().tracing_macros
        },
    )
    .into()
}

pub fn hook<S: HasComponent<Component>>(
    token: texlang_core::token::Token,
    input: &vm::ExpansionInput<S>,
    tex_macro: &texlang_core::texmacro::Macro,
    arguments: &[&[texlang_core::token::Token]],
    reversed_expansion: &[texlang_core::token::Token],
) {
    if input.state().component().tracing_macros <= 0 {
        return;
    }
    let trace = input.trace(token);
    println!(
        "{}{}",
        "Macro expansion trace of ".bold(),
        trace.value.bold()
    );
    println!("{trace}");
    let interner = input.vm().cs_name_interner();
    println!["                        ┌──",];
    print!["              arguments "];
    if arguments.is_empty() {
        print!["│ (none)\n                        "];
    }
    for (i, argument) in arguments.iter().enumerate() {
        print![
            "│ {}{}={} \n                        ",
            "#".bright_yellow(),
            (i + 1).to_string().bright_yellow(),
            write_tokens(*argument, interner).bright_yellow()
        ]
    }

    print!["├──\n replacement definition │ ",];
    for replacement in tex_macro.replacements() {
        match replacement {
            texmacro::Replacement::Tokens(tokens) => {
                print!("{}", write_tokens(tokens.iter().rev(), interner))
            }
            texmacro::Replacement::Parameter(i) => {
                print!(
                    "{}{}",
                    "#".bright_yellow(),
                    (i + 1).to_string().bright_yellow(),
                )
            }
        }
    }

    println![
        "\n                        ├──\n              expansion │ {}",
        write_tokens(reversed_expansion.iter().rev(), interner)
    ];
    println!["                        └──"];
}
