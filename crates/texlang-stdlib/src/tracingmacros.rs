//! Macro tracing facility

use colored::*;
use texlang_core::{command, texmacro, token::write_tokens, variable, vm::HasComponent};

/// Component for storing state related to macro tracing.
#[derive(Default)]
pub struct Component {
    tracing_macros: i32,
}

/// Get the `\tracingmacros` command.
pub fn get_tracingmacros<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_variable(
        |_, _, _| -> anyhow::Result<variable::Variable<S>> {
            Ok(variable::Variable::Int(variable::TypedVariable::new(
                |state: &S, _: u32| -> &i32 { &state.component().tracing_macros },
                |state: &mut S, _: u32| -> &mut i32 { &mut state.component_mut().tracing_macros },
                0,
            )))
        },
        0,
    )
}

pub fn hook<S: HasComponent<Component>>(input: texmacro::HookInput<S>) {
    if input.vm.custom_state.component().tracing_macros <= 0 {
        return;
    }
    let trace = input.vm.trace(input.token);
    println!(
        "{}{}",
        "Macro expansion trace of ".bold(),
        trace.value.bold()
    );
    println!("{trace}");
    let interner = input.vm.cs_name_interner();
    println!["                        ┌──",];
    print!["              arguments "];
    if input.arguments.is_empty() {
        print!["│ (none)\n                        "];
    }
    for (i, argument) in input.arguments.iter().enumerate() {
        print![
            "│ {}{}={} \n                        ",
            "#".bright_yellow(),
            (i + 1).to_string().bright_yellow(),
            write_tokens(*argument, interner).bright_yellow()
        ]
    }

    print!["├──\n replacement definition │ ",];
    for replacement in input.tex_macro.replacements() {
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
        write_tokens(input.reverse_expansion.iter().rev(), interner)
    ];
    println!["                        └──"];
}
