//! Support for running TeX scripts.
//!
//! This module enables using TeX as a scripting language.
//! TeX files are processed using the usual TeX semantics, but instead
//! of typesetting the result and outputting it to PDF (say), the output is written to an IO writer.

use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use texlang::traits::*;
use texlang::*;

pub struct Component {
    io_writer: Rc<RefCell<dyn std::io::Write>>,
    writer: token::Writer,
}

impl Default for Component {
    fn default() -> Self {
        Self {
            io_writer: Rc::new(RefCell::new(std::io::sink())),
            writer: Default::default(),
        }
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Component {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        ().serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Component {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <()>::deserialize(deserializer)?;
        Ok(Default::default())
    }
}

impl Component {
    fn write_token<S: HasComponent<Self>>(input: &mut vm::ExecutionInput<S>, token: token::Token) {
        let vm::Parts {
            state,
            cs_name_interner,
            ..
        } = input.vm_parts();
        let c = state.component_mut();
        c.writer
            .write(
                c.io_writer.borrow_mut().deref_mut(),
                cs_name_interner,
                token,
            )
            .unwrap()
    }
}

/// Get the `\newline` command.
///
/// This adds a newline to the output.
pub fn get_newline<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newline_primitive_fn)
}

fn newline_primitive_fn<S: HasComponent<Component>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    input.state_mut().component_mut().writer.add_newline();
    Ok(())
}

/// Get the `\par` command.
///
/// The `\par` command adds two newlines to the output.
/// Consecutive `\par` commands are treated as one.
pub fn get_par<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(par_primitive_fn)
}

fn par_primitive_fn<S: HasComponent<Component>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    input.state_mut().component_mut().writer.start_paragraph();
    Ok(())
}

/// Run the Texlang interpreter for the provided VM and return the result as a string.
pub fn run_to_string<S: HasComponent<Component>>(
    vm: &mut vm::VM<S>,
) -> Result<String, Box<error::Error>> {
    let buffer = Rc::new(RefCell::new(Vec::<u8>::new()));
    vm.state.component_mut().io_writer = buffer.clone();
    run(vm)?;
    let result: String = std::str::from_utf8(buffer.borrow().deref()).unwrap().into();
    Ok(result)
}

/// Run the Texlang interpreter for the provided VM and return the result as list of tokens.
pub fn run<S: HasComponent<Component>>(vm: &mut vm::VM<S>) -> Result<(), Box<error::Error>> {
    vm.run::<Handlers>()
}

// Set the IO writer that the script component writes to.
pub fn set_io_writer<S: HasComponent<Component>, I: std::io::Write + 'static>(
    vm: &mut vm::VM<S>,
    writer: I,
) {
    vm.state.component_mut().io_writer = Rc::new(RefCell::new(writer))
}

struct Handlers;

impl<S: HasComponent<Component>> vm::Handlers<S> for Handlers {
    fn character_handler(
        input: &mut vm::ExecutionInput<S>,
        token: token::Token,
        _: char,
    ) -> command::Result<()> {
        // TODO: it's really not great that the character is ignored.
        Component::write_token(input, token);
        Ok(())
    }

    fn unexpanded_expansion_command(
        input: &mut vm::ExecutionInput<S>,
        token: token::Token,
    ) -> command::Result<()> {
        Component::write_token(input, token);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[derive(Default)]
    struct State {
        script: Component,
    }

    impl vm::TexlangState for State {}

    implement_has_component![State { script: Component }];

    fn run_script_test(input: &str, want: &str) {
        let built_ins = HashMap::from([("par", get_par()), ("newline", get_newline())]);
        let mut vm = vm::VM::<State>::new(built_ins);
        vm.push_source("testing.tex", input).unwrap();
        let got = run_to_string(&mut vm).unwrap();
        let want = want.to_string();

        if got != want {
            println!("Output is different:");
            println!("------[got]-------");
            println!("{}", got);
            println!("------[want]------");
            println!("{}", want);
            println!("-----------------");
            panic!("run_script test failed");
        }
    }

    macro_rules! script_tests {
        ( $( ($name: ident, $input: expr, $want: expr) ),* $(,)? ) => {
            $(
            #[test]
            fn $name() {
              run_script_test($input, $want);
            }
            )*
        };
    }

    script_tests![
        (char_newline_1, "H\nW", "H W"),
        (newline_1, "H\\newline W", "H\nW"),
        (newline_2, "H\\newline \\newline W", "H\n\nW"),
        (newline_3, "H\\newline \\newline \\newline W", "H\n\n\nW"),
        (par_1, "H\n\n\nW", "H\n\nW"),
        (par_2, "H\n\n\n\n\nW", "H\n\nW"),
    ];
}
