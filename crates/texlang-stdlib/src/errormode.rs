//! Commands that control what to do when errors occur
//!
//! The commands in this module have ownership both over the recoverable error
//! handler and the input terminal.

use std::cell::RefCell;
use std::rc::Rc;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;
use texlang_common as common;

#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    mode: Mode,
    #[cfg_attr(feature = "serde", serde(skip))]
    default_terminal: DefaultTerminal,
    errors: RefCell<Vec<error::Error>>,
}

impl Component {
    /// Set the default terminal.
    pub fn set_default_terminal(&mut self, default_terminal: Rc<RefCell<dyn common::TerminalIn>>) {
        self.default_terminal = DefaultTerminal(default_terminal);
    }
}

/// Wrapper type so we can implement the default trait.
struct DefaultTerminal(Rc<RefCell<dyn common::TerminalIn>>);

impl Default for DefaultTerminal {
    fn default() -> Self {
        Self(Rc::new(RefCell::new(std::io::stdin())))
    }
}

impl common::HasTerminalIn for Component {
    fn terminal_in(&self) -> Rc<RefCell<dyn common::TerminalIn>> {
        match self.mode {
            Mode::ErrorStop | Mode::Scroll => self.default_terminal.0.clone(),
            Mode::Batch | Mode::NonStop => Rc::new(RefCell::new(DisabledTerminalIn {})),
        }
    }
}

#[derive(Default, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum Mode {
    #[default]
    ErrorStop,
    Scroll,
    NonStop,
    Batch,
}

/// Get the `\errorstopmode` command.
pub fn get_errorstopmode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(|_, input: &mut vm::ExecutionInput<S>| {
        set_mode(input, Mode::ErrorStop);
        Ok(())
    })
}

/// Get the `\scrollmode` command.
pub fn get_scrollmode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(|_, input: &mut vm::ExecutionInput<S>| {
        set_mode(input, Mode::Scroll);
        Ok(())
    })
}

/// Get the `\nonstopmode` command.
pub fn get_nonstopmode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(|_, input: &mut vm::ExecutionInput<S>| {
        set_mode(input, Mode::NonStop);
        Ok(())
    })
}

/// Get the `\batchmode` command.
pub fn get_batchmode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(|_, input: &mut vm::ExecutionInput<S>| {
        set_mode(input, Mode::Batch);
        Ok(())
    })
}

pub fn recoverable_error_hook<S: HasComponent<Component> + common::HasLogging>(
    state: &S,
    recoverable_error: Box<error::Error>,
) -> txl::Result<()> {
    match &state.component().mode {
        Mode::ErrorStop => {
            return Err(recoverable_error);
        }
        Mode::Scroll | Mode::NonStop => {
            writeln!(state.terminal_out().borrow_mut(), "{recoverable_error}").unwrap();
        }
        Mode::Batch => {}
    }
    writeln!(state.log_file().borrow_mut(), "{recoverable_error}").unwrap();
    state
        .component()
        .errors
        .borrow_mut()
        .push(*recoverable_error);
    Ok(())
}

fn set_mode<S: HasComponent<Component>>(input: &mut vm::ExecutionInput<S>, mode: Mode) {
    input.state_mut().component_mut().mode = mode;
}

struct DisabledTerminalIn;

impl common::TerminalIn for DisabledTerminalIn {
    fn read_line(&mut self, _: Option<&str>, _: &mut String) -> std::io::Result<()> {
        todo!()
    }
}
