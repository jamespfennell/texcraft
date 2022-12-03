//! The `\global`, `\long` and `\outer` prefix commands
//!
//! The `\long` and `\outer` commands are designed to place restrictions on macros
//!    to avoid performance problems.
//! These restrictions are described in the TeXBook.
//! Texlang does not currently impose these restrictions because hardware is much better than in the 70s,
//!   and minimizing the complexity of Texlang's code is more important than stopping TeX users
//!   from writing slow TeX code.
//! However, Texlang does enforce the rule that these prefix commands can only come before
//!  `\def`, `\gdef`, `\edef` and `\xdef`.
//!
//! # Developer notes on `\global`
//!
//! The `\global` command here is pretty much a nightmare to implement.
//! One of the core principles of the Texlang implentation is to remove global state,
//!   but `\global` makes this really hard.
//! The reason is that it changes the behavior, at run time, of a bunch of
//!   other commands like `\def` and `\advance`, and it also changes the semantics
//!   of variable assignment.
//! It is impossible to scope `\global` tightly because of its wide effects.
//!
//! The approach here has two parts.
//!
//! First, for variable assignment, we just reimplement what happens in the VM
//! except we pass a flag that makes the assignment global. This is not too bad
//! as it's only a few lines of code.
//!
//! For commands, it's a little messier.
//! We maintain a component which has a flag `global` that is set to true by
//!   the `\global` command.
//! Commands that can be prefixed with `\global` read the flag and act accordingly.
//! The problem is that we need the global flag to be reset to false
//!   at some point; otherwise, `\global` would make *all* subsequent assignments global.
//! To do this we introduce a convention: any command which can be prefixed
//!   by `\global` reads the flag a single time using the [Component::read_and_reset_global]
//!   method.
//! This method returns the flag value and resets the flag to false.
//!
//! In order for the convention to work correctly it is essential that *all* code
//!   paths within the command call [read_and_reset_global](Component::read_and_reset_global) -
//!   even if they don't use the result!
//! For example `\gdef` always creates a macro in the global scope, but it still needs to
//!   call [read_and_reset_global](Component::read_and_reset_global).
//! This behavior should be verified with unit tests, and this module provides
//!   an [assert_global_is_false](get_assert_global_is_false) execution command
//!   to make this easy.
//!
//! Finally, commands which can be prefixed with `\global` are manually added
//!   to the hash set inside the [Component].
//! This set is used to validate that the command that follows `\global` is
//!   allowed to be prefixed by it.

use crate::def;
use crate::letassignment;
use crate::variableops;
use std::collections::HashSet;
use texcraft_stdext::collections::groupingmap;
use texlang_core::prelude::*;
use texlang_core::variable;

/// Component for the prefix commands.
pub struct Component {
    scope: groupingmap::Scope,
    prefixable_with_any: HashSet<std::any::TypeId>,
    prefixable_with_global: HashSet<std::any::TypeId>,
}

impl Default for Component {
    fn default() -> Self {
        Component {
            scope: groupingmap::Scope::Local,
            prefixable_with_any: vec![def::def_id()].into_iter().collect(),
            prefixable_with_global: vec![
                variableops::get_variable_op_id(),
                letassignment::let_id(),
            ]
            .into_iter()
            .collect(),
        }
    }
}

impl Component {
    /// Read the value of the global flag and reset the flag to false.
    ///
    /// See the module documentation for correct usage of this method.
    pub fn read_and_reset_global(&mut self) -> groupingmap::Scope {
        let scope = self.scope;
        self.scope = groupingmap::Scope::Local;
        scope
    }
}

#[derive(Default, Clone, Copy)]
struct Prefix {
    global: Option<Token>,
    long: Option<Token>,
    outer: Option<Token>,
}

impl Prefix {
    fn get_one(&self) -> (Token, Kind) {
        if let Some(global_token) = self.global {
            (global_token, Kind::Global)
        } else if let Some(long_token) = self.long {
            (long_token, Kind::Long)
        } else if let Some(outer_token) = self.outer {
            (outer_token, Kind::Outer)
        } else {
            panic!("")
        }
    }
}

/// Get the `\global` command.
pub fn get_global<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_execution(global_primitive_fn).with_id(global_id())
}

struct Global;

fn global_id() -> std::any::TypeId {
    std::any::TypeId::of::<Global>()
}

/// Get the `\long` command.
pub fn get_long<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_execution(long_primitive_fn).with_id(long_id())
}

struct Long;

fn long_id() -> std::any::TypeId {
    std::any::TypeId::of::<Long>()
}

/// Get the `\outer` command.
pub fn get_outer<S: HasComponent<Component>>() -> command::Command<S> {
    command::Command::new_execution(outer_primitive_fn).with_id(outer_id())
}

struct Outer;

fn outer_id() -> std::any::TypeId {
    std::any::TypeId::of::<Outer>()
}

fn global_primitive_fn<S: HasComponent<Component>>(
    global_token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    process_prefixes(
        Prefix {
            global: Some(global_token),
            long: None,
            outer: None,
        },
        input,
    )
}

fn long_primitive_fn<S: HasComponent<Component>>(
    long_token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    process_prefixes(
        Prefix {
            global: None,
            long: Some(long_token),
            outer: None,
        },
        input,
    )
}

fn outer_primitive_fn<S: HasComponent<Component>>(
    outer_token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    process_prefixes(
        Prefix {
            global: None,
            long: None,
            outer: Some(outer_token),
        },
        input,
    )
}

fn process_prefixes<S: HasComponent<Component>>(
    mut prefix: Prefix,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    complete_prefix(&mut prefix, input)?;
    match input.peek()? {
        None => {
            let (prefix_token, prefix_kind) = prefix.get_one();
            Err(Error::EndOfInputAfterPrefix {
                end_of_input: input.trace_end_of_input(),
                prefix_token: input.trace(prefix_token),
                prefix_kind,
            }
            .into())
        }
        Some(&t) => match t.value() {
            Value::ControlSequence(name) => {
                // First check if it's a variable command. If so, assign to the variable at the global scope.
                if let Some(command::Fn::Variable(cmd, addr)) =
                    input.base().commands_map.get_fn(&name)
                {
                    let (cmd, addr) = (*cmd, *addr);
                    assert_only_global_prefix(t, prefix, input)?;
                    input.consume()?;
                    let var = command::resolve(cmd, addr, t, input)?;
                    variable::set_using_input(
                        var,
                        input,
                        match prefix.global {
                            None => groupingmap::Scope::Local,
                            Some(_) => groupingmap::Scope::Global,
                        },
                    )?;
                    return Ok(());
                }
                // Next check if it's a command that can be prefixed by any of the prefix command.
                let component = input.state().component();
                let cmd_id = input.base().commands_map.get_id(&name);
                if component.prefixable_with_any.contains(&cmd_id) {
                    input.state_mut().component_mut().scope = match prefix.global {
                        None => groupingmap::Scope::Local,
                        Some(_) => groupingmap::Scope::Global,
                    };
                    return Ok(());
                }
                // Next check if it's a command that can be prefixed by global only. In this case we check
                // that no other prefixes are present.
                if component.prefixable_with_global.contains(&cmd_id) {
                    assert_only_global_prefix(t, prefix, input)?;
                    input.state_mut().component_mut().scope = match prefix.global {
                        None => groupingmap::Scope::Local,
                        Some(_) => groupingmap::Scope::Global,
                    };
                    return Ok(());
                }
                // If we make it to here, this is not a valid target for the prefix command.
                let (prefix_token, prefix_kind) = prefix.get_one();
                Err(Error::CommandCannotBePrefixed {
                    command_token: input.trace(t),
                    prefix_token: input.trace(prefix_token),
                    prefix_kind,
                }
                .into())
            }
            _ => {
                let (prefix_token, prefix_kind) = prefix.get_one();
                Err(Error::TokenCannotBePrefixed {
                    token: input.trace(t),
                    prefix_token: input.trace(prefix_token),
                    prefix_kind,
                }
                .into())
            }
        },
    }
}

fn complete_prefix<S>(
    prefix: &mut Prefix,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    // TODO: spaces and \relax are allowed after prefixes per TeX source sections 1211 and 404.
    let found_prefix = match input.peek()? {
        None => false,
        Some(&t) => match t.value() {
            Value::ControlSequence(name) => {
                let cmd_id = input.base().commands_map.get_id(&name);
                let global_id = global_id();
                let outer_id = outer_id();
                let long_id = long_id();
                // For some reason a match statement doesn't work here.
                if cmd_id == global_id {
                    prefix.global = Some(t);
                    true
                } else if cmd_id == outer_id {
                    prefix.outer = Some(t);
                    true
                } else if cmd_id == long_id {
                    prefix.long = Some(t);
                    true
                } else {
                    false
                }
            }
            _ => false,
        },
    };
    if !found_prefix {
        return Ok(());
    }
    input.consume()?;
    complete_prefix(prefix, input)
}

fn assert_only_global_prefix<S>(
    token: Token,
    prefix: Prefix,
    input: &vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    if let Some(outer_token) = prefix.outer {
        Err(Error::CommandCannotBePrefixed {
            command_token: input.trace(token),
            prefix_token: input.trace(outer_token),
            prefix_kind: Kind::Outer,
        }
        .into())
    } else if let Some(long_token) = prefix.long {
        Err(Error::CommandCannotBePrefixed {
            command_token: input.trace(token),
            prefix_token: input.trace(long_token),
            prefix_kind: Kind::Long,
        }
        .into())
    } else {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    Global,
    Long,
    Outer,
}

/// Error type for the prefix commands.
#[derive(Debug)]
pub enum Error {
    CommandCannotBePrefixed {
        command_token: trace::Trace,
        prefix_token: trace::Trace,
        prefix_kind: Kind,
    },
    TokenCannotBePrefixed {
        token: trace::Trace,
        prefix_token: trace::Trace,
        prefix_kind: Kind,
    },
    EndOfInputAfterPrefix {
        end_of_input: trace::Trace,
        prefix_token: trace::Trace,
        prefix_kind: Kind,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::CommandCannotBePrefixed {
                command_token,
                prefix_token,
                prefix_kind,
            } => {
                error::DisplayBuilder::new(
                    command_token,
                    format!(
                        "the command {} cannot be prefixed with {}",
                        command_token, prefix_token
                    ),
                ).add_note(match prefix_kind {
                    Kind::Global => {
                        r"see the documention for \global for the list of commands this prefix can be used with"
                    }
                    Kind::Long => {
                        r"the \long prefix can only be used with \def, \gdef, \edef and \xdef (or their aliases)"
                    }
                    Kind::Outer => {
                        r"the \outer prefix can only be used with \def, \gdef, \edef and \xdef (or their aliases)"
                    }
                }).fmt(f)
            }
            Error::TokenCannotBePrefixed {
                token,
                prefix_token,
                prefix_kind: _,
            } => {
                error::DisplayBuilder::new(
                    token,
                    format!(
                        "character tokens cannot be prefixed with {}",
                        prefix_token
                    ),
                ).fmt(f)
            }
            Error::EndOfInputAfterPrefix {
                end_of_input,
                prefix_token,
                prefix_kind: _,
            } => {
                error::DisplayBuilder::new(
                    end_of_input,
                    format!(
                        "end of input after prefix command {}",
                        prefix_token
                    ),
                ).add_note(r"prefix commands must be followed by a valid prefix target like \def").fmt(f)
            },
        }
    }
}

impl std::error::Error for Error {}

/// Get an execution command that checks that the global flag is off.
///
/// This command is used for unit testing Texlang.
/// It tests that functions that can be prefixed with `\global`
/// are following the convention described in the module docs.
/// To use it, create a test for the following TeX snippet:
/// ```tex
/// \global \command <input to command> \assertGlobalIsFalse
/// ```
pub fn get_assert_global_is_false<S: HasComponent<Component>>() -> command::Command<S> {
    fn noop_execution_cmd_fn<S: HasComponent<Component>>(
        _: Token,
        input: &mut vm::ExecutionInput<S>,
    ) -> anyhow::Result<()> {
        match input.state_mut().component_mut().read_and_reset_global() {
            groupingmap::Scope::Global => Err(anyhow::anyhow!("assertion failed: global is true")),
            groupingmap::Scope::Local => Ok(()),
        }
    }
    command::Command::new_execution(noop_execution_cmd_fn)
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        script,
        testutil::{self, *},
        the,
    };
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        exec: script::Component,
        prefix: Component,
        integer: i32,
    }

    implement_has_component![State, (script::Component, exec), (Component, prefix),];

    fn setup_expansion_test() -> HashMap<&'static str, command::Command<State>> {
        HashMap::from([
            ("global", get_global()),
            ("long", get_long()),
            ("outer", get_outer()),
            ("i", get_integer().into()),
            ("the", the::get_the()),
            ("def", def::get_def()),
            ("advance", variableops::get_advance()),
            ("noOpExpansion", testutil::get_noop_expansion_cmd()),
            ("noOpExecution", testutil::get_noop_execution_cmd()),
        ])
    }

    fn get_integer() -> command::VariableFn<State> {
        |_, _, _| -> anyhow::Result<variable::Variable<State>> {
            Ok(variable::Variable::Int(variable::TypedVariable::new(
                |state: &State, _: u32| -> &i32 { &state.integer },
                |state: &mut State, _: u32| -> &mut i32 { &mut state.integer },
                0,
            )))
        }
    }

    expansion_test![non_global, r"\i=5{\i=8}\the\i", "5"];
    expansion_test![non_global_2, r"\i=5\i=6{\i=8}\the\i", "6"];
    expansion_test![non_global_3, r"\i=5{\i=6{\i=8 \the\i}\the\i}\the\i", "865"];
    expansion_test![global, r"\i=5{\global\i=8}\the\i", "8"];
    expansion_test![global_squared, r"\i=5{\global\global\i=8}\the\i", "8"];
    expansion_test![long, r"\long\def\A{Hello}\A", "Hello"];
    expansion_test![outer, r"\outer\def\A{Hello}\A", "Hello"];
    expansion_test![
        many_prefixes,
        r"\long\outer\global\long\global\outer\def\A{Hello}\A",
        "Hello"
    ];

    expansion_failure_test![global_end_of_input, r"\global"];
    expansion_failure_test![global_with_character, r"\global a"];
    expansion_failure_test![global_with_undefined_command, r"\global \undefinedCommand"];
    expansion_failure_test![
        global_with_no_op_expansion_command,
        r"\global \noOpExpansion"
    ];
    expansion_failure_test![
        global_with_no_op_execution_command,
        r"\global \noOpExecution"
    ];
    expansion_failure_test![long_prefix_when_global_allowed, r"\long\advance\i 0"];
    expansion_failure_test![outer_prefix_when_global_allowed, r"\outer\advance\i 0"];
}
