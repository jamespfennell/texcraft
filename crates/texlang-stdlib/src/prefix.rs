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
//! One of the core principles of the Texlang implementation is to remove global state,
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

use crate::alias;
use crate::def;
use crate::math;
use std::collections::HashSet;
use texcraft_stdext::collections::groupingmap;
use texlang_core::token::trace;
use texlang_core::traits::*;
use texlang_core::*;

/// Component for the prefix commands.
pub struct Component {
    scope: groupingmap::Scope,
    global_defs_value: i32,
    can_be_prefixed_with_any: HashSet<command::Tag>,
    can_be_prefixed_with_global: HashSet<command::Tag>,
    global_tag: command::Tag,
    long_tag: command::Tag,
    outer_tag: command::Tag,
}

impl Default for Component {
    fn default() -> Self {
        Component {
            scope: groupingmap::Scope::Local,
            global_defs_value: 0,
            can_be_prefixed_with_any: vec![def::def_tag()].into_iter().collect(),
            can_be_prefixed_with_global: vec![math::get_variable_op_tag(), alias::let_tag()]
                .into_iter()
                .collect(),
            global_tag: GLOBAL_TAG.get(),
            long_tag: LONG_TAG.get(),
            outer_tag: OUTER_TAG.get(),
        }
    }
}

impl Component {
    /// Read the value of the global flag and reset the flag to false.
    ///
    /// See the module documentation for correct usage of this method.
    #[inline]
    pub fn read_and_reset_global(&mut self) -> groupingmap::Scope {
        match self.global_defs_value.cmp(&0) {
            std::cmp::Ordering::Less => groupingmap::Scope::Local,
            std::cmp::Ordering::Equal => {
                std::mem::replace(&mut self.scope, groupingmap::Scope::Local)
            }
            std::cmp::Ordering::Greater => groupingmap::Scope::Global,
        }
    }

    fn set_scope(&mut self, scope: groupingmap::Scope) {
        self.scope = if self.global_defs_value == 0 {
            scope
        } else {
            // If either globaldefs override is enabled we skip setting the scope here so that
            // we can avoid setting it in the (hotter) variable assignment path.
            groupingmap::Scope::Local
        }
    }
}

#[derive(Default, Clone, Copy)]
struct Prefix {
    global: Option<token::Token>,
    long: Option<token::Token>,
    outer: Option<token::Token>,
}

impl Prefix {
    fn get_one(&self) -> (token::Token, Kind) {
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

/// Get the `\globaldefs` command.
pub fn get_globaldefs<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_variable(variable::Command::new_singleton(
        |s, _| &s.component().global_defs_value,
        |s, _| &mut s.component_mut().global_defs_value,
    ))
}

/// Get the `\global` command.
pub fn get_global<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(global_primitive_fn).with_tag(GLOBAL_TAG.get())
}

static GLOBAL_TAG: command::StaticTag = command::StaticTag::new();

#[inline]
pub fn variable_assignment_scope_hook<S: HasComponent<Component>>(
    state: &mut S,
) -> groupingmap::Scope {
    state.component_mut().read_and_reset_global()
}

// Get the `\long` command.
pub fn get_long<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(long_primitive_fn).with_tag(LONG_TAG.get())
}

static LONG_TAG: command::StaticTag = command::StaticTag::new();

/// Get the `\outer` command.
pub fn get_outer<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(outer_primitive_fn).with_tag(OUTER_TAG.get())
}

static OUTER_TAG: command::StaticTag = command::StaticTag::new();

fn global_primitive_fn<S: HasComponent<Component>>(
    global_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
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
    long_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
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
    outer_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
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
) -> command::Result<()> {
    complete_prefix(&mut prefix, input)?;
    match input.peek()? {
        None => Err(error::SimpleEndOfInputError::new(
            input.vm(),
            "end of input while looking for a command to prefix",
        )
        .with_note(
            r"prefix commands (\global, \long, \outer) must be followed by a command to prefix",
        )
        .into()),
        Some(&t) => match t.value() {
            token::Value::ControlSequence(name) => {
                // First check if it's a variable command.
                if let Some(command::Command::Variable(_)) =
                    input.commands_map_mut().get_command(&name)
                {
                    assert_only_global_prefix(t, prefix, input)?;
                    if prefix.global.is_some() {
                        input
                            .state_mut()
                            .component_mut()
                            .set_scope(groupingmap::Scope::Global);
                    }
                    return Ok(());
                }
                // Next check if it's a command that can be prefixed by any of the prefix command.
                let component = input.state().component();
                let tag = input.commands_map().get_tag(&name);
                if let Some(tag) = tag {
                    if component.can_be_prefixed_with_any.contains(&tag) {
                        if prefix.global.is_some() {
                            input
                                .state_mut()
                                .component_mut()
                                .set_scope(groupingmap::Scope::Global);
                        }
                        return Ok(());
                    }
                    // Next check if it's a command that can be prefixed by global only. In this case we check
                    // that no other prefixes are present.
                    if component.can_be_prefixed_with_global.contains(&tag) {
                        assert_only_global_prefix(t, prefix, input)?;
                        if prefix.global.is_some() {
                            input
                                .state_mut()
                                .component_mut()
                                .set_scope(groupingmap::Scope::Global);
                        }
                        return Ok(());
                    }
                }
                // If we make it to here, this is not a valid target for the prefix command.
                let (prefix_token, kind) = prefix.get_one();
                Err(Error {
                    kind,
                    got: input.vm().trace(t),
                    prefix: input.vm().trace(prefix_token),
                }
                .into())
            }
            _ => {
                let (prefix_token, kind) = prefix.get_one();
                Err(Error {
                    kind,
                    got: input.vm().trace(t),
                    prefix: input.vm().trace(prefix_token),
                }
                .into())
            }
        },
    }
}

fn complete_prefix<S: HasComponent<Component>>(
    prefix: &mut Prefix,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    // BUG: spaces and \relax are allowed after prefixes per TeX source sections 1211 and 404.
    let found_prefix = match input.peek()? {
        None => false,
        Some(&t) => match t.value() {
            token::Value::ControlSequence(name) => {
                let tag = input.commands_map().get_tag(&name);
                if tag == Some(input.state().component().global_tag) {
                    prefix.global = Some(t);
                    true
                } else if tag == Some(input.state().component().outer_tag) {
                    prefix.outer = Some(t);
                    true
                } else if tag == Some(input.state().component().long_tag) {
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

fn assert_only_global_prefix<S: TexlangState>(
    token: token::Token,
    prefix: Prefix,
    input: &vm::ExecutionInput<S>,
) -> command::Result<()> {
    if let Some(outer_token) = prefix.outer {
        Err(Error {
            kind: Kind::Outer,
            got: input.vm().trace(token),
            prefix: input.vm().trace(outer_token),
        }
        .into())
    } else if let Some(long_token) = prefix.long {
        Err(Error {
            kind: Kind::Long,
            got: input.vm().trace(token),
            prefix: input.vm().trace(long_token),
        }
        .into())
    } else {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
enum Kind {
    Global,
    Long,
    Outer,
}

#[derive(Debug)]
struct Error {
    kind: Kind,
    got: trace::SourceCodeTrace,
    prefix: trace::SourceCodeTrace,
}

impl error::TexError for Error {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(&self.got)
    }

    fn title(&self) -> String {
        match self.got.token.unwrap().value() {
            token::Value::ControlSequence(_) => {
                format!["this command cannot be prefixed by {}", self.prefix.value]
            }
            _ => format![
                "character tokens cannot be prefixed by {}",
                self.prefix.value
            ],
        }
    }

    fn source_annotation(&self) -> String {
        format!["cannot by prefixed by {}", self.prefix.value]
    }

    fn notes(&self) -> Vec<error::display::Note> {
        let guidance = match self.kind {
            Kind::Global => {
                r"see the documentation for \global for the list of commands it can be used with"
            }
            Kind::Long => {
                r"the \long prefix can only be used with \def, \gdef, \edef and \xdef (or their aliases)"
            }
            Kind::Outer => {
                r"the \outer prefix can only be used with \def, \gdef, \edef and \xdef (or their aliases)"
            }
        };
        vec![
            guidance.into(),
            error::display::Note::SourceCodeTrace("the prefix appeared here:".into(), &self.prefix),
        ]
    }
}

/// Get an execution command that checks that the global flag is off.
///
/// This command is used for unit testing Texlang.
/// It tests that functions that can be prefixed with `\global`
/// are following the convention described in the module docs.
/// To use it, create a test for the following TeX snippet:
/// ```tex
/// \global \command <input to command> \assertGlobalIsFalse
/// ```
pub fn get_assert_global_is_false<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    fn noop_execution_cmd_fn<S: HasComponent<Component>>(
        token: token::Token,
        input: &mut vm::ExecutionInput<S>,
    ) -> command::Result<()> {
        match input.state_mut().component_mut().read_and_reset_global() {
            groupingmap::Scope::Global => Err(error::SimpleTokenError::new(
                input.vm(),
                token,
                "assertion failed: global is true",
            )
            .into()),
            groupingmap::Scope::Local => Ok(()),
        }
    }
    command::BuiltIn::new_execution(noop_execution_cmd_fn)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{script, testing::*, the};
    use std::collections::HashMap;
    use texlang_core::variable;
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        script: script::Component,
        prefix: Component,
        integer: i32,
    }

    impl TexlangState for State {
        fn variable_assignment_scope_hook(state: &mut Self) -> groupingmap::Scope {
            variable_assignment_scope_hook(state)
        }
    }

    implement_has_component![State, (script::Component, script), (Component, prefix),];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("global", get_global()),
            ("globaldefs", get_globaldefs()),
            ("long", get_long()),
            ("outer", get_outer()),
            ("i", get_integer()),
            ("the", the::get_the()),
            ("def", def::get_def()),
            ("advance", math::get_advance()),
            (
                "noOpExpansion",
                command::BuiltIn::new_expansion(|_, _| Ok(vec![])),
            ),
            (
                "noOpExecution",
                command::BuiltIn::new_execution(|_, _| Ok(())),
            ),
        ])
    }

    fn get_integer() -> command::BuiltIn<State> {
        variable::Command::new_singleton(
            |state: &State, _: variable::Index| -> &i32 { &state.integer },
            |state: &mut State, _: variable::Index| -> &mut i32 { &mut state.integer },
        )
        .into()
    }

    test_suite![
        expansion_equality_tests(
            (non_global, r"\i=5{\i=8}\the\i", "5"),
            (non_global_2, r"\i=5\i=6{\i=8}\the\i", "6"),
            (non_global_3, r"\i=5{\i=6{\i=8 \the\i}\the\i}\the\i", "865"),
            (global, r"\i=5{\global\i=8}\the\i", "8"),
            (global_squared, r"\i=5{\global\global\i=8}\the\i", "8"),
            (long, r"\long\def\A{Hello}\A", "Hello"),
            (outer, r"\outer\def\A{Hello}\A", "Hello"),
            (
                many_prefixes,
                r"\long\outer\global\long\global\outer\def\A{Hello}\A",
                "Hello"
            ),
            (global_defs_1, r"\i=5{\globaldefs=1 \i=8}\the\i", "8"),
            (global_defs_2, r"\i=5{\globaldefs=-1\global\i=8}\the\i", "5"),
        ),
        failure_tests(
            (global_end_of_input, r"\global"),
            (global_with_character, r"\global a"),
            (global_with_undefined_command, r"\global \undefinedCommand"),
            (
                global_with_no_op_expansion_command,
                r"\global \noOpExpansion"
            ),
            (
                global_with_no_op_execution_command,
                r"\global \noOpExecution"
            ),
            (long_prefix_when_global_allowed, r"\long\advance\i 0"),
            (outer_prefix_when_global_allowed, r"\outer\advance\i 0"),
        ),
    ];
}
