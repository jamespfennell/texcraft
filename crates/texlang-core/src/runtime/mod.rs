//! The Texlang runtime.
//!
//! This module contains the definition of the runtime environment,
//!     various input streams that wrap the environment,
//!     and the main function that is used to run Texlang.
//! See the runtime documentation in the Texlang book for full documentation.

use super::token::CsName;
use crate::command::Command;
use crate::command::CommandsMap;
use crate::error;
use crate::token::catcode::CatCodeMap;
use crate::token::lexer;
use crate::token::CsNameInterner;
use crate::token::Token;
use crate::token::TracebackId;
use crate::token::Value::ControlSequence;
use crate::variable;
use std::collections::HashMap;

pub mod conditional;
mod streams;
pub use streams::ExecutionInput;
pub use streams::ExpandedInput;
pub use streams::TokenStream;
pub use streams::UnexpandedStream;

pub fn run<S>(
    execution_input: &mut ExecutionInput<S>,
    character_handler: fn(Token, &mut ExecutionInput<S>) -> anyhow::Result<()>,
    undefined_cs_handler: fn(Token, &mut ExecutionInput<S>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    loop {
        let fully_expanded_token = execution_input.next();
        match fully_expanded_token {
            Ok(token) => match token {
                None => {
                    break;
                }
                Some(token) => match token.value() {
                    ControlSequence(name) => {
                        match execution_input.base().commands_map.get(&name) {
                            Some(Command::Execution(cmd_ref)) => {
                                // We need to copy the command to avoid a borrow checker error. This is because
                                // `cmd_ref` keeps an immutable reference to the state alive, but in the `call`
                                // invocation below we pass in a mutable reference to the state. The borrow
                                // checker is actually surfacing a genuine TeX edge case here: if you input
                                // something like \let\let=\def then the reference to the command \let in the
                                // commands map is overwritten at some point during the execution of \let. You
                                // could also imagine implementing an \undef command that would result in the
                                // reference becoming null during the execution.
                                let cmd = *cmd_ref;
                                cmd.call(token, execution_input)?;
                            }
                            Some(Command::Variable(cmd_ref)) => {
                                let cmd = *cmd_ref;
                                let var = cmd.resolve(token, execution_input.regular())?;
                                variable::set(var, token, execution_input)?;
                            }
                            Some(Command::Character(token)) => {
                                character_handler(*token, execution_input)?;
                            }
                            None | Some(Command::Expansion(_)) | Some(Command::Macro(_)) => {
                                undefined_cs_handler(token, execution_input)?;
                            }
                        }
                    }
                    _ => {
                        character_handler(token, execution_input)?;
                    }
                },
            },
            Err(mut error) => {
                error::add_context(&mut error, execution_input);
                return Err(error);
            }
        };
    }
    Ok(())
}

pub fn default_undefined_cs_handler<S>(
    token: Token,
    input: &mut streams::ExecutionInput<S>,
) -> anyhow::Result<()> {
    Err(error::new_undefined_cs_error(token, input.env()))
}

/// The Texlang runtime environment.
///
/// This is sort of a God object that is passed around all of Texcraft code.
/// However we have many strategies for limiting visibility of various parts of it;
///   see the runtime documentation.
pub struct Env<S> {
    pub base_state: BaseState<S>,
    pub custom_state: S,
    pub expansion_controller: ExpansionController,
    pub internal: InternalEnv,
}

/// Parts of the state that are required in every Texlang environment, such as the commands map.
///
/// Note that state means specifically parts of the environment that can be edited by
/// exectution commands.
pub struct BaseState<S> {
    pub cat_code_map: CatCodeMap,
    pub commands_map: CommandsMap<S>,
    pub tracing_macros: i32,
}

impl<S> BaseState<S> {
    pub fn new(cat_code_map: CatCodeMap) -> BaseState<S> {
        BaseState {
            cat_code_map,
            commands_map: Default::default(),
            tracing_macros: 0,
        }
    }
}

impl<S> Env<S> {
    /// Create a new runtime environment.
    pub fn new(initial_cat_codes: CatCodeMap, state: S) -> Env<S> {
        Env {
            custom_state: state,
            base_state: BaseState::new(initial_cat_codes),
            expansion_controller: Default::default(),
            internal: Default::default(),
        }
    }

    /// Add new source code to the environment.
    ///
    /// TeX input source code is organized as a stack.
    /// Pushing source code onto the stack will mean it is executed first.
    pub fn push_source(&mut self, source_code: String) {
        self.internal.push_source(source_code);
    }

    /// Set a command.
    pub fn set_command<A: AsRef<str>, B: Into<Command<S>>>(&mut self, name: A, cmd: B) {
        self.base_state.commands_map.insert(
            self.internal.cs_name_interner.get_or_intern(name),
            B::into(cmd),
        )
    }

    /// Return a regular hash map with all the commands as they are currently defined.
    ///
    /// This function is extremely slow and is only intended to be invoked on error paths.
    pub fn get_commands_as_map(&self) -> HashMap<String, Command<S>> {
        let map_1: HashMap<CsName, Command<S>> = self.base_state.commands_map.to_hash_map();
        let mut map = HashMap::new();
        for (cs_name, cmd) in map_1 {
            let cs_name_str = match self.internal.cs_name_interner.resolve(&cs_name) {
                None => continue,
                Some(cs_name_str) => cs_name_str,
            };
            map.insert(cs_name_str.to_string(), cmd);
        }
        map
    }

    /// Return a reference to the environment control sequence name string interner.
    ///
    /// This interner can be used to resolve [CsName] types into regular strings.
    #[inline]
    pub fn cs_name_interner(&self) -> &CsNameInterner {
        &self.internal.cs_name_interner
    }
}

/// Parts of the runtime environment that are only visible to the runtime itself.
#[derive(Default)]
pub struct InternalEnv {
    // The sources form a stack. We store the top element directly on the env
    // for performance reasons.
    current_source: Source,
    sources: Vec<Source>,

    cs_name_interner: CsNameInterner,
    traceback_checkpoints: HashMap<TracebackId, String>,
    next_free_traceback_id: TracebackId,
}

impl InternalEnv {
    fn push_source(&mut self, source_code: String) {
        self.traceback_checkpoints
            .insert(self.next_free_traceback_id, source_code.clone());
        let source_code_len = source_code.len();
        let mut new_source = Source::new(source_code, self.next_free_traceback_id);
        self.next_free_traceback_id += TracebackId::try_from(source_code_len).unwrap();
        std::mem::swap(&mut new_source, &mut self.current_source);
        self.sources.push(new_source);
    }

    #[inline]
    fn push_expansion(&mut self, expansion: &[Token]) {
        self.current_source
            .expansions
            .extend(expansion.iter().rev());
    }

    #[inline]
    fn expansions_mut(&mut self) -> &mut Vec<Token> {
        &mut self.current_source.expansions
    }

    fn pop_source(&mut self) -> bool {
        match self.sources.pop() {
            None => false,
            Some(source) => {
                self.current_source = source;
                true
            }
        }
    }
}

struct Source {
    expansions: Vec<Token>,
    root: lexer::Lexer,
}

impl Source {
    pub fn new(source_code: String, next_free_traceback_id: TracebackId) -> Source {
        Source {
            expansions: Vec::with_capacity(32),
            root: lexer::Lexer::new(source_code, next_free_traceback_id),
        }
    }
}

impl Default for Source {
    fn default() -> Self {
        Source::new("".to_string(), 0)
    }
}

/// Type used for managing expansion.
#[derive(Default)]
pub struct ExpansionController {
    pub conditional: conditional::Component,
}
