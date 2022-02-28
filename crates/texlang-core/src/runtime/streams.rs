//! Expansion and execution driver.

use crate::prelude::*;
use crate::token;
use crate::token::Token;

/// Stream that returns input tokens without performing expansion.
///
/// The unexpanded stream is used when reading tokens without performing expansion;
/// e.g., when reading the replacement text for a macro defined using `\def`.
pub struct UnexpandedStream<S> {
    env: runtime::Env<S>,
}

/// Trait indicating some part of the state is mutably accesible to expansion commands.
/// 
/// In general only execution commands can mutate the state.
/// Expansion commands cannot.
/// This is important for maintaining the rough invariant that commands either modify
/// the state (execution) or modify the token stream (expansion)
/// but not both.
/// To enforce this, the [ExpandedInput] type does not have a way of returning a mutable
/// reference to the state.
/// 
/// However, there are special situations in which expansion commands do need to maintain
/// some mutable state.
/// Currently, the only example is the collection of conditional commands 
/// in the standard libary (`\ifodd`, `\else`, `\fi`, etc.).
/// These commands maintain a record of the conditional brances that were taken
///     and uses this for error reporting.
/// 
/// This trait enables this use case.
/// The part of the state that can be mutated by expansion commands is called the expansion state.
/// If the state implements this trait, a mutable reference to the expansion state
/// can be obtained through the [ExpandedInput] type's [expansion_state_mut](ExpandedInput::expansion_state_mut) method.
/// 
/// The standard library's [texlang_stdlib::StdLibExpansionState] is an example of this pattern.
pub trait HasExpansionState {
    /// The type of the expansion state.
    type E;

    /// Returns a mutable reference to the expansion state.
    fn expansion_state_mut(&mut self) -> &mut Self::E;
}

impl<S> stream::Stream for UnexpandedStream<S> {
    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        if let Some(token) = self.env.internal.current_source.expansions.pop() {
            return Ok(Some(token));
        }
        if let Some(token) = self.env.internal.current_source.root.next(
            &self.env.base_state.cat_code_map,
            &mut self.env.internal.cs_name_interner,
        )? {
            return Ok(Some(token));
        }
        self.next_recurse()
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        if let Some(token) = self.env.internal.current_source.expansions.last() {
            return Ok(Some(unsafe { launder(token) }));
        }
        if let Some(token) = self.env.internal.current_source.root.next(
            &self.env.base_state.cat_code_map,
            &mut self.env.internal.cs_name_interner,
        )? {
            self.env.internal.current_source.expansions.push(token);
            return Ok(self.env.internal.current_source.expansions.last());
        }
        self.peek_recurse()
    }
}

impl<S: HasExpansionState> UnexpandedStream<S> {
    /// Returns a mutable reference to the expansion state.
    #[inline]
    pub fn expansion_state_mut(&mut self) -> &mut S::E {
        self.env.custom_state.expansion_state_mut()
    }
}

impl<S> UnexpandedStream<S> {
    /// Returns a reference to the environment.
    #[inline]
    pub fn env(&self) -> &runtime::Env<S> {
        &self.env
    }

    /// Returns a reference to the base state.
    #[inline]
    pub fn base(&self) -> &runtime::BaseState<S> {
        &self.env.base_state
    }

    /// Returns a reference to the stack of expanded tokens for the current source.
    #[inline]
    pub fn expansions_mut(&mut self) -> &mut Vec<token::Token> {
        self.env.internal.expansions_mut()
    }

    /// Pushs the references tokens to the top of the stack of expanded tokens for the current source.
    #[inline]
    pub fn push_expansion(&mut self, expansion: &[token::Token]) {
        self.env.internal.push_expansion(expansion)
    }

    fn next_recurse(&mut self) -> anyhow::Result<Option<token::Token>> {
        if self.env.internal.pop_source() {
            self.next()
        } else {
            Ok(None)
        }
    }

    fn peek_recurse(&mut self) -> anyhow::Result<Option<&token::Token>> {
        if self.env.internal.pop_source() {
            self.peek()
        } else {
            Ok(None)
        }
    }
}

/// Provides access to the input stream (with or without expansion) and the state.
///
/// This type satisfies the [Stream](stream::Stream) trait. As a stream, it returns expanded tokens
/// from the input. To read the input without performing expansion, use the
/// [unexpanded_stream](ExpandedInput::unexpanded_stream) method.
pub struct ExpandedInput<S> {
    raw_stream: UnexpandedStream<S>,
    scratch_space: Vec<Token>,
}

impl<S> stream::Stream for ExpandedInput<S> {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        let (token, command) = match self.raw_stream.next()? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (token, self.base().commands_map.get(&name)),
                _ => return Ok(Some(token)),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let output = command.call(token, self)?;
                self.raw_stream.env.internal.push_expansion(&output);
                self.next()
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                command.call(token, self)?;
                self.next()
            }
            _ => Ok(Some(token)),
        }
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        let (token, command) = match self.raw_stream.peek()? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (
                    unsafe { launder(token) },
                    self.raw_stream.env.base_state.commands_map.get(&name),
                ),
                _ => return Ok(Some(unsafe { launder(token) })),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let token = *token;
                let _ = self.raw_stream.next();
                let output = command.call(token, self)?;
                self.raw_stream.env.internal.push_expansion(&output);
                self.peek()
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                let token = *token;
                let _ = self.raw_stream.next();
                command.call(token, self)?;
                self.peek()
            }
            _ => Ok(Some(unsafe { launder(token) })),
        }
    }
}

impl<S: HasExpansionState> ExpandedInput<S> {
    #[inline]
    pub fn expansion_state_mut(&mut self) -> &mut S::E {
        self.raw_stream.env.custom_state.expansion_state_mut()
    }
}

impl<S> ExpandedInput<S> {
    /// Returns a reference to the environment.
    #[inline]
    pub fn env(&self) -> &runtime::Env<S> {
        &self.raw_stream.env
    }

    /// Returns a reference to the base state.
    #[inline]
    pub fn base(&self) -> &runtime::BaseState<S> {
        &self.raw_stream.env.base_state
    }

    /// Returns a reference to the state.
    ///
    /// For access to the base state, use [base](ExpandedInput::base) instead.
    #[inline]
    pub fn state(&self) -> &S {
        &self.raw_stream.env.custom_state
    }

    /// Returns the unexpanded stream that backs this expanded input.
    ///
    /// The unexpanded stream is used when reading tokens without performing expansion;
    /// e.g., when reading the replacement text for a macro defined using `\def`.
    #[inline]
    pub fn unexpanded_stream(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.raw_stream
    }

    pub fn unexpanded_and_scratch_space(&mut self) -> (&mut UnexpandedStream<S>, &mut Vec<Token>) {
        (&mut self.raw_stream, &mut self.scratch_space)
    }

    /// Expands the next token in the input stream.
    ///
    /// The return value is true if expansion occured, and false otherwise.
    ///
    /// This method is not recursive. Only a single expansion will be performed. The next
    /// token may be expandable, and to expand it this function needs to be invoked again.
    pub fn expand_next(&mut self) -> anyhow::Result<bool> {
        let (token, command) = match self.raw_stream.peek()? {
            None => return Ok(false),
            Some(token) => match token.value() {
                ControlSequence(name) => (*token, self.base().commands_map.get(&name)),
                _ => return Ok(false),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let _ = self.raw_stream.next();
                let output = command.call(token, self)?;
                self.raw_stream.env.internal.push_expansion(&output);
                Ok(true)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                let _ = self.raw_stream.next();
                command.call(token, self)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}

pub struct ExecutionInput<S> {
    raw_stream: ExpandedInput<S>,
}

impl<S> stream::Stream for ExecutionInput<S> {
    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        self.raw_stream.next()
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.raw_stream.peek()
    }
}

impl<S> ExecutionInput<S> {
    #[inline]
    pub fn env(&self) -> &runtime::Env<S> {
        &self.raw_stream.raw_stream.env
    }

    #[inline]
    pub fn take_env(self) -> runtime::Env<S> {
        self.raw_stream.raw_stream.env
    }

    #[inline]
    pub fn base(&self) -> &runtime::BaseState<S> {
        &self.raw_stream.raw_stream.env.base_state
    }

    #[inline]
    pub fn base_mut(&mut self) -> &mut runtime::BaseState<S> {
        &mut self.raw_stream.raw_stream.env.base_state
    }

    #[inline]
    pub fn state(&self) -> &S {
        &self.raw_stream.raw_stream.env.custom_state
    }

    #[inline]
    pub fn state_mut(&mut self) -> &mut S {
        &mut self.raw_stream.raw_stream.env.custom_state
    }

    #[inline]
    pub fn unexpanded_stream(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.raw_stream.raw_stream
    }

    #[inline]
    pub fn expand_next(&mut self) -> anyhow::Result<bool> {
        self.raw_stream.expand_next()
    }

    #[inline]
    pub fn regular(&mut self) -> &mut ExpandedInput<S> {
        &mut self.raw_stream
    }

    pub fn new(state: runtime::Env<S>) -> ExecutionInput<S> {
        ExecutionInput::<S> {
            raw_stream: ExpandedInput::<S> {
                raw_stream: UnexpandedStream::<S> { env: state },
                scratch_space: Vec::new(),
            },
        }
    }
}

/// Strips the lifetime from the token.
///
/// This function is intended to get around limitations of the borrow checker only. It
/// should only be used when the code is actually fine but the borrow checker is being
/// too conservative. Don't do anything fancy.
///
/// See this question for the type of code this function is designed for:
/// https://stackoverflow.com/questions/69680201/is-this-use-of-unsafe-trivially-safe
#[inline]
unsafe fn launder<'a, 'b>(token: &'a Token) -> &'b Token {
    &*(token as *const Token)
}
