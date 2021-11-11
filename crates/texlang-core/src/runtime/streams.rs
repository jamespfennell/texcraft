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

impl<S> UnexpandedStream<S> {
    /// Returns a reference to the base state.
    #[inline]
    pub fn base(&self) -> &runtime::Env<S> {
        &self.env
    }

    /// Returns a mutable reference to the input controller.
    #[inline]
    pub fn controller_mut(&mut self) -> &mut runtime::ExpansionController {
        &mut self.env.expansion_controller
    }

    #[inline]
    pub fn expansions_mut(&mut self) -> &mut Vec<token::Token> {
        self.env.internal.expansions_mut()
    }

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

/// Type that provides access to the input stream (with or without expansion) and the state.
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

impl<S> ExpandedInput<S> {
    /// Returns a reference to the base state.
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

    /// Returns a reference to the input controller.
    #[inline]
    pub fn controller(&self) -> &runtime::ExpansionController {
        &self.raw_stream.env.expansion_controller
    }

    /// Returns a mutable reference to the input controller.
    #[inline]
    pub fn controller_mut(&mut self) -> &mut runtime::ExpansionController {
        &mut self.raw_stream.env.expansion_controller
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
    pub fn controller(&self) -> &runtime::ExpansionController {
        &self.raw_stream.raw_stream.env.expansion_controller
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
