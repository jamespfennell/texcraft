//! Expansion and execution driver.

use crate::tex::command::library::conditional;
use crate::tex::command::Command;
use crate::tex::prelude::*;
use crate::tex::token;
use crate::tex::token::catcode::RawCatCode;
use crate::tex::token::lexer;
use crate::tex::token::CsNameInterner;
use crate::tex::token::Token;
use std::collections::HashMap;
use std::io;
use std::mem;
use std::rc;

pub fn exec<S>(
    execution_input: &mut ExecutionInput<S>,
    err_for_undefined_cs: bool,
) -> anyhow::Result<Vec<Token>> {
    let undefined_cs_handler = match err_for_undefined_cs {
        true => default_undefined_cs_handler,
        false => handle_character,
    };
    run(execution_input, handle_character, undefined_cs_handler)?;
    let mut result = Vec::new();
    mem::swap(&mut result, &mut execution_input.base_mut().exec_output);
    Ok(result)
}

pub fn run<S>(
    execution_input: &mut ExecutionInput<S>,
    // TODO: all these handlers should just be regular execution functions and be defined on the base
    character_handler: fn(token::Token, &mut Base<S>) -> anyhow::Result<()>,
    undefined_cs_handler: fn(token::Token, &mut Base<S>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    loop {
        let fully_expanded_token = execution_input.next();
        match fully_expanded_token {
            Ok(token) => match token {
                None => {
                    execution_input.regular().controller_mut().clear();
                    break;
                }
                Some(token) => match token.value() {
                    Character(..) => {
                        character_handler(token, execution_input.base_mut())?;
                    }
                    ControlSequence(_, name) => match execution_input.base().get_command(&name) {
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
                            cmd.call(token, execution_input)?;
                        }
                        Some(Command::Character(c, cat_code)) => {
                            let token = Token::new_character(*c, *cat_code);
                            character_handler(token, execution_input.base_mut())?;
                        }
                        _ => {
                            undefined_cs_handler(token, execution_input.base_mut())?;
                        }
                    },
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

fn handle_character<S>(mut token: token::Token, state: &mut Base<S>) -> anyhow::Result<()> {
    if let token::Value::Character('\n', cat_code) = token.value() {
        token = Token::new_character(' ', cat_code);
    }
    state.exec_output.push(token);
    state.num_trailing_newlines = 0;
    Ok(())
}

fn default_undefined_cs_handler<S>(token: token::Token, state: &mut Base<S>) -> anyhow::Result<()> {
    Err(error::new_undefined_cs_error(token, state))
}

/// Type used for managing input.
pub struct InputController {
    current_source: Source,
    sources: Vec<Source>,

    last_non_empty_line: Option<rc::Rc<token::Line>>,

    pub conditional: conditional::Component,
}

impl InputController {
    pub fn new_with_source(reader: Box<dyn io::BufRead>) -> InputController {
        let mut controller = InputController::new_empty();
        controller.push_new_source(reader);
        controller
    }

    // TODO: AsRef<str>
    pub fn new_with_str<Str: Into<String>>(s: Str) -> InputController {
        InputController::new_with_source(Box::new(io::Cursor::new(s.into())))
    }

    pub fn new_empty() -> InputController {
        InputController {
            current_source: Source::new_empty(),
            sources: Vec::new(),
            last_non_empty_line: None,
            conditional: conditional::Component::new(),
        }
    }

    pub fn push_new_source(&mut self, reader: Box<dyn io::BufRead>) {
        let mut new_source = Source::new(reader);
        std::mem::swap(&mut new_source, &mut self.current_source);
        self.sources.push(new_source);
    }

    pub fn push_new_str<Str: Into<String>>(&mut self, s: Str) {
        self.push_new_source(Box::new(io::Cursor::new(s.into())));
    }

    pub fn push_expansion(&mut self, expansion: &[Token]) {
        self.clear_next_token();
        self.current_source
            .expansions
            .extend(expansion.iter().rev());
    }

    pub fn push_single_token(&mut self, token: Token) {
        self.clear_next_token();
        self.current_source.expansions.push(token);
    }

    pub fn last_non_empty_line(&self) -> Option<rc::Rc<token::Line>> {
        self.last_non_empty_line.clone()
    }

    pub fn expansions_mut(&mut self) -> &mut Vec<Token> {
        self.clear_next_token();
        &mut self.current_source.expansions
    }

    #[inline]
    fn next(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<Option<token::Token>> {
        if let Some(token) = self.current_source.next_token.take() {
            return Ok(Some(token));
        }
        if let Some(token) = self.current_source.expansions.pop() {
            return Ok(Some(token));
        }
        if let Some(token) = self.current_source.root.next(cat_code_map, interner)? {
            return Ok(Some(token));
        }
        self.next_recurse(cat_code_map, interner)
    }

    fn next_recurse(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<Option<token::Token>> {
        if self.pop_source() {
            self.next(cat_code_map, interner)
        } else {
            Ok(None)
        }
    }

    #[inline]
    fn peek(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<Option<&token::Token>> {
        if self.current_source.next_token.is_some() {
            return Ok(self.current_source.next_token.as_ref());
        }
        let has_expanded_token = self.current_source.expansions.last().is_some();
        if has_expanded_token {
            return Ok(self.current_source.expansions.last());
        }
        if let Some(token) = self.current_source.root.next(cat_code_map, interner)? {
            self.current_source.next_token = Some(token);
            return Ok(self.current_source.next_token.as_ref());
        }
        self.peek_recurse(cat_code_map, interner)
    }

    fn peek_recurse(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<Option<&token::Token>> {
        if self.pop_source() {
            self.peek(cat_code_map, interner)
        } else {
            Ok(None)
        }
    }

    fn pop_source(&mut self) -> bool {
        match self.sources.pop() {
            None => {
                // self.current_source = Source::new_empty();
                false
            }
            Some(source) => {
                self.current_source = source;
                true
            }
        }
    }

    fn clear_next_token(&mut self) {
        if let Some(token) = self.current_source.next_token.take() {
            self.current_source.expansions.push(token);
        }
    }

    pub fn clear(&mut self) {
        //self.current_source = Source::new_empty();
        //self.sources = vec![];
    }
}

impl Default for InputController {
    fn default() -> Self {
        Self::new_empty()
    }
}

struct Source {
    next_token: Option<Token>,
    expansions: Vec<Token>,
    root: lexer::Lexer,
}

impl Source {
    fn new(reader: Box<dyn io::BufRead>) -> Source {
        Source {
            next_token: None,
            expansions: Vec::with_capacity(32),
            root: lexer::Lexer::new(reader),
        }
    }

    fn new_empty() -> Source {
        Source::new(Box::new(io::Cursor::new("")))
    }
}

/// Stream that returns input tokens without performing expansion.
///
/// The unexpanded stream is used when reading tokens without performing expansion;
/// e.g., when reading the replacement text for a macro defined using `\def`.
pub struct UnexpandedStream<S> {
    state: Base<S>,
    input: InputController,
}

impl<S> stream::Stream for UnexpandedStream<S> {
    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        let (a, b) = self.state.input_components();
        self.input.next(a, b)
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        let (a, b) = self.state.input_components();
        self.input.peek(a, b)
    }
}

/// Type that provides access to the input stream (with or without expansion) and the state.
///
/// This type satisfies the [Stream](stream::Stream) trait. As a stream, it returns expanded tokens
/// from the input. To read the input without performing expansion, use the
/// [unexpanded_stream](ExpandedInput::unexpanded_stream) method.
pub struct ExpandedInput<S> {
    raw_stream: UnexpandedStream<S>,
}

impl<S> stream::Stream for ExpandedInput<S> {
    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        while self.expand_next()? {}
        self.raw_stream.next()
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        while self.expand_next()? {}
        self.raw_stream.peek()
    }
}

impl<S> ExpandedInput<S> {
    /// Returns a reference to the base state.
    #[inline]
    pub fn base(&self) -> &Base<S> {
        &self.raw_stream.state
    }

    /// Returns a reference to the state.
    ///
    /// For access to the base state, use [base](ExpandedInput::base) instead.
    #[inline]
    pub fn state(&self) -> &S {
        &self.raw_stream.state.state
    }

    /// Returns a reference to the input controller.
    #[inline]
    pub fn controller(&self) -> &InputController {
        &self.raw_stream.input
    }

    /// Returns a mutable reference to the input controller.
    #[inline]
    pub fn controller_mut(&mut self) -> &mut InputController {
        &mut self.raw_stream.input
    }

    /// Returns the unexpanded stream that backs this expanded input.
    ///
    /// The unexpanded stream is used when reading tokens without performing expansion;
    /// e.g., when reading the replacement text for a macro defined using `\def`.
    #[inline]
    pub fn unexpanded_stream(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.raw_stream
    }

    /// Expands the next token in the input stream.
    ///
    /// The return value is true if expansion occured, and false otherwise.
    ///
    /// This method is not recursive. Only a single expansion will be performed. The next
    /// token may be expandable, and to expand it this function needs to be invoked again.
    pub fn expand_next(&mut self) -> anyhow::Result<bool> {
        let command = match self.raw_stream.peek()? {
            None => return Ok(false),
            Some(token) => match token.value() {
                Character(..) => return Ok(false),
                ControlSequence(_, ref name) => self.base().get_command(name),
            },
        };
        let command = match command {
            Some(command::Command::Expansion(command)) => command.clone(),
            _ => return Ok(false),
        };
        let token = self.raw_stream.next()?.unwrap();
        let output = command.call(token, self)?;
        self.controller_mut().push_expansion(&output);
        Ok(true)
    }
}

// I have no idea why aliasing Input<S> to this works; i.e., why the lifetime
// parameter can be elided.
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
    pub fn base(&self) -> &Base<S> {
        &self.raw_stream.raw_stream.state
    }

    #[inline]
    pub fn base_mut(&mut self) -> &mut Base<S> {
        &mut self.raw_stream.raw_stream.state
    }

    #[inline]
    pub fn take_base(self) -> Base<S> {
        self.raw_stream.raw_stream.state
    }

    #[inline]
    pub fn state(&self) -> &S {
        &self.raw_stream.raw_stream.state.state
    }

    #[inline]
    pub fn state_mut(&mut self) -> &mut S {
        &mut self.raw_stream.raw_stream.state.state
    }

    #[inline]
    pub fn controller(&self) -> &InputController {
        &self.raw_stream.raw_stream.input
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

    pub fn new(state: Base<S>) -> ExecutionInput<S> {
        ExecutionInput::<S> {
            raw_stream: ExpandedInput::<S> {
                raw_stream: UnexpandedStream::<S> {
                    state,
                    input: InputController::new_empty(),
                },
            },
        }
    }

    pub fn new_with_source(state: Base<S>, reader: Box<dyn io::BufRead>) -> ExecutionInput<S> {
        ExecutionInput::<S> {
            raw_stream: ExpandedInput::<S> {
                raw_stream: UnexpandedStream::<S> {
                    state,
                    input: InputController::new_with_source(reader),
                },
            },
        }
    }
    pub fn new_with_str<Str: Into<String>>(state: Base<S>, s: Str) -> ExecutionInput<S> {
        ExecutionInput::<S> {
            raw_stream: ExpandedInput::<S> {
                raw_stream: UnexpandedStream::<S> {
                    state,
                    input: InputController::new_with_str(s),
                },
            },
        }
    }
}
