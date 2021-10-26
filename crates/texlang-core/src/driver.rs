//! Expansion and execution driver.

use crate::command::Command;
use crate::prelude::*;
use crate::state::InputRelatedState;
use crate::token;
use crate::token::lexer;

use crate::token::Token;
use crate::variable;

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
                    ControlSequence(name) => match execution_input.base().get_command(&name) {
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
                            character_handler(*token, execution_input.base_mut())?;
                        }
                        None | Some(Command::Expansion(_)) | Some(Command::Macro(_)) => {
                            undefined_cs_handler(token, execution_input.base_mut())?;
                        }
                    },
                    _ => {
                        character_handler(token, execution_input.base_mut())?;
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

fn handle_character<S>(mut token: token::Token, state: &mut Base<S>) -> anyhow::Result<()> {
    if let Some('\n') = token.char() {
        token = Token::new_space(' ');
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

    #[inline]
    pub fn push_expansion(&mut self, expansion: &[Token]) {
        self.current_source
            .expansions
            .extend(expansion.iter().rev());
    }

    #[inline]
    pub fn push_single_token(&mut self, token: Token) {
        self.current_source.expansions.push(token);
    }

    pub fn last_non_empty_line(&self) -> Option<rc::Rc<token::Line>> {
        self.last_non_empty_line.clone()
    }

    #[inline]
    pub fn expansions_mut(&mut self) -> &mut Vec<Token> {
        &mut self.current_source.expansions
    }

    #[inline]
    fn next(
        &mut self,
        input_related_state: &mut InputRelatedState,
    ) -> anyhow::Result<Option<token::Token>> {
        if let Some(token) = self.current_source.expansions.pop() {
            return Ok(Some(token));
        }
        if let Some(token) = self.current_source.root.next(input_related_state)? {
            return Ok(Some(token));
        }
        self.next_recurse(input_related_state)
    }

    fn next_recurse(
        &mut self,
        input_related_state: &mut InputRelatedState,
    ) -> anyhow::Result<Option<token::Token>> {
        if self.pop_source() {
            self.next(input_related_state)
        } else {
            Ok(None)
        }
    }

    #[inline]
    fn peek(
        &mut self,
        input_related_state: &mut InputRelatedState,
    ) -> anyhow::Result<Option<&token::Token>> {
        if let Some(token) = self.current_source.expansions.last() {
            return Ok(Some(unsafe { launder(token) }));
        }
        if let Some(token) = self.current_source.root.next(input_related_state)? {
            self.current_source.expansions.push(token);
            return Ok(self.current_source.expansions.last());
        }
        self.peek_recurse(input_related_state)
    }

    fn peek_recurse(
        &mut self,
        input_related_state: &mut InputRelatedState,
    ) -> anyhow::Result<Option<&token::Token>> {
        if self.pop_source() {
            self.peek(input_related_state)
        } else {
            Ok(None)
        }
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
    expansions: Vec<Token>,
    root: lexer::Lexer,
}

impl Source {
    fn new(reader: Box<dyn io::BufRead>) -> Source {
        Source {
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
        self.input.next(self.state.input_related())
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.input.peek(self.state.input_related())
    }
}

impl<S> UnexpandedStream<S> {
    /// Returns a reference to the base state.
    #[inline]
    pub fn base(&self) -> &Base<S> {
        &self.state
    }

    /// Returns a mutable reference to the input controller.
    #[inline]
    pub fn controller_mut(&mut self) -> &mut InputController {
        &mut self.input
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
                ControlSequence(name) => (token, self.base().get_command(&name)),
                _ => return Ok(Some(token)),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let output = command.call(token, self)?;
                self.controller_mut().push_expansion(&output);
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
        let (input_related_state, commands_map) =
            self.raw_stream.state.input_related_and_commands();
        let (token, command) = match self.raw_stream.input.peek(input_related_state)? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (token, commands_map.get(&name.to_usize())),
                _ => return Ok(Some(unsafe { launder(token) })),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let token = *token;
                let _ = self.raw_stream.next();
                let output = command.call(token, self)?;
                self.controller_mut().push_expansion(&output);
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
                ControlSequence(name) => (*token, self.base().get_command(&name)),
                _ => return Ok(false),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let _ = self.raw_stream.next();
                let output = command.call(token, self)?;
                self.controller_mut().push_expansion(&output);
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
                scratch_space: Vec::new(),
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
                scratch_space: Vec::new(),
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

pub mod conditional {
    use crate::token::Token;
    /// A component that is attached to the input unit for keeping track of conditional branches
    /// as they are expanded.
    pub struct Component {
        // branches is a stack where each element corresponds to a conditional that is currently
        // expanding. A nested conditional is further up the stack than the conditional it is
        // nested in.
        //
        // This stack is used to
        // verify that \else and \fi tokens are valid; i.e., if a \else is encountered, the current
        // conditional must be true otherwise the \else is invalid.
        pub branches: Vec<Branch>,
    }

    #[derive(Debug)]
    pub enum BranchKind {
        // The true branch of an if conditional.
        True,
        // The false branch of an if conditional, or the default branch of a switch statement.
        Else,
        // A regular case brach of a switch statement.
        Switch,
    }

    #[derive(Debug)]
    pub struct Branch {
        pub _token: Token,
        pub kind: BranchKind,
    }

    impl Component {
        pub fn new() -> Component {
            Component {
                branches: Vec::new(),
            }
        }
    }

    impl Default for Component {
        fn default() -> Self {
            Self::new()
        }
    }
}
