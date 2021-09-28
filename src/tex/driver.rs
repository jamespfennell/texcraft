//! Expansion and execution driver.

use crate::tex::command::Command;
use crate::tex::input;
use crate::tex::prelude::*;
use crate::tex::token;
use std::mem;

pub fn exec<S>(
    state: &mut Base<S>,
    input: &mut input::Unit,
    err_for_undefined_cs: bool,
) -> anyhow::Result<Vec<Token>> {
    let undefined_cs_handler = match err_for_undefined_cs {
        true => default_undefined_cs_handler,
        false => handle_character,
    };
    run(state, input, handle_character, undefined_cs_handler)?;
    let mut result = Vec::new();
    mem::swap(&mut result, &mut state.exec_output);
    Ok(result)
}

pub fn run<S>(
    state: &mut Base<S>,
    input: &mut input::Unit,
    character_handler: fn(token::Token, &mut Base<S>) -> anyhow::Result<()>,
    undefined_cs_handler: fn(token::Token, &mut Base<S>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    loop {
        let fully_expanded_token = ExpansionInput::<S>::new(state, input).next();
        match fully_expanded_token {
            Ok(token) => match token {
                None => {
                    input.clear();
                    break;
                }
                Some(mut token) => match &token.value {
                    Character(..) => {
                        character_handler(token, state)?;
                    }
                    ControlSequence(_, name) => match state.get_command(name) {
                        Some(Command::Execution(cmd_ref)) => {
                            // We need to copy the command to avoid a borrow checker error.
                            // This is because `cmd_ref` keeps an immutable reference to the state alive, but in the `call`
                            // invocation below we pass in a mutable reference to the state. The borrow
                            // checker is actually surfacing a genuine TeX edge case here: if you input something like \let\let=\def
                            // then the reference to the command \let in the commands map is overwritten at some point
                            // during the execution of \let. You could also imagine implementing an \undef command that would
                            // result in the reference becoming null during the execution.
                            let cmd = *cmd_ref;
                            let mut expanding_input = ExecutionInput::<S>::new(state, input);
                            cmd.call(token, &mut expanding_input)?;
                        }
                        Some(Command::Variable(cmd_ref)) => {
                            let cmd = *cmd_ref;
                            let mut expanding_input = ExecutionInput::<S>::new(state, input);
                            cmd.call(token, &mut expanding_input)?;
                        }
                        Some(Command::Character(c, cat_code)) => {
                            token.value = Character(*c, *cat_code);
                            character_handler(token, state)?;
                        }
                        _ => {
                            undefined_cs_handler(token, state)?;
                        }
                    },
                },
            },
            Err(mut error) => {
                error::add_context(&mut error, state, input);
                return Err(error);
            }
        };
    }
    Ok(())
}

fn handle_character<S>(mut token: token::Token, state: &mut Base<S>) -> anyhow::Result<()> {
    if let token::Value::Character('\n', cat_code) = token.value {
        token.value = token::Value::Character(' ', cat_code);
    }
    state.exec_output.push(token);
    state.num_trailing_newlines = 0;
    Ok(())
}

fn default_undefined_cs_handler<S>(token: token::Token, state: &mut Base<S>) -> anyhow::Result<()> {
    Err(error::new_undefined_cs_error(token, state))
}

struct RawStream<'a, S> {
    state: &'a Base<S>,
    input: &'a mut input::Unit,
}

impl<'a, S> stream::Stream for RawStream<'a, S> {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        self.input.next(self.state.cat_code_map())
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.input.peek(self.state.cat_code_map())
    }
}

// I have no idea why aliasing Input<S> to this works; i.e., why the lifetime
// parameter can be elided.
pub struct ExpansionInput<'a, S> {
    raw_stream: RawStream<'a, S>,
}

impl<'a, S> stream::Stream for ExpansionInput<'a, S> {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        while self.expand_next()? {}
        self.raw_stream.next()
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        while self.expand_next()? {}
        self.raw_stream.peek()
    }
}

impl<'a, S> ExpansionInput<'a, S> {
    pub fn new(state: &'a Base<S>, input: &'a mut input::Unit) -> ExpansionInput<'a, S> {
        ExpansionInput::<S> {
            raw_stream: RawStream::<S> { state, input },
        }
    }

    pub fn base(&self) -> &Base<S> {
        self.raw_stream.state
    }

    pub fn state(&self) -> &S {
        &self.raw_stream.state.state
    }

    pub fn controller(&self) -> &input::Unit {
        self.raw_stream.input
    }

    pub fn controller_mut(&mut self) -> &mut input::Unit {
        &mut self.raw_stream.input
    }
}

impl<'a, S> ExpansionInput<'a, S> {
    pub fn unexpanded_stream(&mut self) -> &mut dyn Stream {
        &mut self.raw_stream
    }

    pub fn expand_next(&mut self) -> anyhow::Result<bool> {
        expand_next(self.raw_stream.state, self.raw_stream.input)
    }
}

/// This type is the same as [RawStream] except it stores a mutable reference to the state.
/// It is used as the backing type for [ExecutionInput].
pub struct RawStreamMutState<'a, S> {
    state: &'a mut Base<S>,
    input: &'a mut input::Unit,
}

impl<'a, S> stream::Stream for RawStreamMutState<'a, S> {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        self.input.next(self.state.cat_code_map())
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.input.peek(self.state.cat_code_map())
    }
}

// I have no idea why aliasing Input<S> to this works; i.e., why the lifetime
// parameter can be elided.
pub struct ExecutionInput<'a, S> {
    raw_stream: RawStreamMutState<'a, S>,
}

impl<'a, S> stream::Stream for ExecutionInput<'a, S> {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        while self.expand_next()? {}
        self.raw_stream.next()
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        while self.expand_next()? {}
        self.raw_stream.peek()
    }
}

impl<'a, S> ExecutionInput<'a, S> {
    pub fn base(&self) -> &Base<S> {
        self.raw_stream.state
    }

    pub fn base_mut(&mut self) -> &mut Base<S> {
        &mut self.raw_stream.state
    }

    pub fn state(&self) -> &S {
        &self.raw_stream.state.state
    }

    pub fn state_mut(&mut self) -> &mut S {
        &mut self.raw_stream.state.state
    }

    pub fn controller(&self) -> &input::Unit {
        self.raw_stream.input
    }

    pub fn unexpanded_stream(&mut self) -> &mut RawStreamMutState<'a, S> {
        &mut self.raw_stream
    }

    pub fn expand_next(&mut self) -> anyhow::Result<bool> {
        expand_next(self.raw_stream.state, self.raw_stream.input)
    }

    pub fn regular(&mut self) -> ExpansionInput<S> {
        ExpansionInput::<S>::new(self.raw_stream.state, &mut self.raw_stream.input)
    }

    pub fn new(state: &'a mut Base<S>, input: &'a mut input::Unit) -> ExecutionInput<'a, S> {
        ExecutionInput::<S> {
            raw_stream: RawStreamMutState::<S> { state, input },
        }
    }
}

fn expand_next<S>(state: &Base<S>, input: &mut input::Unit) -> anyhow::Result<bool> {
    let command = match input.peek(state.cat_code_map())? {
        None => None,
        Some(token) => match token.value {
            Character(..) => None,
            ControlSequence(_, ref name) => state.get_command(name),
        },
    };
    let command = match command {
        Some(command::Command::Expansion(command)) => command,
        _ => return Ok(false),
    };
    let token = input.next(state.cat_code_map())?.unwrap();
    let output = command.call(token, &mut ExpansionInput::<S>::new(state, input))?;
    input.push_expansion(output);
    Ok(true)
}
