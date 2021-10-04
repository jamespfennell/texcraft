//! Input control.

use crate::tex::command::library::conditional;
use crate::tex::token;
use crate::tex::token::catcode::RawCatCode;
use crate::tex::token::lexer;
use crate::tex::token::CsNameInterner;
use crate::tex::token::Token;
use std::collections::HashMap;
use std::io;
use std::rc;

/// Structure used for controlling input. TODO: rename controller again
pub struct Unit {
    current_source: Source,
    sources: Vec<Source>,

    last_non_empty_line: Option<rc::Rc<token::Line>>,

    pub conditional: conditional::Component,
}

impl Unit {
    // TODO: accept initial_source
    pub fn new() -> Unit {
        Unit {
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
    pub fn next(
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
    pub fn peek(
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

    pub fn peek_recurse(
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

impl Default for Unit {
    fn default() -> Self {
        Self::new()
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
