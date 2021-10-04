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

/// Structure used for controlling input.
pub struct Unit {
    sources: Vec<Source>,
    cache: Option<Token>,
    last_non_empty_line: Option<rc::Rc<token::Line>>,

    pub conditional: conditional::Component,
}

impl Unit {
    pub fn new() -> Unit {
        Unit {
            sources: Vec::new(),
            cache: None,
            last_non_empty_line: None,
            conditional: conditional::Component::new(),
        }
    }

    pub fn push_new_source(&mut self, reader: Box<dyn io::BufRead>) {
        self.vacate_cache();
        self.sources.push(Source::new_reader(reader));
    }

    pub fn push_new_str<Str: Into<String>>(&mut self, s: Str) {
        self.vacate_cache();
        self.push_new_source(Box::new(io::Cursor::new(s.into())));
    }

    pub fn push_expansion(&mut self, expansion: Vec<Token>) {
        self.vacate_cache();
        self.sources.push(Source::Vec(VecSource::new(expansion)));
    }

    pub fn push_single_token(&mut self, token: Token) {
        self.vacate_cache();
        self.sources.push(Source::Singleton(Some(token)));
    }

    pub fn last_non_empty_line(&self) -> Option<rc::Rc<token::Line>> {
        self.last_non_empty_line.clone()
    }

    #[inline]
    pub fn next(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<Option<token::Token>> {
        if self.cache.is_none() {
            self.populate_cache(cat_code_map, interner)?;
        }
        Ok(self.cache.take())
    }

    #[inline]
    pub fn peek(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<Option<&token::Token>> {
        if self.cache.is_none() {
            self.populate_cache(cat_code_map, interner)?;
        }
        Ok(self.cache.as_ref())
    }

    pub fn populate_cache(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
        interner: &mut CsNameInterner,
    ) -> anyhow::Result<()> {
        loop {
            self.cache = match self.sources.last_mut() {
                None => return Ok(()),
                Some(Source::Vec(vec)) => vec.next(),
                Some(Source::Singleton(t)) => {
                    std::mem::swap(&mut self.cache, t);
                    self.sources.pop();
                    return Ok(());
                }
                Some(Source::Reader(lexer)) => lexer.next(cat_code_map, interner)?,
            };
            if self.cache.is_some() {
                return Ok(());
            }
            self.sources.pop();
        }
    }

    pub fn clear(&mut self) {
        self.sources = vec![];
    }

    #[inline]
    fn vacate_cache(&mut self) {
        if let Some(t) = self.cache.take() {
            self.sources.push(Source::Singleton(Some(t)));
        }
    }
}

impl Default for Unit {
    fn default() -> Self {
        Self::new()
    }
}

enum Source {
    Reader(lexer::Lexer),
    Vec(VecSource),
    Singleton(Option<Token>),
}

impl Source {
    fn new_reader(reader: Box<dyn io::BufRead>) -> Source {
        let mut interner = CsNameInterner::new();
        Source::Reader(lexer::Lexer::new(reader, &mut interner))
    }
}

struct VecSource {
    data: Vec<Token>,
}

impl VecSource {
    fn new(mut data: Vec<Token>) -> VecSource {
        data.reverse();
        VecSource { data }
    }

    fn next(&mut self) -> Option<Token> {
        self.data.pop()
    }
}
