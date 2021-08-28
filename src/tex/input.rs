//! Input control.

use crate::tex::command::library::conditional;
use crate::tex::token;
use crate::tex::token::catcode::RawCatCode;
use crate::tex::token::lexer;
use crate::tex::token::stream;
use crate::tex::token::stream::Stream;
use std::collections::HashMap;
use std::io;
use std::rc;

/// Structure used for controlling input.
pub struct Unit {
    sources: Vec<Source>,
    last_non_empty_line: Option<rc::Rc<token::Line>>,

    pub conditional: conditional::Component,
}

impl Unit {
    pub fn new() -> Unit {
        Unit {
            sources: Vec::new(),
            last_non_empty_line: None,
            conditional: conditional::Component::new(),
        }
    }

    pub fn push_new_source(&mut self, reader: Box<dyn io::BufRead>) -> () {
        self.sources.push(Source::new(reader));
    }

    pub fn push_new_str<Str: Into<String>>(&mut self, s: Str) -> () {
        self.push_new_source(Box::new(io::Cursor::new(s.into())));
    }

    pub fn push_expansion(&mut self, expansion: stream::VecStream) -> () {
        if let Some(source) = self.sources.last_mut() {
            source.expansions.push(expansion);
        }
    }

    pub fn last_non_empty_line(&self) -> Option<rc::Rc<token::Line>> {
        self.last_non_empty_line.clone()
    }

    pub fn next(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
    ) -> anyhow::Result<Option<token::Token>> {
        self.prepare_imut_peek(cat_code_map)?;
        Ok(match self.sources.last_mut() {
            None => None,
            Some(source) => source.expansions.next()?,
        })
    }

    pub fn peek(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
    ) -> anyhow::Result<Option<&token::Token>> {
        self.prepare_imut_peek(cat_code_map)?;
        self.imut_peek()
    }

    pub fn prepare_imut_peek(
        &mut self,
        cat_code_map: &HashMap<u32, RawCatCode>,
    ) -> anyhow::Result<()> {
        loop {
            match self.sources.last_mut() {
                None => break,
                Some(top) => {
                    if top.expansions.peek()? != None {
                        break;
                    }
                    match top.root.next(cat_code_map)? {
                        None => {
                            self.last_non_empty_line = top.root.last_non_empty_line();
                            // We don't pop off the last source because we may still be processing the file.
                            // For example if the request happened via an expansion of the last command in
                            // the file. Think of a file that ends in `\number 4`.
                            if self.sources.len() > 1 {
                                self.sources.pop();
                            } else {
                                break;
                            }
                            continue;
                        }
                        Some(t) => {
                            // TODO: This is kind of hacky. It would be nice to move this caching down to the source
                            // where it really belongs.
                            top.expansions.push(stream::VecStream::new_singleton(t));
                            break;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn imut_peek(&self) -> anyhow::Result<Option<&token::Token>> {
        Ok(match self.sources.last() {
            None => None,
            Some(source) => source.expansions.imut_peek()?,
        })
    }

    pub fn clear(&mut self) {
        self.sources = vec![];
    }
}

struct Source {
    root: lexer::Lexer,
    expansions: stream::StackStream<stream::VecStream>,
}

impl Source {
    fn new(reader: Box<dyn io::BufRead>) -> Source {
        Source {
            root: lexer::Lexer::new(reader),
            expansions: stream::StackStream::new(),
        }
    }

    /*
    fn next(
        &mut self,
        cat_codes: &ScopedMap<char, RawCatCode>,
    ) -> anyhow::Result<Option<token::Token>> {
        match self.expansions.next()? {
            None => Ok(self.root.next(&cat_codes)?),
            Some(t) => Ok(Some(t)),
        }
    }
     */
}
