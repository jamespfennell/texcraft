//! Box language parse tree.

use super::error;
use super::lexer;
use super::Str;

#[derive(Debug, PartialEq, Eq)]
pub struct Arg<'a> {
    pub value: Value<'a>,
    pub source: Str<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value<'a> {
    Integer(i32),
    Scaled(core::Scaled),
    InfiniteGlue(core::Scaled, core::GlueOrder),
    String(&'a str),
    List(Vec<FuncCall<'a>>),
}

impl<'a> Value<'a> {
    pub fn description(&self) -> &'static str {
        use Value::*;
        match self {
            Integer(_) => "an integer",
            Scaled(_) => "a number",
            InfiniteGlue(_, _) => "an infinite glue component",
            String(_) => "a string",
            List(_) => "a list",
        }
    }
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        use Value::*;
        match self {
            Integer(i) => write!(f, "{i}"),
            Scaled(scaled) => write!(f, "{scaled}"),
            InfiniteGlue(scaled, glue_order) => {
                scaled.display_no_units(f)?;
                write!(f, "{glue_order}")
            }
            String(s) => write!(f, r#""{s}""#),
            List(func_calls) => {
                let indent = " ".repeat(depth);
                writeln!(f, "[")?;
                for call in func_calls {
                    call.display(f, depth + 2)?;
                }
                write!(f, "{indent}]")?;
                Ok(())
            }
        }
    }
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, 0)
    }
}

impl<'a> Arg<'a> {
    fn parse(l: &mut lexer::Lexer<'a>) -> Self {
        let token = l.next().unwrap();
        let value = match token.value {
            lexer::TokenValue::String(s) => Value::String(s),
            lexer::TokenValue::Integer(n) => Value::Integer(n),
            lexer::TokenValue::Scaled(n) => Value::Scaled(n),
            lexer::TokenValue::InfiniteGlue(s, o) => Value::InfiniteGlue(s, o),
            lexer::TokenValue::SquareOpen => {
                return Arg {
                    value: Value::List(parse_list(l)),
                    source: Str {
                        end: l.cur_pos(),
                        ..token.source
                    },
                }
            }
            a => {
                panic!("can't parse value {a:?}")
            }
        };
        Arg {
            value,
            source: token.source,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncCall<'a> {
    pub func_name: Str<'a>,
    pub pos_args: Vec<Arg<'a>>,
    pub keyword_args: Vec<(Str<'a>, Arg<'a>)>,
}

impl<'a> FuncCall<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        let indent = " ".repeat(depth);
        let multiline = {
            let many_args = self.pos_args.len() + 2 * self.keyword_args.len() > 5;
            let has_list_pos_arg = self
                .pos_args
                .iter()
                .any(|arg| matches!(arg.value, Value::List(_)));
            let has_list_keyword_arg = self
                .keyword_args
                .iter()
                .any(|(_, arg)| matches!(arg.value, Value::List(_)));
            many_args || has_list_pos_arg || has_list_keyword_arg
        };
        write!(f, "{indent}{}(", self.func_name)?;
        if multiline {
            for arg in &self.pos_args {
                write!(f, "\n{indent}  ")?;
                arg.value.display(f, depth + 2)?;
                write!(f, ",")?;
            }
            for (keyword, arg) in &self.keyword_args {
                write!(f, "\n{indent}  {keyword}=")?;
                arg.value.display(f, depth + 2)?;
                write!(f, ",")?;
            }
            writeln!(f, "\n{indent})")?;
        } else {
            let mut remaining_args = self.pos_args.len() + self.keyword_args.len();
            for arg in &self.pos_args {
                arg.value.display(f, depth + 2)?;
                remaining_args = remaining_args.saturating_sub(1);
                if remaining_args > 0 {
                    write!(f, ", ")?;
                }
            }
            for (keyword, arg) in &self.keyword_args {
                write!(f, "{keyword}=")?;
                arg.value.display(f, depth + 2)?;
                remaining_args = remaining_args.saturating_sub(1);
                if remaining_args > 0 {
                    write!(f, ", ")?;
                }
            }
            writeln!(f, ")")?;
        }

        Ok(())
    }
}

impl<'a> std::fmt::Display for FuncCall<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, 0)
    }
}

pub fn parse_list<'a>(l: &mut lexer::Lexer<'a>) -> Vec<FuncCall<'a>> {
    let mut v = vec![];
    // One loop iterator for each func call.
    loop {
        let Some(next) = l.next() else {
            // We assume that this function was called WITHOUT parsing [.
            // Otherwise we would expect the list to end with ].
            // TODO: return an error if this is not true
            // We should pass Option<Token>
            return v;
        };
        // parse function name
        match next.value {
            lexer::TokenValue::Keyword(func_name) => func_name,
            lexer::TokenValue::SquareClose => {
                // We assume that this function was called after parsing [.
                // TODO: return an error if this is not true
                return v;
            }
            _ => todo!("error: expected keyword"),
        };
        let mut func_call = FuncCall {
            func_name: next.source,
            pos_args: vec![],
            keyword_args: vec![],
        };
        // parse (
        assert_eq!(
            l.next().map(|t| t.value),
            Some(lexer::TokenValue::RoundOpen)
        );
        // parse the arguments
        loop {
            let next = l.peek().unwrap();
            match next.value {
                // no more arguments
                lexer::TokenValue::RoundClose => {}
                // key=value argument
                lexer::TokenValue::Keyword(_) => {
                    l.next();
                    assert_eq!(l.next().map(|t| t.value), Some(lexer::TokenValue::Equal));
                    let value = Arg::parse(l);
                    func_call.keyword_args.push((next.source, value));
                }
                // positional argument
                _ => {
                    let value = Arg::parse(l);
                    if let Some(kw) = func_call.keyword_args.last() {
                        l.error(error::Error::PositionalArgAfterKeywordArg {
                            positional_arg: value.source.clone(),
                            keyword_arg: Str {
                                value: kw.1.source.value,
                                start: kw.0.start,
                                end: kw.1.source.end,
                            },
                        });
                    } else {
                        func_call.pos_args.push(value);
                    }
                }
            }
            match l.next().map(|t| t.value) {
                Some(lexer::TokenValue::RoundClose) => {
                    break;
                }
                Some(lexer::TokenValue::Comma) => {
                    continue;
                }
                a => panic!("unexpected token {a:?}"),
            }
        }
        v.push(func_call);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn list_variable() {
        let input = "a(b=[c()])";
        let want = vec![FuncCall {
            func_name: "a".into(),
            pos_args: vec![],
            keyword_args: vec![(
                "b".into(),
                Arg {
                    value: Value::List(vec![FuncCall {
                        func_name: "c".into(),
                        pos_args: vec![],
                        keyword_args: vec![],
                    }]),
                    source: "[c()]".into(),
                },
            )],
        }];

        let mut l = lexer::Lexer::new(input);
        let got = parse_list(&mut l);
        assert_eq!(got, want);
        assert_eq!(Ok(()), l.check_errors());
    }
}
