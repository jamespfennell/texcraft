//! Box language parse tree.

use std::borrow::Cow;

use super::lexer;
use super::Str;

/// An argument to a function.
#[derive(Debug, PartialEq, Eq)]
pub struct Arg<'a> {
    pub comments: Vec<&'a str>,
    pub value: Value<'a>,
    pub source: Str<'a>,
}

/// The value of an argument to a function.
#[derive(Debug, PartialEq, Eq)]
pub enum Value<'a> {
    Integer(i32),
    Scaled(core::Scaled),
    InfiniteGlue(core::Scaled, core::GlueOrder),
    String(Cow<'a, str>),
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
    fn parse(l: &mut lexer::Lexer<'a>, mut comments: Vec<&'a str>) -> Self {
        let token = l.next(&mut comments).unwrap();
        let value = match token.value {
            lexer::TokenValue::String(s) => Value::String(s),
            lexer::TokenValue::Integer(n) => Value::Integer(n),
            lexer::TokenValue::Scaled(n) => Value::Scaled(n),
            lexer::TokenValue::InfiniteGlue(s, o) => Value::InfiniteGlue(s, o),
            lexer::TokenValue::SquareOpen => {
                return Arg {
                    comments,
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
            comments,
            value,
            source: token.source,
        }
    }
}

/// A function call.
#[derive(Debug, PartialEq, Eq)]
pub struct FuncCall<'a> {
    /// Comments before the function call.
    pub comments: Vec<&'a str>,
    /// Name of the function.
    pub func_name: Str<'a>,
    /// Arguments to the function.
    ///
    /// For each element, the first part of the tuple is [`None`]
    /// if it's a positional argument, or the argument name if it's a
    /// keyword argument.
    /// The second element of the tuple is the value of the argument.
    pub args: Vec<(Option<Str<'a>>, Arg<'a>)>,
    /// Comments between the last argument and the closing parenthesis.
    pub trailing_comments: Vec<&'a str>,
}

impl<'a> FuncCall<'a> {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        let indent = " ".repeat(depth);
        for comment in &self.comments {
            writeln!(f, "{indent}#{comment}")?;
        }
        write!(f, "{indent}{}(", self.func_name)?;
        let multiline = {
            let many_args = self.args.len() > 4;
            let has_list_arg = self
                .args
                .iter()
                .any(|(_, arg)| matches!(arg.value, Value::List(_)));
            let has_arg_comments = self.args.iter().any(|(_, arg)| !arg.comments.is_empty());
            many_args || has_list_arg || has_arg_comments || !self.trailing_comments.is_empty()
        };
        if multiline {
            for (keyword, arg) in &self.args {
                for comment in &arg.comments {
                    write!(f, "\n{indent}  #{comment}")?;
                }
                write!(f, "\n{indent}  ")?;
                if let Some(keyword) = keyword {
                    write!(f, "{keyword}=")?;
                }
                arg.value.display(f, depth + 2)?;
                write!(f, ",")?;
            }
            for comment in &self.trailing_comments {
                write!(f, "\n{indent}  #{comment}")?;
            }
            writeln!(f, "\n{indent})")?;
        } else {
            let mut remaining_args = self.args.len();
            for (keyword, arg) in &self.args {
                if let Some(keyword) = keyword {
                    write!(f, "{keyword}=")?;
                }
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
        let mut comments = vec![];
        let Some(next) = l.next(&mut comments) else {
            // We assume that this function was called WITHOUT parsing [.
            // Otherwise we would expect the list to end with ].
            // TODO: return an error if this is not true
            // We should pass Option<Token>
            // TODO: return any trailing comments too!
            return v;
        };
        // parse function name
        match next.value {
            lexer::TokenValue::Keyword(func_name) => func_name,
            lexer::TokenValue::SquareClose => {
                // We assume that this function was called after parsing [.
                // TODO: return an error if this is not true
                // TODO: return any trailing comments too!
                return v;
            }
            _ => todo!("error: expected keyword"),
        };
        let mut func_call = FuncCall {
            comments,
            func_name: next.source,
            args: vec![],
            trailing_comments: vec![],
        };
        // parse (
        assert_eq!(
            l.next(&mut func_call.comments).map(|t| t.value),
            Some(lexer::TokenValue::RoundOpen)
        );
        // parse the arguments
        loop {
            let mut comments = vec![];
            let next = l.peek(&mut comments).unwrap();
            let mut arg = match next.value {
                // no more arguments
                lexer::TokenValue::RoundClose => {
                    // consume the token
                    l.next(&mut comments);
                    func_call.trailing_comments = comments;
                    break;
                }
                // key=value argument
                lexer::TokenValue::Keyword(_) => {
                    l.next(&mut comments);
                    assert_eq!(
                        l.next(&mut comments).map(|t| t.value),
                        Some(lexer::TokenValue::Equal)
                    );
                    let value = Arg::parse(l, comments);
                    (Some(next.source), value)
                }
                // positional argument
                _ => {
                    let value = Arg::parse(l, comments);
                    (None, value)
                }
            };
            let mut comments = vec![];
            match l.next(&mut comments).map(|t| t.value) {
                Some(lexer::TokenValue::RoundClose) => {
                    func_call.args.push(arg);
                    func_call.trailing_comments = comments;
                    break;
                }
                Some(lexer::TokenValue::Comma) => {
                    arg.1.comments.extend_from_slice(&comments);
                    func_call.args.push(arg);
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

    fn run_parse_test(input: &str, want: Vec<FuncCall>) {
        let mut l = lexer::Lexer::new(input);
        let got = parse_list(&mut l);
        assert_eq!(got, want);
        assert_eq!(Ok(()), l.check_errors());
    }

    macro_rules! parse_tests {
        ( $( (
            $name: ident,
            $input: expr,
            $want: expr,
        ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let want = $want;
                    run_parse_test(&input, want);
                }
            )+
        };
    }

    parse_tests!((
        basic_case,
        "a(b=[c()])",
        vec![FuncCall {
            comments: vec![],
            func_name: "a".into(),
            args: vec![(
                Some("b".into()),
                Arg {
                    comments: vec![],
                    value: Value::List(vec![FuncCall {
                        comments: vec![],
                        func_name: "c".into(),
                        args: vec![],
                        trailing_comments: vec![],
                    }]),
                    source: "[c()]".into(),
                },
            )],
            trailing_comments: vec![],
        }],
    ),);

    fn run_comments_test(
        input: &str,
        func_comments: Vec<&'static str>,
        arg_1_comments: Vec<&'static str>,
        arg_2_comments: Vec<&'static str>,
        trailing_comments: Vec<&'static str>,
    ) {
        let want = vec![FuncCall {
            comments: func_comments,
            func_name: "f".into(),
            args: vec![
                (
                    None,
                    Arg {
                        comments: arg_1_comments,
                        value: Value::Integer(3),
                        source: "3".into(),
                    },
                ),
                (
                    Some("key".into()),
                    Arg {
                        comments: arg_2_comments,
                        value: Value::Integer(4),
                        source: "4".into(),
                    },
                ),
            ],
            trailing_comments,
        }];
        run_parse_test(input, want);
    }

    macro_rules! comment_tests {
        ( $( (
            $name: ident,
            $input: expr,
            $(
                func_comments: $func_comments: expr,
            )?
            $(
                arg_1_comments: $arg_1_comments: expr,
            )?
            $(
                arg_2_comments: $arg_2_comments: expr,
            )?
            $(
                trailing_comments: $trailing_comments: expr,
            )?
        ), )+ ) => {
            $(
                #[test]
                #[allow(unused_assignments, unused_mut)]
                fn $name() {
                    let input = $input;
                    let mut func_comments = vec![];
                    $(
                        func_comments = $func_comments;
                    )?
                    let mut arg_1_comments = vec![];
                    $(
                        arg_1_comments = $arg_1_comments;
                    )?
                    let mut arg_2_comments = vec![];
                    $(
                        arg_2_comments = $arg_2_comments;
                    )?
                    let mut trailing_comments = vec![];
                    $(
                        trailing_comments = $trailing_comments;
                    )?
                    run_comments_test(&input, func_comments, arg_1_comments, arg_2_comments, trailing_comments);
                }
            )+
        };
    }

    comment_tests!(
        (
            comment_0,
            "f(3,key=4,)",
        ),
        (
            comment_1,
            "#X\nf(3,key=4,)",
            func_comments: vec!["X"],
        ),
        (
            comment_2,
            "f#X\n(3,key=4,)",
            func_comments: vec!["X"],
        ),
        (
            comment_3,
            "f(#X\n3,key=4,)",
            arg_1_comments: vec!["X"],
        ),
        (
            comment_4,
            "f(3#X\n,key=4,)",
            arg_1_comments: vec!["X"],
        ),
        (
            comment_5,
            "f(3,#X\nkey=4,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_6,
            "f(3,key#X\n=4,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_7,
            "f(3,key=#X\n4,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_8,
            "f(3,key=4#X\n,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_9,
            "f(3,key=4,#X\n)",
            trailing_comments: vec!["X"],
        ),
        (
            comment_10,
            "f(3,key=4#X\n)",
            trailing_comments: vec!["X"],
        ),
    );
}
