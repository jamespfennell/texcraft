//! Concrete syntax tree for property list files
//!
//! The CST, or parse tree, of a property list file mostly just represents the nesting of parenthesis.
//! The AST is the most useful representation.
//!
//! Rough form of the production rules, where `*` means 0 or more, `+` means at least 1, `?` means optional:
//!
//! - `<property list> -> <list element>*`
//! - `<list element> -> <open parenthesis><word>(<whitespace>+<word>)*<property list><closed parentheses>`
//! - `<word>` -> ASCII string without whitespace or parentheses. Can be empty.
//! - `<whitespace> -> <space><whitespace>? | <newline><whitespace>?`
//! - `<open parenthesis> -> '('<whitespace>?`
//! - `<closed parenthesis> -> ')'<whitespace>?`
use super::error::Error;
use super::lexer::*;

/// Concrete syntax tree for property list files
#[derive(Debug, PartialEq, Eq)]
pub struct Cst(pub Vec<Node>);

impl Cst {
    /// Build an CST directly from source code.
    pub fn build(source: &str) -> (Cst, Vec<Error>) {
        let lexer = Lexer::new(source);
        let mut errors = vec![];
        let cst = Cst::build_from_lexer(lexer, &mut errors);
        (cst, errors)
    }

    /// Build an AST from an instance of the lexer.
    pub fn build_from_lexer(lexer: Lexer, errors: &mut Vec<Error>) -> Cst {
        let mut input = Input {
            lexer,
            next: None,
            opening_parenthesis_spans: vec![],
            errors,
        };
        Cst(parse_root_nodes(&mut input))
    }

    /// Display the CST.
    pub fn display(&self, indent: usize) -> Display {
        Display { cst: self, indent }
    }
}

/// Helper type for displaying CSTs.
pub struct Display<'a> {
    cst: &'a Cst,
    indent: usize,
}

impl<'a> std::fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in &self.cst.0 {
            node.write(f, self.indent, 0)?;
        }
        Ok(())
    }
}

/// Node in the CST.
#[derive(Debug, PartialEq, Eq)]
pub struct Node {
    /// Byte index of the opening parenthesis for this node in the source file
    pub opening_parenthesis_span: usize,
    /// Value of the node.
    pub value: NodeValue,
    /// Byte index of the closing parenthesis for this node in the source file
    pub closing_parenthesis_span: usize,
}

impl Node {
    fn write(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
        current_indent: usize,
    ) -> std::fmt::Result {
        match &self.value {
            NodeValue::Comment(e) => {
                write!(f, "{}(COMMENT", " ".repeat(current_indent))?;
                write_balanced_elements(e, f, current_indent + indent, true)?;
            }
            NodeValue::Regular(v) => {
                write!(f, "{}({}", " ".repeat(current_indent), &v.key)?;
                if !v.data.is_empty() {
                    write!(f, " {}", &v.data)?;
                }
                if !v.children.is_empty() {
                    writeln!(f)?;
                }
                for child in &v.children {
                    child.write(f, indent, current_indent + indent)?;
                }
                if !v.children.is_empty() {
                    write!(f, "{}", " ".repeat(current_indent + indent))?;
                }
                writeln!(f, ")")?;
            }
        }
        Ok(())
    }

    /// Convert the node into balanced elements.
    pub fn into_balanced_elements(&self) -> Vec<BalancedElem> {
        match &self.value {
            NodeValue::Comment(v) => v.clone(),
            NodeValue::Regular(regular_node_value) => {
                let mut s = regular_node_value.key.clone();
                if !regular_node_value.data.is_empty() {
                    s.push(' ');
                    s.push_str(&regular_node_value.data);
                }
                let mut v = vec![BalancedElem::String(s)];
                for child in &regular_node_value.children {
                    v.push(BalancedElem::Vec(child.into_balanced_elements()))
                }
                v
            }
        }
    }
}

/// Value of a node in the CST.
#[derive(Debug, PartialEq, Eq)]
pub enum NodeValue {
    /// A comment node.
    Comment(Vec<BalancedElem>),
    /// A regular non-comment node.
    Regular(RegularNodeValue),
}

/// Value of a regular node in the CST.
#[derive(Debug, PartialEq, Eq)]
pub struct RegularNodeValue {
    /// Key of the node; e.g., `CHECKSUM`.
    pub key: String,
    /// Span of the key in the source file
    pub key_span: std::ops::Range<usize>,
    /// Raw data of the node.
    pub data: String,
    /// Span of the raw data in the source file.
    pub data_span: std::ops::Range<usize>,
    /// Child nodes, for nodes that themselves contain property lists. E.g. `CHARACTER`.
    pub children: Vec<Node>,
}

/// Element of a comment node.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BalancedElem {
    String(String),
    Vec(Vec<BalancedElem>),
}

fn write_balanced_elements(
    elements: &[BalancedElem],
    f: &mut std::fmt::Formatter<'_>,
    current_indent: usize,
    mut word_before: bool,
) -> std::fmt::Result {
    for element in elements {
        match element {
            BalancedElem::String(s) => {
                if word_before {
                    write!(f, " ")?;
                }
                write!(f, "{s}")?;
                word_before = true;
            }
            BalancedElem::Vec(v) => {
                if word_before {
                    writeln!(f)?;
                }
                write!(f, "{}(", " ".repeat(current_indent))?;
                write_balanced_elements(v, f, current_indent, false)?;
                word_before = false;
            }
        }
    }
    if !word_before {
        write!(f, "{}", " ".repeat(current_indent))?;
    }
    writeln!(f, ")")?;
    Ok(())
}

impl From<&str> for BalancedElem {
    fn from(value: &str) -> Self {
        BalancedElem::String(value.into())
    }
}

struct Input<'a> {
    lexer: Lexer,
    next: Option<Token>,
    // Used for building error messages when opening spans are not matched.
    opening_parenthesis_spans: Vec<usize>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Input<'a> {
    fn next(&mut self) -> Option<Token> {
        self.refill_next();
        self.next.take()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.refill_next();
        self.next.as_ref()
    }

    fn next_or_closing(&mut self) -> Token {
        match self.next() {
            None => {
                self.errors.push(Error::UnbalancedOpeningParenthesis {
                    // We pop the last opening brace span because it's being matched by the closing brace we return here.
                    opening_parenthesis_span: self.opening_parenthesis_spans.pop().unwrap(),
                    end_span: self.lexer.source_length(),
                });
                Token(TokenKind::ClosedParenthesis, self.lexer.source_length())
            }
            Some(t) => t,
        }
    }

    fn read_word(&mut self) -> (String, std::ops::Range<usize>) {
        remove_whitespace(self);
        let (string, span_start) = match self.next.take() {
            Some(Token(TokenKind::Word(string), span_start)) => (string, span_start),
            Some(token) => {
                let span_start = token.1;
                self.next = Some(token);
                ("".to_string(), span_start)
            }
            _ => ("".to_string(), self.lexer.source_length()),
        };
        let span = span_start..span_start + string.len();
        (string, span)
    }

    fn refill_next(&mut self) {
        while self.next.is_none() {
            match self.lexer.next() {
                None => return,
                Some(Ok(token)) => {
                    match token.0 {
                        TokenKind::OpenParenthesis => self.opening_parenthesis_spans.push(token.1),
                        TokenKind::ClosedParenthesis => {
                            self.opening_parenthesis_spans.pop();
                        }
                        _ => {}
                    }
                    self.next = Some(token)
                }
                Some(Err(error)) => self.errors.push(error),
            }
        }
    }
}

fn remove_whitespace(input: &mut Input) {
    while let Some(Token(TokenKind::Whitespace(..), _)) = input.peek() {
        input.next();
    }
}

fn finish_accumulating_string(
    input: &mut Input,
    mut data: String,
    span_start: usize,
    allow_newlines: bool,
) -> (String, std::ops::Range<usize>) {
    let mut whitespace_to_flush = 0_usize;
    loop {
        match input.peek() {
            Some(Token(TokenKind::Word(s), _)) => {
                for _ in 0..whitespace_to_flush {
                    data.push(' ');
                }
                whitespace_to_flush = 0;
                data.push_str(s);
            }
            Some(Token(TokenKind::Whitespace(n, contains_newlines), _)) => {
                if *contains_newlines && !allow_newlines {
                    break;
                }
                whitespace_to_flush += *n;
            }
            _ => {
                break;
            }
        }
        input.next();
    }
    let final_span = span_start..span_start + data.len();
    (data, final_span)
}

fn parse_node(input: &mut Input, opening_parenthesis_span: usize) -> Node {
    // Parse the key
    let (key, key_span) = input.read_word();

    // Parse the content of the node, which is either a comment or regular node.
    let (value, closing_token) = if key == "COMMENT" {
        let (balanced_elements, closing_token) = parse_balanced_elements(input);
        (NodeValue::Comment(balanced_elements), closing_token)
    } else {
        let (first_word, initial_span) = input.read_word();
        let (data, data_span) =
            finish_accumulating_string(input, first_word, initial_span.start, true);
        let (children, closing_token) = parse_inner_nodes(input);
        (
            NodeValue::Regular(RegularNodeValue {
                key,
                key_span,
                data,
                data_span,
                children,
            }),
            closing_token,
        )
    };
    Node {
        opening_parenthesis_span,
        value,
        closing_parenthesis_span: closing_token.1,
    }
}

/// PLtoTF.2014.82
fn parse_root_nodes(input: &mut Input) -> Vec<Node> {
    let mut r: Vec<Node> = vec![];
    loop {
        let token = match input.next() {
            None => return r,
            Some(token) => token,
        };
        match token.0 {
            TokenKind::Word(s) => {
                let span = token.1..token.1 + s.len();
                input
                    .errors
                    .push(Error::JunkInPropertyList { value: s, span })
            }
            TokenKind::OpenParenthesis => {
                r.push(parse_node(input, token.1));
            }
            TokenKind::ClosedParenthesis => input
                .errors
                .push(Error::UnexpectedRightParenthesis { span: token.1 }),
            TokenKind::Whitespace(..) => {}
        }
    }
}

/// PLtoTF.2014.92, and other sections. In PLtoTF this logic is duplicated for each type of inner list.
fn parse_inner_nodes(input: &mut Input) -> (Vec<Node>, Token) {
    let mut r: Vec<Node> = vec![];
    loop {
        let token = input.next_or_closing();
        match token.0 {
            TokenKind::Word(s) => {
                let span = token.1..token.1 + s.len();
                input
                    .errors
                    .push(Error::JunkInPropertyList { value: s, span })
            }
            TokenKind::OpenParenthesis => {
                r.push(parse_node(input, token.1));
            }
            TokenKind::ClosedParenthesis => return (r, token),
            TokenKind::Whitespace(..) => {}
        }
    }
}

fn parse_balanced_elements(input: &mut Input) -> (Vec<BalancedElem>, Token) {
    let mut r: Vec<BalancedElem> = vec![];
    loop {
        let token = input.next_or_closing();
        match token.0 {
            TokenKind::Word(word) => {
                let (s, _) = finish_accumulating_string(input, word, token.1, false);
                r.push(BalancedElem::String(s));
            }
            TokenKind::OpenParenthesis => {
                let (inner, _) = parse_balanced_elements(input);
                r.push(BalancedElem::Vec(inner));
            }
            TokenKind::ClosedParenthesis => return (r, token),
            TokenKind::Whitespace(..) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(source: &str, want: Vec<Node>, want_errors: Vec<Error>) {
        let (got, got_errors) = Cst::build(source);
        assert_eq!(got, Cst(want));
        assert_eq!(got_errors, want_errors);
    }

    macro_rules! cst_test {
        ( $( ($name: ident, $input: expr, $want: expr, $want_errors: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let want = $want;
                    let want_errors = $want_errors;
                    run(input, want, want_errors);
                }
            )+
        };
    }

    cst_test!(
        (
            basic,
            " (Hello World (Nested One) (Nested  Two Two) (Nested Three\nThree))",
            vec![Node {
                opening_parenthesis_span: 1,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "Hello".into(),
                    key_span: 2..7,
                    data: "World".into(),
                    data_span: 8..13,
                    children: vec![
                        Node {
                            opening_parenthesis_span: 14,
                            value: NodeValue::Regular(RegularNodeValue {
                                key: "Nested".into(),
                                key_span: 15..21,
                                data: "One".into(),
                                data_span: 22..25,
                                children: vec![],
                            }),
                            closing_parenthesis_span: 25,
                        },
                        Node {
                            opening_parenthesis_span: 27,
                            value: NodeValue::Regular(RegularNodeValue {
                                key: "Nested".into(),
                                key_span: 28..34,
                                data: "Two Two".into(),
                                data_span: 36..43,
                                children: vec![],
                            }),
                            closing_parenthesis_span: 43,
                        },
                        Node {
                            opening_parenthesis_span: 45,
                            value: NodeValue::Regular(RegularNodeValue {
                                key: "Nested".into(),
                                key_span: 46..52,
                                data: "Three Three".into(),
                                data_span: 53..64,
                                children: vec![],
                            }),
                            closing_parenthesis_span: 64,
                        },
                    ],
                }),
                closing_parenthesis_span: 65,
            }],
            vec![],
        ),
        (
            basic_never_ends,
            "(Hello",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "Hello".into(),
                    key_span: 1..6,
                    data: "".into(),
                    data_span: 6..6,
                    children: vec![],
                }),
                closing_parenthesis_span: 6,
            }],
            vec![Error::UnbalancedOpeningParenthesis {
                opening_parenthesis_span: 0,
                end_span: 6
            }],
        ),
        (
            basic_never_ends_2,
            "(",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "".into(),
                    key_span: 1..1,
                    data: "".into(),
                    data_span: 1..1,
                    children: vec![],
                }),
                closing_parenthesis_span: 1,
            }],
            vec![Error::UnbalancedOpeningParenthesis {
                opening_parenthesis_span: 0,
                end_span: 1
            }],
        ),
        (
            empty_node,
            "()",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "".into(),
                    key_span: 1..1,
                    data: "".into(),
                    data_span: 1..1,
                    children: vec![],
                }),
                closing_parenthesis_span: 1,
            }],
            vec![],
        ),
        (
            garbage_string_in_list,
            "(Hello World) Garbage (Hola Mundo)",
            vec![
                Node {
                    opening_parenthesis_span: 0,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hello".into(),
                        key_span: 1..6,
                        data: "World".into(),
                        data_span: 7..12,
                        children: vec![],
                    }),
                    closing_parenthesis_span: 12,
                },
                Node {
                    opening_parenthesis_span: 22,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hola".into(),
                        key_span: 23..27,
                        data: "Mundo".into(),
                        data_span: 28..33,
                        children: vec![],
                    }),
                    closing_parenthesis_span: 33,
                },
            ],
            vec![Error::JunkInPropertyList {
                value: "Garbage".into(),
                span: 14..21,
            }],
        ),
        (
            garbage_closed_parenthesis_in_list,
            "(Hello World) ) (Hola Mundo)",
            vec![
                Node {
                    opening_parenthesis_span: 0,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hello".into(),
                        key_span: 1..6,
                        data: "World".into(),
                        data_span: 7..12,
                        children: vec![],
                    }),
                    closing_parenthesis_span: 12,
                },
                Node {
                    opening_parenthesis_span: 16,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hola".into(),
                        key_span: 17..21,
                        data: "Mundo".into(),
                        data_span: 22..27,
                        children: vec![],
                    }),
                    closing_parenthesis_span: 27,
                },
            ],
            vec![Error::UnexpectedRightParenthesis { span: 14 },],
        ),
        (
            comment,
            "(COMMENT World (Nested One) Hello (Nested Two))",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Comment(vec![
                    BalancedElem::String("World".into()),
                    BalancedElem::Vec(vec![BalancedElem::String("Nested One".into()),]),
                    BalancedElem::String("Hello".into()),
                    BalancedElem::Vec(vec![BalancedElem::String("Nested Two".into()),]),
                ]),
                closing_parenthesis_span: 46,
            }],
            vec![],
        ),
        (
            comment_with_newlines,
            "(COMMENT World (Nested\nOne) Hello (Nested\nTwo))",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Comment(vec![
                    BalancedElem::String("World".into()),
                    BalancedElem::Vec(vec![
                        BalancedElem::String("Nested".into()),
                        BalancedElem::String("One".into()),
                    ]),
                    BalancedElem::String("Hello".into()),
                    BalancedElem::Vec(vec![
                        BalancedElem::String("Nested".into()),
                        BalancedElem::String("Two".into()),
                    ]),
                ]),
                closing_parenthesis_span: 46,
            }],
            vec![],
        ),
        (
            comment_never_ends,
            "(COMMENT World (",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Comment(vec![
                    BalancedElem::String("World".into()),
                    BalancedElem::Vec(vec![]),
                ]),
                closing_parenthesis_span: 16,
            }],
            vec![
                Error::UnbalancedOpeningParenthesis {
                    opening_parenthesis_span: 15,
                    end_span: 16
                },
                Error::UnbalancedOpeningParenthesis {
                    opening_parenthesis_span: 0,
                    end_span: 16
                },
            ],
        ),
        (
            lexer_error_propagated,
            "(Hello World ä)",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "Hello".into(),
                    key_span: 1..6,
                    data: "World".into(),
                    data_span: 7..12,
                    children: vec![],
                }),
                closing_parenthesis_span: 15,
            }],
            vec![Error::InvalidCharacter('ä', 13)],
        ),
    );
}
