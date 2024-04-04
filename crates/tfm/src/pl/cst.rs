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
use super::error::{ParseWarning, ParseWarningKind};
use super::lexer::*;

/// Concrete syntax tree for property list files
#[derive(Debug, PartialEq, Eq)]
pub struct Cst(pub Vec<Node>);

impl Cst {
    /// Build an CST directly from source code.
    pub fn from_pl_source_code(source: &str) -> (Cst, Vec<ParseWarning>) {
        let lexer = Lexer::new(source);
        let mut errors = vec![];
        let cst = Cst::from_lexer(lexer, &mut errors);
        (cst, errors)
    }

    /// Build an AST from an instance of the lexer.
    pub fn from_lexer(lexer: Lexer, errors: &mut Vec<ParseWarning>) -> Cst {
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
                write_balanced_elements(
                    e,
                    f,
                    current_indent + indent,
                    WriteBalancedElementsState::Start,
                )?;
            }
            NodeValue::Regular(v) => {
                write!(f, "{}({}", " ".repeat(current_indent), &v.key)?;
                if let Some(data) = &v.data {
                    write!(f, " {}", data)?;
                }
                if let Some(children) = &v.children {
                    writeln!(f)?;
                    for child in children {
                        child.write(f, indent, current_indent + indent)?;
                    }
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
                if let Some(data) = &regular_node_value.data {
                    s.push(' ');
                    s.push_str(data);
                }
                let mut v = vec![BalancedElem::String(s)];
                if let Some(children) = &regular_node_value.children {
                    for child in children {
                        v.push(BalancedElem::Vec(child.into_balanced_elements()))
                    }
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
    /// Data of the node.
    ///
    /// The values `None` and `Some("".to_string())` are semantically the same.
    /// However they are displayed differently.
    /// For example the stop node is displayed as `(STOP)` but an empty coding scheme node
    ///     is displayed as `(CODINGSCHEME )`.
    pub data: Option<String>,
    /// Span of the raw data in the source file.
    pub data_span: std::ops::Range<usize>,
    /// Child nodes, for nodes that themselves contain property lists. E.g. `CHARACTER`.
    ///
    /// The values `None` and `Some(vec![])` are semantically the same.
    /// However they are displayed differently.
    /// For example the stop node is displayed as `(STOP)` but an empty lig table node
    ///     is displayed as `(LIGTABLE\n)`.
    pub children: Option<Vec<Node>>,
}

/// Element of a comment node.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum BalancedElem {
    String(String),
    Vec(Vec<BalancedElem>),
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum WriteBalancedElementsState {
    Start,
    AfterString,
    AfterVec,
}

fn write_balanced_elements(
    elements: &[BalancedElem],
    f: &mut std::fmt::Formatter<'_>,
    current_indent: usize,
    mut state: WriteBalancedElementsState,
) -> std::fmt::Result {
    for element in elements {
        match element {
            BalancedElem::String(s) => {
                match state {
                    WriteBalancedElementsState::Start => {
                        if !s.is_empty() {
                            write!(f, " ")?;
                        }
                    }
                    WriteBalancedElementsState::AfterString => {
                        write!(f, "\n{}", " ".repeat(current_indent))?;
                    }
                    WriteBalancedElementsState::AfterVec => {}
                }
                write!(f, "{s}")?;
                state = WriteBalancedElementsState::AfterString;
            }
            BalancedElem::Vec(v) => {
                if state != WriteBalancedElementsState::AfterVec {
                    writeln!(f)?;
                }
                write!(f, "{}(", " ".repeat(current_indent))?;
                state = WriteBalancedElementsState::AfterVec;
                write_balanced_elements(v, f, current_indent, state)?;
            }
        }
    }
    if state == WriteBalancedElementsState::AfterVec {
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
    errors: &'a mut Vec<ParseWarning>,
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
                // We pop the last opening brace span because it's being matched by the closing brace we return here.
                let opening_parenthesis_span_start = self.opening_parenthesis_spans.pop().unwrap();
                let span_start = self.lexer.source_length();
                self.errors.push(ParseWarning {
                    span: span_start..span_start,
                    knuth_pltotf_offset: Some(span_start),
                    kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                        opening_parenthesis_span: opening_parenthesis_span_start
                            ..opening_parenthesis_span_start + 1,
                    },
                });
                Token(TokenKind::ClosedParenthesis, self.lexer.source_length())
            }
            Some(t) => t,
        }
    }

    fn read_word(&mut self) -> AccumulatedString {
        remove_whitespace(self);
        let (value, span_start) = match self.next.take() {
            Some(Token(TokenKind::Word { value }, span_start)) => (value, span_start),
            Some(token) => {
                let span_start = token.1;
                self.next = Some(token);
                ("".to_string(), span_start)
            }
            _ => ("".to_string(), self.lexer.source_length()),
        };
        let span = span_start..span_start + value.chars().map(|_| 1).sum::<usize>();
        AccumulatedString { value, span }
    }

    fn refill_next(&mut self) {
        while self.next.is_none() {
            match self.lexer.next() {
                None => return,
                Some(token) => {
                    match token.0 {
                        TokenKind::OpenParenthesis => self.opening_parenthesis_spans.push(token.1),
                        TokenKind::ClosedParenthesis => {
                            self.opening_parenthesis_spans.pop();
                        }
                        _ => {}
                    }
                    self.next = Some(token)
                }
            }
        }
    }
}

fn remove_whitespace(input: &mut Input) {
    while let Some(Token(TokenKind::Whitespace { .. }, _)) = input.peek() {
        input.next();
    }
}

struct AccumulatedString {
    value: String,
    span: std::ops::Range<usize>,
}

fn finish_accumulating_string(
    input: &mut Input,
    // TODO: make this a method on AccumulatedString
    mut data: AccumulatedString,
    allow_newlines: bool,
) -> AccumulatedString {
    let mut whitespace_to_flush = 0_usize;
    loop {
        match input.peek() {
            Some(Token(TokenKind::Word { value }, _)) => {
                for _ in 0..whitespace_to_flush {
                    data.value.push(' ');
                }
                whitespace_to_flush = 0;
                data.value.push_str(value);
            }
            Some(Token(
                TokenKind::Whitespace {
                    len: n,
                    contains_newlines,
                },
                _,
            )) => {
                if *contains_newlines && !allow_newlines {
                    break;
                }
                whitespace_to_flush += *n;
            }
            Some(Token(TokenKind::OpenParenthesis, _))
            | Some(Token(TokenKind::ClosedParenthesis, _)) => {
                for _ in 0..whitespace_to_flush {
                    data.value.push(' ');
                }
                break;
            }
            None => {
                break;
            }
        }
        input.next();
    }
    let final_span =
        data.span.start..data.span.start + data.value.chars().map(|_| 1).sum::<usize>();
    data.span = final_span;
    data
}

fn parse_node(input: &mut Input, opening_parenthesis_span: usize) -> Node {
    // Parse the key
    let AccumulatedString {
        value: key,
        span: key_span,
    } = input.read_word();

    // Parse the content of the node, which is either a comment or regular node.
    let (value, closing_token) = if key == "COMMENT" {
        let (balanced_elements, closing_token) = parse_balanced_elements(input);
        (NodeValue::Comment(balanced_elements), closing_token)
    } else {
        let string = input.read_word();
        let string = finish_accumulating_string(input, string, true);
        let (children, closing_token) = parse_inner_nodes(input);
        (
            NodeValue::Regular(RegularNodeValue {
                key,
                key_span,
                data: Some(string.value),
                data_span: string.span,
                children: Some(children),
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
            TokenKind::Word { value } => {
                let span = token.1..token.1 + value.len();
                let s = AccumulatedString { value, span };
                add_junk_string_error(input, s);
            }
            TokenKind::OpenParenthesis => {
                r.push(parse_node(input, token.1));
            }
            TokenKind::ClosedParenthesis => input.errors.push(ParseWarning {
                span: token.1..token.1 + 1,
                knuth_pltotf_offset: Some(token.1),
                kind: ParseWarningKind::UnexpectedClosingParenthesis,
            }),
            TokenKind::Whitespace { .. } => {}
        }
    }
}

/// PLtoTF.2014.92, and other sections. In PLtoTF this logic is duplicated for each type of inner list.
fn parse_inner_nodes(input: &mut Input) -> (Vec<Node>, Token) {
    let mut r: Vec<Node> = vec![];
    loop {
        let token = input.next_or_closing();
        match token.0 {
            TokenKind::Word { value } => {
                let span = token.1..token.1 + value.len();
                let s = AccumulatedString { value, span };
                add_junk_string_error(input, s);
            }
            TokenKind::OpenParenthesis => {
                r.push(parse_node(input, token.1));
            }
            TokenKind::ClosedParenthesis => return (r, token),
            TokenKind::Whitespace { .. } => {}
        }
    }
}

fn add_junk_string_error(input: &mut Input, data: AccumulatedString) {
    let AccumulatedString { value, span } = finish_accumulating_string(input, data, true);
    input.errors.push(ParseWarning {
        span: span.clone(),
        knuth_pltotf_offset: Some(span.start + 1),
        kind: ParseWarningKind::JunkInsidePropertyList { junk: value },
    });
}

fn parse_balanced_elements(input: &mut Input) -> (Vec<BalancedElem>, Token) {
    let mut r: Vec<BalancedElem> = vec![];
    loop {
        let token = input.next_or_closing();
        match token.0 {
            TokenKind::Word { value } => {
                let s = AccumulatedString { value, span: 0..0 };
                let s = finish_accumulating_string(input, s, false);
                r.push(BalancedElem::String(s.value));
            }
            TokenKind::OpenParenthesis => {
                let (inner, _) = parse_balanced_elements(input);
                r.push(BalancedElem::Vec(inner));
            }
            TokenKind::ClosedParenthesis => return (r, token),
            TokenKind::Whitespace { .. } => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(source: &str, want: Vec<Node>, want_errors: Vec<ParseWarning>) {
        let (got, got_errors) = Cst::from_pl_source_code(source);
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
                    data: Some("World ".into()),
                    data_span: 8..14,
                    children: vec![
                        Node {
                            opening_parenthesis_span: 14,
                            value: NodeValue::Regular(RegularNodeValue {
                                key: "Nested".into(),
                                key_span: 15..21,
                                data: Some("One".into()),
                                data_span: 22..25,
                                children: vec![].into(),
                            }),
                            closing_parenthesis_span: 25,
                        },
                        Node {
                            opening_parenthesis_span: 27,
                            value: NodeValue::Regular(RegularNodeValue {
                                key: "Nested".into(),
                                key_span: 28..34,
                                data: Some("Two Two".into()),
                                data_span: 36..43,
                                children: vec![].into(),
                            }),
                            closing_parenthesis_span: 43,
                        },
                        Node {
                            opening_parenthesis_span: 45,
                            value: NodeValue::Regular(RegularNodeValue {
                                key: "Nested".into(),
                                key_span: 46..52,
                                data: Some("Three Three".into()),
                                data_span: 53..64,
                                children: vec![].into(),
                            }),
                            closing_parenthesis_span: 64,
                        },
                    ]
                    .into(),
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
                    data: Some("".into()),
                    data_span: 6..6,
                    children: vec![].into(),
                }),
                closing_parenthesis_span: 6,
            }],
            vec![ParseWarning {
                span: 6..6,
                knuth_pltotf_offset: Some(6),
                kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                    opening_parenthesis_span: 0..1,
                },
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
                    data: Some("".into()),
                    data_span: 1..1,
                    children: vec![].into(),
                }),
                closing_parenthesis_span: 1,
            }],
            vec![ParseWarning {
                span: 1..1,
                knuth_pltotf_offset: Some(1),
                kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                    opening_parenthesis_span: 0..1,
                },
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
                    data: Some("".into()),
                    data_span: 1..1,
                    children: vec![].into(),
                }),
                closing_parenthesis_span: 1,
            }],
            vec![],
        ),
        (
            garbage_string_in_list,
            "(Hello World) Garbage String (Hola Mundo)",
            vec![
                Node {
                    opening_parenthesis_span: 0,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hello".into(),
                        key_span: 1..6,
                        data: Some("World".into()),
                        data_span: 7..12,
                        children: vec![].into(),
                    }),
                    closing_parenthesis_span: 12,
                },
                Node {
                    opening_parenthesis_span: 29,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hola".into(),
                        key_span: 30..34,
                        data: Some("Mundo".into()),
                        data_span: 35..40,
                        children: vec![].into(),
                    }),
                    closing_parenthesis_span: 40,
                },
            ],
            vec![ParseWarning {
                span: 14..29,
                knuth_pltotf_offset: Some(15),
                kind: ParseWarningKind::JunkInsidePropertyList {
                    junk: "Garbage String ".into(),
                },
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
                        data: Some("World".into()),
                        data_span: 7..12,
                        children: vec![].into(),
                    }),
                    closing_parenthesis_span: 12,
                },
                Node {
                    opening_parenthesis_span: 16,
                    value: NodeValue::Regular(RegularNodeValue {
                        key: "Hola".into(),
                        key_span: 17..21,
                        data: Some("Mundo".into()),
                        data_span: 22..27,
                        children: vec![].into(),
                    }),
                    closing_parenthesis_span: 27,
                },
            ],
            vec![ParseWarning {
                span: 14..15,
                knuth_pltotf_offset: Some(14),
                kind: ParseWarningKind::UnexpectedClosingParenthesis,
            }],
        ),
        (
            comment,
            "(COMMENT World (Nested One) Hello (Nested Two))",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Comment(vec![
                    BalancedElem::String("World ".into()),
                    BalancedElem::Vec(vec![BalancedElem::String("Nested One".into()),]),
                    BalancedElem::String("Hello ".into()),
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
                    BalancedElem::String("World ".into()),
                    BalancedElem::Vec(vec![
                        BalancedElem::String("Nested".into()),
                        BalancedElem::String("One".into()),
                    ]),
                    BalancedElem::String("Hello ".into()),
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
                    BalancedElem::String("World ".into()),
                    BalancedElem::Vec(vec![]),
                ]),
                closing_parenthesis_span: 16,
            }],
            vec![
                ParseWarning {
                    span: 16..16,
                    knuth_pltotf_offset: Some(16),
                    kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                        opening_parenthesis_span: 15..16,
                    }
                },
                ParseWarning {
                    span: 16..16,
                    knuth_pltotf_offset: Some(16),
                    kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                        opening_parenthesis_span: 0..1,
                    }
                },
            ],
        ),
        (
            non_visible_ascii_char,
            "(Hello Worldä)",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "Hello".into(),
                    key_span: 1..6,
                    data: Some("Worldä".into()),
                    data_span: 7..13,
                    children: vec![].into(),
                }),
                closing_parenthesis_span: 13,
            }],
            vec![],
        ),
        (
            trailing_space,
            "(Hello World )",
            vec![Node {
                opening_parenthesis_span: 0,
                value: NodeValue::Regular(RegularNodeValue {
                    key: "Hello".into(),
                    key_span: 1..6,
                    data: Some("World ".into()),
                    data_span: 7..13,
                    children: vec![].into(),
                }),
                closing_parenthesis_span: 13,
            },],
            vec![],
        ),
    );
}
