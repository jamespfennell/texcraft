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

/// Concrete syntax tree for property list files
#[derive(Debug, PartialEq, Eq)]
pub struct Cst(pub Vec<Node>);

impl Cst {
    /// Build an CST directly from source code.
    pub fn from_pl_source_code(source: &str) -> (Cst, Vec<ParseWarning>) {
        parse(source)
    }

    /// Display the CST.
    pub fn display(
        &self,
        starting_indent: usize,
        additional_indent: usize,
    ) -> impl std::fmt::Display + '_ {
        DisplaySlice {
            nodes: &self.0,
            starting_indent,
            additional_indent,
        }
    }
}

/// Helper type for displaying slices of nodes.
struct DisplaySlice<'a> {
    nodes: &'a [Node],
    starting_indent: usize,
    additional_indent: usize,
}

impl<'a> std::fmt::Display for DisplaySlice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in self.nodes {
            write!(
                f,
                "{}",
                node.display(self.starting_indent, self.additional_indent)
            )?;
        }
        Ok(())
    }
}

/// Helper type for displaying nodes.
struct DisplayNode<'a> {
    node: &'a Node,
    starting_indent: usize,
    additional_indent: usize,
}

impl Node {
    /// Display the node.
    pub fn display(
        &self,
        starting_indent: usize,
        additional_indent: usize,
    ) -> impl std::fmt::Display + '_ {
        DisplayNode {
            node: self,
            starting_indent,
            additional_indent,
        }
    }
}

impl<'a> std::fmt::Display for DisplayNode<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let starting_indent = self.starting_indent;
        let additional_indent = self.additional_indent;
        match &self.node {
            // TODO: better handle all of the cases here with preceding newlines and trailing newlines
            Node::Comment(e) => {
                if e.contains('\n') {
                    // TODO this assumes e starts and ends with a newline?
                    // these kinds of bugs will appear if we write a PLST formatter
                    write!(f, "{}(COMMENT{e}", " ".repeat(starting_indent))?;
                    writeln!(f, "{})", " ".repeat(starting_indent + additional_indent))?;
                } else {
                    writeln!(f, "{}(COMMENT {e})", " ".repeat(starting_indent))?;
                }
            }
            Node::Regular(v) => {
                write!(f, "{}({}", " ".repeat(starting_indent), &v.key)?;
                if let Some(data) = &v.data {
                    write!(f, " {}", data)?;
                }
                if let Some(children) = &v.children {
                    writeln!(f)?;
                    for child in children {
                        write!(
                            f,
                            "{}",
                            child.display(starting_indent + additional_indent, additional_indent)
                        )?;
                    }
                    write!(f, "{}", " ".repeat(starting_indent + additional_indent))?;
                }
                writeln!(f, ")")?;
            }
        }
        Ok(())
    }
}

/// Node in the CST.
#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    /// A comment node.
    Comment(String),
    /// A regular non-comment node.
    Regular(RegularNode),
}

/// Value of a regular node in the CST.
#[derive(Debug, PartialEq, Eq)]
pub struct RegularNode {
    /// Span of the opening parenthesis for this node in the source file.
    ///
    /// It always has length 1.
    pub opening_parenthesis_span: std::ops::Range<usize>,
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
    /// Span of the closing parenthesis for this node in the source file.
    ///
    /// It either has length 1 (for a closing parenthesis that appears in the source file)
    /// or 0 (for a closing parenthesis automatically added to balance an unbalanced opening parenthesis).
    pub closing_parenthesis_span: std::ops::Range<usize>,
}

fn parse(s: &str) -> (Cst, Vec<ParseWarning>) {
    let mut warnings = vec![];
    let mut cst = Cst(vec![]);
    let mut stack: Vec<RegularNode> = vec![];
    let push = |cst: &mut Cst, stack: &mut Vec<RegularNode>, node: Node| match stack.last_mut() {
        None => cst.0.push(node),
        Some(tail) => tail.children.get_or_insert(vec![]).push(node),
    };

    let mut iter = ParseIter::new(s);
    while let Some((pos, c)) = iter.peek() {
        match c {
            '(' => {
                iter.next();
                let (key, key_span) = iter.accumulate_key();

                if key == "COMMENT" {
                    // todo: trim whitespace from the front of the comment?
                    let mut comment = String::new();
                    let mut comment_stack: Vec<usize> = vec![pos];
                    for (u, c) in iter.by_ref() {
                        match c {
                            '(' => {
                                comment_stack.push(u);
                            }
                            ')' => {
                                comment_stack.pop();
                                if comment_stack.is_empty() {
                                    break;
                                }
                            }
                            _ => {}
                        }
                        comment.push(c);
                    }
                    // If there are unbalanced opening parentheses in a comment, only 2 warnings
                    // are printed irrespective of the depth. The first warning is for the opening
                    // parenthesis starting the comment, and the second warning is for the opening
                    // parenthesis that increases the level to 1.
                    while comment_stack.len() > 2 {
                        comment.push(')');
                        comment_stack.pop();
                    }
                    if comment_stack.len() == 2 {
                        comment.push(')');
                    }
                    while let Some(u) = comment_stack.pop() {
                        let pos = iter.next_pos;
                        warnings.push(ParseWarning {
                            span: pos..pos,
                            knuth_pltotf_offset: Some(pos),
                            kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                                opening_parenthesis_span: u..u + 1,
                            },
                        });
                    }
                    push(&mut cst, &mut stack, Node::Comment(comment));
                } else {
                    iter.trim_whitespace();
                    let (data, data_span) = iter.accumulate_string();
                    let value = RegularNode {
                        opening_parenthesis_span: pos..pos + 1,
                        key,
                        key_span,
                        data: Some(data),
                        data_span,
                        children: Some(vec![]),
                        // the correct value is figured out later
                        closing_parenthesis_span: 0..0,
                    };
                    stack.push(value);
                }
            }
            ')' => {
                iter.next();
                match stack.pop() {
                    None => warnings.push(ParseWarning {
                        span: pos..pos + 1,
                        knuth_pltotf_offset: Some(pos),
                        kind: ParseWarningKind::UnexpectedClosingParenthesis,
                    }),
                    Some(mut finished) => {
                        finished.closing_parenthesis_span = pos..pos + 1;
                        push(&mut cst, &mut stack, Node::Regular(finished));
                    }
                }
            }
            ' ' | '\n' => {
                iter.next();
                continue;
            }
            _ => {
                let (junk, span) = iter.accumulate_string();
                warnings.push(ParseWarning {
                    span: span.clone(),
                    knuth_pltotf_offset: Some(span.start + 1),
                    kind: ParseWarningKind::JunkInsidePropertyList { junk },
                })
            }
        }
    }
    while let Some(mut finished) = stack.pop() {
        let pos = iter.next_pos;
        warnings.push(ParseWarning {
            span: pos..pos,
            knuth_pltotf_offset: Some(pos),
            kind: ParseWarningKind::UnbalancedOpeningParenthesis {
                opening_parenthesis_span: finished.opening_parenthesis_span.clone(),
            },
        });
        finished.closing_parenthesis_span = pos..pos;
        push(&mut cst, &mut stack, Node::Regular(finished));
    }
    warnings.extend(iter.warnings);
    (cst, warnings)
}

struct ParseIter<'a> {
    iter: std::iter::Peekable<super::Chars<'a>>,
    warnings: Vec<ParseWarning>,
    next_pos: usize,
}

impl<'a> Iterator for ParseIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|c| {
            let pos = self.next_pos;
            self.next_pos += 1;
            (pos, c)
        })
    }
}

impl<'a> ParseIter<'a> {
    fn peek(&mut self) -> Option<<Self as Iterator>::Item> {
        self.iter.peek().copied().map(|c| (self.next_pos, c))
    }
    fn new(s: &'a str) -> Self {
        Self {
            iter: super::Chars::new(s).peekable(),
            warnings: vec![],
            next_pos: 0,
        }
    }
    fn trim_whitespace(&mut self) {
        while let Some((_, c)) = self.peek() {
            match c {
                ' ' | '\n' => {
                    self.next();
                }
                _ => break,
            }
        }
    }
    fn accumulate_key(&mut self) -> (String, std::ops::Range<usize>) {
        self.accumulate_internal(|c| c.is_alphanumeric() || c == '/' || c == '>')
    }
    fn accumulate_string(&mut self) -> (String, std::ops::Range<usize>) {
        self.accumulate_internal(|c| c != ')' && c != '(')
    }
    fn accumulate_internal<F: Fn(char) -> bool>(
        &mut self,
        is_allowed_char: F,
    ) -> (String, std::ops::Range<usize>) {
        let mut s = String::new();
        let mut start: Option<usize> = None;
        while let Some((pos, c)) = self.peek() {
            if is_allowed_char(c) {
                self.next();
                s.push(c);
                start.get_or_insert(pos);
            } else {
                break;
            }
        }
        let start = start.unwrap_or(self.next_pos);
        (s, start..self.next_pos)
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
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 1..2,
                key: "Hello".into(),
                key_span: 2..7,
                data: Some("World ".into()),
                data_span: 8..14,
                children: vec![
                    Node::Regular(RegularNode {
                        opening_parenthesis_span: 14..15,
                        key: "Nested".into(),
                        key_span: 15..21,
                        data: Some("One".into()),
                        data_span: 22..25,
                        children: vec![].into(),
                        closing_parenthesis_span: 25..26,
                    }),
                    Node::Regular(RegularNode {
                        opening_parenthesis_span: 27..28,
                        key: "Nested".into(),
                        key_span: 28..34,
                        data: Some("Two Two".into()),
                        data_span: 36..43,
                        children: vec![].into(),
                        closing_parenthesis_span: 43..44,
                    }),
                    Node::Regular(RegularNode {
                        opening_parenthesis_span: 45..46,
                        key: "Nested".into(),
                        key_span: 46..52,
                        data: Some("Three\nThree".into()),
                        data_span: 53..64,
                        children: vec![].into(),
                        closing_parenthesis_span: 64..65,
                    }),
                ]
                .into(),
                closing_parenthesis_span: 65..66,
            }),],
            vec![],
        ),
        (
            basic_never_ends,
            "(Hello",
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 0..1,
                key: "Hello".into(),
                key_span: 1..6,
                data: Some("".into()),
                data_span: 6..6,
                children: vec![].into(),
                closing_parenthesis_span: 6..6,
            }),],
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
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 0..1,
                key: "".into(),
                key_span: 1..1,
                data: Some("".into()),
                data_span: 1..1,
                children: vec![].into(),
                closing_parenthesis_span: 1..1,
            }),],
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
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 0..1,
                key: "".into(),
                key_span: 1..1,
                data: Some("".into()),
                data_span: 1..1,
                children: vec![].into(),
                closing_parenthesis_span: 1..2,
            }),],
            vec![],
        ),
        (
            crlf,
            "(CRLF\r\nGOOD\r\r\nVALUE\r\n)",
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 0..1,
                key: "CRLF".into(),
                key_span: 1..5,
                data: Some("GOOD\nVALUE\n".into()),
                data_span: 6..17,
                children: vec![].into(),
                closing_parenthesis_span: 17..18,
            }),],
            vec![],
        ),
        (
            garbage_string_in_list,
            "(Hello World) Garbage String (Hola Mundo)",
            vec![
                Node::Regular(RegularNode {
                    opening_parenthesis_span: 0..1,
                    key: "Hello".into(),
                    key_span: 1..6,
                    data: Some("World".into()),
                    data_span: 7..12,
                    children: vec![].into(),
                    closing_parenthesis_span: 12..13,
                }),
                Node::Regular(RegularNode {
                    opening_parenthesis_span: 29..30,
                    key: "Hola".into(),
                    key_span: 30..34,
                    data: Some("Mundo".into()),
                    data_span: 35..40,
                    children: vec![].into(),
                    closing_parenthesis_span: 40..41,
                }),
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
                Node::Regular(RegularNode {
                    opening_parenthesis_span: 0..1,
                    key: "Hello".into(),
                    key_span: 1..6,
                    data: Some("World".into()),
                    data_span: 7..12,
                    children: vec![].into(),
                    closing_parenthesis_span: 12..13,
                }),
                Node::Regular(RegularNode {
                    opening_parenthesis_span: 16..17,
                    key: "Hola".into(),
                    key_span: 17..21,
                    data: Some("Mundo".into()),
                    data_span: 22..27,
                    children: vec![].into(),
                    closing_parenthesis_span: 27..28,
                }),
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
            vec![Node::Comment(
                " World (Nested One) Hello (Nested Two)".into()
            )],
            vec![],
        ),
        (
            comment_with_newlines,
            "(COMMENT World (Nested\nOne) Hello (Nested\nTwo))",
            vec![Node::Comment(
                " World (Nested\nOne) Hello (Nested\nTwo)".into()
            ),],
            vec![],
        ),
        (
            comment_trailing_space,
            "(COMMENT World )",
            vec![Node::Comment(" World ".into()),],
            vec![],
        ),
        (
            comment_never_ends,
            "(COMMENT World (",
            vec![Node::Comment(" World ()".into())],
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
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 0..1,
                key: "Hello".into(),
                key_span: 1..6,
                data: Some("Worldä".into()),
                data_span: 7..13,
                children: vec![].into(),
                closing_parenthesis_span: 13..14,
            }),],
            vec![],
        ),
        (
            trailing_space,
            "(Hello World )",
            vec![Node::Regular(RegularNode {
                opening_parenthesis_span: 0..1,
                key: "Hello".into(),
                key_span: 1..6,
                data: Some("World ".into()),
                data_span: 7..13,
                children: vec![].into(),
                closing_parenthesis_span: 13..14,
            }),],
            vec![],
        ),
    );
}
