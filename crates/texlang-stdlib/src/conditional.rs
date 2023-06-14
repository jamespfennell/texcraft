//! Control flow primitives (if, else, switch)
//!

use std::cell::RefCell;
use std::cmp::Ordering;
use texlang_core::token::trace;
use texlang_core::traits::*;
use texlang_core::*;

pub const ELSE_DOC: &str = "Start the else branch of a conditional or switch statement";
pub const IFCASE_DOC: &str = "Begin a switch statement";
pub const IFNUM_DOC: &str = "Compare two variables";
pub const IFODD_DOC: &str = "Check if a variable is odd";
pub const IFTRUE_DOC: &str = "Evaluate the true branch";
pub const IFFALSE_DOC: &str = "Evaluate the false branch";
pub const FI_DOC: &str = "End a conditional or switch statement";
pub const OR_DOC: &str = "Begin the next branch of a switch statement";

/// A component for keeping track of conditional branches as they are expanded.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    // Branches is a stack where each element corresponds to a conditional that is currently
    // expanding. A nested conditional is further up the stack than the conditional it is
    // nested in.
    //
    // This stack is used to
    // verify that \else and \fi tokens are valid; i.e., if a \else is encountered, the current
    // conditional must be true otherwise the \else is invalid. For correct TeX code, the stack
    // is never actually used.
    //
    // Because the conditional commands are expansion commands, they cannot get a mutable reference
    // to the state. We thus wrap the branches in a ref cell to support mutating them through
    // an immutable reference.
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "serialize_branches",
            deserialize_with = "deserialize_branches"
        )
    )]
    branches: RefCell<Vec<Branch>>,

    // We cache the tag values inside the component for performance reasons.
    #[cfg_attr(feature = "serde", serde(skip))]
    tags: Tags,
}

#[cfg(feature = "serde")]
fn serialize_branches<S>(input: &RefCell<Vec<Branch>>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::Serialize;
    let slice: &[Branch] = &input.borrow();
    slice.serialize(serializer)
}

#[cfg(feature = "serde")]
fn deserialize_branches<'de, D>(deserializer: D) -> Result<RefCell<Vec<Branch>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::Deserialize;
    let vec = Vec::<Branch>::deserialize(deserializer)?;
    Ok(RefCell::new(vec))
}

struct Tags {
    if_tag: command::Tag,
    else_tag: command::Tag,
    or_tag: command::Tag,
    fi_tag: command::Tag,
}

impl Default for Tags {
    fn default() -> Self {
        Self {
            if_tag: IF_TAG.get(),
            else_tag: ELSE_TAG.get(),
            or_tag: OR_TAG.get(),
            fi_tag: FI_TAG.get(),
        }
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum BranchKind {
    // The true branch of an if conditional.
    True,
    // The false branch of an if conditional, or the default branch of a switch statement.
    Else,
    // A regular case brach of a switch statement.
    Switch,
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct Branch {
    _token: token::Token,
    kind: BranchKind,
}

impl Component {
    pub fn new() -> Component {
        Component {
            branches: RefCell::new(Vec::new()),
            tags: Default::default(),
        }
    }
}

impl Default for Component {
    fn default() -> Self {
        Self::new()
    }
}

fn push_branch<S: HasComponent<Component>>(input: &mut vm::ExpansionInput<S>, branch: Branch) {
    input.state().component().branches.borrow_mut().push(branch)
}

fn pop_branch<S: HasComponent<Component>>(input: &mut vm::ExpansionInput<S>) -> Option<Branch> {
    input.state().component().branches.borrow_mut().pop()
}

static IF_TAG: command::StaticTag = command::StaticTag::new();
static ELSE_TAG: command::StaticTag = command::StaticTag::new();
static OR_TAG: command::StaticTag = command::StaticTag::new();
static FI_TAG: command::StaticTag = command::StaticTag::new();

// The `true_case` function is executed whenever a conditional evaluates to true.
fn true_case<S: HasComponent<Component>>(
    token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    push_branch(
        input,
        Branch {
            _token: token,
            kind: BranchKind::True,
        },
    );
    Ok(Vec::new())
}

// The `false_case` function is executed whenever a conditional evaluates to false.
//
// The function scans forward in the input stream, discarding all tokens, until it encounters
// either a \else or \fi command.
fn false_case<S: HasComponent<Component>>(
    original_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    let mut depth = 0;
    while let Some(token) = input.unexpanded().next()? {
        if let token::Value::ControlSequence(name) = &token.value() {
            // TODO: use switch
            let tag = input.commands_map().get_tag(name);
            if tag == Some(input.state().component().tags.else_tag) && depth == 0 {
                push_branch(
                    input,
                    Branch {
                        _token: original_token,
                        kind: BranchKind::Else,
                    },
                );
                return Ok(Vec::new());
            }
            if tag == Some(input.state().component().tags.if_tag) {
                depth += 1;
            }
            if tag == Some(input.state().component().tags.fi_tag) {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
    }
    let branch = pop_branch(input);
    Err(FalseBranchEndOfInputError {
        trace: input.vm().trace_end_of_input(),
        branch,
    }
    .into())
}

#[derive(Debug)]
struct FalseBranchEndOfInputError {
    trace: trace::SourceCodeTrace,
    branch: Option<Branch>,
}

impl error::TexError for FalseBranchEndOfInputError {
    fn kind(&self) -> error::Kind {
        error::Kind::EndOfInput(&self.trace)
    }

    fn title(&self) -> String {
        "unexpected end of input while expanding an `if` command".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![
            "each `if` command must be terminated by a `fi` command, with an optional `else` in between".into(),
            "this `if` command evaluated to false, and the input ended while skipping the true branch".into(),
            "this is the `if` command involved in the error:".into(),
            format!["{:?}", self.branch].into(),
        ]
    }
}

macro_rules! create_if_primitive {
    ($if_fn: ident, $if_primitive_fn: ident, $get_if: ident, $docs: expr) => {
        fn $if_primitive_fn<S: HasComponent<Component>>(
            token: token::Token,
            input: &mut vm::ExpansionInput<S>,
        ) -> Result<Vec<token::Token>, Box<error::Error>> {
            match $if_fn(input)? {
                true => true_case(token, input),
                false => false_case(token, input),
            }
        }

        pub fn $get_if<S: HasComponent<Component>>() -> command::BuiltIn<S> {
            command::BuiltIn::new_expansion($if_primitive_fn)
                .with_tag(IF_TAG.get())
                .with_doc($docs)
        }
    };
}

fn if_true<S>(_: &mut vm::ExpansionInput<S>) -> Result<bool, Box<error::Error>> {
    Ok(true)
}

fn if_false<S>(_: &mut vm::ExpansionInput<S>) -> Result<bool, Box<error::Error>> {
    Ok(false)
}

fn if_num<S: TexlangState>(stream: &mut vm::ExpansionInput<S>) -> Result<bool, Box<error::Error>> {
    let (a, o, b) = <(i32, Ordering, i32)>::parse(stream)?;
    Ok(a.cmp(&b) == o)
}

fn if_odd<S: TexlangState>(stream: &mut vm::ExpansionInput<S>) -> Result<bool, Box<error::Error>> {
    let n = i32::parse(stream)?;
    Ok((n % 2) == 1)
}

create_if_primitive![if_true, if_true_primitive_fn, get_if_true, IFTRUE_DOC];
create_if_primitive![if_false, if_false_primitive_fn, get_if_false, IFFALSE_DOC];
create_if_primitive![if_num, if_num_primitive_fn, get_if_num, IFNUM_DOC];
create_if_primitive![if_odd, if_odd_primitive_fn, get_if_odd, IFODD_DOC];

fn if_case_primitive_fn<S: HasComponent<Component>>(
    ifcase_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    // TODO: should we reading the number from the unexpanded stream? Probably!
    let mut cases_to_skip = i32::parse(input)?;
    if cases_to_skip == 0 {
        push_branch(
            input,
            Branch {
                _token: ifcase_token,
                kind: BranchKind::Switch,
            },
        );
        return Ok(Vec::new());
    }
    let mut depth = 0;
    while let Some(token) = input.unexpanded().next()? {
        if let token::Value::ControlSequence(name) = &token.value() {
            // TODO: switch
            let tag = input.commands_map().get_tag(name);
            if tag == Some(input.state().component().tags.or_tag) && depth == 0 {
                cases_to_skip -= 1;
                if cases_to_skip == 0 {
                    push_branch(
                        input,
                        Branch {
                            _token: ifcase_token,
                            kind: BranchKind::Switch,
                        },
                    );
                    return Ok(Vec::new());
                }
            }
            if tag == Some(input.state().component().tags.else_tag) && depth == 0 {
                push_branch(
                    input,
                    Branch {
                        _token: ifcase_token,
                        kind: BranchKind::Else,
                    },
                );
                return Ok(Vec::new());
            }
            if tag == Some(input.state().component().tags.if_tag) {
                depth += 1;
            }
            if tag == Some(input.state().component().tags.fi_tag) {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
    }
    Err(IfCaseEndOfInputError {
        trace: input.trace_end_of_input(),
    }
    .into())
}

#[derive(Debug)]
struct IfCaseEndOfInputError {
    trace: trace::SourceCodeTrace,
}

impl error::TexError for IfCaseEndOfInputError {
    fn kind(&self) -> error::Kind {
        error::Kind::EndOfInput(&self.trace)
    }

    fn title(&self) -> String {
        "unexpected end of input while expanding an `ifcase` command".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![
            "each `ifcase` command must be matched by a `or`, `else` or `fi` command".into(),
            "this `ifcase` case evaluated to %d and we skipped %d cases before the input ran out"
                .into(),
            "this is the `ifnum` command involved in the error:".into(),
        ]
    }
}

/// Get the `\ifcase` primitive.
pub fn get_if_case<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(if_case_primitive_fn).with_tag(IF_TAG.get())
}

fn or_primitive_fn<S: HasComponent<Component>>(
    ifcase_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    let branch = pop_branch(input);
    // For an or command to be valid, we must be in a switch statement
    let is_valid = match branch {
        None => false,
        Some(branch) => matches!(branch.kind, BranchKind::Switch),
    };
    if !is_valid {
        return Err(error::SimpleTokenError::new(
            input.vm(),
            ifcase_token,
            "unexpected `or` command",
        )
        .into());
    }

    let mut depth = 0;
    while let Some(token) = input.unexpanded().next()? {
        if let token::Value::ControlSequence(name) = &token.value() {
            let tag = input.commands_map().get_tag(name);
            if tag == Some(input.state().component().tags.if_tag) {
                depth += 1;
            }
            if tag == Some(input.state().component().tags.fi_tag) {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
    }
    Err(OrEndOfInputError {
        trace: input.vm().trace_end_of_input(),
    }
    .into())
}

#[derive(Debug)]
struct OrEndOfInputError {
    trace: trace::SourceCodeTrace,
}

impl error::TexError for OrEndOfInputError {
    fn kind(&self) -> error::Kind {
        error::Kind::EndOfInput(&self.trace)
    }

    fn title(&self) -> String {
        "unexpected end of input while expanding an `or` command".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![
        "each `or` command must be terminated by a `fi` command".into(),
        "this `or` corresponds to an `ifcase` command that evaluated to %d, and the input ended while skipping the remaining cases".into(),
        "this is the `ifcase` command involved in the error:".into(),
        "this is the `or` command involved in the error:".into(),
        ]
    }
}

/// Get the `\or` primitive.
pub fn get_or<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(or_primitive_fn).with_tag(OR_TAG.get())
}

fn else_primitive_fn<S: HasComponent<Component>>(
    else_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    let branch = pop_branch(input);
    // For else token to be valid, we must be in the true branch of a conditional
    let is_valid = match branch {
        None => false,
        Some(branch) => matches!(branch.kind, BranchKind::True | BranchKind::Switch),
    };
    if !is_valid {
        return Err(error::SimpleTokenError::new(
            input.vm(),
            else_token,
            "unexpected `else` command",
        )
        .into());
    }

    // Now consume all of the tokens until the next \fi
    let mut depth = 0;
    while let Some(token) = input.unexpanded().next()? {
        if let token::Value::ControlSequence(name) = &token.value() {
            // TODO: switch
            let tag = input.commands_map().get_tag(name);
            if tag == Some(input.state().component().tags.if_tag) {
                depth += 1;
            }
            if tag == Some(input.state().component().tags.fi_tag) {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
    }
    Err(ElseEndOfInputError {
        trace: input.vm().trace_end_of_input(),
    }
    .into())
}

#[derive(Debug)]
struct ElseEndOfInputError {
    trace: trace::SourceCodeTrace,
}

impl error::TexError for ElseEndOfInputError {
    fn kind(&self) -> error::Kind {
        error::Kind::EndOfInput(&self.trace)
    }

    fn title(&self) -> String {
        "unexpected end of input while expanding an `else` command".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![
            "each `else` command must be terminated by a `fi` command".into(),
            "this `else` corresponds to an `if` command that evaluated to true, and the input ended while skipping the false branch".into(),
            "this is the `if` command involved in the error:".into(),
            "this is the `else` command involved in the error:".into(),
        ]
    }
}

/// Get the `\else` primitive.
pub fn get_else<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(else_primitive_fn).with_tag(ELSE_TAG.get())
}

/// Get the `\fi` primitive.
fn fi_primitive_fn<S: HasComponent<Component>>(
    token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<Vec<token::Token>, Box<error::Error>> {
    let branch = pop_branch(input);
    // For a \fi primitive to be valid, we must be in a conditional.
    // Note that we could be in the false branch: \iftrue\else\fi
    // Or in the true branch: \iftrue\fi
    // Or in a switch statement.
    if branch.is_none() {
        return Err(
            error::SimpleTokenError::new(input.vm(), token, "unexpected `fi` command").into(),
        );
    }
    Ok(Vec::new())
}

pub fn get_fi<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(fi_primitive_fn).with_tag(FI_TAG.get())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{script, testing::*};
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct State {
        conditional: Component,
        exec: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![State, (Component, conditional), (script::Component, exec),];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("else", get_else()),
            ("fi", get_fi()),
            ("ifcase", get_if_case()),
            ("iffalse", get_if_false()),
            ("ifnum", get_if_num()),
            ("ifodd", get_if_odd()),
            ("iftrue", get_if_true()),
            ("or", get_or()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (iftrue_base_case, r"\iftrue a\else b\fi c", r"ac"),
            (iftrue_no_else, r"\iftrue a\fi c", r"ac"),
            (
                iftrue_skip_nested_ifs,
                r"\iftrue a\else b\iftrue \else c\fi d\fi e",
                r"ae"
            ),
            (iffalse_base_case, r"\iffalse a\else b\fi c", r"bc"),
            (iffalse_no_else, r"\iffalse a\fi c", r"c"),
            (
                iffalse_skip_nested_ifs,
                r"\iffalse \iftrue a\else b\fi c\else d\fi e",
                r"de"
            ),
            (
                iffalse_and_iftrue_1,
                r"\iffalse a\else b\iftrue c\else d\fi e\fi f",
                r"bcef"
            ),
            (
                iffalse_and_iftrue_2,
                r"\iftrue a\iffalse b\else c\fi d\else e\fi f",
                r"acdf"
            ),
            (ifnum_less_than_true, r"\ifnum 4<5a\else b\fi c", r"ac"),
            (ifnum_less_than_false, r"\ifnum 5<4a\else b\fi c", r"bc"),
            (ifnum_equal_true, r"\ifnum 4=4a\else b\fi c", r"ac"),
            (ifnum_equal_false, r"\ifnum 5=4a\else b\fi c", r"bc"),
            (ifnum_greater_than_true, r"\ifnum 5>4a\else b\fi c", r"ac"),
            (ifnum_greater_than_false, r"\ifnum 4>5a\else b\fi c", r"bc"),
            (ifodd_odd, r"\ifodd 3a\else b\fi c", r"ac"),
            (ifodd_even, r"\ifodd 4a\else b\fi c", r"bc"),
            (ifcase_zero_no_ors, r"\ifcase 0 a\else b\fi c", r"ac"),
            (ifcase_zero_one_or, r"\ifcase 0 a\or b\else c\fi d", r"ad"),
            (ifcase_one, r"\ifcase 1 a\or b\else c\fi d", r"bd"),
            (
                ifcase_one_more_cases,
                r"\ifcase 1 a\or b\or c\else d\fi e",
                r"be"
            ),
            (ifcase_else_no_ors, r"\ifcase 1 a\else b\fi c", r"bc"),
            (ifcase_else_one_or, r"\ifcase 2 a\or b\else c\fi d", r"cd"),
            (ifcase_no_matching_case, r"\ifcase 3 a\or b\or c\fi d", r"d"),
            (
                ifcase_nested,
                r"\ifcase 1 a\or b\ifcase 1 c\or d\or e\else f\fi g\or h\fi i",
                r"bdgi"
            ),
        ),
        serde_tests(
            (serde_if, r"\iftrue true ", r"branch \else false branch \fi"),
            (
                serde_ifcase,
                r"\ifcase 2 a\or b\or executed ",
                r"case \or d \fi"
            )
        ),
        failure_tests(
            (iftrue_end_of_input, r"\iftrue a\else b"),
            (iffalse_end_of_input, r"\iffalse a"),
            (else_not_expected, r"a\else"),
            (fi_not_expected, r"a\fi"),
            (or_not_expected, r"a\or"),
        ),
    ];
}
