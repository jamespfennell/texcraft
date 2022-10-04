//! Conditional primitives (if, else, fi, and switch)
//!
//! # Writing new conditional primitives

use std::any;
use texlang_core::parse;
use texlang_core::prelude::*;
use texlang_core::runtime::HasComponent;
use texlang_core::runtime::HasExpansionState;

pub const ELSE_DOC: &str = "Start the else branch of a conditional or switch statement";
pub const IFCASE_DOC: &str = "Begin a switch statement";
pub const IFNUM_DOC: &str = "Compare two variables";
pub const IFODD_DOC: &str = "Check if a variable is odd";
pub const IFTRUE_DOC: &str = "Evaluate the true branch";
pub const IFFALSE_DOC: &str = "Evaluate the false branch";
pub const FI_DOC: &str = "End a conditional or switch statement";
pub const OR_DOC: &str = "Begin the next branch of a switch statement";

/// A component that is attached to the input unit for keeping track of conditional branches
/// as they are expanded.
pub struct Component {
    // branches is a stack where each element corresponds to a conditional that is currently
    // expanding. A nested conditional is further up the stack than the conditional it is
    // nested in.
    //
    // This stack is used to
    // verify that \else and \fi tokens are valid; i.e., if a \else is encountered, the current
    // conditional must be true otherwise the \else is invalid.
    branches: Vec<Branch>,
}

#[derive(Debug)]
enum BranchKind {
    // The true branch of an if conditional.
    True,
    // The false branch of an if conditional, or the default branch of a switch statement.
    Else,
    // A regular case brach of a switch statement.
    Switch,
}

#[derive(Debug)]
struct Branch {
    _token: Token,
    kind: BranchKind,
}

impl Component {
    pub fn new() -> Component {
        Component {
            branches: Vec::new(),
        }
    }
}

impl Default for Component {
    fn default() -> Self {
        Self::new()
    }
}

fn branches<S>(input: &mut runtime::ExpansionInput<S>) -> &mut Vec<Branch>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    &mut input.expansion_state_mut().component_mut().branches
}

enum If {}
enum Else {}
enum Or {}
enum Fi {}

fn if_id() -> any::TypeId {
    any::TypeId::of::<If>()
}
fn else_id() -> any::TypeId {
    any::TypeId::of::<Else>()
}
fn or_id() -> any::TypeId {
    any::TypeId::of::<Or>()
}
fn fi_id() -> any::TypeId {
    any::TypeId::of::<Fi>()
}

// The `true_case` function is executed whenever a conditional evaluates to true.
fn true_case<S>(token: Token, input: &mut runtime::ExpansionInput<S>) -> anyhow::Result<Vec<Token>>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    branches(input).push(Branch {
        _token: token,
        kind: BranchKind::True,
    });
    Ok(Vec::new())
}

// The `false_case` function is executed whenever a conditional evaluates to false.
//
// The function scans forward in the input stream, discarding all tokens, until it encounters
// either a \else or \fi command.
fn false_case<S>(
    original_token: Token,
    input: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    let mut depth = 0;
    let mut last_token = None;
    while let Some(token) = input.unexpanded().next()? {
        if let ControlSequence(name) = &token.value() {
            let id = input.base().commands_map.get_id(name);
            if id == else_id() && depth == 0 {
                branches(input).push(Branch {
                    _token: original_token,
                    kind: BranchKind::Else,
                });
                return Ok(Vec::new());
            }
            if id == if_id() {
                depth += 1;
            }
            if id == fi_id() {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
        last_token = Some(token)
    }
    let token = match last_token {
        None => original_token,
        Some(token) => token,
    };
    let branch = branches(input).pop();
    Err(error::TokenError::new(
        token,
        "unexpected end of input while expanding an `if` command")
            .add_note("each `if` command must be terminated by a `fi` command, with an optional `else` in between")
            .add_note("this `if` command evaluated to false, and the input ended while skipping the true branch")
            .add_note("this is the `if` command involved in the error:")
            .add_note(format!["{:?}", branch])
            .cast()
    )
}

macro_rules! create_if_primitive {
    ($if_fn: ident, $if_primitive_fn: ident, $get_if: ident, $docs: expr) => {
        fn $if_primitive_fn<S>(
            token: Token,
            input: &mut runtime::ExpansionInput<S>,
        ) -> anyhow::Result<Vec<Token>>
        where
            S: HasExpansionState,
            S::E: HasComponent<Component>,
        {
            match $if_fn(input)? {
                true => true_case(token, input),
                false => false_case(token, input),
            }
        }

        pub fn $get_if<S>() -> command::Command<S>
        where
            S: HasExpansionState,
            S::E: HasComponent<Component>,
        {
            command::Command::new_expansion($if_primitive_fn).with_id(if_id())
        }
    };
}

fn if_true<S>(_: &mut runtime::ExpansionInput<S>) -> anyhow::Result<bool> {
    Ok(true)
}

fn if_false<S>(_: &mut runtime::ExpansionInput<S>) -> anyhow::Result<bool> {
    Ok(false)
}

fn if_num<S>(stream: &mut runtime::ExpansionInput<S>) -> anyhow::Result<bool> {
    let a: i32 = parse::parse_number(stream)?;
    let r = parse::parse_relation(stream)?;
    let b: i32 = parse::parse_number(stream)?;
    // println!("Cmp {} {:?} {}", a, r, b);
    Ok(match r {
        parse::Relation::LessThan => a < b,
        parse::Relation::Equal => a == b,
        parse::Relation::GreaterThan => a > b,
    })
}

fn if_odd<S>(stream: &mut runtime::ExpansionInput<S>) -> anyhow::Result<bool> {
    let n: i32 = parse::parse_number(stream)?;
    Ok((n % 2) == 1)
}

create_if_primitive![if_true, if_true_primitive_fn, get_if_true, IFTRUE_DOC];
create_if_primitive![if_false, if_false_primitive_fn, get_if_false, IFFALSE_DOC];
create_if_primitive![if_num, if_num_primitive_fn, get_if_num, IFNUM_DOC];
create_if_primitive![if_odd, if_odd_primitive_fn, get_if_odd, IFODD_DOC];

fn if_case_primitive_fn<S>(
    ifcase_token: Token,
    input: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    // TODO: should we reading the number from the unexpanded stream? Probably!
    let mut cases_to_skip: i32 = parse::parse_number(input)?;
    if cases_to_skip == 0 {
        branches(input).push(Branch {
            _token: ifcase_token,
            kind: BranchKind::Switch,
        });
        return Ok(Vec::new());
    }
    let mut depth = 0;
    let mut last_token = None;
    while let Some(token) = input.unexpanded().next()? {
        if let ControlSequence(name) = &token.value() {
            let id = input.base().commands_map.get_id(name);
            if id == or_id() && depth == 0 {
                cases_to_skip -= 1;
                if cases_to_skip == 0 {
                    branches(input).push(Branch {
                        _token: ifcase_token,
                        kind: BranchKind::Switch,
                    });
                    return Ok(Vec::new());
                }
            }
            if id == else_id() && depth == 0 {
                branches(input).push(Branch {
                    _token: ifcase_token,
                    kind: BranchKind::Else,
                });
                return Ok(Vec::new());
            }
            if id == if_id() {
                depth += 1;
            }
            if id == fi_id() {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
        last_token = Some(token);
    }
    let token = match last_token {
        None => ifcase_token,
        Some(token) => token,
    };
    Err(error::TokenError::new(
        token,
        "unexpected end of input while expanding an `ifcase` command",
    )
    .add_note("each `ifcase` command must be matched by a `or`, `else` or `fi` command")
    .add_note("this `ifcase` case evaluated to %d and we skipped %d cases before the input ran out")
    .add_note("this is the `ifnum` command involved in the error:")
    .cast())
}

/// Get the `\ifcase` primitive.
pub fn get_if_case<S>() -> command::Command<S>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    command::Command::new_expansion(if_case_primitive_fn).with_id(if_id())
}

fn or_primitive_fn<S>(
    ifcase_token: Token,
    input: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    let branch = branches(input).pop();
    // For an or command to be valid, we must be in a switch statement
    let is_valid = match branch {
        None => false,
        Some(branch) => matches!(branch.kind, BranchKind::Switch),
    };
    if !is_valid {
        return Err(error::TokenError::new(ifcase_token, "unexpected `or` command").cast());
    }

    let mut depth = 0;
    let mut last_token = None;
    while let Some(token) = input.unexpanded().next()? {
        if let ControlSequence(name) = &token.value() {
            let id = input.base().commands_map.get_id(name);
            if id == if_id() {
                depth += 1;
            }
            if id == fi_id() {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
        last_token = Some(token);
    }
    let token = match last_token {
        None => ifcase_token,
        Some(token) => token,
    };
    Err(error::TokenError::new(
        token,
        "unexpected end of input while expanding an `or` command".to_string()
    ).add_note(
            "each `or` command must be terminated by a `fi` command".to_string()
    ).add_note(
            "this `or` corresponds to an `ifcase` command that evaluated to %d, and the input ended while skipping the remaining cases".to_string()
    ).add_note(
            "this is the `ifcase` command involved in the error:".to_string()
    ).add_note(
            "this is the `or` command involved in the error:".to_string()
    ).cast()
    )
}

/// Get the `\or` primitive.
pub fn get_or<S>() -> command::Command<S>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    command::Command::new_expansion(or_primitive_fn).with_id(or_id())
}

fn else_primitive_fn<S>(
    else_token: Token,
    input: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    let branch = branches(input).pop();
    // For else token to be valid, we must be in the true branch of a conditional
    let is_valid = match branch {
        None => false,
        Some(branch) => matches!(branch.kind, BranchKind::True | BranchKind::Switch),
    };
    if !is_valid {
        return Err(error::TokenError::new(else_token, "unexpected `else` command").cast());
    }

    // Now consume all of the tokens until the next \fi
    let mut depth = 0;
    let mut last_token = None;
    while let Some(token) = input.unexpanded().next()? {
        if let ControlSequence(name) = &token.value() {
            let id = input.base().commands_map.get_id(name);
            if id == if_id() {
                depth += 1;
            }
            if id == fi_id() {
                depth -= 1;
                if depth < 0 {
                    return Ok(Vec::new());
                }
            }
        }
        last_token = Some(token);
    }
    let token = match last_token {
        None => else_token,
        Some(token) => token,
    };
    Err(error::TokenError::new(
        token,
        "unexpected end of input while expanding an `else` command")
            .add_note("each `else` command must be terminated by a `fi` command")
            .add_note("this `else` corresponds to an `if` command that evaluated to true, and the input ended while skipping the false branch")
            .add_note("this is the `if` command involved in the error:")
            .add_note("this is the `else` command involved in the error:")
    .cast())
}

/// Get the `\else` primitive.
pub fn get_else<S>() -> command::Command<S>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    command::Command::new_expansion(else_primitive_fn).with_id(else_id())
}

/// Get the `\fi` primitive.
fn fi_primitive_fn<S>(
    token: Token,
    input: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    let branch = branches(input).pop();
    // For a \fi primitive to be valid, we must be in a conditional.
    // Note that we could be in the false branch: \iftrue\else\fi
    // Or in the true branch: \iftrue\fi
    // Or in a switch statement.
    if branch.is_none() {
        return Err(error::TokenError::new(token, "unexpected `fi` command").cast());
    }
    Ok(Vec::new())
}

pub fn get_fi<S>() -> command::Command<S>
where
    S: HasExpansionState,
    S::E: HasComponent<Component>,
{
    command::Command::new_expansion(fi_primitive_fn).with_id(fi_id())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{script, testutil::*};
    use texlang_core::runtime::implement_has_component;

    #[derive(Default)]
    struct State {
        conditional: Component,
        exec: script::Component,
    }

    implement_has_component![State, (Component, conditional), (script::Component, exec),];

    impl runtime::HasExpansionState for State {
        type E = State;

        fn expansion_state_mut(&mut self) -> &mut Self::E {
            self
        }
    }

    fn setup_expansion_test(s: &mut runtime::Env<State>) {
        s.set_command("else", get_else());
        s.set_command("fi", get_fi());
        s.set_command("ifcase", get_if_case());
        s.set_command("iffalse", get_if_false());
        s.set_command("ifnum", get_if_num());
        s.set_command("ifodd", get_if_odd());
        s.set_command("iftrue", get_if_true());
        s.set_command("or", get_or());
    }

    expansion_test![iftrue_base_case, r"\iftrue a\else b\fi c", r"ac"];
    expansion_test![iftrue_no_else, r"\iftrue a\fi c", r"ac"];
    expansion_test![
        iftrue_skip_nested_ifs,
        r"\iftrue a\else b\iftrue \else c\fi d\fi e",
        r"ae"
    ];
    expansion_failure_test![iftrue_end_of_input, r"\iftrue a\else b"];
    expansion_test![iffalse_base_case, r"\iffalse a\else b\fi c", r"bc"];
    expansion_test![iffalse_no_else, r"\iffalse a\fi c", r"c"];
    expansion_test![
        iffalse_skip_nested_ifs,
        r"\iffalse \iftrue a\else b\fi c\else d\fi e",
        r"de"
    ];
    expansion_test![
        iffalse_and_iftrue_1,
        r"\iffalse a\else b\iftrue c\else d\fi e\fi f",
        r"bcef"
    ];
    expansion_test![
        iffalse_and_iftrue_2,
        r"\iftrue a\iffalse b\else c\fi d\else e\fi f",
        r"acdf"
    ];
    expansion_failure_test![iffalse_end_of_input, r"\iffalse a"];
    expansion_failure_test![else_not_expected, r"a\else"];
    expansion_failure_test![fi_not_expected, r"a\fi"];
    expansion_failure_test![or_not_expected, r"a\or"];

    expansion_test![ifnum_less_than_true, r"\ifnum 4<5a\else b\fi c", r"ac"];
    expansion_test![ifnum_less_than_false, r"\ifnum 5<4a\else b\fi c", r"bc"];
    expansion_test![ifnum_equal_true, r"\ifnum 4=4a\else b\fi c", r"ac"];
    expansion_test![ifnum_equal_false, r"\ifnum 5=4a\else b\fi c", r"bc"];
    expansion_test![ifnum_greater_than_true, r"\ifnum 5>4a\else b\fi c", r"ac"];
    expansion_test![ifnum_greater_than_false, r"\ifnum 4>5a\else b\fi c", r"bc"];

    expansion_test![ifodd_odd, r"\ifodd 3a\else b\fi c", r"ac"];
    expansion_test![ifodd_even, r"\ifodd 4a\else b\fi c", r"bc"];

    expansion_test![ifcase_zero_no_ors, r"\ifcase 0 a\else b\fi c", r"ac"];
    expansion_test![ifcase_zero_one_or, r"\ifcase 0 a\or b\else c\fi d", r"ad"];
    expansion_test![ifcase_one, r"\ifcase 1 a\or b\else c\fi d", r"bd"];
    expansion_test![
        ifcase_one_more_cases,
        r"\ifcase 1 a\or b\or c\else d\fi e",
        r"be"
    ];
    expansion_test![ifcase_else_no_ors, r"\ifcase 1 a\else b\fi c", r"bc"];
    expansion_test![ifcase_else_one_or, r"\ifcase 2 a\or b\else c\fi d", r"cd"];
    expansion_test![ifcase_no_matching_case, r"\ifcase 3 a\or b\or c\fi d", r"d"];
    expansion_test![
        ifcase_nested,
        r"\ifcase 1 a\or b\ifcase 1 c\or d\or e\else f\fi g\or h\fi i",
        r"bdgi"
    ];
}
