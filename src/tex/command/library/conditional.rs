//! Conditional primitives (if, else, fi, and switch)
//!
//! # Writing new conditional primitives

use crate::tex::parse;
use crate::tex::prelude::*;
use std::any;

const ELSE_DOC: &str = "Start the else branch of a conditional or switch statement";
const IFCASE_DOC: &str = "Begin a switch statement";
const IFNUM_DOC: &str = "Compare two variables";
const IFODD_DOC: &str = "Check if a variable is odd";
const IFTRUE_DOC: &str = "Evaluate the true branch";
const IFFALSE_DOC: &str = "Evaluate the false branch";
const FI_DOC: &str = "End a conditional or switch statement";
const OR_DOC: &str = "Begin the next branch of a switch statement";

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
    token: Token,
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

fn branches<'a, S>(input: &'a mut command::ExpansionInput<S>) -> &'a mut Vec<Branch> {
    &mut input.controller_mut().conditional.branches
}

enum If {}
enum Else {}
enum Or {}
enum Fi {}

fn if_id() -> Option<any::TypeId> {
    Some(any::TypeId::of::<If>())
}
fn else_id() -> Option<any::TypeId> {
    Some(any::TypeId::of::<Else>())
}
fn or_id() -> Option<any::TypeId> {
    Some(any::TypeId::of::<Or>())
}
fn fi_id() -> Option<any::TypeId> {
    Some(any::TypeId::of::<Fi>())
}

// The `true_case` function is executed whenever a conditional evaluates to true.
fn true_case<S>(
    token: Token,
    input: &mut command::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    input.controller_mut().conditional.branches.push(Branch {
        token,
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
    input: &mut command::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    let mut depth = 0;
    let mut last_token = None;
    while let Some(token) = input.unexpanded_stream().next()? {
        if let ControlSequence(_, name) = &token.value() {
            if let Some(c) = input.base().get_command(name) {
                if c.id() == else_id() && depth == 0 {
                    // TODO: push the token
                    input.controller_mut().conditional.branches.push(Branch {
                        token: original_token,
                        kind: BranchKind::Else,
                    });
                    return Ok(Vec::new());
                }
                if c.id() == if_id() {
                    depth += 1;
                }
                if c.id() == fi_id() {
                    depth -= 1;
                    if depth < 0 {
                        return Ok(Vec::new());
                    }
                }
            }
        }
        last_token = Some(token)
    }
    let token = match last_token {
        None => original_token,
        Some(token) => token,
    };
    let branch = input.controller_mut().conditional.branches.pop();
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
            input: &mut ExpansionInput<S>,
        ) -> anyhow::Result<Vec<Token>> {
            match $if_fn(input)? {
                true => true_case(token, input),
                false => false_case(token, input),
            }
        }

        pub fn $get_if<S>() -> command::ExpansionPrimitive<S> {
            command::ExpansionPrimitive {
                call_fn: $if_primitive_fn,
                docs: $docs,
                id: Some(any::TypeId::of::<If>()),
            }
        }
    };
}

fn if_true<S>(_: &mut command::ExpansionInput<S>) -> anyhow::Result<bool> {
    Ok(true)
}

fn if_false<S>(_: &mut command::ExpansionInput<S>) -> anyhow::Result<bool> {
    Ok(false)
}

fn if_num<S>(stream: &mut command::ExpansionInput<S>) -> anyhow::Result<bool> {
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

fn if_odd<S>(stream: &mut command::ExpansionInput<S>) -> anyhow::Result<bool> {
    let n: i32 = parse::parse_number(stream)?;
    Ok((n % 2) == 1)
}

create_if_primitive![if_true, if_true_primitive_fn, get_if_true, IFTRUE_DOC];
create_if_primitive![if_false, if_false_primitive_fn, get_if_false, IFFALSE_DOC];
create_if_primitive![if_num, if_num_primitive_fn, get_if_num, IFNUM_DOC];
create_if_primitive![if_odd, if_odd_primitive_fn, get_if_odd, IFODD_DOC];

fn if_case_primitive_fn<S>(
    ifcase_token: Token,
    input: &mut ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    // TODO: should we reading the number from the unexpanded stream? Probably!
    let mut cases_to_skip: i32 = parse::parse_number(input)?;
    if cases_to_skip == 0 {
        branches(input).push(Branch {
            token: ifcase_token,
            kind: BranchKind::Switch,
        });
        return Ok(Vec::new());
    }
    let mut depth = 0;
    let mut last_token = None;
    while let Some(token) = input.unexpanded_stream().next()? {
        if let ControlSequence(_, name) = &token.value() {
            if let Some(c) = input.base().get_command(name) {
                if c.id() == or_id() && depth == 0 {
                    cases_to_skip -= 1;
                    if cases_to_skip == 0 {
                        branches(input).push(Branch {
                            token: ifcase_token,
                            kind: BranchKind::Switch,
                        });
                        return Ok(Vec::new());
                    }
                }
                if c.id() == else_id() && depth == 0 {
                    branches(input).push(Branch {
                        token: ifcase_token,
                        kind: BranchKind::Else,
                    });
                    return Ok(Vec::new());
                }
                if c.id() == if_id() {
                    depth += 1;
                }
                if c.id() == fi_id() {
                    depth -= 1;
                    if depth < 0 {
                        return Ok(Vec::new());
                    }
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
pub fn get_if_case<S>() -> command::ExpansionPrimitive<S> {
    command::ExpansionPrimitive {
        call_fn: if_case_primitive_fn,
        docs: IFCASE_DOC,
        id: Some(any::TypeId::of::<If>()),
    }
}

fn or_primitive_fn<S>(
    ifcase_token: Token,
    input: &mut ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
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
    while let Some(token) = input.unexpanded_stream().next()? {
        if let ControlSequence(_, name) = &token.value() {
            if let Some(c) = input.base().get_command(name) {
                if c.id() == if_id() {
                    depth += 1;
                }
                if c.id() == fi_id() {
                    depth -= 1;
                    if depth < 0 {
                        return Ok(Vec::new());
                    }
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
pub fn get_or<S>() -> command::ExpansionPrimitive<S> {
    command::ExpansionPrimitive {
        call_fn: or_primitive_fn,
        docs: OR_DOC,
        id: Some(any::TypeId::of::<Or>()),
    }
}

fn else_primitive_fn<S>(
    else_token: Token,
    input: &mut command::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    let branch = input.controller_mut().conditional.branches.pop();
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
    while let Some(token) = input.unexpanded_stream().next()? {
        if let ControlSequence(_, name) = &token.value() {
            if let Some(c) = input.base().get_command(name) {
                if c.id() == if_id() {
                    depth += 1;
                }
                if c.id() == fi_id() {
                    depth -= 1;
                    if depth < 0 {
                        return Ok(Vec::new());
                    }
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
pub fn get_else<S>() -> command::ExpansionPrimitive<S> {
    command::ExpansionPrimitive {
        call_fn: else_primitive_fn,
        docs: ELSE_DOC,
        id: Some(any::TypeId::of::<Else>()),
    }
}

/// Get the `\fi` primitive.
fn fi_primitive_fn<S>(token: Token, input: &mut ExpansionInput<S>) -> anyhow::Result<Vec<Token>> {
    let branch = input.controller_mut().conditional.branches.pop();
    // For a \fi primitive to be valid, we must be in a conditional.
    // Note that we could be in the false branch: \iftrue\else\fi
    // Or in the true branch: \iftrue\fi
    // Or in a switch statement.
    if branch.is_none() {
        return Err(error::TokenError::new(token, "unexpected `fi` command").cast());
    }
    Ok(Vec::new())
}

pub fn get_fi<S>() -> command::ExpansionPrimitive<S> {
    command::ExpansionPrimitive {
        call_fn: fi_primitive_fn,
        docs: FI_DOC,
        id: Some(any::TypeId::of::<Fi>()),
    }
}

/// Add all of the conditionals defined in this module to the provided state.
pub fn add_all_conditionals<S>(s: &mut Base<S>) {
    s.set_command("iftrue", get_if_true());
    s.set_command("iffalse", get_if_false());
    s.set_command("ifnum", get_if_num());
    s.set_command("ifodd", get_if_odd());
    s.set_command("ifcase", get_if_case());
    s.set_command("or", get_or());
    s.set_command("else", get_else());
    s.set_command("fi", get_fi());
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::tex::driver;
    use crate::tex::input;
    use crate::tex::state::Base;
    use crate::tex::token::catcode;

    struct State;
    fn new_state() -> State {
        State {}
    }

    fn setup_expansion_test(s: &mut Base<State>) {
        add_all_conditionals(s);
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
