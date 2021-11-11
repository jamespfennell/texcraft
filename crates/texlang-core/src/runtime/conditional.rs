use crate::token::Token;
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
    pub branches: Vec<Branch>,
}

#[derive(Debug)]
pub enum BranchKind {
    // The true branch of an if conditional.
    True,
    // The false branch of an if conditional, or the default branch of a switch statement.
    Else,
    // A regular case brach of a switch statement.
    Switch,
}

#[derive(Debug)]
pub struct Branch {
    pub _token: Token,
    pub kind: BranchKind,
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
