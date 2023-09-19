use crate::token;
use crate::token::trace;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Error {
    propagated: Vec<PropagatedError>,
    root: TexError,
}

impl From<&super::Error> for Error {
    fn from(mut value: &super::Error) -> Self {
        let mut propagated: Vec<PropagatedError> = Default::default();
        loop {
            match value {
                super::Error::Tex(root) => {
                    return Self {
                        propagated,
                        root: root.as_ref().into(),
                    }
                }
                super::Error::Propagated(p) => {
                    propagated.push(p.into());
                    value = &p.error
                }
            }
        }
    }
}

impl From<Error> for super::Error {
    fn from(value: Error) -> Self {
        let mut result = super::Error::Tex(Box::<TexError>::new(value.root));
        for propagated in value.propagated.into_iter().rev() {
            result = super::Error::Propagated(super::PropagatedError {
                context: propagated.context,
                token: propagated.token,
                trace: propagated.trace,
                error: Box::new(result),
            })
        }
        result
    }
}

#[derive(Serialize, Deserialize)]
struct PropagatedError {
    context: super::PropagationContext,
    token: token::Token,
    trace: trace::SourceCodeTrace,
}

impl From<&super::PropagatedError> for PropagatedError {
    fn from(value: &super::PropagatedError) -> Self {
        Self {
            context: value.context.clone(),
            token: value.token,
            trace: value.trace.clone(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Kind {
    Token(trace::SourceCodeTrace),
    EndOfInput(trace::SourceCodeTrace),
    FailedPrecondition,
}

impl<'a> From<super::Kind<'a>> for Kind {
    fn from(value: super::Kind<'a>) -> Self {
        match value {
            super::Kind::Token(trace) => Kind::Token(trace.clone()),
            super::Kind::EndOfInput(trace) => Kind::EndOfInput(trace.clone()),
            super::Kind::FailedPrecondition => Kind::FailedPrecondition,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct TexError {
    kind: Kind,
    title: String,
    // TODO notes: Vec<super::display::Note<'static>>,
    source_annotation: String,
}

impl From<&dyn super::TexError> for TexError {
    fn from(value: &dyn super::TexError) -> Self {
        Self {
            kind: value.kind().into(),
            title: value.title(),
            source_annotation: value.source_annotation(),
        }
    }
}

impl super::TexError for TexError {
    fn kind(&self) -> super::Kind {
        match &self.kind {
            Kind::Token(trace) => super::Kind::Token(trace),
            Kind::EndOfInput(trace) => super::Kind::EndOfInput(trace),
            Kind::FailedPrecondition => super::Kind::FailedPrecondition,
        }
    }

    fn title(&self) -> String {
        self.title.clone()
    }

    fn source_annotation(&self) -> String {
        self.source_annotation.clone()
    }
}
