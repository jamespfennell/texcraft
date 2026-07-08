use boxworks_text as bwt;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

pub struct Component {
    text_preprocessor: bwt::TextPreprocessorImpl,
}

impl Default for Component {
    fn default() -> Self {
        Self {
            text_preprocessor: bwt::TextPreprocessorImpl::new(bwt::Params::plain_tex_defaults()),
        }
    }
}

/// Get the `\sfcode` command.
pub fn get_sfcode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_array(
        ref_fn,
        mut_fn,
        variable::IndexResolver::Dynamic(resolver_fn),
    )
    .into()
}

fn ref_fn<S: HasComponent<Component>>(state: &S, index: variable::Index) -> &i32 {
    state
        .component()
        .text_preprocessor
        .params
        .space_factor_codes
        .0
        .get(index.0)
        .unwrap()
}

fn mut_fn<S: HasComponent<Component>>(state: &mut S, index: variable::Index) -> &mut i32 {
    state
        .component_mut()
        .text_preprocessor
        .params
        .space_factor_codes
        .0
        .get_mut(index.0)
        .unwrap()
}

fn resolver_fn<S: HasComponent<Component>>(
    _: token::Token,
    input: &mut vm::ExpandedStream<S>,
) -> txl::Result<variable::Index> {
    let index = parse::Uint::<256>::parse(input)?;
    Ok(index.0.into())
}
