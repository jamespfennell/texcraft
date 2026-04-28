//! # Texlang integration for Boxworks.

use boxworks_knuthplass as bwk;
use texlang as txl;

macro_rules! variables {
    (
        $( (
            $tex_name: ident,
            $getter_name: ident,
            $component: ty,
            $field: ident,
            $kind: ty,
        ), )+
    ) => {
        $(

            #[doc=concat!(
                "Get the `\\", stringify!($tex_name), "` command.\n\n",
                "This is a command for a singleton variable of type [`",
                stringify!($kind),"`]. ",
                "It requires that the state contain the component [`",
                stringify!($component), "`]."
            )]
            pub fn $getter_name<S: txl::vm::HasComponent<$component>>() -> txl::command::BuiltIn<S> {
                txl::variable::Command::new_singleton(
                    |state: &S, _: txl::variable::Index| -> & $kind { &state.component().$field },
                    |state: &mut S, _: txl::variable::Index| -> &mut $kind {
                        &mut state.component_mut().$field
                    },
                )
                .into()
            }
        )+
    };
}

variables!(
    (
        exhyphenpenalty,
        get_exhyphenpenalty,
        bwk::Params,
        ex_hyphen_penalty,
        i32,
    ),
    (
        hyphenpenalty,
        get_hyphenpenalty,
        bwk::Params,
        hyphen_penalty,
        i32,
    ),
    (
        tracingparagraphs,
        get_tracingparagraphs,
        bwk::Params,
        tracing_paragraphs,
        i32,
    ),
    (
        parfillskip,
        get_parfillskip,
        bwk::Params,
        par_fill_skip,
        common::Glue,
    ),
);
