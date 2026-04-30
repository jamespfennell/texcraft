use boxworks_knuthplass as bwk;
use texlang as txl;

pub struct Component {
    params: bwk::Params,
    _tracing_paragraphs: i32,
}

impl Component {
    pub fn invoke_line_breaker(&mut self) {
        // self.line_breaker.break_line(list, params, font_width, line_widths, debug_logger);
    }
}

macro_rules! params_variables {
    (
        $( (
            $tex_name: ident,
            $getter_name: ident,
            $field: ident,
            $kind: ty,
        ), )+
    ) => {
        $(
            #[doc=concat!(
                "Get the `\\", stringify!($tex_name), "` command.\n\n",
                "This is a command for a singleton variable of type [`",
                stringify!($kind),"`]. ",
                "It requires that the state contain the component [`Component`].",
            )]
            pub fn $getter_name<S: txl::vm::HasComponent<Component>>() -> txl::command::BuiltIn<S> {
                txl::variable::Command::new_singleton(
                    |state: &S, _: txl::variable::Index| -> & $kind { &state.component().params.$field },
                    |state: &mut S, _: txl::variable::Index| -> &mut $kind {
                        &mut state.component_mut().params.$field
                    },
                )
                .into()
            }
        )+
    };
}

params_variables!(
    (adjdemerits, get_adjdemerits, adj_demerits, i32,),
    (
        doublehyphendemerits,
        get_doublehyphendemerits,
        double_hyphen_demerits,
        i32,
    ),
    (exhyphenpenalty, get_exhyphenpenalty, ex_hyphen_penalty, i32,),
    (
        finalhyphendemerits,
        get_finalhyphendemerits,
        final_hyphen_demerits,
        i32,
    ),
    (hyphenpenalty, get_hyphenpenalty, hyphen_penalty, i32,),
    (leftskip, get_leftskip, left_skip, common::Glue,),
    (linepenalty, get_linepenalty, line_penalty, i32,),
    (looseness, get_looseness, looseness, i32,),
    (parfillskip, get_parfillskip, par_fill_skip, common::Glue,),
    (pretolerance, get_pretolerance, pre_tolerance, i32,),
    (rightskip, get_rightskip, right_skip, common::Glue,),
    (tolerance, get_tolerance, tolerance, i32,),
);
