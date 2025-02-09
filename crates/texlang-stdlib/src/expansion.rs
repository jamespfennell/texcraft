//! Commands that alter the expansion process

use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

/// Get the `\noexpand` command.
pub fn get_noexpand<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(noexpand_fn).with_tag(NO_EXPAND_TAG.get())
}

static NO_EXPAND_TAG: command::StaticTag = command::StaticTag::new();

fn noexpand_fn<S>(_: token::Token, _: &mut vm::ExpansionInput<S>) -> txl::Result<()> {
    panic!(
        "The \\noexpand expansion function is never invoked directly. \
         Instead, the primitive operates through the \\noexpand hook, \
         which is a method on the `TexlangState` trait. \
         Ensure your Texcraft VM is configured to use this hook."
    )
}

#[inline]
pub fn noexpand_hook<S: TexlangState>(
    token: token::Token,
    input: &mut vm::ExpansionInput<S>,
    tag: Option<command::Tag>,
) -> txl::Result<Option<token::Token>> {
    // Fast path: this is not the \noexpand command.
    // We want this check to be inlined into the VM functions that perform expansion.
    if tag != Some(NO_EXPAND_TAG.get()) {
        return Ok(None);
    }
    // Slow path: this is the \noexpand command.
    // We don't want this check to be inlined into the VM because it will take up space in the instruction cache.
    noexpand_hook_finish(token, input)
}

#[cold]
fn noexpand_hook_finish<S: TexlangState>(
    _token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<Option<token::Token>> {
    Ok(Some(input.unexpanded().next(NoExpandEndOfInputError {})?))
}

#[derive(Debug)]
struct NoExpandEndOfInputError;

impl error::EndOfInputError for NoExpandEndOfInputError {
    fn doing(&self) -> String {
        r"determining which token to suppress expansion for".into()
    }
}

/// Get the simple `\expandafter` command.
///
/// This is the simplest implementation of the command, and the
///     same implementation as in the original TeX.
pub fn get_expandafter_simple<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(expandafter_simple_fn)
}

/// Get the optimized `\expandafter` command.
///
/// This is a more complex implementation of `\expandafter` that is optimized for handling
/// repeated `\expandafter` tokens.
/// It contains two optimizations, as described below.
/// These optimizations where developed "theoretically" and with no benchmarking, so it
/// remains to be seen if they actually make expansion faster.
/// For this reason both the optimized and simple `\expandafter` implementations are maintained.
///
/// We now describe the two optimizations.
///  
/// **First**, `\expandafter` control sequences are often linked together in the following format:
///
/// ```tex
/// \expandafter<token 1>\expandafter<token 2>...\expandafter<token n><token n+1>
/// ```
///
/// Here, to expand the first `\expandafter` we just need to expand `<token n+1>`.
/// In the original implementation of TeX this works via recursion: the ith `\expandafter`
/// requests expansion of the second-to-next token, which is the (i+1)th `\expandafter`.
/// After n recursions, the last token is finally expanded.
/// In the optimized implementation here, the token stream is scanned ahead for as long as the pattern
/// `\expandafter<token i>` repeats.
/// The token `<token n+1>` is expanded and the intermediate `\expandafter` tokens are dropped from the input.
/// This is still an O(n) operation, but results in only 1 Rust function stack being used, rather than n.
///
/// **Second**, `\expandafter` commands are often grouped together like this:
///
/// ```tex
/// \expandafter\expandafter\expandafter\A\expandafter\B\C
/// ```
///
/// This TeX code causes `\C` to be expanded first, then `\B\` and finally `\A`.
/// When the leading `\expandafter` is expanded, the first optimization kicks in and `\C` will be expanded, leaving:
///
/// ```tex
/// \expandafter\A\B\Cexpanded
/// ```
///
/// The second optimization is that the leading `\expandafter` that is left over will also be expanded
///     without yielding control to the main expansion loop.
/// If, after this pass, the leading token is again an `\expandafter` token, it will be expanded too.
/// This process continues repeatedly until no `\expandafter` tokens are left at the start of the token stream.
pub fn get_expandafter_optimized<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(expandafter_optimized_fn)
}

fn expandafter_simple_fn<S: TexlangState>(
    _expandafter_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    let first = input.unexpanded().next(EndOfInputError { first: true })?;
    let second = input.unexpanded().next(EndOfInputError { first: false })?;
    input.expansions_mut().push(second);
    input.expanded().expand_once()?;
    input.expansions_mut().push(first);
    Ok(())
}

fn expandafter_optimized_fn<S: TexlangState>(
    expandafter_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    let mut buffer: Vec<token::Token> = input.checkout_token_buffer();
    // The optimization implemented here is that if we have the following input:
    // \xa <token 1> \xa <token 2> ... \xa <token N> <token N+1>
    // we expand <token N+1> once.
    loop {
        let first = input.unexpanded().next(EndOfInputError { first: true })?;
        buffer.push(first);
        let second = input.unexpanded().next(EndOfInputError { first: false })?;
        if second.value() != expandafter_token.value() {
            input.expansions_mut().push(second);
            break;
        }
    }
    input.expanded().expand_once()?;
    input.expansions_mut().extend(buffer.iter().rev());
    input.return_token_buffer(buffer);
    Ok(())
}

#[derive(Debug)]
struct EndOfInputError {
    first: bool,
}
impl error::EndOfInputError for EndOfInputError {
    fn doing(&self) -> String {
        format![
            r"reading the {} token after \expandafter",
            if self.first { "first" } else { "second" }
        ]
    }
}

/// Get the `\relax` command.
pub fn get_relax<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(|_, _| Ok(()))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::prefix;
    use std::collections::HashMap;
    use texlang_testing::*;

    #[derive(Default)]
    pub struct State {
        prefix: prefix::Component,
        testing: TestingComponent,
    }

    impl TexlangState for State {
        fn expansion_override_hook(
            token: token::Token,
            input: &mut vm::ExpansionInput<Self>,
            tag: Option<command::Tag>,
        ) -> txl::Result<Option<token::Token>> {
            noexpand_hook(token, input, tag)
        }
    }

    implement_has_component![State {
        prefix: prefix::Component,
        testing: TestingComponent,
    }];

    fn built_in_commands(optimized: bool) -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", crate::def::get_def()),
            ("let", crate::alias::get_let()),
            ("noexpand", get_noexpand()),
            ("integer", TestingComponent::get_integer()),
            (
                "xa",
                match optimized {
                    true => get_expandafter_optimized(),
                    false => get_expandafter_simple(),
                },
            ),
        ])
    }

    test_suite![
        @option(TestOption::BuiltInCommandsDyn(Box::new(|| { built_in_commands(true) }))),
        @option(TestOption::AllowUndefinedCommands(true)),
        expansion_equality_tests(
            (simple_case, r"\def\a{Hello}\noexpand\a", r"\a"),
            (
                expandafter_and_noexpand_1,
                r"\def\a#1\b{Hello '#1'}\def\b{World}\a\b",
                "Hello ''"
            ),
            (
                expandafter_and_noexpand_2,
                r"\def\a#1\b{Hello '#1'}\def\b{World}\a\b\b",
                "Hello ''World"
            ),
            (
                expandafter_and_noexpand_3,
                r"\def\a#1\b{Hello '#1'}\def\b{World}\xa\a\b\b",
                "Hello 'World'"
            ),
            (
                expandafter_and_noexpand_4,
                r"\def\a#1\b{Hello '#1'}\def\b{World}\xa\a\noexpand\b\b",
                "Hello ''World"
            ),
            (
                only_expands_once,
                r"\def\A{\B}\def\B{Hello}\xa\noexpand\A",
                r"\B",
            ),
            (
                peek_consumes_noexpand_1,
                r"\def\A{\B}\def\B{Hello}\integer = 1 \noexpand\A",
                r"\A",
            ),
            (
                peek_consumes_noexpand_2,
                r"\def\A{\B}\def\B{Hello}\integer = 1\noexpand\A",
                r"Hello",
            ),
            // peek
        ),
        fatal_error_tests((end_of_input, r"\noexpand"),),
    ];

    static PREFIX: &str = r"\def\mk#1#2{\def#1##1\notes##2\end{##1\notes##2#2\end}}\mk\a a\mk\b b\mk\c c\mk\d d\def\notes#1\end{#1}";
    static POSTFIX: &str = r"\notes\end";

    #[macro_export]
    macro_rules! expandafter_test {
        ( $( ( $name: ident, $lhs: expr, $rhs: expr ) ),* $(,)? ) => {
            mod expandafter_simple {
                use super::*;
                test_suite![
                    @option(TestOption::BuiltInCommandsDyn(Box::new(|| { built_in_commands(false) }))),
                    expansion_equality_tests(
                        $(
                            ( $name, format!("{}{}{}", PREFIX, $lhs, POSTFIX), $rhs ),
                        )*
                    ),
                ];
            }
            mod expandafter_optimized {
                use super::*;
                test_suite![
                    @option(TestOption::BuiltInCommandsDyn(Box::new(|| { built_in_commands(true) }))),
                    expansion_equality_tests(
                        $(
                            ( $name, format!("{}{}{}", PREFIX, $lhs, POSTFIX), $rhs ),
                        )*
                    ),
                ];
            }
        };
    }

    expandafter_test![
        // In the following test we alias \xa to \other so that the \expandafter optimizations don't kick in.
        // This makes \other behave like the unoptimized \expandafter.
        // It then triggers the case when the optimized \expandafter is only supposed to expand the input
        // once, rather the repeatedly expanding to remove the \expandafter's at the front.
        (
            expandafter_only_once,
            r"\let\other=\xa \other\noexpand\xa\xa\xa\a\b",
            r"\noexpand\xa ba"
        ),
        (texbook_p374_3, r"\xa\a\b", r"ba"),
        (texbook_p374_4, r"\xa\xa\xa\a\xa\b\c", "cba"),
        (
            texbook_p374_5,
            r"\xa\xa\xa\xa\xa\xa\xa\a\xa\xa\xa\b\xa\c\d",
            "dcba"
        ),
        /*
        All of the following permutation cases were generated by hand, but there's actually
        an algorithmic way to generate them. For each macro, count the number of \expandafter=\xa
        tokens that come before. Then calculate the shift:
        - if #\xa is 0, the shift is 0.
        - if #\xa is 1, the shift is 1.
        - if #\xa is 3, the shift is 2.
        - if #\xa is 7, the shift is 3.
        (In general if #\xa is 2^n-1, the shift is n-1.)
        Now work backwards through the tokens `\a\b\c\d`. For each token, move it right by the associated
        shift. You then get the result of the expansion.

        For example: `\xa\xa\xa\a\xa\b\c\d`.
        The #\xa numbers are: 3 1 0 0
        The shifts are: 2 1 0 0
        Then
        - start:         `\a\b\c\d`
        - shift \d by 0: `\a\b\c\d`
        - shift \c by 0: `\a\b\c\d`
        - shift \b by 1: `\a\c\b\d`
        - shift \a by 2: `\c\b\a\d` <- this is the expansion
        */
        (permutation_abcd, r"\a\b\c\d", "abcd"),
        (permutation_abdc, r"\a\b\xa\c\d", "abdc"),
        (permutation_acbd, r"\a\xa\b\c\d", "acbd"),
        (permutation_acdb, r"\a\xa\xa\xa\b\c\d", "acdb"),
        (permutation_adbc, r"\a\xa\b\xa\c\d", "adbc"),
        (permutation_adcb, r"\a\xa\xa\xa\b\xa\c\d", "adcb"),
        (permutation_bacd, r"\xa\a\b\c\d", "bacd"),
        (permutation_badc, r"\xa\a\b\xa\c\d", "badc"),
        (permutation_bcad, r"\xa\xa\xa\a\b\c\d", "bcad"),
        (permutation_bcda, r"\xa\xa\xa\xa\xa\xa\xa\a\b\c\d", "bcda"),
        (permutation_bdac, r"\xa\xa\xa\a\b\xa\c\d", "bdac"),
        (
            permutation_bdca,
            r"\xa\xa\xa\xa\xa\xa\xa\a\b\xa\c\d",
            "bdca"
        ),
        (permutation_cabd, r"\xa\a\xa\b\c\d", "cabd"),
        (permutation_cadb, r"\xa\a\xa\xa\xa\b\c\d", "cadb"),
        (permutation_cbad, r"\xa\xa\xa\a\xa\b\c\d", "cbad"),
        (
            permutation_cbda,
            r"\xa\xa\xa\xa\xa\xa\xa\a\xa\xa\xa\b\c\d",
            "cdba"
        ),
        (permutation_cdab, r"\xa\xa\xa\a\xa\xa\xa\b\c\d", "cdab"),
        (
            permutation_cdba,
            r"\xa\xa\xa\xa\xa\xa\xa\a\xa\xa\xa\b\c\d",
            "cdba"
        ),
        (permutation_dabc, r"\xa\a\xa\b\xa\c\d", "dabc"),
        (permutation_dacb, r"\xa\a\xa\xa\xa\b\xa\c\d", "dacb"),
        (permutation_dbac, r"\xa\xa\xa\a\xa\b\xa\c\d", "dbac"),
        (
            permutation_dbca,
            r"\xa\xa\xa\xa\xa\xa\xa\a\xa\b\xa\c\d",
            "dbca"
        ),
        (permutation_dcab, r"\xa\xa\xa\a\xa\xa\xa\b\xa\c\d", "dcab"),
        (
            permutation_dcba,
            r"\xa\xa\xa\xa\xa\xa\xa\a\xa\xa\xa\b\xa\c\d",
            "dcba"
        ),
        (
            expandafter_last_after_first_pass,
            r"\xa\xa\xa\a\xa\xa\b\c\d",
            "bdac"
        ),
    ];

    fn run_expandafter_failure_test(input: &str, optimized: bool) {
        let options = vec![TestOption::BuiltInCommandsDyn(Box::new(|| {
            built_in_commands(optimized)
        }))];
        run_fatal_error_test(&input, &options, false);
    }

    #[macro_export]
    macro_rules! expandafter_fatal_error_test {
        ($( ( $name: ident, $input: expr), )+) => {
            $(
            mod $name {
                #[test]
                fn simple() {
                    super::run_expandafter_failure_test($input, false);
                }
                #[test]
                fn optimized() {
                    super::run_expandafter_failure_test($input, true);
                }
            }
            )+
        };
    }

    expandafter_fatal_error_test![
        (expandafter_missing_1st_token, r"\xa"),
        (expandafter_missing_2nd_token, r"\xa\a"),
        (expandafter_missing_1st_token_nested, r"\xa\xa\xa\a\xa\xa\b"),
        (
            expandafter_missing_2nd_token_nested,
            r"\def\A{}\xa\xa\xa\A\A"
        ),
    ];
}
