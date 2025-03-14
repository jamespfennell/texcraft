//! Primitives for inputting source code from files and the terminal

use std::cell::RefCell;
use std::rc::Rc;
use texlang::parse::{FileLocation, OptionalEquals};
use texlang::prelude as txl;
use texlang::token::lexer;
use texlang::token::trace;
use texlang::traits::*;
use texlang::*;
use texlang_common as common;

use crate::conditional::{self, Condition};

/// Get the `\input` expansion primitive.
pub fn get_input<S: TexlangState + common::HasFileSystem>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(input_fn)
}

fn input_fn<S: TexlangState + common::HasFileSystem>(
    input_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    let file_location = FileLocation::parse(input)?;
    let (file_path, source_code) =
        match texlang_common::read_file_to_string(input.vm(), file_location, "tex") {
            Ok(ok) => ok,
            Err(err) => {
                return Err(input.fatal_error(err));
            }
        };
    if input.vm().num_current_sources() > 100 {
        return Err(input.fatal_error(TooManyInputs {}));
    }
    input.push_source(input_token, file_path, source_code)?;
    Ok(())
}

#[derive(Debug)]
struct TooManyInputs {}

impl error::TexError for TooManyInputs {
    fn kind(&self) -> error::Kind {
        error::Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        "too many input levels (100)".into()
    }
}

/// Get the `\endinput` expansion primitive.
pub fn get_endinput<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(endinput_fn)
}

fn endinput_fn<S: TexlangState>(
    _: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    input.end_current_file();
    Ok(())
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component<const N: usize> {
    #[cfg_attr(feature = "serde", serde(with = "texcraft_stdext::serde_tools::array"))]
    files: [Option<Box<lexer::Lexer>>; N],
}

impl<const N: usize> Component<N> {
    fn take_file(&mut self, index: i32) -> Option<Box<lexer::Lexer>> {
        let u: usize = match index.try_into() {
            Ok(u) => u,
            Err(_) => return None,
        };
        match self.files.get_mut(u) {
            None => None,
            Some(file_or) => file_or.take(),
        }
    }

    fn return_file(&mut self, index: i32, file: Box<lexer::Lexer>) {
        let u: usize = index.try_into().unwrap();
        *self.files.get_mut(u).unwrap() = Some(file);
    }
}

impl<const N: usize> Default for Component<N> {
    fn default() -> Self {
        // We construct the array as a vector first because the element type
        // cannot be cloned and so we can't use the standard array constructor.
        // But note that this isn't inefficient, because the array will
        // simply use the vector's buffer.
        let v: Vec<Option<Box<lexer::Lexer>>> = (0..N).map(|_| None).collect();
        Self {
            files: v.try_into().unwrap(),
        }
    }
}

/// Get the `\openin` execution primitive.
pub fn get_openin<const N: usize, S: HasComponent<Component<N>> + common::HasFileSystem>(
) -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(openin_fn)
}

fn openin_fn<const N: usize, S: HasComponent<Component<N>> + common::HasFileSystem>(
    openin_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let (u, _, file_location) = <(parse::Uint<N>, OptionalEquals, FileLocation)>::parse(input)?;
    let lexer_or = match texlang_common::read_file_to_string(input.vm(), file_location, "tex") {
        // If the file fails to open TeX does not error out.
        // Instead, TeX users are expected to test the result with \ifeof.
        Err(_) => None,
        Ok((file_path, source_code)) => {
            let source_code = ensure_ends_in_newline(source_code);
            let trace_key_range = input.tracer_mut().register_source_code(
                Some(openin_token),
                trace::Origin::File(file_path),
                &source_code,
            );
            Some(Box::new(lexer::Lexer::new(source_code, trace_key_range)))
        }
    };
    *input
        .state_mut()
        .component_mut()
        .files
        .get_mut(u.0)
        .unwrap() = lexer_or;
    Ok(())
}

fn ensure_ends_in_newline(mut s: String) -> String {
    if !s.ends_with('\n') {
        s.push('\n')
    }
    s
}

/// Get the `\closein` execution primitive.
pub fn get_closein<const N: usize, S: HasComponent<Component<N>>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(closein_fn)
}

fn closein_fn<const N: usize, S: HasComponent<Component<N>>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let u = parse::Uint::<N>::parse(input)?;
    *input
        .state_mut()
        .component_mut()
        .files
        .get_mut(u.0)
        .unwrap() = None;
    Ok(())
}

/// Get the `\read` execution primitive.
pub fn get_read<const N: usize, S: HasComponent<Component<N>> + common::HasTerminalIn>(
) -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(read_fn)
}

fn read_fn<const N: usize, S: HasComponent<Component<N>> + common::HasTerminalIn>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let (index, _, cmd_ref_or) = <(i32, To, Option<token::CommandRef>)>::parse(input)?;

    #[derive(Copy, Clone, PartialEq, Eq)]
    enum Mode {
        File,
        Terminal,
    }

    let terminal_in = input.state().terminal_in().clone();
    let (mut lexer, mode, prompt) = match input.state_mut().component_mut().take_file(index) {
        Some(file) => (file, Mode::File, None),
        None => {
            let prompt = if index < 0 {
                None
            } else {
                Some(format!(
                    r"{}=",
                    match cmd_ref_or {
                        None => "unreachable".to_string(),
                        Some(cmd_ref) => cmd_ref.to_string(input.vm().cs_name_interner()),
                    }
                ))
            };
            (
                read_from_terminal(&terminal_in, input, &prompt)?,
                Mode::Terminal,
                prompt,
            )
        }
    };

    let mut tokens = vec![];
    let mut more_lines_exist = true;
    let mut braces: Vec<token::Token> = vec![];
    loop {
        let vm::Parts {
            state,
            cs_name_interner,
            ..
        } = input.vm_parts();
        match (lexer.next(state, cs_name_interner, true), mode) {
            (lexer::Result::Token(token), _) => {
                match token.cat_code() {
                    Some(types::CatCode::BeginGroup) => {
                        braces.push(token);
                    }
                    Some(types::CatCode::EndGroup) => {
                        if braces.pop().is_none() {
                            more_lines_exist = drain_line(&mut lexer, state, cs_name_interner);
                            break;
                        }
                    }
                    _ => (),
                };
                tokens.push(token);
            }
            (lexer::Result::InvalidCharacter(c, trace_key), _) => {
                return Err(input.fatal_error(lexer::InvalidCharacterError::new(
                    input.vm(),
                    c,
                    trace_key,
                )))
            }
            (lexer::Result::EndOfLine, Mode::File) => {
                if braces.is_empty() {
                    break;
                }
            }
            (lexer::Result::EndOfInput, Mode::File) => {
                if let Some(unmatched_brace) = braces.pop() {
                    return Err(input.fatal_error(UnmatchedBracesError { unmatched_brace }));
                }
                more_lines_exist = false;
                break;
            }
            (lexer::Result::EndOfLine | lexer::Result::EndOfInput, Mode::Terminal) => {
                if !braces.is_empty() {
                    lexer = read_from_terminal(&terminal_in, input, &prompt)?;
                    continue;
                }
                break;
            }
        }
    }
    if mode == Mode::File && more_lines_exist {
        input.state_mut().component_mut().return_file(index, lexer);
    }
    tokens.reverse();
    let user_defined_macro =
        texmacro::Macro::new(vec![], vec![], vec![texmacro::Replacement::Tokens(tokens)]);
    if let Some(cmd_ref) = cmd_ref_or {
        input
            .commands_map_mut()
            .insert_macro(cmd_ref, user_defined_macro, scope);
    }
    Ok(())
}

fn drain_line<S: TexlangState>(
    file: &mut lexer::Lexer,
    state: &S,
    cs_name_interner: &mut token::CsNameInterner,
) -> bool {
    loop {
        match file.next(state, cs_name_interner, true) {
            lexer::Result::Token(_) | lexer::Result::InvalidCharacter(_, _) => {}
            lexer::Result::EndOfLine => {
                return true;
            }
            lexer::Result::EndOfInput => {
                return false;
            }
        }
    }
}

fn read_from_terminal<S: TexlangState>(
    terminal_in: &Rc<RefCell<dyn common::TerminalIn>>,
    input: &mut vm::ExecutionInput<S>,
    prompt: &Option<String>,
) -> txl::Result<Box<lexer::Lexer>> {
    let mut buffer = String::new();
    if let Err(err) = terminal_in
        .borrow_mut()
        .read_line(prompt.as_deref(), &mut buffer)
    {
        return Err(input.fatal_error(IoError {
            title: "failed to read from the terminal".into(),
            underlying_error: err,
        }));
    }
    let trace_key_range =
        input
            .tracer_mut()
            .register_source_code(None, trace::Origin::Terminal, &buffer);
    Ok(Box::new(lexer::Lexer::new(buffer, trace_key_range)))
}

/// Get the `\ifeof` conditional expansion primitive.
pub fn get_ifeof<const N: usize, S>() -> command::BuiltIn<S>
where
    S: HasComponent<Component<N>> + HasComponent<conditional::Component>,
{
    IsEof::build_if_command()
}

struct IsEof<const N: usize>;

impl<const N: usize, S> conditional::Condition<S> for IsEof<N>
where
    S: HasComponent<Component<N>> + HasComponent<conditional::Component>,
{
    fn evaluate(input: &mut vm::ExpansionInput<S>) -> txl::Result<bool> {
        let u = parse::Uint::<N>::parse(input)?;
        Ok(HasComponent::<Component<N>>::component(input.state())
            .files
            .get(u.0)
            .unwrap()
            .is_none())
    }
}

#[derive(Debug)]
struct IoError {
    title: String,
    underlying_error: std::io::Error,
}

impl error::TexError for IoError {
    fn kind(&self) -> error::Kind {
        error::Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        self.title.clone()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![format!("underlying filesystem error: {}", self.underlying_error).into()]
    }
}

#[derive(Debug)]
struct UnmatchedBracesError {
    unmatched_brace: token::Token,
}

impl error::TexError for UnmatchedBracesError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(self.unmatched_brace)
    }

    fn title(&self) -> String {
        "file has an unmatched opening brace".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![r"files being read with the \read primitive must match all opening braces with closing braces".into()]
    }
}

/// When parsed, this type consumes a required `to` keyword from the input stream.
struct To;

impl Parsable for To {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        texlang::parse::parse_keyword(input, "to")?;
        Ok(To {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{def, expansion, prefix};
    use std::collections::HashMap;
    use texlang_testing::*;

    #[derive(Default)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct State {
        conditional: conditional::Component,
        input: Component<16>,
        prefix: prefix::Component,
        testing: TestingComponent,
        #[cfg_attr(feature = "serde", serde(skip))]
        file_system: Rc<RefCell<common::InMemoryFileSystem>>,
        #[cfg_attr(feature = "serde", serde(skip))]
        terminal_in: Rc<RefCell<common::MockTerminalIn>>,
    }

    impl TexlangState for State {}

    impl common::HasFileSystem for State {
        fn file_system(&self) -> Rc<RefCell<dyn common::FileSystem>> {
            self.file_system.clone()
        }
    }

    impl common::HasTerminalIn for State {
        fn terminal_in(&self) -> Rc<RefCell<dyn common::TerminalIn>> {
            self.terminal_in.clone()
        }
    }

    implement_has_component![State{
        conditional: conditional::Component,
        input: Component<16>,
        prefix: prefix::Component,
        testing: TestingComponent,
    }];

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("closein", get_closein()),
            ("def", def::get_def()),
            ("else", conditional::get_else()),
            ("endinput", get_endinput()),
            ("fi", conditional::get_fi()),
            ("ifeof", get_ifeof()),
            ("input", get_input()),
            ("openin", get_openin()),
            ("read", get_read()),
            ("relax", expansion::get_relax()),
        ])
    }

    fn custom_vm_initialization(vm: &mut vm::VM<State>) {
        let mut fs = common::InMemoryFileSystem::new(&vm.working_directory.as_ref().unwrap());
        fs.add_string_file("file1.tex", "content1\n");
        fs.add_string_file("file2.tex", "content2%\n");
        fs.add_string_file("file3.tex", r"\input nested/file4");
        fs.add_string_file("nested/file4.tex", "content4");
        fs.add_string_file("file5.tex", "file1.tex");
        fs.add_string_file("recursive.tex", r"\input recursive.tex content");
        vm.state.file_system = Rc::new(RefCell::new(fs));
    }

    test_suite!(
        @options(
            TestOption::BuiltInCommands(built_in_commands),
            TestOption::CustomVMInitialization(custom_vm_initialization),
        ),
        expansion_equality_tests(
            (basic_case, r"\input file1 hello", "content1 hello"),
            (input_together, r"\input file2 hello", r"content2hello"),
            (basic_case_with_ext, r"\input file1.tex", r"content1 "),
            (nested, r"\input file3", r"content4"),
            (nested_2, r"\input \input file5", r"content1 "),
        ),
        fatal_error_tests(
            (file_does_not_exist, r"\input doesNotExist"),
            (recursive_input, r"\input recursive s"),
        ),
    );

    fn end_input_vm_initialization(vm: &mut vm::VM<State>) {
        let mut fs = common::InMemoryFileSystem::new(&vm.working_directory.as_ref().unwrap());
        fs.add_string_file(
            "file1.tex",
            "Hello\\def\\Macro{Hola\\endinput Mundo}\\Macro World\n",
        );
        vm.state.file_system = Rc::new(RefCell::new(fs));
    }

    test_suite!(
        @option(TestOption::BuiltInCommands(built_in_commands)),
        @option(TestOption::CustomVMInitialization(end_input_vm_initialization)),
        expansion_equality_tests(
            (end_input_simple, r"Hello\endinput World", "Hello",),
            (
                end_input_in_second_file,
                r"Before\input file1 After",
                "BeforeHelloHolaMundoAfter"
            ),
        ),
    );

    fn read_vm_initialization(vm: &mut vm::VM<State>) {
        let mut fs = common::InMemoryFileSystem::new(&vm.working_directory.as_ref().unwrap());
        fs.add_string_file("file1.tex", "1\n2%\n3");
        fs.add_string_file("file2.tex", "1{\n2\n3}");
        fs.add_string_file("file3.tex", "1}1\n2");
        fs.add_string_file("file4.tex", "");
        fs.add_string_file("file5.tex", "hello { world");
        vm.state.file_system = Rc::new(RefCell::new(fs));

        let mut terminal_in: common::MockTerminalIn = Default::default();
        terminal_in.add_line("first-line");
        terminal_in.add_line("second-line {");
        terminal_in.add_line("third-line }");
        terminal_in.add_line("fourth}line");
        vm.state.terminal_in = Rc::new(RefCell::new(terminal_in));
    }

    test_suite!(
        @options(
            TestOption::BuiltInCommands(built_in_commands),
            TestOption::CustomVMInitialization(read_vm_initialization),
        ),
        expansion_equality_tests(
            (
                ifeof_nothing_open,
                r"\ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                ifeof_non_existent_file,
                r"\openin 0 doesNotExist \ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                ifeof_file_exists,
                r"\openin 0 file1 \ifeof 0 Closed\else Open\fi",
                "Open",
            ),
            (
                ifeof_non_existent_file_2,
                r"\openin 0 file1 \openin 0 doesNotExist \ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                ifeof_file_closed,
                r"\openin 0 file1 \closein 0 \ifeof 0 Closed\else Open\fi",
                "Closed",
            ),
            (
                read_1,
                r"\openin 0 file1\read 0 to \line line1='\line'\read 0 to \line line2='\line'\read 0 to \line line3='\line'\ifeof 0 Closed\else Open\fi",
                "line1='1 'line2='2'line3='3 'Closed",
            ),
            (
                read_2,
                r"\openin 0 file2\read 0 to ~line1='~'\ifeof 0 Closed\else Open\fi",
                "line1='1{ 2 3} 'Closed",
            ),
            (
                read_3,
                r"\openin 0 file3\read 0 to \line line1='\line'\read 0 to \line line2='\line'",
                "line1='1'line2='2 '",
            ),
            (
                read_4,
                r"\def\par{par}\openin 0 file4\read 0 to \line line1='\line'\ifeof 0 Closed\else Open\fi",
                "line1='par'Closed",
            ),
            (
                read_from_terminal,
                r"\read 0 to \line line1='\line'\read 0 to \line line2='\line'\read 0 to \line line3='\line'",
                "line1='first-line 'line2='second-line { third-line } 'line3='fourth'",
            ),
        ),
        serde_tests((
            ifeof_file_exists_serde,
            r"\openin 0 file1 ",
            r"\ifeof 0 Closed\else Open\fi"
        ),),
        fatal_error_tests(
            (
                file_has_unmatched_braces,
                r"\openin 0 file5 \read 0 to \X (\X)",
            ),
            (
                failed_to_read_from_terminal,
                r"\read 0 to \X \read 0 to \X \read 0 to \X \read 0 to \X",
            ),
        ),
    );
}
