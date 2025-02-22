//! # Font loading subsystem for Texlang
//!
//! This crate implements font loading and
//! font variable management for Texlang.

use core::FontFormat;
use texlang as txl;
use texlang::command;
use texlang::error;
use texlang::token;
use texlang::traits::*;
use texlang::types;
use texlang::vm;

/// Get the `\nullfont` command.
pub fn get_nullfont<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_font(texlang::types::Font::NULL_FONT)
}

static FONT_TAG: command::StaticTag = command::StaticTag::new();

/// Component needed to use the `\font` command.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FontComponent {
    font_infos: Vec<FontInfo>,
    next_id: types::Font,
}

impl FontComponent {
    pub fn get_command_ref_for_font<S: HasComponent<FontComponent>>(
        state: &S,
        font: types::Font,
    ) -> Option<token::CommandRef> {
        // TODO: this needs to return the special frozen command ref
        // Main problem: where is this registered?
        // vm.frozen_command_register(command, "name") -> CsName
        // vm.frozen_command_update_name(CsName, "newName")
        // where "nullfont" just means the thing that is returned from \string
        // we may need to update this though? E.g. when a font is given
        // a new CSname \font a path/to/file \font b path/to/file
        // TODO: also need to update this when \font reruns
        let font_info = state.component().font_infos.get(font.0 as usize).unwrap();
        Some(font_info.command_ref)
    }
    pub fn is_current_font_command<S: HasComponent<FontComponent>>(
        state: &S,
        tag: command::Tag,
    ) -> bool {
        _ = state;
        tag == FONT_TAG.get()
    }
    pub fn initialize<S: HasComponent<FontComponent>>(vm: &mut txl::vm::VM<S>) {
        let cs_name = vm.cs_name_interner_mut().get_or_intern("nullfont");
        vm.state.component_mut().font_infos.push(FontInfo {
            command_ref: token::CommandRef::ControlSequence(cs_name),
            font_name: "nullfont".to_string(),
            path: None,
        });
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct FontInfo {
    command_ref: token::CommandRef,
    font_name: String,
    path: Option<std::path::PathBuf>,
}

impl Default for FontComponent {
    fn default() -> Self {
        Self {
            font_infos: vec![],
            next_id: types::Font(1),
        }
    }
}

/// Get the `\font` command.
pub fn get_font<S>() -> command::BuiltIn<S>
where
    S: TexlangState + texlang_common::HasFileSystem + HasComponent<FontComponent> + HasFontRepo,
{
    command::BuiltIn::new_execution(font_primitive_fn).with_tag(FONT_TAG.get())
}

pub trait HasFontRepo {
    type FontRepo: FontRepo;
    fn font_repo_mut(&mut self) -> &mut Self::FontRepo;
}

/// A font repository is where font data is stored.
///
/// We currently envisage that typesetting engines will contain
/// a font repo that they will use for getting font metric data.
pub trait FontRepo {
    /// Format of files that are store in this repo
    type Format: core::FontFormat;
    fn add_font(&mut self, id: types::Font, font: Self::Format);
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NoOpFontRepo<T>(std::marker::PhantomData<T>);

impl<T> Default for NoOpFontRepo<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: core::FontFormat> FontRepo for NoOpFontRepo<T> {
    type Format = T;

    fn add_font(&mut self, _: types::Font, _: Self::Format) {}
}

/// TeX.2014.1257
fn font_primitive_fn<S>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<error::Error>>
where
    S: TexlangState + texlang_common::HasFileSystem + HasComponent<FontComponent> + HasFontRepo,
{
    type FontFormat<S> = <<S as HasFontRepo>::FontRepo as FontRepo>::Format;
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let (command_ref_or, _, file_location) = <(
        Option<token::CommandRef>,
        texlang::parse::OptionalEquals,
        texlang::parse::FileLocation,
    )>::parse(input)?;
    let Some((path, tfm_bytes)) = texlang_common::read_file_to_bytes(
        input.vm(),
        file_location,
        FontFormat::<S>::DEFAULT_FILE_EXTENSION,
    )?
    else {
        return Ok(());
    };

    let font = match FontFormat::<S>::parse(&tfm_bytes) {
        Ok(font) => font,
        Err(err) => {
            let err = FontError {
                inner: Box::new(err),
            };
            input.vm().error(err)?;
            return Ok(());
        }
    };

    let Some(command_ref) = command_ref_or else {
        return Ok(());
    };

    // TODO: scan the font_size_specification, section 1258
    // TODO: does this happen before or after file reading?
    let component = input.state_mut().component_mut();
    let id = component.next_id;
    component.next_id = types::Font(component.next_id.0.checked_add(1).unwrap());

    input.state_mut().font_repo_mut().add_font(id, font);
    input.state_mut().component_mut().font_infos.push(FontInfo {
        command_ref,
        font_name: match path.with_extension("").file_name() {
            Some(file_name) => file_name.to_string_lossy().into(),
            None => "".to_string(),
        },
        path: Some(path),
    });
    input
        .commands_map_mut()
        .insert(command_ref, command::Command::Font(id), scope);
    Ok(())
}

#[derive(Debug)]
struct FontError {
    inner: Box<dyn std::error::Error>,
}

impl txl::error::TexError for FontError {
    fn kind(&self) -> error::Kind {
        txl::error::Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        format!("Font file is invalid: {}", self.inner)
    }
}

/// Get the `\fontname` command.
pub fn get_fontname<S>() -> command::BuiltIn<S>
where
    S: HasComponent<FontComponent>,
{
    command::BuiltIn::new_expansion(fontname_primitive_fn)
}

/// TeX.2014.1257
fn fontname_primitive_fn<S>(
    token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> Result<(), Box<error::Error>>
where
    S: HasComponent<FontComponent>,
{
    let font = types::Font::parse(input)?;
    let font_info = input
        .state()
        .component()
        .font_infos
        .get(font.0 as usize)
        .expect("font has been defined");
    // Would be nice to avoid the allocation here
    let font_name: String = font_info.font_name.to_string();
    input.push_string_tokens(token, &font_name);
    Ok(())
}

/// Registers marker for the `\scriptfont` command.
pub struct ScriptFontMarker;

/// Get the `\scriptfont` command.
pub fn get_scriptfont<
    S: HasComponent<texlang_stdlib::registers::Component<types::Font, 16, ScriptFontMarker>>,
>() -> command::BuiltIn<S> {
    texlang_stdlib::registers::new_registers_command()
}

/// Registers marker for the `\scriptscriptfont` command.
pub struct ScriptScriptFontMarker;

/// Get the `\scriptscriptfont` command.
pub fn get_scriptscriptfont<
    S: HasComponent<texlang_stdlib::registers::Component<types::Font, 16, ScriptScriptFontMarker>>,
>() -> command::BuiltIn<S> {
    texlang_stdlib::registers::new_registers_command()
}

/// Registers marker for the `\textfont` command.
pub struct TextFontMarker;

/// Get the `\textfont` command.
pub fn get_textfont<
    S: HasComponent<texlang_stdlib::registers::Component<types::Font, 16, TextFontMarker>>,
>() -> command::BuiltIn<S> {
    texlang_stdlib::registers::new_registers_command()
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use super::*;
    use texlang::{command, implement_has_component, vm::TexlangState};
    use texlang_testing::*;

    #[derive(Debug, PartialEq, Eq)]
    struct MockFont(u8);
    #[derive(Debug)]
    struct MockFontError;
    impl std::error::Error for MockFontError {}
    impl std::fmt::Display for MockFontError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "invalid font file")
        }
    }
    impl core::FontFormat for MockFont {
        const DEFAULT_FILE_EXTENSION: &'static str = "mock";
        type Error = MockFontError;
        fn parse(b: &[u8]) -> Result<Self, Self::Error> {
            match b.first().copied() {
                None => Err(MockFontError {}),
                Some(u) => Ok(MockFont(u)),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    enum Record {
        AddFont(types::Font, MockFont),
        EnableFont(types::Font),
    }
    #[derive(Default)]
    struct Recorder {
        records: Vec<Record>,
    }
    impl FontRepo for Recorder {
        type Format = MockFont;
        fn add_font(&mut self, id: types::Font, font: Self::Format) {
            self.records.push(Record::AddFont(id, font));
        }
    }

    #[derive(Default)]
    struct State {
        records: Recorder,
        font: FontComponent,
        script_font: texlang_stdlib::registers::Component<types::Font, 16, ScriptFontMarker>,
        script_script_font:
            texlang_stdlib::registers::Component<types::Font, 16, ScriptScriptFontMarker>,
        text_font: texlang_stdlib::registers::Component<types::Font, 16, TextFontMarker>,
        registers: texlang_stdlib::registers::Component<i32, 256>,
        prefix: texlang_stdlib::prefix::Component,
        testing: texlang_testing::TestingComponent,
        file_system: Rc<RefCell<texlang_common::InMemoryFileSystem>>,
    }
    impl TexlangState for State {
        fn enable_font_hook(&mut self, font: types::Font) {
            self.records.records.push(Record::EnableFont(font));
        }
        fn variable_assignment_scope_hook(
            state: &mut Self,
        ) -> texcraft_stdext::collections::groupingmap::Scope {
            texlang_stdlib::prefix::variable_assignment_scope_hook(state)
        }
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedError,
        ) -> Result<(), Box<dyn error::TexError>> {
            texlang_testing::TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
        fn is_current_font_command(&self, tag: command::Tag) -> bool {
            FontComponent::is_current_font_command(self, tag)
        }
    }
    impl texlang_stdlib::the::TheCompatible for State {
        fn get_command_ref_for_font(&self, font: types::Font) -> Option<token::CommandRef> {
            FontComponent::get_command_ref_for_font(self, font)
        }
    }
    implement_has_component![State {
        font: FontComponent,
        script_font: texlang_stdlib::registers::Component<types::Font, 16, ScriptFontMarker>,
        script_script_font: texlang_stdlib::registers::Component<types::Font, 16, ScriptScriptFontMarker>,
        text_font: texlang_stdlib::registers::Component<types::Font, 16, TextFontMarker>,
        registers: texlang_stdlib::registers::Component<i32, 256>,
        prefix: texlang_stdlib::prefix::Component,
        testing: texlang_testing::TestingComponent,
    }];
    impl HasFontRepo for State {
        type FontRepo = Recorder;
        fn font_repo_mut(&mut self) -> &mut Self::FontRepo {
            &mut self.records
        }
    }
    impl texlang_common::HasFileSystem for State {
        fn file_system(&self) -> Rc<RefCell<dyn texlang_common::FileSystem>> {
            self.file_system.clone()
        }
    }

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("font", get_font()),
            ("fontname", get_fontname()),
            ("nullfont", get_nullfont()),
            ("scriptfont", get_scriptfont()),
            ("scriptscriptfont", get_scriptscriptfont()),
            ("textfont", get_textfont()),
            //
            ("count", texlang_stdlib::registers::get_count()),
            ("def", texlang_stdlib::def::get_def()),
            ("global", texlang_stdlib::prefix::get_global()),
            ("the", texlang_stdlib::the::get_the()),
        ])
    }

    fn custom_vm_initialization(vm: &mut vm::VM<State>) {
        FontComponent::initialize(vm);
        vm.state
            .prefix
            .register_globally_prefixable_command(FONT_TAG.get());
        let mut fs =
            texlang_common::InMemoryFileSystem::new(&vm.working_directory.as_ref().unwrap());
        fs.add_bytes_file("a.mock", &[1]);
        fs.add_bytes_file("b.mock", &[2]);
        fs.add_bytes_file("invalid.mock", &[]);
        vm.state.file_system = Rc::new(RefCell::new(fs));
    }

    fn want_records(want: Vec<Record>) -> impl Fn(&State) {
        move |state: &State| {
            assert_eq!(state.records.records, want);
        }
    }

    test_suite![
        @options(
            TestOption::BuiltInCommands(built_in_commands),
            TestOption::CustomVMInitialization(custom_vm_initialization),
        ),
        state_tests(
            (
                nullfont,
                r"\nullfont",
                want_records(vec![
                    Record::EnableFont(types::Font(0)),
                ]),
            ),
            (
                load_one_font,
                r"\font \fontA a \fontA",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                ]),
            ),
            (
                load_one_font_extension,
                r"\font \fontA a.mock \fontA",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                enable_nesting_1,
                r"\font \fontA a \nullfont{\fontA}",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(0)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(0)),
                ])
            ),
            (
                enable_nesting_2,
                r"\font\fontA a \font\fontB b \nullfont\fontB{\fontA}",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::AddFont(types::Font(2), MockFont(2)),
                    Record::EnableFont(types::Font(0)),
                    Record::EnableFont(types::Font(2)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(2)),
                ])
            ),
            (
                enable_nesting_3,
                r"\font\fontA a \font\fontB b \nullfont{\fontA\fontB}",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::AddFont(types::Font(2), MockFont(2)),
                    Record::EnableFont(types::Font(0)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(2)),
                    Record::EnableFont(types::Font(0)),
                ])
            ),
            (
                local_definition_and_enable,
                r"\def\fontA{macro}{\font\fontA a \fontA}\fontA",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(0)),  // end group
                    // The second \fontA expands the macro, doesn't enable the font
                ])
            ),
            (
                global_enable,
                r"{\font\fontA a \global\fontA}",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                    // End group doesn't re-enable the null font.
                ])
            ),
            (
                global_definition,
                r"\def\fontA{macro}{\global\font\fontA a \fontA}\fontA",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(0)),  // end group
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_defaults_to_null_font,
                r"\the\textfont3",
                want_records(vec![
                    Record::EnableFont(types::Font::NULL_FONT),
                ])
            ),
            (
                current_font_defaults_to_null_font,
                r"\the\font",
                want_records(vec![
                    Record::EnableFont(types::Font::NULL_FONT),
                ])
            ),
            (
                current_font_after_change,
                r"\font\fontA a \fontA \the\font",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_assignment_1,
                r"\font\fontA a \textfont3=\fontA \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_assignment_1_with_the,
                r"\font\fontA a \textfont3=\the\fontA \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_assignment_2,
                r"\font\fontA a \scriptfont3=\fontA \textfont3=\scriptfont3 \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_assignment_2_with_the,
                r"\font\fontA a \scriptfont3=\fontA \textfont3=\the\scriptfont3 \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_assignment_3,
                r"\font\fontA a \fontA \textfont3=\font \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_assignment_3_with_the,
                r"\font\fontA a \fontA \textfont3=\the\font \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::EnableFont(types::Font(1)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_nesting,
                r"\font\fontA a \font\fontB b \textfont3=\fontA { \textfont3=\fontB } \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::AddFont(types::Font(2), MockFont(2)),
                    Record::EnableFont(types::Font(1)),
                ])
            ),
            (
                variable_global,
                r"\font\fontA a \font\fontB b \textfont3=\fontA { \global\textfont3=\fontB } \the\textfont3",
                want_records(vec![
                    Record::AddFont(types::Font(1), MockFont(1)),
                    Record::AddFont(types::Font(2), MockFont(2)),
                    Record::EnableFont(types::Font(2)),
                ])
            ),
        ),
        expansion_equality_tests(
            (
                fontname_1,
                r"\font\fontA a b\fontname\fontA",
                r"ba",
            ),
            (
                fontname_2,
                r"\font\fontA a \fontname\font\fontA-\fontname\font",
                r"nullfont-a",
            ),
            (
                fontname_nullfont,
                r"\fontname\nullfont",
                r"nullfont",
            ),
        ),
        recoverable_failure_tests(
            (
                font_file_does_not_exist,
                r"\font\fontA doesNotExist ",
                r"",
            ),
            (
                font_file_not_provided,
                r"\def\A{Hello}\font\fontA\def\A{Hola}\A",
                r"Hola",
            ),
            (
                font_file_is_invalid,
                r"\font\fontA invalid ",
                r"",
            ),
            (
                font_command_missing_control_sequence,
                r"\font a word2 word3",
                r"word2 word3",
            ),
            (
                bad_assignment_character,
                r"\textfont 1 = A",
                r"A",
            ),
            (
                bad_assignment_variable_int,
                r"\textfont 1 = \count 2 3 \the \count 2",
                r"3",
            ),
            (
                bad_assignment_execution,
                r"\textfont 1 = \def \A {Hello}\A",
                r"Hello",
            ),
        ),
    ];
}

/*
TODOs

static_cs_name: \def\fontA{haha}\the\font % still works
Similar:
{
    \textfont 0 = \nullfont

    \def \nullfont{nullfont macro invoked}

    Here: \expandafter \string \the \textfont 0

    \nullfont{}

    \the \textfont 0
}




fontname: \fontname \the \font etc.
\skewchar\fontA?

integer_cast_fails: \count 1 = \fontA  (\fontA still gets enabled)

string: \string \fontA
string_of_wierd_control_sequence: \expandafter \string \the \textfont 3
    where the control sequence that \textfont 3 was defined under has
    been redefined.

wierd control sequence not matched in macros:
    \def \test #1\fontA{Captured-#1-}
    \test Hello \fontA  % prints Hello
    \expandafter\test \the\scriptfont 0 \fontA  % \the\scriptfont is not matched!
    % and so \fontA gets enabled because it's returend in the macro expansion

if, ifx especially for all these wierd tokens

\font\A a
\font\B a
\the \A returns \B
*/
