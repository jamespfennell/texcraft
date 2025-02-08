//! Common abstractions used in Texlang

use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};
use texlang::prelude as txl;
use texlang::vm::TexlangState;

/// Implementations of this trait can provide access to the file system.
///
/// This trait is intended to be implemented by the state and used as a trait
/// bound in Texlang primitives like `\input` that require a file system.
///
/// The filesystem is returned in a dynamic pointer to avoid complicating
/// the trait with a generic parameter.
/// File system operations are rate in TeX documents so the overhead
/// of a vtable lookup is negligable.
pub trait HasFileSystem {
    fn file_system(&self) -> Rc<RefCell<dyn FileSystem>> {
        Rc::new(RefCell::new(RealFileSystem {}))
    }
}

/// File system operations that TeX may need to perform.
///
/// These operations are extracted to a trait so that they be mocked out in unit testing
///     and in execution contexts like WASM.
pub trait FileSystem {
    /// Read the entire contents of a file into a string.
    ///
    /// This is implemented by [std::fs::read_to_string].
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String>;

    /// Read the entire contents of a file into a bytes buffer.
    ///
    /// This is implemented by [std::fs::read].
    fn read_to_bytes(&self, path: &std::path::Path) -> std::io::Result<Vec<u8>>;

    /// Write a slice of bytes to a file.
    ///
    /// This is implemented by [std::fs::write].
    fn write_bytes(&self, path: &std::path::Path, contents: &[u8]) -> std::io::Result<()>;
}

pub fn read_file_to_string<S: HasFileSystem + TexlangState>(
    vm: &texlang::vm::VM<S>,
    file_location: texlang::parse::FileLocation,
    default_extension: &str,
) -> txl::Result<(std::path::PathBuf, String)> {
    let file_path = file_location.determine_full_path(
        vm.working_directory
            .as_ref()
            .map(std::path::PathBuf::as_ref),
        default_extension,
    );
    match vm
        .state
        .file_system()
        .borrow_mut()
        .read_to_string(&file_path)
    {
        Ok(source_code) => Ok((file_path, source_code)),
        Err(err) => Err(vm.fatal_error(IoError {
            title: format!("could not read from `{}`", file_path.display()),
            underlying_error: err,
        })),
    }
}

pub fn read_file_to_bytes<S: HasFileSystem + TexlangState>(
    vm: &texlang::vm::VM<S>,
    file_location: texlang::parse::FileLocation,
    default_extension: &str,
) -> txl::Result<Option<(std::path::PathBuf, Vec<u8>)>> {
    let file_path = file_location.determine_full_path(
        vm.working_directory
            .as_ref()
            .map(std::path::PathBuf::as_ref),
        default_extension,
    );
    match vm
        .state
        .file_system()
        .borrow_mut()
        .read_to_bytes(&file_path)
    {
        Ok(source_code) => Ok(Some((file_path, source_code))),
        Err(err) => {
            vm.error(IoError {
                title: format!("could not read from `{}`", file_path.display()),
                underlying_error: err,
            })?;
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct IoError {
    pub title: String,
    pub underlying_error: std::io::Error,
}

impl texlang::error::TexError for IoError {
    fn kind(&self) -> texlang::error::Kind {
        texlang::error::Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        self.title.clone()
    }

    fn notes(&self) -> Vec<texlang::error::display::Note> {
        vec![format!("underlying filesystem error: {}", self.underlying_error).into()]
    }
}

/// Implementation of the file system trait the uses the real file system.
pub struct RealFileSystem;

impl FileSystem for RealFileSystem {
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
    fn read_to_bytes(&self, path: &std::path::Path) -> std::io::Result<Vec<u8>> {
        std::fs::read(path)
    }
    fn write_bytes(&self, path: &std::path::Path, contents: &[u8]) -> std::io::Result<()> {
        std::fs::write(path, contents)
    }
}

/// In-memory filesystem for use in unit tests.
///
/// This type mocks out the file system operations in the VM.
/// It provides an in-memory system to which "files" can be added before the test runs.
/// It is designed to help test primitives that interact with the filesystem.
///
/// Given a VM, the file system can be set as follows:
/// ```
/// # use texlang::vm;
/// # use std::rc::Rc;
/// # use std::cell::RefCell;
/// # use texlang_common::*;
/// # use std::collections::HashMap;
/// #[derive(Default)]
/// struct State {
///     file_system: Rc<RefCell<InMemoryFileSystem>>,
/// }
/// let mut vm = vm::VM::<State>::new_with_built_in_commands(
///     HashMap::new(),  // empty set of built-in commands
/// );
/// let mut mock_file_system = InMemoryFileSystem::new(&vm.working_directory.as_ref().unwrap());
/// mock_file_system.add_string_file("file/path.tex", "file content");
/// vm.state.file_system = Rc::new(RefCell::new(mock_file_system));
/// ```
#[derive(Default)]
pub struct InMemoryFileSystem {
    working_directory: std::path::PathBuf,
    string_files: HashMap<std::path::PathBuf, String>,
    bytes_files: HashMap<std::path::PathBuf, Vec<u8>>,
}

impl InMemoryFileSystem {
    /// Create a new in-memory file system.
    ///
    /// Typically the working directory is taken from the VM.
    pub fn new(working_directory: &std::path::Path) -> Self {
        Self {
            working_directory: working_directory.into(),
            string_files: Default::default(),
            bytes_files: Default::default(),
        }
    }
    /// Add a string file to the in-memory file system.
    ///
    /// The provided path is relative to the working directory
    pub fn add_string_file(&mut self, relative_path: &str, content: &str) {
        let mut path = self.working_directory.clone();
        path.push(relative_path);
        self.string_files.insert(path, content.to_string());
    }
    /// Add a bytes file to the in-memory file system.
    ///
    /// The provided path is relative to the working directory
    pub fn add_bytes_file(&mut self, relative_path: &str, content: &[u8]) {
        let mut path = self.working_directory.clone();
        path.push(relative_path);
        self.bytes_files.insert(path, content.into());
    }
}

impl FileSystem for InMemoryFileSystem {
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
        match self.string_files.get(path) {
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "not found",
            )),
            Some(content) => Ok(content.clone()),
        }
    }
    fn read_to_bytes(&self, path: &std::path::Path) -> std::io::Result<Vec<u8>> {
        match self.bytes_files.get(path) {
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "not found",
            )),
            Some(content) => Ok(content.clone()),
        }
    }
    fn write_bytes(&self, _: &std::path::Path, _: &[u8]) -> std::io::Result<()> {
        unimplemented!()
    }
}

/// Implementations of this trait can provide access to an output terminal and a log file.
pub trait HasLogging {
    /// Return the output terminal.
    ///
    /// The default implementation returns standard out.
    fn terminal_out(&self) -> Rc<RefCell<dyn std::io::Write>> {
        Rc::new(RefCell::new(std::io::stdout()))
    }

    /// Return the log file.
    ///
    /// The default implementation returns a sink that writes nothing.
    fn log_file(&self) -> Rc<RefCell<dyn std::io::Write>> {
        Rc::new(RefCell::new(std::io::sink()))
    }
}

/// Implementations of this trait can provide access to an input terminal.
///
/// This trait is intended to be implemented by the state and used as a trait
/// bound in Texlang primitives like `\read` that require an input terminal.
pub trait HasTerminalIn {
    fn terminal_in(&self) -> Rc<RefCell<dyn TerminalIn>> {
        Rc::new(RefCell::new(std::io::stdin()))
    }
}

/// Input operations from the terminal.
pub trait TerminalIn {
    /// Read a line from the terminal and append it to the provided buffer.
    fn read_line(&mut self, prompt: Option<&str>, buffer: &mut String) -> std::io::Result<()>;
}

impl TerminalIn for std::io::Stdin {
    fn read_line(&mut self, prompt: Option<&str>, buffer: &mut String) -> std::io::Result<()> {
        if let Some(prompt) = prompt {
            eprint!("\n{prompt}")
        }
        std::io::Stdin::read_line(self, buffer)?;
        Ok(())
    }
}

/// A mock version of [`TerminalIn`].
///
/// This type wraps a vector of strings.
/// The first call to [`TerminalIn::read_line`] returns the first string;
///   the second call returns the second string;
///   and so on.
/// When the vector is exhausted, `read_line` returns an IO error of
///   kind [std::io::ErrorKind::UnexpectedEof].
#[derive(Default)]
pub struct MockTerminalIn(usize, Vec<String>);

impl MockTerminalIn {
    /// Add a new line to be returned from the mock terminal.
    pub fn add_line<S: Into<String>>(&mut self, line: S) {
        self.1.push(line.into());
    }
}

impl TerminalIn for MockTerminalIn {
    fn read_line(&mut self, _: Option<&str>, buffer: &mut String) -> std::io::Result<()> {
        match self.1.get(self.0) {
            None => Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "mock terminal input exhausted",
            )),
            Some(line) => {
                buffer.push_str(line);
                self.0 += 1;
                Ok(())
            }
        }
    }
}
