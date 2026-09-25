use clap::Parser;

fn main() {
    if let Err(err) = Cli::parse().run() {
        if !err.is_empty() {
            eprintln!("Error: {err}");
        }
        std::process::exit(1);
    }
}

/// Tools for working with DVI files.
#[derive(Debug, clap::Parser)]
#[command(
    name = "DVI tools",
    author = "The Texcraft Project",
    version = "0.1",
    about,
    long_about,
    max_term_width(100)
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

impl Cli {
    fn run(self) -> Result<(), String> {
        match self.command {
            Command::Diff(diff) => diff.run(),
            Command::Inspect(inspect) => inspect.run(),
            Command::Normalize(normalize) => normalize.run(),
            Command::Text(text) => text.run(),
        }
    }
}

#[derive(Clone, Debug, clap::Subcommand)]
enum Command {
    /// Diff the contents of two DVI files.
    Diff(Diff),
    /// Print the contents of a DVI file in human-readable format.
    Inspect(Inspect),
    /// Normalize a DVI file.
    Normalize(Normalize),
    /// Convert a DVI file to plain text.
    Text(Text),
}

#[derive(Clone, Debug, Parser)]
struct Inspect {
    /// Path to the DVI file.
    path: std::path::PathBuf,

    /// Prefix each line of the output with the offset in the DVI file
    /// of the operation on that line.
    #[arg(short = 'o', long)]
    offsets: bool,

    /// Don't indent the output on push and pop operations.
    ///
    /// This is useful when diffing the output for two DVI files whose
    /// push and pop operations are paired up differently.
    #[arg(short = 'I', long)]
    no_indentation: bool,
}

impl Inspect {
    fn run(self) -> Result<(), String> {
        let options = dvi::display::Options {
            byte_offsets: self.offsets,
            indentation: !self.no_indentation,
        };
        print!("{}", inspect_file(&self.path, options)?);
        Ok(())
    }
}

#[derive(Clone, Debug, Parser)]
struct Diff {
    /// Path to the first DVI file.
    left: std::path::PathBuf,

    /// Path to the second DVI file.
    right: std::path::PathBuf,

    /// Number of lines of context to print around each difference.
    #[arg(short = 'c', long, default_value_t = 3)]
    context: usize,

    /// Don't indent the output on push and pop operations.
    ///
    /// This is useful when the two DVI files pair up their push and pop
    /// operations differently, because in that case indentation makes the
    /// diff much larger than the true difference between the files.
    #[arg(short = 'I', long)]
    no_indentation: bool,
}

impl Diff {
    fn run(self) -> Result<(), String> {
        // Byte offsets are never printed because a difference early in the
        // files changes the offset of every operation after it, which would
        // make the entire remainder of the diff spurious.
        let options = dvi::display::Options {
            byte_offsets: false,
            indentation: !self.no_indentation,
        };
        let left = inspect_file(&self.left, options.clone())?;
        let right = inspect_file(&self.right, options)?;
        if left == right {
            return Ok(());
        }

        let patch = diffy::DiffOptions::new()
            .set_context_len(self.context)
            .set_original_filename(self.left.display().to_string())
            .set_modified_filename(self.right.display().to_string())
            .create_patch(&left, &right);
        let formatter = diffy::PatchFormatter::new();
        // Diffy does not decide on its own whether to use color.
        #[cfg(feature = "color")]
        let formatter = if use_color() {
            formatter.with_color()
        } else {
            formatter
        };
        print!("{}", formatter.fmt_patch(&patch));

        // The files differ, so exit with a non-zero code.
        // The error is empty because the diff itself has already been
        // printed and there is no error message to add to it.
        Err("".into())
    }
}

/// Determine whether colors should be used in the output.
///
/// The environment variables consulted here are part of the widely
/// implemented informal standard at <https://bixense.com/clicolors/>.
#[cfg(feature = "color")]
fn use_color() -> bool {
    use std::io::IsTerminal;
    let var = |key: &str| std::env::var_os(key).filter(|value| !value.is_empty());
    if var("CLICOLOR_FORCE").is_some_and(|value| value != "0") {
        return true;
    }
    if var("NO_COLOR").is_some() || var("CLICOLOR").is_some_and(|value| value == "0") {
        return false;
    }
    // Colors are only for humans: if the output is being piped somewhere,
    // the escape sequences would just be noise.
    std::io::stdout().is_terminal()
}

/// Read the DVI file at the provided path and format its operations
/// in the human-readable format.
fn inspect_file(path: &std::path::Path, options: dvi::display::Options) -> Result<String, String> {
    let b = match std::fs::read(path) {
        Ok(b) => b,
        Err(err) => return Err(format!("failed to read `{}`: {}", path.display(), err)),
    };
    let mut result = Ok(());
    let mut deserializer = dvi::Deserializer::new(&b, &mut result);
    let mut formatter = dvi::display::Formatter::new(options);
    let mut out = String::new();
    loop {
        // The offset must be read before the operation is deserialized.
        let byte_offset = deserializer.byte_offset();
        let Some(op) = deserializer.next() else {
            break;
        };
        // Writing to a string cannot fail.
        formatter.write_op(&mut out, &op, byte_offset).unwrap();
    }
    match result {
        Ok(()) => Ok(out),
        Err(err) => Err(format!("failed to parse `{}`: {}", path.display(), err)),
    }
}

#[derive(Clone, Debug, Parser)]
struct Normalize {
    /// Path to the DVI file to normalize.
    input: std::path::PathBuf,

    /// Path to write the normalized DVI file.
    output: std::path::PathBuf,
}

impl Normalize {
    fn run(self) -> Result<(), String> {
        let b = match std::fs::read(&self.input) {
            Ok(b) => b,
            Err(err) => {
                return Err(format!(
                    "failed to read `{}`: {}",
                    self.input.display(),
                    err
                ))
            }
        };
        let mut result = Ok(());
        let mut i1 = dvi::Deserializer::new(&b, &mut result);
        let i2 = dvi::transforms::VarRemover::new(&mut i1);
        let b = dvi::serialize(i2);
        result.map_err(|err| format!("{}", err))?;
        std::fs::write(&self.output, b)
            .map_err(|err| format!("failed to write `{}`: {}", self.output.display(), err))
    }
}

#[derive(Clone, Debug, Parser)]
struct Text {
    /// Path to the DVI file.
    path: std::path::PathBuf,
}

impl Text {
    fn run(self) -> Result<(), String> {
        let b = match std::fs::read(&self.path) {
            Ok(b) => b,
            Err(err) => return Err(format!("failed to read `{}`: {}", self.path.display(), err)),
        };
        let mut s = String::new();
        let mut result = Ok(());
        let mut i1 = dvi::Deserializer::new(&b, &mut result);
        let i2 = dvi::transforms::VarRemover::new(&mut i1);
        for op in i2 {
            match op {
                dvi::Op::Char { char, move_h: _ } => match char.try_into() {
                    Ok(c @ '!'..='Z' | c @ 'a'..='z') => {
                        s.push(c);
                    }
                    _ => {
                        s.push_str(&format!("<{}>", char));
                    }
                },
                dvi::Op::Right(d) => {
                    // assume a kern
                    if d < 30_000 {
                        continue;
                    }
                    s.push(' ');
                }
                dvi::Op::Down(_) => {
                    s.push('\n');
                }
                dvi::Op::Rule { .. }
                | dvi::Op::Move(_)
                | dvi::Op::SetVar(_, _)
                | dvi::Op::NoOp
                | dvi::Op::BeginPage { .. }
                | dvi::Op::EndPage
                | dvi::Op::Push
                | dvi::Op::Pop => {}
                dvi::Op::EnableFont(_)
                | dvi::Op::Extension(_)
                | dvi::Op::DefineFont { .. }
                | dvi::Op::Preamble { .. }
                | dvi::Op::BeginPostamble { .. }
                | dvi::Op::EndPostamble { .. } => {}
            }
        }
        result.map_err(|err| format!("{}", err))?;
        println!("{s}");
        Ok(())
    }
}
