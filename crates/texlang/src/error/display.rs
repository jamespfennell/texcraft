use crate::error;
use crate::token::trace::{self, SourceCodeTrace};
use colored::*;

pub fn format_error(f: &mut std::fmt::Formatter<'_>, err: &error::Error) -> std::fmt::Result {
    let (stack, root) = err.stack_view();
    let (error_line, immediate_command) = match root.kind() {
        error::Kind::Token(s) => (s, stack.last()),
        error::Kind::EndOfInput(s) => (s, stack.last()),
        error::Kind::FailedPrecondition => (&stack.last().unwrap().trace, None),
    };

    let line = PrimaryLine {
        kind: PrimaryLineKind::Error,
        source: error_line,
        title: root.title(),
        token_annotation: root.source_annotation(),
        notes: root.notes().iter().map(|n| format!["{n}"]).collect(),
    };
    write!(f, "{line}")?;

    if let Some(err) = immediate_command {
        let line = PrimaryLine {
            kind: PrimaryLineKind::Context,
            source: &err.trace,
            title: format!["this error occurred while {}:", err.context.action()],
            token_annotation: "".into(),
            notes: vec![format![
                "this is the full stack trace of the error:\n\n{}",
                ErrorStack(stack)
            ]],
        };
        write!(f, "\n{line}")?;
    }

    Ok(())
}

#[derive(Debug)]
struct PrimaryLine<'a> {
    kind: PrimaryLineKind,
    source: &'a trace::SourceCodeTrace,
    title: String,
    token_annotation: String,
    notes: Vec<String>,
}

#[derive(Debug)]
enum PrimaryLineKind {
    Error,
    Context,
}

impl PrimaryLineKind {
    fn color(&self) -> colored::Color {
        match self {
            PrimaryLineKind::Error => colored::Color::BrightRed,
            PrimaryLineKind::Context => colored::Color::Yellow,
        }
    }
}

impl std::fmt::Display for PrimaryLineKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PrimaryLineKind::Error => "Error",
                PrimaryLineKind::Context => "Context",
            }
            .color(self.color())
            .bold()
        )
    }
}

impl<'a> std::fmt::Display for PrimaryLine<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let margin_width = self.source.line_number.to_string().len() + 1;
        let printer = Printer {
            indent: margin_width,
        };
        writeln!(f, "{}: {}", self.kind, self.title.bold())?;
        fmt_source_code_trace(
            &printer,
            f,
            self.source,
            &self.token_annotation,
            self.kind.color(),
        )?;

        for (i, note) in self.notes.iter().enumerate() {
            let mut note_lines = note.trim_end().lines();
            let first_note_line = match note_lines.next() {
                None => continue,
                Some(s) => s,
            };
            printer.new_line().print(f)?;
            printer
                .new_line()
                .with_separator('=')
                .with_content(format!["{} {}", "note:".bold(), first_note_line])
                .print(f)?;
            for line in note_lines {
                let mut l = printer.new_line().with_content(format!["      {line}"]);
                if i == self.notes.len() - 1 {
                    l = l.with_separator(' ');
                }
                l.print(f)?;
            }
        }

        Ok(())
    }
}

struct ErrorStack<'a>(Vec<&'a error::PropagatedError>);

impl<'a> std::fmt::Display for ErrorStack<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (u, propagated) in self.0.iter().rev().enumerate() {
            if u > 0 {
                writeln!(f)?;
            }
            let (s, p) = (&propagated.trace, &propagated.context);
            fmt_source_code_trace_light(f, s, 2, PrimaryLineKind::Context.color(), p.action())?;
        }
        Ok(())
    }
}

struct Printer {
    indent: usize,
}

struct PrintLineBuilder<'a> {
    printer: &'a Printer,
    margin_content: String,
    separator: Option<char>,
    content: String,
    indent_adjustment: usize,
}

impl Printer {
    fn new_line(&self) -> PrintLineBuilder {
        PrintLineBuilder {
            printer: self,
            margin_content: "".into(),
            separator: Some('|'),
            content: "".into(),
            indent_adjustment: 0,
        }
    }
}

impl<'a> PrintLineBuilder<'a> {
    fn with_content<T: Into<String>>(mut self, content: T) -> Self {
        self.content = content.into();
        self
    }
    fn with_margin_content<T: Into<String>>(mut self, content: T) -> Self {
        self.margin_content = content.into();
        self
    }
    fn with_indent_adjustment(mut self, u: usize) -> Self {
        self.indent_adjustment = u;
        self
    }
    fn with_separator(mut self, c: char) -> Self {
        self.separator = Some(c);
        self
    }
    fn without_separator(mut self) -> Self {
        self.separator = None;
        self
    }

    fn print(self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent: usize = self
            .printer
            .indent
            .saturating_sub(self.indent_adjustment)
            .saturating_sub(self.margin_content.len() + 1);
        let margin_content = format!["{}{} ", " ".repeat(indent), self.margin_content];
        let separator = match self.separator {
            None => "".to_string(),
            Some(c) => format!["{c} "],
        };
        writeln!(
            f,
            "{}{}{}",
            margin_content.bright_cyan(),
            separator.bright_cyan(),
            self.content
        )
    }
}

fn fmt_source_code_trace(
    printer: &Printer,
    f: &mut std::fmt::Formatter<'_>,
    s: &trace::SourceCodeTrace,
    annotation: &str,
    annotation_color: colored::Color,
) -> std::fmt::Result {
    printer
        .new_line()
        .without_separator()
        .with_indent_adjustment(1)
        .with_content(format!(
            "{} {}:{}:{}",
            ">>>".bright_cyan().bold(),
            s.file_name.display(),
            s.line_number,
            s.index + 1
        ))
        .print(f)?;
    printer.new_line().print(f)?;
    printer
        .new_line()
        .with_margin_content(format!["{}", s.line_number])
        .with_content(highlight_substring(&s.line_content, s.index, s.value.len()))
        .print(f)?;
    printer
        .new_line()
        .with_content(format![
            "{}{} {}",
            " ".repeat(s.index),
            "^".repeat(s.value.len()).color(annotation_color).bold(),
            annotation.color(annotation_color).bold(),
        ])
        .print(f)?;
    Ok(())
}

fn highlight_substring(line: &str, start: usize, length: usize) -> String {
    if line.len() < start + length {
        return line.into();
    }
    format![
        "{}{}{}",
        &line[..start],
        line[start..start + length].bold(),
        line[start + length..].trim_end(),
    ]
}

fn fmt_source_code_trace_light(
    f: &mut std::fmt::Formatter<'_>,
    s: &SourceCodeTrace,
    indent: usize,
    underline_color: colored::Color,
    annotation: &str,
) -> std::fmt::Result {
    let prefix = format!(
        "{}{}:{}:{}",
        " ".repeat(indent),
        s.file_name.display(),
        s.line_number,
        s.index + 1
    );
    writeln!(
        f,
        "{}  {}",
        prefix,
        highlight_substring(&s.line_content, s.index, s.value.len())
    )?;
    writeln!(
        f,
        "{}  {} {}",
        " ".repeat(prefix.len() + s.index),
        "^".repeat(s.value.len()).color(underline_color).bold(),
        annotation,
    )?;
    Ok(())
}

#[derive(Debug)]
pub enum Note<'a> {
    Text(String),
    SourceCodeTrace(String, &'a trace::SourceCodeTrace),
}

impl<'a, T: Into<String>> From<T> for Note<'a> {
    fn from(value: T) -> Self {
        Note::Text(value.into())
    }
}

impl<'a> std::fmt::Display for Note<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Note::Text(s) => write!(f, "{s}"),
            Note::SourceCodeTrace(s, trace) => {
                write!(f, "{s}\n\n")?;
                fmt_source_code_trace_light(f, trace, 2, PrimaryLineKind::Error.color(), "")
            }
        }
    }
}
