//! Human readable formatting of DVI operations.
//!
//! The format is designed to be easy to read and, sometime in the future,
//! e parseable back into the same list of [`Op`] values.
//! Each operation is printed like a function call whose name is the
//! snake case version of the operation, and whose arguments are the
//! payload of the operation:
//!
//! ```
//! # use dvi::{Op, Var};
//! assert_eq!(dvi::display::format_ops([Op::SetVar(Var::W, 300)]), "set_var(w, 300)\n");
//! ```
//!
//! Operations with no payload are printed without parentheses:
//!
//! ```
//! # use dvi::Op;
//! assert_eq!(dvi::display::format_ops([Op::Push, Op::Pop]), "push\npop\n");
//! ```
//!
//! Operations with a large payload place each argument on its own line:
//!
//! ```
//! # use dvi::Op;
//! assert_eq!(
//!     dvi::display::format_ops([Op::Preamble{
//!         dvi_format: 2,
//!         unit_numerator: 25400000,
//!         unit_denominator: 473628672,
//!         magnification: 1000,
//!         comment: "Texcraft".into(),
//!     }]),
//!     r#"preamble(
//!   dvi_format: 2,
//!   unit_numerator: 25400000,
//!   unit_denominator: 473628672,
//!   magnification: 1000,
//!   comment: "Texcraft",
//! )
//! "#,
//! );
//! ```
//!
//! Finally, [`Op::Push`] and [`Op::Pop`] operations introduce indentation,
//! and some operations are annotated with a comment describing runtime
//! state that is not visible in the operation itself:
//!
//! ```
//! # use dvi::{Op, Var};
//! assert_eq!(
//!     dvi::display::format_ops([
//!         Op::SetVar(Var::W, 300),
//!         Op::Push,
//!         Op::Move(Var::W),
//!         Op::Pop,
//!     ]),
//!     "set_var(w, 300)\npush\n  move(w)  // 300\npop\n",
//! );
//! ```
//!
//! Indentation can be disabled, and byte offsets can be printed, using
//! the [`Options`] type.

use crate::{Op, Values, Var};
use std::fmt::Write;

/// String used for one level of indentation.
const INDENT: &str = "  ";

/// Width of the byte offset column.
const OFFSET_WIDTH: usize = 8;

/// Options for the [`Formatter`].
///
/// The default options print no byte offsets and apply indentation:
///
/// ```
/// let options: dvi::display::Options = Default::default();
/// assert_eq!(options.byte_offsets, false);
/// assert_eq!(options.indentation, true);
/// ```
#[derive(Clone, Debug)]
pub struct Options {
    /// If true, every line is prefixed with a column containing the offset
    /// in the DVI data of the operation on that line.
    ///
    /// Only the first line of a multi-line operation displays the offset;
    /// subsequent lines are padded so that the operations stay aligned.
    pub byte_offsets: bool,
    /// If true, [`Op::Push`] and [`Op::Pop`] operations introduce and remove
    /// indentation.
    ///
    /// Indentation makes the structure of a DVI file much easier to see, but
    /// it is worth disabling when diffing the output for two DVI files whose
    /// push and pop operations are paired up differently. In that case
    /// indentation makes the diff much larger than the true difference
    /// between the files.
    pub indentation: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            byte_offsets: false,
            indentation: true,
        }
    }
}

/// Formatter for lists of DVI operations.
///
/// Most of the format depends only on the operation being printed, and for
/// that the [`std::fmt::Display`] implementation on [`Op`] suffices.
/// This type exists because two parts of the format depend on the operations
/// that came before:
///
/// - [`Op::Push`] and [`Op::Pop`] operations introduce and remove indentation.
///
/// - Some operations are annotated with a comment giving runtime state.
///   For example [`Op::Move`] is annotated with the current value of the
///   variable being moved by, as that value does not appear in the operation.
///
/// The formatter tracks this state using the [`Values`] data structure:
///
/// ```
/// let ops = vec![
///     dvi::Op::SetVar(dvi::Var::Y, 300),
///     dvi::Op::Move(dvi::Var::Y),
/// ];
/// let mut formatter: dvi::display::Formatter = Default::default();
/// let mut out = String::new();
/// for op in &ops {
///     formatter.write_op(&mut out, op, 0).unwrap();
/// }
/// assert_eq!(out, "set_var(y, 300)\nmove(y)  // 300\n");
/// ```
#[derive(Default)]
pub struct Formatter {
    options: Options,
    values: Values,
    /// Scratch buffer, reused across operations.
    buffer: String,
}

impl Formatter {
    /// Create a new formatter with the provided options.
    pub fn new(options: Options) -> Self {
        Self {
            options,
            values: Default::default(),
            buffer: Default::default(),
        }
    }
    /// Write a single operation, followed by a newline.
    ///
    /// Operations must be provided in the order they appear in the DVI data,
    /// otherwise the indentation and the comments will be wrong.
    /// The byte offset is the offset of the operation in the DVI data;
    /// it is only printed if the [`Options::byte_offsets`] option is set.
    pub fn write_op<W: Write>(
        &mut self,
        w: &mut W,
        op: &Op,
        byte_offset: usize,
    ) -> std::fmt::Result {
        // The indentation is the stack depth, except that a pop is printed
        // at the depth of the push it matches so that the two line up.
        let depth = self.values.stack_depth();
        let (indent, stack_underflow) = match op {
            Op::Pop => match depth.checked_sub(1) {
                Some(indent) => (indent, false),
                // A pop that is not matched by a push is invalid DVI data,
                // but this is exactly the kind of file this formatter is
                // used to debug, so print it with a marker instead of
                // panicking.
                None => (0, true),
            },
            _ => (depth, false),
        };

        self.buffer.clear();
        write_op(&mut self.buffer, op, Some(&self.values));
        if stack_underflow {
            self.buffer.push_str("  // stack underflow");
        }

        let indent = if self.options.indentation { indent } else { 0 };
        for (i, line) in self.buffer.lines().enumerate() {
            if self.options.byte_offsets {
                if i == 0 {
                    write!(w, "{byte_offset:>OFFSET_WIDTH$}  ")?;
                } else {
                    write!(w, "{:>OFFSET_WIDTH$}  ", "")?;
                }
            }
            for _ in 0..indent {
                w.write_str(INDENT)?;
            }
            w.write_str(line)?;
            w.write_char('\n')?;
        }

        self.values.update(op);
        Ok(())
    }

    /// Get the options the formatter was created with.
    pub fn options(&self) -> &Options {
        &self.options
    }

    /// Get the values the formatter has accumulated so far.
    pub fn values(&self) -> &Values {
        &self.values
    }
}

/// Format a list of operations as a string, using the default [`Options`].
///
/// Because the byte offsets of the operations are not known, this function
/// cannot be used with the [`Options::byte_offsets`] option.
///
/// ```
/// let ops = vec![dvi::Op::Right(300), dvi::Op::Down(-4)];
/// assert_eq!(dvi::display::format_ops(ops), "right(300)\ndown(-4)\n");
/// ```
pub fn format_ops<I: IntoIterator<Item = Op>>(ops: I) -> String {
    let mut formatter: Formatter = Default::default();
    let mut out = String::new();
    for op in ops {
        // Writing to a string cannot fail.
        formatter.write_op(&mut out, &op, 0).unwrap();
    }
    out
}

impl std::fmt::Display for Op {
    /// Display a single operation.
    ///
    /// Operations whose format depends on the operations that came before
    /// are displayed without that context: no indentation is applied, and
    /// comments describing runtime state are omitted.
    /// Use a [`Formatter`] to display a list of operations.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();
        write_op(&mut out, self, None);
        f.write_str(&out)
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Var::W => "w",
            Var::X => "x",
            Var::Y => "y",
            Var::Z => "z",
        })
    }
}

/// Write the operation to the provided string, without indentation.
///
/// If values are provided, comments describing runtime state are included.
fn write_op(out: &mut String, op: &Op, values: Option<&Values>) {
    match op {
        Op::Char { char, move_h } => {
            // The DVI format has separate set and put commands, and printing
            // them separately avoids a positional boolean argument.
            out.push_str(if *move_h { "char(" } else { "put_char(" });
            write_char(out, *char);
            out.push(')');
        }
        Op::Rule {
            height,
            width,
            move_h,
        } => {
            let name = if *move_h { "rule" } else { "put_rule" };
            write!(out, "{name}(height: {height}, width: {width})").unwrap();
        }
        // TODO: consider collapsing runs of no_op operations, which TeX emits
        // as padding. This would make the output much shorter for some files,
        // at the cost of the output no longer round-tripping.
        Op::NoOp => out.push_str("no_op"),
        Op::BeginPage {
            parameters,
            previous_begin_page,
        } => {
            let parameters: Vec<String> = parameters.iter().map(i32::to_string).collect();
            write_multiline(
                out,
                "begin_page",
                &[
                    ("parameters", format!("[{}]", parameters.join(", "))),
                    ("previous_begin_page", previous_begin_page.to_string()),
                ],
            );
        }
        Op::EndPage => out.push_str("end_page"),
        Op::Push => out.push_str("push"),
        Op::Pop => out.push_str("pop"),
        Op::Right(d) => write!(out, "right({d})").unwrap(),
        Op::Move(var) => {
            write!(out, "move({var})").unwrap();
            if let Some(values) = values {
                // The value moved by does not appear in the operation, so
                // print it as a comment.
                write!(out, "  // {}", values.var(*var)).unwrap();
            }
        }
        Op::SetVar(var, i) => write!(out, "set_var({var}, {i})").unwrap(),
        Op::Down(d) => write!(out, "down({d})").unwrap(),
        Op::EnableFont(f) => write!(out, "enable_font({f})").unwrap(),
        Op::Extension(b) => match std::str::from_utf8(b) {
            Ok(s) => write!(out, "extension({s:?})").unwrap(),
            Err(_) => {
                out.push_str("extension(bytes: \"");
                for byte in b {
                    write!(out, "{byte:02x}").unwrap();
                }
                out.push_str("\")");
            }
        },
        Op::DefineFont {
            number,
            checksum,
            at_size,
            design_size,
            area,
            name,
        } => write_multiline(
            out,
            "define_font",
            &[
                ("number", number.to_string()),
                ("checksum", checksum.to_string()),
                ("at_size", at_size.to_string()),
                ("design_size", design_size.to_string()),
                ("area", format!("{area:?}")),
                ("name", format!("{name:?}")),
            ],
        ),
        Op::Preamble {
            dvi_format,
            unit_numerator,
            unit_denominator,
            magnification,
            comment,
        } => write_multiline(
            out,
            "preamble",
            &[
                ("dvi_format", dvi_format.to_string()),
                ("unit_numerator", unit_numerator.to_string()),
                ("unit_denominator", unit_denominator.to_string()),
                ("magnification", magnification.to_string()),
                ("comment", format!("{comment:?}")),
            ],
        ),
        Op::BeginPostamble {
            final_begin_page,
            unit_numerator,
            unit_denominator,
            magnification,
            largest_height,
            largest_width,
            max_stack_depth,
            num_pages,
        } => write_multiline(
            out,
            "begin_postamble",
            &[
                ("final_begin_page", final_begin_page.to_string()),
                ("unit_numerator", unit_numerator.to_string()),
                ("unit_denominator", unit_denominator.to_string()),
                ("magnification", magnification.to_string()),
                ("largest_height", largest_height.to_string()),
                ("largest_width", largest_width.to_string()),
                ("max_stack_depth", max_stack_depth.to_string()),
                ("num_pages", num_pages.to_string()),
            ],
        ),
        Op::EndPostamble {
            postamble,
            dvi_format,
            num_223_bytes,
        } => write!(
            out,
            "end_postamble(postamble: {postamble}, dvi_format: {dvi_format}, num_223_bytes: {num_223_bytes})"
        )
        .unwrap(),
    }
}

/// Write an operation with each argument on its own line.
fn write_multiline(out: &mut String, name: &str, args: &[(&str, String)]) {
    write!(out, "{name}(").unwrap();
    for (key, value) in args {
        write!(out, "\n{INDENT}{key}: {value},").unwrap();
    }
    out.push_str("\n)");
}

/// Write the argument of a char or put_char operation.
///
/// Printable ASCII characters are written as character literals because
/// this makes the output of the formatter much easier to read.
/// All other characters are written as numbers.
fn write_char(out: &mut String, char: u32) {
    match char::from_u32(char) {
        Some(c) if c.is_ascii_graphic() || c == ' ' => match c {
            '\'' => out.push_str(r"'\''"),
            '\\' => out.push_str(r"'\\'"),
            _ => {
                out.push('\'');
                out.push(c);
                out.push('\'');
            }
        },
        _ => write!(out, "{char}").unwrap(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! format_tests {
        ( $( ($name: ident, $ops: expr, $want: expr), )+ ) => {
            $(
            #[test]
            fn $name() {
                let ops: Vec<Op> = $ops.into();
                let got = format_ops(ops);
                let want: &str = $want;
                assert_eq!(got, want);
            }
            )+
        };
    }

    format_tests!(
        (
            no_payload,
            [Op::NoOp, Op::EndPage],
            "no_op\nend_page\n"
        ),
        (
            char_printable,
            [Op::Char{char: 'A' as u32, move_h: true}],
            "char('A')\n"
        ),
        (
            char_space,
            [Op::Char{char: ' ' as u32, move_h: true}],
            "char(' ')\n"
        ),
        (
            char_quote,
            [Op::Char{char: '\'' as u32, move_h: true}],
            "char('\\'')\n"
        ),
        (
            char_backslash,
            [Op::Char{char: '\\' as u32, move_h: true}],
            "char('\\\\')\n"
        ),
        (
            char_not_printable,
            [Op::Char{char: 0, move_h: true}],
            "char(0)\n"
        ),
        (
            char_large,
            [Op::Char{char: 1000, move_h: true}],
            "char(1000)\n"
        ),
        (
            put_char,
            [Op::Char{char: 'A' as u32, move_h: false}],
            "put_char('A')\n"
        ),
        (
            rule,
            [Op::Rule{height: 1, width: 2, move_h: true}],
            "rule(height: 1, width: 2)\n"
        ),
        (
            put_rule,
            [Op::Rule{height: 1, width: 2, move_h: false}],
            "put_rule(height: 1, width: 2)\n"
        ),
        (
            extension_string,
            [Op::Extension("papersize=8.5in,11in".into())],
            "extension(\"papersize=8.5in,11in\")\n"
        ),
        (
            extension_bytes,
            [Op::Extension(vec![0xff, 0x01])],
            "extension(bytes: \"ff01\")\n"
        ),
        (
            push_and_pop_indent,
            [Op::Push, Op::Push, Op::NoOp, Op::Pop, Op::Pop, Op::NoOp],
            "push\n  push\n    no_op\n  pop\npop\nno_op\n"
        ),
        (
            unmatched_pop,
            [Op::Pop],
            "pop  // stack underflow\n"
        ),
        (
            too_many_pops,
            [Op::Push, Op::Pop, Op::Pop, Op::NoOp],
            "push\npop\npop  // stack underflow\nno_op\n"
        ),
        (
            unmatched_push,
            [Op::Push, Op::Push, Op::NoOp],
            "push\n  push\n    no_op\n"
        ),
        (
            begin_page_resets_indentation,
            [
                Op::Push,
                Op::Push,
                Op::BeginPage{parameters: [0; 10], previous_begin_page: -1},
                Op::NoOp,
            ],
            "push\n  push\n    begin_page(\n      parameters: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],\n      previous_begin_page: -1,\n    )\nno_op\n"
        ),
        (
            move_comment,
            [Op::SetVar(Var::W, 300), Op::Move(Var::W)],
            "set_var(w, 300)\nmove(w)  // 300\n"
        ),
        (
            move_comment_is_zero_by_default,
            [Op::Move(Var::Z)],
            "move(z)  // 0\n"
        ),
        (
            move_comment_after_pop,
            [
                Op::SetVar(Var::W, 100),
                Op::Push,
                Op::SetVar(Var::W, 200),
                Op::Move(Var::W),
                Op::Pop,
                Op::Move(Var::W),
            ],
            "set_var(w, 100)\npush\n  set_var(w, 200)\n  move(w)  // 200\npop\nmove(w)  // 100\n"
        ),
        (
            move_comment_after_unmatched_pop,
            [
                Op::SetVar(Var::W, 100),
                Op::Pop,
                Op::Move(Var::W),
            ],
            "set_var(w, 100)\npop  // stack underflow\nmove(w)  // 100\n"
        ),
        (
            move_comment_after_begin_page,
            [
                Op::SetVar(Var::W, 100),
                Op::BeginPage{parameters: [0; 10], previous_begin_page: -1},
                Op::Move(Var::W),
            ],
            "set_var(w, 100)\nbegin_page(\n  parameters: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],\n  previous_begin_page: -1,\n)\nmove(w)  // 0\n"
        ),
    );

    #[test]
    fn byte_offsets() {
        let ops = [
            Op::Push,
            Op::DefineFont {
                number: 0,
                checksum: 1,
                at_size: 2,
                design_size: 3,
                area: "".into(),
                name: "cmr10".into(),
            },
            Op::Pop,
        ];
        let mut formatter = Formatter::new(Options {
            byte_offsets: true,
            ..Default::default()
        });
        let mut got = String::new();
        for (i, op) in ops.iter().enumerate() {
            formatter.write_op(&mut got, op, i * 10).unwrap();
        }
        let want = r#"       0  push
      10    define_font(
              number: 0,
              checksum: 1,
              at_size: 2,
              design_size: 3,
              area: "",
              name: "cmr10",
            )
      20  pop
"#;
        assert_eq!(got, want);
    }

    #[test]
    fn indentation_disabled() {
        let ops = vec![
            Op::Push,
            Op::Push,
            Op::NoOp,
            Op::Pop,
            Op::Pop,
            Op::Pop,
            Op::NoOp,
        ];
        let mut formatter = Formatter::new(Options {
            indentation: false,
            ..Default::default()
        });
        let mut got = String::new();
        for op in &ops {
            formatter.write_op(&mut got, op, 0).unwrap();
        }
        // Disabling indentation does not disable the stack underflow marker,
        // which does not depend on indentation.
        let want = "push\npush\nno_op\npop\npop\npop  // stack underflow\nno_op\n";
        assert_eq!(got, want);
    }

    #[test]
    fn indentation_disabled_with_byte_offsets() {
        let ops = [Op::Push, Op::NoOp];
        let mut formatter = Formatter::new(Options {
            byte_offsets: true,
            indentation: false,
        });
        let mut got = String::new();
        for (i, op) in ops.iter().enumerate() {
            formatter.write_op(&mut got, op, i).unwrap();
        }
        assert_eq!(got, "       0  push\n       1  no_op\n");
    }

    #[test]
    fn display_single_op_omits_comment() {
        assert_eq!(format!("{}", Op::Move(Var::W)), "move(w)");
    }
}
