//! File locations
//!
//! See section 511 of the TeXBook.

use crate::token;
use crate::traits::*;
use crate::vm;

/// Representation of a file location in TeX
#[derive(PartialEq, Eq, Debug)]
pub struct FileLocation {
    pub path: String,
    pub extension: Option<String>,
    pub area: Option<String>,
}

impl<S: TexlangState> Parsable<S> for FileLocation {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        let mut raw_string = String::new();
        let mut area_delimiter = None;
        let mut ext_delimiter = None;
        loop {
            let t = match input.peek()? {
                None => break,
                Some(t) => t,
            };
            if let token::Value::Space(_) = t.value() {
                let _ = input.consume();
                break;
            }
            let c = match t.char() {
                None => break,
                Some(c) => c,
            };
            let _ = input.consume();
            match c {
                '>' | ':' => {
                    area_delimiter = Some(raw_string.len() + 1);
                    ext_delimiter = None;
                }
                '.' => {
                    ext_delimiter = Some(raw_string.len());
                }
                _ => (),
            }
            raw_string.push(c);
        }

        Ok(FileLocation {
            path: raw_string
                [area_delimiter.unwrap_or(0)..ext_delimiter.unwrap_or(raw_string.len())]
                .into(),
            extension: ext_delimiter.map(|j| raw_string[j..].into()),
            area: area_delimiter.map(|i| raw_string[..i].into()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::testing::*;

    parse_success_tests![
        (
            path_only,
            "path/to/file",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: None,
                area: None,
            },
        ),
        (
            path_only_newline,
            "path/to/file\n",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: None,
                area: None,
            },
        ),
        (
            path_only_control_sequence,
            r"path/to/file\relax more",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: None,
                area: None,
            },
        ),
        (
            path_only_trailing_word,
            "path/to/file something",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: None,
                area: None,
            },
        ),
        (
            extension_only,
            ".tex",
            FileLocation {
                path: "".to_string(),
                extension: Some(".tex".to_string()),
                area: None,
            },
        ),
        (
            path_and_extension,
            "path/to/file.tex",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: Some(".tex".to_string()),
                area: None,
            },
        ),
        (
            path_and_area_with_langle,
            "area>path/to/file",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: None,
                area: Some("area>".to_string()),
            },
        ),
        (
            path_and_area_with_colon,
            "area:path/to/file",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: None,
                area: Some("area:".to_string()),
            },
        ),
        (
            area_only,
            "area:",
            FileLocation {
                path: "".to_string(),
                extension: None,
                area: Some("area:".to_string()),
            },
        ),
        (
            path_and_extension_and_area_with_colon,
            "area:path/to/file.tex",
            FileLocation {
                path: "path/to/file".to_string(),
                extension: Some(".tex".to_string()),
                area: Some("area:".to_string()),
            },
        ),
    ];
}
