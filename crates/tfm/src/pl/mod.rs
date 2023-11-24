//! The property list file format
//!
//! This module contains a work-in-progress parser for property list files.

use std::collections::HashMap;

use crate::{ligkern, Char, CharData, Header, Number, Params};
use error::Error;

pub mod ast;
pub mod cst;
pub mod error;
pub mod lexer;

/// Complete contents of a property list (.pl) file.
#[derive(PartialEq, Eq, Debug)]
pub struct File {
    pub header: Header,
    pub design_units: Number,
    pub char_data: HashMap<Char, CharData>,
    pub lig_kern_boundary_char: Option<Char>,
    pub lig_kern_boundary_char_entrypoint: Option<usize>,
    pub lig_kern_entrypoints: HashMap<Char, usize>,
    pub lig_kern_instructions: Vec<ligkern::lang::Instruction>,
    pub params: Params,
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Header::property_list_default(),
            design_units: Number::UNITY,
            char_data: Default::default(),
            lig_kern_boundary_char: Default::default(),
            lig_kern_boundary_char_entrypoint: None,
            lig_kern_entrypoints: Default::default(),
            lig_kern_instructions: Default::default(),
            params: Default::default(),
        }
    }
}

impl File {
    /// Build a File from PL source code.
    pub fn build(source: &str) -> (File, Vec<Error>) {
        let (ast, mut errors) = ast::Ast::build(source);
        let file = File::build_from_ast(ast, &mut errors);
        (file, errors)
    }

    /// Build a File from an PST.
    pub fn build_from_ast(ast: ast::Ast, errors: &mut Vec<error::Error>) -> File {
        let mut file: File = Default::default();
        let mut lig_kern_precedes = false;

        for node in ast.0 {
            match node {
                ast::Root::Checksum(v) => {
                    file.header.checksum = v.data;
                }
                ast::Root::DesignSize(v) => {
                    file.header.design_size = v.data;
                }
                ast::Root::DesignUnits(v) => {
                    file.design_units = v.data;
                }
                ast::Root::CodingScheme(v) => {
                    file.header.character_coding_scheme = v.data;
                }
                ast::Root::Family(v) => {
                    file.header.font_family = v.data;
                }
                ast::Root::Face(v) => {
                    file.header.face = Some(v.data);
                }
                ast::Root::SevenBitSafeFlag(v) => {
                    file.header.seven_bit_safe = Some(v.data);
                }
                ast::Root::Header(v) => match v.left.checked_sub(18) {
                    None => errors.push(Error::InvalidHeaderIndex { span: v.left_span }),
                    Some(i) => {
                        let i = i as usize;
                        if file.header.additional_data.len() <= i {
                            file.header.additional_data.resize(i + 1, 0);
                        }
                        file.header.additional_data[i] = v.right;
                    }
                },
                ast::Root::FontDimension(b) => {
                    for node in b.children {
                        match node {
                            ast::FontDimension::NamedParam(named_param, v) => {
                                file.params.set_named(named_param, v.data);
                            }
                            ast::FontDimension::IndexedParam(v) => {
                                // TODO: param=0 not allowed PLtoTF.2014.93
                                file.params.set(v.left as usize, v.right);
                            }
                            ast::FontDimension::Comment(_) => {}
                        }
                    }
                }
                ast::Root::LigTable(b) => {
                    for node in b.children {
                        match node {
                            ast::LigTable::Label(v) => {
                                let u = file.lig_kern_instructions.len();
                                match v.data {
                                    ast::LigTableLabel::Char(c) => {
                                        file.lig_kern_entrypoints.insert(c, u);
                                    }
                                    ast::LigTableLabel::BoundaryChar => {
                                        file.lig_kern_boundary_char_entrypoint = Some(u);
                                    }
                                }
                                lig_kern_precedes = false;
                            }
                            ast::LigTable::Lig(post_lig_operation, v) => {
                                file.lig_kern_instructions.push(ligkern::lang::Instruction {
                                    next_instruction: Some(0),
                                    right_char: v.left,
                                    operation: ligkern::lang::Operation::Ligature {
                                        char_to_insert: v.right,
                                        post_lig_operation,
                                    },
                                });
                                lig_kern_precedes = true;
                            }
                            ast::LigTable::Kern(v) => {
                                file.lig_kern_instructions.push(ligkern::lang::Instruction {
                                    next_instruction: Some(0),
                                    right_char: v.left,
                                    operation: ligkern::lang::Operation::Kern(v.right),
                                });
                                lig_kern_precedes = true;
                            }
                            ast::LigTable::Stop(_) => {
                                if lig_kern_precedes {
                                    file.lig_kern_instructions
                                        .last_mut()
                                        .unwrap()
                                        .next_instruction = None;
                                } else {
                                    // TODO: error
                                }
                                lig_kern_precedes = false;
                            }
                            ast::LigTable::Skip(v) => {
                                if lig_kern_precedes {
                                    file.lig_kern_instructions
                                        .last_mut()
                                        .unwrap()
                                        .next_instruction = Some(v.data);
                                } else {
                                    // TODO: error
                                }
                                lig_kern_precedes = false;
                            }
                            ast::LigTable::Comment(_) => {}
                        }
                    }
                }
                ast::Root::BoundaryChar(v) => {
                    file.lig_kern_boundary_char = Some(v.data);
                }
                ast::Root::Character(b) => {
                    let char_data = file.char_data.entry(b.data).or_default();
                    for node in b.children {
                        match node {
                            ast::Character::Width(v) => {
                                char_data.width = v.data;
                            }
                            ast::Character::Height(v) => {
                                char_data.height = v.data;
                            }
                            ast::Character::Depth(v) => {
                                char_data.depth = v.data;
                            }
                            ast::Character::ItalicCorrection(v) => {
                                char_data.italic_correction = v.data;
                            }
                            ast::Character::NextLarger(_) => todo!(),
                            ast::Character::ExtensibleCharacter(_) => todo!(),
                            ast::Character::Comment(_) => {}
                        }
                    }
                }
                ast::Root::Comment(_) => {}
            }
        }

        file
    }
}

#[cfg(test)]
mod tests {
    use crate::Face;

    use super::*;

    fn run(source: &str, want: File) {
        let (got, errors) = File::build(source);
        assert_eq!(errors, vec![]);
        assert_eq!(got, want);
    }

    macro_rules! build_tests {
        ( $( ($name: ident, $source: expr, $want: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let source = $source;
                    let want = $want;
                    run(source, want);
                }
            )+
        };
    }

    build_tests!(
        (
            checksum,
            "(CHECKSUM H 7)",
            File {
                header: Header {
                    checksum: 0x7,
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            design_size,
            "(DESIGNSIZE R 3.0)",
            File {
                header: Header {
                    design_size: Number::UNITY * 3,
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            design_units,
            "(DESIGNUNITS R 2.0)",
            File {
                design_units: Number::UNITY * 2,
                ..Default::default()
            },
        ),
        (
            coding_scheme,
            "(CODINGSCHEME Hola Mundo)",
            File {
                header: Header {
                    character_coding_scheme: "Hola Mundo".into(),
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            font_family,
            "(FAMILY Hello World)",
            File {
                header: Header {
                    font_family: "Hello World".into(),
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            face,
            "(FACE H 29)",
            File {
                header: Header {
                    face: Some(Face::Other(0x29)),
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            seven_bit_safe_flag,
            "(SEVENBITSAFEFLAG TRUE)",
            File {
                header: Header {
                    seven_bit_safe: Some(true),
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            additional_header_data,
            "(HEADER D 20 H 1234567)",
            File {
                header: Header {
                    additional_data: vec![0, 0, 0x1234567],
                    ..Header::property_list_default()
                },
                ..Default::default()
            },
        ),
        (
            boundary_char,
            "(BOUNDARYCHAR C a)",
            File {
                lig_kern_boundary_char: Some('a'.try_into().unwrap()),
                ..Default::default()
            },
        ),
        (
            named_param,
            "(FONTDIMEN (STRETCH D 13.0))",
            File {
                params: Params(vec![Number::ZERO, Number::ZERO, Number::UNITY * 13]),
                ..Default::default()
            },
        ),
        (
            indexed_param,
            "(FONTDIMEN (PARAMETER D 2 D 15.0))",
            File {
                params: Params(vec![Number::ZERO, Number::UNITY * 15]),
                ..Default::default()
            },
        ),
        (
            kern,
            "(LIGTABLE (KRN C r D 15.0))",
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(0),
                    right_char: 'r'.try_into().unwrap(),
                    operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                },],
                ..Default::default()
            },
        ),
        (
            kern_with_stop,
            "(LIGTABLE (KRN C r D 15.0) (STOP))",
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: 'r'.try_into().unwrap(),
                    operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                },],
                ..Default::default()
            },
        ),
        (
            kern_with_skip,
            "(LIGTABLE (KRN C r D 15.0) (SKIP D 3))",
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(3),
                    right_char: 'r'.try_into().unwrap(),
                    operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                },],
                ..Default::default()
            },
        ),
        (
            lig,
            "(LIGTABLE (LIG/> C r C t))",
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(0),
                    right_char: 'r'.try_into().unwrap(),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: 't'.try_into().unwrap(),
                        post_lig_operation: ligkern::lang::PostLigOperation::RetainRightMoveToRight,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_entrypoints,
            "(LIGTABLE (LABEL C e) (KRN C r D 15.0) (LABEL C d))",
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(0),
                    right_char: 'r'.try_into().unwrap(),
                    operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                },],
                lig_kern_entrypoints: HashMap::from([
                    ('e'.try_into().unwrap(), 0),
                    ('d'.try_into().unwrap(), 1),
                ]),
                ..Default::default()
            },
        ),
        (
            lig_kern_boundary_char_entrypoint,
            "(LIGTABLE (LABEL BOUNDARYCHAR) (KRN C r D 15.0))",
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(0),
                    right_char: 'r'.try_into().unwrap(),
                    operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                },],
                lig_kern_boundary_char_entrypoint: Some(0),
                ..Default::default()
            },
        ),
        (
            char_width,
            "(CHARACTER C r (CHARWD D 15.0))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        width: Number::UNITY * 15,
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_height,
            "(CHARACTER C r (CHARHT D 15.0))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        height: Number::UNITY * 15,
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_depth,
            "(CHARACTER C r (CHARDP D 15.0))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        depth: Number::UNITY * 15,
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_italic_correction,
            "(CHARACTER C r (CHARIC D 15.0))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        italic_correction: Number::UNITY * 15,
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
    );
}
