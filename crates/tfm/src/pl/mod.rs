//! The property list file format
//!
//! This module contains a work-in-progress parser for property list files.

use std::collections::HashMap;

use crate::{ligkern, Char, CharData, Header, NamedParam, Number, Params};
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

    /// Build a File from an AST.
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

    /// Convert a TFM file into a PL file.
    pub fn from_tfm_file(tfm_file: crate::Font) -> File {
        let mut char_data = HashMap::<Char, CharData>::new();
        let mut lig_kern_entrypoints = HashMap::<Char, usize>::new();
        let mut c = tfm_file.smallest_char_code;
        for info in tfm_file.char_infos.into_iter() {
            char_data.insert(
                c,
                CharData {
                    width: tfm_file
                        .widths
                        .get(info.width_index as usize)
                        .copied()
                        .unwrap_or_default(),
                    height: tfm_file
                        .heights
                        .get(info.height_index as usize)
                        .copied()
                        .unwrap_or_default(),
                    depth: tfm_file
                        .depths
                        .get(info.depth_index as usize)
                        .copied()
                        .unwrap_or_default(),
                    italic_correction: tfm_file
                        .italic_corrections
                        .get(info.italic_index as usize)
                        .copied()
                        .unwrap_or_default(),
                },
            );
            match info.tag {
                crate::CharTag::None => {}
                crate::CharTag::Ligature(u) => {
                    lig_kern_entrypoints.insert(c, u as usize);
                }
                crate::CharTag::List(_) => todo!(),
                crate::CharTag::Extension(_) => todo!(),
            }
            c = Char(c.0.checked_add(1).unwrap());
        }
        // TODO: consider having this logic when we deserialize TFM files?
        // Or should the TFM type be a low level representation of the file?
        let lig_kern_instructions: Vec<ligkern::lang::Instruction> = tfm_file
            .lig_kern_instructions
            .into_iter()
            .map(|mut i| {
                if let ligkern::lang::Operation::Kern(payload) = &mut i.operation {
                    // TODO: log a warning if the index is not in the kerns array as
                    // in TFtoPL.2014.76
                    *payload = tfm_file
                        .kern
                        .get(payload.0 as usize)
                        .copied()
                        .unwrap_or_default()
                }
                i
            })
            .collect();
        File {
            header: tfm_file.header,
            design_units: Number::UNITY,
            char_data,
            lig_kern_boundary_char: None,
            lig_kern_boundary_char_entrypoint: None,
            lig_kern_entrypoints,
            lig_kern_instructions,
            params: tfm_file.params,
        }
    }

    /// Lower a File to an AST.
    pub fn lower(&self) -> ast::Ast {
        let mut roots = vec![];

        // First output the header. This is TFtoPL.2014.48-57.
        if !self.header.font_family.is_empty() {
            let s = sanitize_string(&self.header.font_family);
            roots.push(ast::Root::Family(s.into()))
        }
        if let Some(face) = self.header.face {
            roots.push(ast::Root::Face(face.into()))
        }
        for (i, &u) in self.header.additional_data.iter().enumerate() {
            let i: u8 = i.try_into().unwrap();
            roots.push(ast::Root::Header((i, u).into()))
        }
        #[derive(Clone, Copy)]
        enum FontType {
            Vanilla,
            TexMathSy,
            TexMathEx,
        }
        let font_type = {
            let scheme = self.header.character_coding_scheme.to_uppercase();
            if scheme.starts_with("TEX MATH SY") {
                FontType::TexMathSy
            } else if scheme.starts_with("TEX MATH EX") {
                FontType::TexMathEx
            } else {
                FontType::Vanilla
            }
        };
        if !self.header.character_coding_scheme.is_empty() {
            let s = sanitize_string(&self.header.character_coding_scheme);
            roots.push(ast::Root::CodingScheme(s.into()))
        }
        roots.extend([
            ast::Root::DesignSize(self.header.design_size.into()),
            ast::Root::Comment(vec!["DESIGNSIZE IS IN POINTS".into()]),
            ast::Root::Comment(vec!["OTHER SIZES ARE MULTIPLES OF DESIGNSIZE".into()]),
            ast::Root::Checksum(self.header.checksum.into()),
        ]);
        if self.header.seven_bit_safe == Some(true) {
            roots.push(ast::Root::SevenBitSafeFlag(true.into()));
        }
        // Next the parameters. This is TFtoPL.2014.58-61
        let params: Vec<ast::FontDimension> = self
            .params
            .0
            .iter()
            .enumerate()
            .map(|(i, &param)| {
                let i: u8 = (i + 1).try_into().unwrap();
                // TFtoPL.2014.61
                // TODO: check that each parameter *except* SLANT is in the range [-16.0, 16.0] per TFtoPL.2014.60
                let named_param = match (i, font_type) {
                    (1, _) => NamedParam::Slant,
                    (2, _) => NamedParam::Space,
                    (3, _) => NamedParam::Stretch,
                    (4, _) => NamedParam::Shrink,
                    (5, _) => NamedParam::XHeight,
                    (6, _) => NamedParam::Quad,
                    (7, _) => NamedParam::ExtraSpace,
                    (8, FontType::TexMathSy) => NamedParam::Num1,
                    (9, FontType::TexMathSy) => NamedParam::Num2,
                    (10, FontType::TexMathSy) => NamedParam::Num3,
                    (11, FontType::TexMathSy) => NamedParam::Denom1,
                    (12, FontType::TexMathSy) => NamedParam::Denom2,
                    (13, FontType::TexMathSy) => NamedParam::Sup1,
                    (14, FontType::TexMathSy) => NamedParam::Sup2,
                    (15, FontType::TexMathSy) => NamedParam::Sup3,
                    (16, FontType::TexMathSy) => NamedParam::Sub1,
                    (17, FontType::TexMathSy) => NamedParam::Sub2,
                    (18, FontType::TexMathSy) => NamedParam::SupDrop,
                    (19, FontType::TexMathSy) => NamedParam::SubDrop,
                    (20, FontType::TexMathSy) => NamedParam::Delim1,
                    (21, FontType::TexMathSy) => NamedParam::Delim2,
                    (22, FontType::TexMathSy) => NamedParam::AxisHeight,
                    // TODO: finish this
                    _ => {
                        return ast::FontDimension::IndexedParam((i, param).into());
                    }
                };
                ast::FontDimension::NamedParam(named_param, param.into())
            })
            .collect();
        roots.push(ast::Root::FontDimension(((), params).into()));

        let ordered_chars = {
            let mut v: Vec<Char> = self.char_data.keys().copied().collect();
            v.sort();
            v
        };

        // Ligtable
        let mut l = Vec::<ast::LigTable>::new();
        let mut index_to_labels = HashMap::<usize, Vec<Char>>::new();
        for c in &ordered_chars {
            if let Some(entrypoint) = self.lig_kern_entrypoints.get(c) {
                index_to_labels.entry(*entrypoint).or_default().push(*c);
            }
        }
        let build_lig_kern_op =
            |instruction: &ligkern::lang::Instruction| match instruction.operation {
                ligkern::lang::Operation::Kern(kern) => {
                    ast::LigTable::Kern((instruction.right_char, kern).into())
                }
                ligkern::lang::Operation::Ligature {
                    char_to_insert,
                    post_lig_operation,
                } => ast::LigTable::Lig(
                    post_lig_operation,
                    (instruction.right_char, char_to_insert).into(),
                ),
            };
        for (index, instruction) in self.lig_kern_instructions.iter().enumerate() {
            for label in index_to_labels.get(&index).unwrap_or(&vec![]) {
                l.push(ast::LigTable::Label(
                    ast::LigTableLabel::Char(*label).into(),
                ));
            }
            l.push(build_lig_kern_op(instruction));
            match instruction.next_instruction {
                None => l.push(ast::LigTable::Stop(().into())),
                Some(0) => {}
                // TODO: potentially write a warning if i is too big like in TFtoPL.2014.74.
                Some(i) => l.push(ast::LigTable::Skip(i.into())),
            }
        }
        if !l.is_empty() {
            roots.push(ast::Root::LigTable(((), l).into()))
        }

        // Characters
        for c in &ordered_chars {
            let data = match self.char_data.get(c) {
                None => continue,
                Some(data) => data,
            };
            let mut v = vec![];
            if data.width != Number::ZERO {
                v.push(ast::Character::Width(data.width.into()));
            }
            if data.height != Number::ZERO {
                v.push(ast::Character::Height(data.height.into()));
            }
            if data.depth != Number::ZERO {
                v.push(ast::Character::Depth(data.depth.into()));
            }
            if data.italic_correction != Number::ZERO {
                v.push(ast::Character::ItalicCorrection(
                    data.italic_correction.into(),
                ));
            }

            if let Some(index) = self.lig_kern_entrypoints.get(c) {
                let mut l = vec![];
                let mut index = *index;
                loop {
                    let instruction = match self.lig_kern_instructions.get(index) {
                        // TODO: potentially write a warning if the index is too big like in TFtoPL.2014.74.
                        None => break,
                        Some(instruction) => instruction,
                    };
                    l.push(build_lig_kern_op(instruction));
                    index = match instruction.next_instruction {
                        None => break,
                        Some(inc) => index + 1 + (inc as usize),
                    };
                }
                v.push(ast::Character::Comment(
                    l.into_iter()
                        .map(|n| {
                            // TODO: need to wire in the char display format
                            cst::BalancedElem::Vec(n.into_balanced_elements(Default::default()))
                        })
                        .collect(),
                ));
            }
            roots.push(ast::Root::Character((*c, v).into()));
        }

        ast::Ast(roots)
    }

    /// Display this file.
    ///
    /// This function returns a helper type that implements the [std::fmt::Display]
    /// trait and can be used in `print!` and similar macros.
    pub fn display(&self, indent: usize, char_display_format: CharDisplayFormat) -> Display {
        Display {
            pl_file: self,
            indent,
            char_display_format,
        }
    }
}

fn sanitize_string(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '(' | ')' => '/', // todo: log a warning
            ' '..='~' => c.to_ascii_uppercase(),
            _ => '?', // todo: log a warning
        })
        .collect()
}

/// Helper type for displaying files.
///
/// Use the [File::display] method to construct this type.
pub struct Display<'a> {
    pl_file: &'a File,
    indent: usize,
    char_display_format: CharDisplayFormat,
}

impl<'a> std::fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ast = self.pl_file.lower();
        let cst = ast.lower(self.char_display_format);
        cst.display(self.indent).fmt(f)
    }
}

/// Specification for how to display characters when printing property list data.
#[derive(Default, Debug, Clone, Copy)]
pub enum CharDisplayFormat {
    /// Letters and numbers are output in PL ASCII format (e.g. `C A`), and
    /// other characters are output in octal (e.g. `O 14`).
    #[default]
    Default,
    /// Visible ASCII characters except ( and ) are output in PL ASCII format (e.g. `C A`), and
    /// other characters are output in octal (e.g. `O 14`).
    Ascii,
    /// All characters are output in octal (e.g. `O 14`)
    Octal,
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
