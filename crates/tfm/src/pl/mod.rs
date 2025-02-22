/*!
The property list (.pl) file format.

|                                     | from .pl source code              | to .pl source code    | from lower level         | to lower level
|-------------------------------------|-----------------------------------|-----------------------|--------------------------|----
| fully parsed .pl file ([`File`])    | [`File::from_pl_source_code`]     | [`File::display`]     | [`File::from_ast`]       | [`File::lower`]
| abstract syntax tree ([`ast::Ast`]) | [`ast::Ast::from_pl_source_code`] | `ast::Ast::display` (TODO) | [`ast::Ast::from_cst`]   | [`ast::Ast::lower`]
| concrete syntax tree ([`cst::Cst`]) | [`cst::Cst::from_pl_source_code`] | [`cst::Cst::display`] | N/A | N/A

*/

use std::collections::{BTreeMap, HashMap};

use crate::{
    format::{self, ExtensibleRecipe},
    ligkern,
    pl::ast::{DesignSize, ParameterNumber},
    Char, Header, NamedParameter, NextLargerProgram, NextLargerProgramWarning, FixWord,
};

pub mod ast;
pub mod cst;
mod error;
pub use error::*;

/// Maximum number of lig/kern instructions in a property list file.
///
/// This limit is defined without explanation in PLtoTF.2014.3.
/// Here's an explanation for the specific value.
///
/// First, in a TFM file the sub-file sizes, including the number of lig/kern instructions `nl`,
///     are 8-bit integers in the range `[0, i16::MAX]`.
/// (I don't know why the range is restricted like this, maybe for portability.)
/// Thus the maximum number of lig/kern instructions is less than or equal to `i16::MAX`.
///
/// Second, after a PL file is read some additional instructions may need to be prepended to support
///     LABEL entries with an index larger than u8::MAX.
/// This is the "embarrassing problem" described in PLtoTF.2014.138.
/// In TFM files the index for the starting lig/kern instruction for a character is a u8.
/// To support higher indices, a special lig/kern instruction is prepended to the list of instructions.
/// This instruction specifies where to actually start.
/// The payload for this instruction supports 16-bit integers.
///
/// There are 257 possible characters (the usual 256 plus the boundary character),
///     and thus we may need to insert up to 257 additional instructions.
/// After this we still need to be under the `i16::MAX limit`.
/// So the limit is `i16::MAX - 257`.
pub const MAX_LIG_KERN_INSTRUCTIONS: u16 = (i16::MAX as u16) - 257;

/// Data about one character in a .pl file.
#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct CharDimensions {
    pub width: Option<FixWord>,
    pub height: Option<FixWord>,
    pub depth: Option<FixWord>,
    pub italic_correction: Option<FixWord>,
}

/// Tag of a character in a .pl file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CharTag {
    Ligature(u16),
    List(Char),
    Extension(format::ExtensibleRecipe),
}

impl CharTag {
    pub fn ligature(&self) -> Option<u16> {
        match self {
            CharTag::Ligature(l) => Some(*l),
            _ => None,
        }
    }
    pub fn list(&self) -> Option<Char> {
        match self {
            CharTag::List(c) => Some(*c),
            _ => None,
        }
    }
    pub fn extension(&self) -> Option<ExtensibleRecipe> {
        match self {
            CharTag::Extension(u) => Some(u.clone()),
            CharTag::Ligature(_) | CharTag::List(_) => None,
        }
    }
}

/// Complete contents of a property list (.pl) file.
#[derive(PartialEq, Eq, Debug)]
pub struct File {
    pub header: Header,
    pub design_units: FixWord,
    pub char_dimens: BTreeMap<Char, CharDimensions>,
    pub char_tags: BTreeMap<Char, CharTag>,
    /// Tags that have been unset, but whose discriminant is still written to a .tfm file by PLtoTF.
    pub unset_char_tags: BTreeMap<Char, u8>,
    pub lig_kern_program: ligkern::lang::Program,
    pub params: Vec<FixWord>,

    /// Additional widths that appear in the plst file but not appear in the fully parsed file.
    ///
    /// This can happen due to the following plst listing:
    /// ```txt
    /// (CHARACTER C X (CHARWD D 8.0))
    /// (CHARACTER C X (CHARWD D 9.0))
    /// ```
    /// In this case the width `8.0` is not in the fully parsed file because it is overwritten
    ///     by `9.0`.
    /// However pltotf still writes the `8.0` width to the .tfm file.
    pub additional_widths: Vec<FixWord>,
    /// Additional heights; similar to additional widths.
    pub additional_heights: Vec<FixWord>,
    /// Additional depths; similar to additional widths.
    pub additional_depths: Vec<FixWord>,
    /// Additional italic corrections; similar to additional widths.
    pub additional_italics: Vec<FixWord>,
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Header::pl_default(),
            design_units: FixWord::ONE,
            char_dimens: Default::default(),
            char_tags: Default::default(),
            unset_char_tags: Default::default(),
            lig_kern_program: Default::default(),
            params: Default::default(),
            additional_widths: vec![],
            additional_heights: vec![],
            additional_depths: vec![],
            additional_italics: vec![],
        }
    }
}

impl File {
    /// Build a File from PL source code.
    pub fn from_pl_source_code(source: &str) -> (File, Vec<ParseWarning>) {
        let (ast, mut errors) = ast::Ast::from_pl_source_code(source);
        let file = File::from_ast(ast, &mut errors);
        (file, errors)
    }

    /// Return a map from characters to the lig/kern entrypoint for that character.
    pub fn lig_kern_entrypoints(&self, include_orphans: bool) -> HashMap<Char, u16> {
        self.char_tags
            .iter()
            .filter(|(c, _)| self.char_dimens.contains_key(c) || include_orphans)
            .filter_map(|d| match d.1 {
                CharTag::Ligature(l) => Some((*d.0, *l)),
                _ => None,
            })
            .collect()
    }

    /// Clear all lig/kern data from the file.
    pub fn clear_lig_kern_data(&mut self) {
        // PLtoTF.2014.125
        self.char_tags = self
            .char_tags
            .iter()
            .filter_map(|(c, tag)| match tag {
                CharTag::Ligature(_) => None,
                _ => Some((*c, tag.clone())),
            })
            .collect();
        self.lig_kern_program = Default::default();
    }

    /// Build a File from an AST.
    pub fn from_ast(ast: ast::Ast, errors: &mut Vec<error::ParseWarning>) -> File {
        let mut file: File = Default::default();
        let mut lig_kern_precedes = false;

        let mut next_larger_span = HashMap::<Char, std::ops::Range<usize>>::new();

        for node in ast.0 {
            match node {
                ast::Root::Checksum(v) => {
                    file.header.checksum = Some(v.data);
                }
                ast::Root::DesignSize(v) => {
                    if let DesignSize::Valid(design_size) = v.data {
                        file.header.design_size = design_size;
                    }
                }
                ast::Root::DesignUnits(v) => {
                    file.design_units = v.data;
                }
                ast::Root::CodingScheme(v) => {
                    file.header.character_coding_scheme = Some(v.data);
                }
                ast::Root::Family(v) => {
                    file.header.font_family = Some(v.data);
                }
                ast::Root::Face(v) => {
                    file.header.face = Some(v.data);
                }
                ast::Root::SevenBitSafeFlag(v) => {
                    file.header.seven_bit_safe = Some(v.data);
                }
                ast::Root::Header(v) => match v.left.0.checked_sub(18) {
                    None => errors.push(ParseWarning {
                        span: v.left_span.clone(),
                        knuth_pltotf_offset: Some(v.left_span.end),
                        kind: ParseWarningKind::HeaderIndexIsTooSmall,
                    }),
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
                        let (number, value) = match node {
                            ast::FontDimension::NamedParam(named_param, v) => {
                                (named_param.number() as usize, v.data)
                            }
                            ast::FontDimension::IndexedParam(v) => (v.left.0 as usize, v.right),
                            ast::FontDimension::Comment(_) => continue,
                        };
                        let index = number - 1;
                        if file.params.len() < number {
                            file.params.resize(number, Default::default());
                        }
                        file.params[index] = value;
                    }
                }
                ast::Root::LigTable(b) => {
                    for node in b.children {
                        let mut insert_lig_kern_instruction = |instruction, span| {
                            if file.lig_kern_program.instructions.len()
                                < MAX_LIG_KERN_INSTRUCTIONS as usize
                            {
                                file.lig_kern_program.instructions.push(instruction);
                            } else {
                                // TODO: add a test for this case
                                errors.push(error::ParseWarning {
                                    span,
                                    knuth_pltotf_offset: None,
                                    kind: ParseWarningKind::LigTableIsTooBig,
                                });
                            }
                        };
                        match node {
                            ast::LigTable::Label(v) => {
                                let u: u16 = file.lig_kern_program.instructions.len().try_into().expect("lig_kern_instructions.len()<= MAX_LIG_KERN_INSTRUCTIONS which is a u16");
                                match v.data {
                                    ast::LigTableLabel::Char(c) => {
                                        // TODO: error if the tag is already set
                                        file.char_tags.insert(c, CharTag::Ligature(u));
                                    }
                                    ast::LigTableLabel::BoundaryChar => {
                                        file.lig_kern_program.left_boundary_char_entrypoint =
                                            Some(u);
                                    }
                                }
                                lig_kern_precedes = false;
                            }
                            ast::LigTable::Lig(post_lig_operation, v) => {
                                insert_lig_kern_instruction(
                                    ligkern::lang::Instruction {
                                        next_instruction: Some(0),
                                        right_char: v.left,
                                        operation: ligkern::lang::Operation::Ligature {
                                            char_to_insert: v.right,
                                            post_lig_operation,
                                            post_lig_tag_invalid: false,
                                        },
                                        // TODO: should the span of the entire LIG node not just some of the data
                                    },
                                    v.left_span,
                                );
                                lig_kern_precedes = true;
                            }
                            ast::LigTable::Kern(v) => {
                                insert_lig_kern_instruction(
                                    ligkern::lang::Instruction {
                                        next_instruction: Some(0),
                                        right_char: v.left,
                                        operation: ligkern::lang::Operation::Kern(v.right),
                                        // TODO: should the span of the entire KRN node
                                    },
                                    v.left_span,
                                );
                                lig_kern_precedes = true;
                            }
                            ast::LigTable::Stop(_) => {
                                if lig_kern_precedes {
                                    file.lig_kern_program
                                        .instructions
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
                                    file.lig_kern_program
                                        .instructions
                                        .last_mut()
                                        .unwrap()
                                        .next_instruction = Some(v.data.0);
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
                    file.lig_kern_program.right_boundary_char = Some(v.data);
                }
                ast::Root::Character(b) => {
                    let char_dimens = file.char_dimens.entry(b.data).or_default();
                    for node in b.children {
                        match node {
                            ast::Character::Width(v) => {
                                if let Some(additional_width) =
                                    char_dimens.width.replace(v.data.unwrap_or(FixWord::ZERO))
                                {
                                    file.additional_widths.push(additional_width);
                                }
                            }
                            ast::Character::Height(v) => {
                                if let Some(additional_height) = char_dimens.height.replace(v.data)
                                {
                                    file.additional_heights.push(additional_height);
                                }
                            }
                            ast::Character::Depth(v) => {
                                if let Some(additional_depth) = char_dimens.depth.replace(v.data) {
                                    file.additional_depths.push(additional_depth);
                                }
                            }
                            ast::Character::ItalicCorrection(v) => {
                                if let Some(additional_italic) =
                                    char_dimens.italic_correction.replace(v.data)
                                {
                                    file.additional_italics.push(additional_italic);
                                }
                            }
                            ast::Character::NextLarger(c) => {
                                // TODO: warning if tag != CharTag::None
                                file.char_tags.insert(b.data, CharTag::List(c.data));
                                next_larger_span.insert(b.data, c.data_span);
                            }
                            ast::Character::ExtensibleCharacter(e) => {
                                let mut recipe: ExtensibleRecipe = Default::default();
                                for node in e.children {
                                    match node {
                                        ast::ExtensibleCharacter::Top(v) => {
                                            recipe.top = Some(v.data)
                                        }
                                        ast::ExtensibleCharacter::Middle(v) => {
                                            recipe.middle = Some(v.data)
                                        }
                                        ast::ExtensibleCharacter::Bottom(v) => {
                                            recipe.bottom = Some(v.data)
                                        }
                                        ast::ExtensibleCharacter::Replicated(v) => {
                                            recipe.rep = v.data
                                        }
                                        ast::ExtensibleCharacter::Comment(_) => {}
                                    }
                                }
                                // TODO: warning if tag != CharTag::None
                                file.char_tags.insert(b.data, CharTag::Extension(recipe));
                            }
                            ast::Character::Comment(_) => {}
                        }
                    }
                }
                ast::Root::Comment(_) => {}
            }
        }

        // In pltotf the file is parsed in a single pass, whereas here we parse it in at least 2
        // passes (CST, then AST). As a result some of the warnings will be out of order versus
        // pltotf - e.g., all CST level warnings will come first, whereas in pltotf the warnings are
        // interleaved depending on where they appear in the file. This is easy to fix.
        errors.sort_by_key(|w| {
            w.knuth_pltotf_offset
                .expect("all warnings generated so far have an offset populated")
        });

        // PLtoTF.2014.116
        if let Some(final_instruction) = file.lig_kern_program.instructions.last_mut() {
            if final_instruction.next_instruction == Some(0) {
                final_instruction.next_instruction = None;
            }
        }

        // Validate and fix the lig kern program.
        let lig_kern_seven_bit_safe = {
            for err in crate::ligkern::CompiledProgram::compile(
                &file.lig_kern_program,
                &[],
                file.lig_kern_entrypoints(true), // todo include orphans?
            )
            .1
            {
                errors.push(ParseWarning {
                    span: 0..0, // todo
                    knuth_pltotf_offset: None,
                    kind: ParseWarningKind::CycleInLigKernProgram(err),
                });
                file.clear_lig_kern_data();
            }
            file.lig_kern_program
                .is_seven_bit_safe(file.lig_kern_entrypoints(false))
        };

        // Validate and fix next larger tags
        let next_larger_seven_bit_safe = {
            let (program, next_larger_warnings) = NextLargerProgram::new(
                file.char_tags
                    .iter()
                    .filter_map(|(c, t)| t.list().map(|next_larger| (*c, next_larger))),
                |c| file.char_dimens.contains_key(&c),
                false,
            );
            for warning in next_larger_warnings {
                match &warning {
                    NextLargerProgramWarning::NonExistentCharacter {
                        original: _,
                        next_larger,
                    } => {
                        file.char_dimens.insert(
                            *next_larger,
                            CharDimensions {
                                width: Some(FixWord::ZERO),
                                ..Default::default()
                            },
                        );
                    }
                    NextLargerProgramWarning::InfiniteLoop { original, .. } => {
                        let discriminant = file
                            .char_tags
                            .remove(original)
                            .and_then(|t| t.list())
                            .expect("this char has a NEXTLARGER SPEC");
                        file.unset_char_tags.insert(*original, discriminant.0);
                    }
                }
                let span = next_larger_span
                    .get(&warning.bad_char())
                    .cloned()
                    .expect("every char with a next larger tag had a NEXTLARGER AST node");
                errors.push(ParseWarning {
                    span,
                    knuth_pltotf_offset: None,
                    kind: ParseWarningKind::CycleInNextLargerProgram(warning),
                });
            }
            program.is_seven_bit_safe()
        };

        // PLtoTF.2014.112
        let mut extensible_seven_bit_safe = true;
        file.char_tags
            .iter()
            .filter_map(|(c, t)| t.extension().map(|t| (c, t)))
            .for_each(|(c, e)| {
                if c.is_seven_bit() && !e.is_seven_bit() {
                    extensible_seven_bit_safe = false;
                }
                for c in e.chars() {
                    if let std::collections::btree_map::Entry::Vacant(v) = file.char_dimens.entry(c)
                    {
                        v.insert(Default::default());
                        // todo warning
                    };
                }
            });

        let seven_bit_safe =
            lig_kern_seven_bit_safe && next_larger_seven_bit_safe && extensible_seven_bit_safe;
        if file.header.seven_bit_safe == Some(true) && !seven_bit_safe {
            errors.push(ParseWarning {
                span: 0..0, //todo,
                knuth_pltotf_offset: None,
                kind: ParseWarningKind::NotReallySevenBitSafe,
            });
        }
        file.header.seven_bit_safe = Some(seven_bit_safe);

        file
    }
}

impl From<crate::format::File> for File {
    fn from(tfm_file: crate::format::File) -> Self {
        let char_dimens: BTreeMap<Char, CharDimensions> = tfm_file
            .char_dimens
            .iter()
            .map(|(c, info)| {
                (
                    *c,
                    CharDimensions {
                        width: match info.width_index {
                            format::WidthIndex::Invalid => None,
                            format::WidthIndex::Valid(n) => {
                                tfm_file.widths.get(n.get() as usize).copied()
                            }
                        },
                        height: if info.height_index == 0 {
                            None
                        } else {
                            tfm_file.heights.get(info.height_index as usize).copied()
                        },
                        depth: if info.depth_index == 0 {
                            None
                        } else {
                            tfm_file.depths.get(info.depth_index as usize).copied()
                        },
                        italic_correction: if info.italic_index == 0 {
                            None
                        } else {
                            tfm_file
                                .italic_corrections
                                .get(info.italic_index as usize)
                                .copied()
                        },
                    },
                )
            })
            .collect();

        let mut lig_kern_program = tfm_file.lig_kern_program;
        lig_kern_program.pack_kerns(&tfm_file.kerns);
        let lig_kern_entrypoints: HashMap<Char, u16> = tfm_file
            .char_tags
            .iter()
            .filter_map(|(c, t)| {
                t.ligature()
                    .and_then(|l| lig_kern_program.unpack_entrypoint(l).ok())
                    .map(|e| (*c, e))
            })
            .collect();

        let char_tags = tfm_file
            .char_tags
            .into_iter()
            .filter_map(|(c, tag)| match tag {
                format::CharTag::Ligature(_) => lig_kern_entrypoints
                    .get(&c)
                    .map(|e| (c, CharTag::Ligature(*e))),
                format::CharTag::List(l) => Some((c, CharTag::List(l))),
                // If the extension index is invalid we drop the tag.
                format::CharTag::Extension(i) => tfm_file
                    .extensible_chars
                    .get(i as usize)
                    .cloned()
                    .map(|t| (c, CharTag::Extension(t))),
            })
            .collect();

        File {
            header: tfm_file.header,
            design_units: FixWord::ONE,
            char_dimens,
            char_tags,
            unset_char_tags: Default::default(),
            lig_kern_program,
            params: tfm_file.params,
            additional_widths: vec![],
            additional_heights: vec![],
            additional_depths: vec![],
            additional_italics: vec![],
        }
    }
}

impl File {
    /// Lower a File to an AST.
    pub fn lower(&self, char_display_format: CharDisplayFormat) -> ast::Ast {
        let mut roots = vec![];

        // First output the header. This is TFtoPL.2014.48-57.
        if let Some(font_family) = &self.header.font_family {
            roots.push(ast::Root::Family(font_family.clone().into()))
        }
        if let Some(face) = self.header.face {
            roots.push(ast::Root::Face(face.into()))
        }
        for (i, &u) in self.header.additional_data.iter().enumerate() {
            let i: u8 = i.try_into().unwrap();
            let i = i.checked_add(18).unwrap(); // TODO: gotta be a warning here
            roots.push(ast::Root::Header((ast::DecimalU8(i), u).into()))
        }
        #[derive(Clone, Copy)]
        enum FontType {
            Vanilla,
            TexMathSy,
            TexMathEx,
        }
        let font_type = {
            let scheme = match &self.header.character_coding_scheme {
                None => String::new(),
                Some(scheme) => scheme.to_uppercase(),
            };
            if scheme.starts_with("TEX MATH SY") {
                FontType::TexMathSy
            } else if scheme.starts_with("TEX MATH EX") {
                FontType::TexMathEx
            } else {
                FontType::Vanilla
            }
        };
        if let Some(scheme) = &self.header.character_coding_scheme {
            roots.push(ast::Root::CodingScheme(scheme.clone().into()));
        }
        roots.extend([
            ast::Root::DesignSize(
                if self.header.design_size_valid {
                    DesignSize::Valid(self.header.design_size)
                } else {
                    DesignSize::Invalid
                }
                .into(),
            ),
            ast::Root::Comment("DESIGNSIZE IS IN POINTS".into()),
            ast::Root::Comment("OTHER SIZES ARE MULTIPLES OF DESIGNSIZE".into()),
            ast::Root::Checksum(self.header.checksum.unwrap_or_default().into()),
        ]);
        if self.header.seven_bit_safe == Some(true) {
            roots.push(ast::Root::SevenBitSafeFlag(true.into()));
        }
        // Next the parameters. This is TFtoPL.2014.58-61
        let params: Vec<ast::FontDimension> = self
            .params
            .iter()
            .enumerate()
            .filter_map(|(i, &param)| {
                let i: u16 = match (i + 1).try_into() {
                    Ok(i) => i,
                    Err(_) => return None,
                };
                // TFtoPL.2014.61
                let named_param = match (i, font_type) {
                    (1, _) => NamedParameter::Slant,
                    (2, _) => NamedParameter::Space,
                    (3, _) => NamedParameter::Stretch,
                    (4, _) => NamedParameter::Shrink,
                    (5, _) => NamedParameter::XHeight,
                    (6, _) => NamedParameter::Quad,
                    (7, _) => NamedParameter::ExtraSpace,
                    (8, FontType::TexMathSy) => NamedParameter::Num1,
                    (9, FontType::TexMathSy) => NamedParameter::Num2,
                    (10, FontType::TexMathSy) => NamedParameter::Num3,
                    (11, FontType::TexMathSy) => NamedParameter::Denom1,
                    (12, FontType::TexMathSy) => NamedParameter::Denom2,
                    (13, FontType::TexMathSy) => NamedParameter::Sup1,
                    (14, FontType::TexMathSy) => NamedParameter::Sup2,
                    (15, FontType::TexMathSy) => NamedParameter::Sup3,
                    (16, FontType::TexMathSy) => NamedParameter::Sub1,
                    (17, FontType::TexMathSy) => NamedParameter::Sub2,
                    (18, FontType::TexMathSy) => NamedParameter::SupDrop,
                    (19, FontType::TexMathSy) => NamedParameter::SubDrop,
                    (20, FontType::TexMathSy) => NamedParameter::Delim1,
                    (21, FontType::TexMathSy) => NamedParameter::Delim2,
                    (22, FontType::TexMathSy) => NamedParameter::AxisHeight,
                    (8, FontType::TexMathEx) => NamedParameter::DefaultRuleThickness,
                    (9, FontType::TexMathEx) => NamedParameter::BigOpSpacing1,
                    (10, FontType::TexMathEx) => NamedParameter::BigOpSpacing2,
                    (11, FontType::TexMathEx) => NamedParameter::BigOpSpacing3,
                    (12, FontType::TexMathEx) => NamedParameter::BigOpSpacing4,
                    (13, FontType::TexMathEx) => NamedParameter::BigOpSpacing5,
                    _ => {
                        let parameter_number = ParameterNumber(i);
                        return Some(ast::FontDimension::IndexedParam(
                            (parameter_number, param).into(),
                        ));
                    }
                };
                Some(ast::FontDimension::NamedParam(named_param, param.into()))
            })
            .collect();
        if !params.is_empty() {
            roots.push(ast::Root::FontDimension(((), params).into()));
        }

        // Ligtable
        if let Some(boundary_char) = self.lig_kern_program.right_boundary_char {
            roots.push(ast::Root::BoundaryChar(boundary_char.into()));
        }
        let index_to_labels = {
            let mut m = HashMap::<usize, Vec<Char>>::new();
            for (c, tag) in &self.char_tags {
                if let CharTag::Ligature(index) = tag {
                    m.entry(*index as usize).or_default().push(*c);
                }
            }
            m.values_mut().for_each(|v| v.sort());
            m
        };
        let mut l = Vec::<ast::LigTable>::new();
        let build_lig_kern_op = |instruction: &ligkern::lang::Instruction| match instruction
            .operation
        {
            ligkern::lang::Operation::Kern(kern) => {
                Some(ast::LigTable::Kern((instruction.right_char, kern).into()))
            }
            ligkern::lang::Operation::KernAtIndex(_) => {
                panic!("tfm::pl::File lig/kern programs cannot contain `KernAtIndex` operations. Use a `Kern` operation instead.");
            }
            ligkern::lang::Operation::EntrypointRedirect(_, _) => None,
            ligkern::lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
                post_lig_tag_invalid: _,
            } => Some(ast::LigTable::Lig(
                post_lig_operation,
                (instruction.right_char, char_to_insert).into(),
            )),
        };

        // When we fixed the (LIGTABLE (LABEL BOUNDARYCHAR) bug, number of failures went from 26652 -> 12004
        let mut unreachable_elems: Option<String> = None;
        let flush_unreachable_elems = |elems: &mut Option<String>, l: &mut Vec<ast::LigTable>| {
            if let Some(elems) = elems.take() {
                l.push(ast::LigTable::Comment(elems))
            }
        };
        for (index, (reachable, instruction)) in self
            .lig_kern_program
            .reachable_iter(
                self.char_tags
                    .iter()
                    .filter_map(|(c, t)| t.ligature().map(|l| (*c, l))),
            )
            .zip(&self.lig_kern_program.instructions)
            .enumerate()
        {
            match reachable {
                ligkern::lang::ReachableIterItem::Reachable { adjusted_skip } => {
                    flush_unreachable_elems(&mut unreachable_elems, &mut l);
                    if let Some(e) = self.lig_kern_program.left_boundary_char_entrypoint {
                        if e as usize == index {
                            l.push(ast::LigTable::Label(
                                ast::LigTableLabel::BoundaryChar.into(),
                            ));
                        }
                    }
                    for label in index_to_labels.get(&index).unwrap_or(&vec![]) {
                        l.push(ast::LigTable::Label(
                            ast::LigTableLabel::Char(*label).into(),
                        ));
                    }
                    if let Some(op) = build_lig_kern_op(instruction) {
                        l.push(op);
                    }
                    match adjusted_skip {
                        // Note in the first branch here we may push Skip(0)
                        Some(i) => l.push(ast::LigTable::Skip(ast::DecimalU8(i).into())),
                        None => {
                            match instruction.next_instruction {
                                None => l.push(ast::LigTable::Stop(().into())),
                                Some(0) => {}
                                // TODO: potentially write a warning if i is too big like in TFtoPL.2014.74.
                                Some(i) => l.push(ast::LigTable::Skip(ast::DecimalU8(i).into())),
                            }
                        }
                    }
                }
                ligkern::lang::ReachableIterItem::Unreachable => {
                    let unreachable_elems = unreachable_elems.get_or_insert_with(|| {
                        // TODO: shouldn't have to add a starting space here!
                        // Need to fix Node::write in the CST code
                        " THIS PART OF THE PROGRAM IS NEVER USED!\n".to_string()
                    });
                    if let Some(op) = build_lig_kern_op(instruction) {
                        unreachable_elems
                            .push_str(&format!["{}", op.lower(char_display_format).display(6, 3)]);
                    }
                }
                ligkern::lang::ReachableIterItem::Passthrough => {}
            }
        }
        flush_unreachable_elems(&mut unreachable_elems, &mut l);
        if !self.lig_kern_program.instructions.is_empty() {
            roots.push(ast::Root::LigTable(((), l).into()))
        }

        // Characters
        let ordered_chars = {
            let mut v: Vec<Char> = self.char_dimens.keys().copied().collect();
            v.sort();
            v
        };
        for c in &ordered_chars {
            let data = match self.char_dimens.get(c) {
                None => continue, // TODO: this can't happen. Fix
                Some(data) => data,
            };
            let mut v = vec![];
            match data.width {
                None => {
                    v.push(ast::Character::Width(None.into()));
                }
                Some(width) => {
                    v.push(ast::Character::Width(Some(width).into()));
                }
            };
            if let Some(height) = data.height {
                v.push(ast::Character::Height(height.into()));
            }
            if let Some(depth) = data.depth {
                v.push(ast::Character::Depth(depth.into()));
            }
            if let Some(italic_correction) = data.italic_correction {
                v.push(ast::Character::ItalicCorrection(italic_correction.into()));
            }

            match self.char_tags.get(c) {
                None => {}
                Some(CharTag::Ligature(entrypoint)) => {
                    let l: Vec<cst::Node> = self
                        .lig_kern_program
                        .instructions_for_entrypoint(*entrypoint)
                        .map(|(_, b)| b)
                        .filter_map(build_lig_kern_op)
                        .map(|n| n.lower(char_display_format))
                        .collect();
                    if l.is_empty() {
                        v.push(ast::Character::Comment("\n".into()));
                    } else {
                        v.push(ast::Character::Comment(format![
                            "\n{}",
                            cst::Cst(l).display(6, 3)
                        ]));
                    }
                }
                Some(CharTag::List(c)) => v.push(ast::Character::NextLarger((*c).into())),
                Some(CharTag::Extension(recipe)) => {
                    // TFtoPL.2014.86
                    let mut r = vec![];
                    if let Some(top) = recipe.top {
                        r.push(ast::ExtensibleCharacter::Top(top.into()));
                    }
                    if let Some(middle) = recipe.middle {
                        r.push(ast::ExtensibleCharacter::Middle(middle.into()));
                    }
                    if let Some(bottom) = recipe.bottom {
                        r.push(ast::ExtensibleCharacter::Bottom(bottom.into()));
                    }
                    let rep = if self.char_dimens.contains_key(&recipe.rep) {
                        recipe.rep
                    } else {
                        *c
                    };
                    r.push(ast::ExtensibleCharacter::Replicated(rep.into()));
                    v.push(ast::Character::ExtensibleCharacter(((), r).into()))
                }
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
        let ast = self.pl_file.lower(self.char_display_format);
        let cst = ast.lower(self.char_display_format);
        let d = cst.display(0, self.indent);
        d.fmt(f)
    }
}

/// Specification for how to display characters when printing property list data.
#[derive(Default, Debug, Clone, Copy)]
pub enum CharDisplayFormat {
    /// Letters and numbers are output in PL ASCII format (e.g. `C A`), and
    /// other characters are output in octal (e.g. `O 14`).
    /// TODO: rename this variant to AlphanumericAscii or something like that.
    #[default]
    Default,
    /// Visible ASCII characters except ( and ) are output in PL ASCII format (e.g. `C A`), and
    /// other characters are output in octal (e.g. `O 14`).
    Ascii,
    /// All characters are output in octal (e.g. `O 14`)
    Octal,
}

/// Iterator over the characters in the strings but with canonical Unix line endings.
struct Chars<'a> {
    s: &'a str,
}

impl<'a> Chars<'a> {
    fn new(s: &'a str) -> Self {
        Self { s }
    }
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let mut iter = self.s.chars();
        match iter.next() {
            Some(mut c) => {
                if c == '\r' {
                    let mut skipped = 1;
                    loop {
                        match iter.next() {
                            Some('\r') => {
                                skipped += 1;
                                continue;
                            }
                            Some('\n') => {
                                c = '\n';
                                self.s = &self.s[skipped..];
                                break;
                            }
                            _ => break,
                        }
                    }
                }
                self.s = &self.s[c.len_utf8()..];
                Some(c)
            }
            None => None,
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.s.len()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{format::WidthIndex, Face};

    fn run_from_pl_source_code_test(source: &str, mut want: File) {
        want.header.seven_bit_safe = Some(true);
        let (got, errors) = File::from_pl_source_code(source);
        assert_eq!(errors, vec![]);
        assert_eq!(got, want);
    }

    macro_rules! from_pl_source_code_tests {
        ( $( ($name: ident, $source: expr, $want: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let source = $source;
                    let want = $want;
                    run_from_pl_source_code_test(source, want);
                }
            )+
        };
    }

    from_pl_source_code_tests!(
        (
            checksum,
            "(CHECKSUM H 7)",
            File {
                header: Header {
                    checksum: Some(0x7),
                    ..Header::pl_default()
                },
                ..Default::default()
            },
        ),
        (
            checksum_0,
            "(CHECKSUM O 0)",
            File {
                header: Header {
                    checksum: Some(0),
                    ..Header::pl_default()
                },
                ..Default::default()
            },
        ),
        (
            design_size,
            "(DESIGNSIZE R 3.0)",
            File {
                header: Header {
                    design_size: (FixWord::ONE * 3).into(),
                    ..Header::pl_default()
                },
                ..Default::default()
            },
        ),
        (
            design_units,
            "(DESIGNUNITS R 2.0)",
            File {
                design_units: FixWord::ONE * 2,
                ..Default::default()
            },
        ),
        (
            coding_scheme,
            "(CODINGSCHEME Hola Mundo)",
            File {
                header: Header {
                    character_coding_scheme: Some("Hola Mundo".into()),
                    ..Header::pl_default()
                },
                ..Default::default()
            },
        ),
        (
            font_family,
            "(FAMILY Hello World)",
            File {
                header: Header {
                    font_family: Some("Hello World".into()),
                    ..Header::pl_default()
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
                    ..Header::pl_default()
                },
                ..Default::default()
            },
        ),
        (
            seven_bit_safe_flag,
            "(SEVENBITSAFEFLAG FALSE)",
            File {
                header: Header {
                    seven_bit_safe: Some(false),
                    ..Header::pl_default()
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
                    ..Header::pl_default()
                },
                ..Default::default()
            },
        ),
        (
            boundary_char,
            "(BOUNDARYCHAR C a)",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![],
                    right_boundary_char: Some('a'.try_into().unwrap()),
                    left_boundary_char_entrypoint: None,
                    passthrough: Default::default(),
                },
                ..Default::default()
            },
        ),
        (
            named_param,
            "(FONTDIMEN (STRETCH D 13.0))",
            File {
                params: vec![FixWord::ZERO, FixWord::ZERO, FixWord::ONE * 13],
                ..Default::default()
            },
        ),
        (
            indexed_param,
            "(FONTDIMEN (PARAMETER D 2 D 15.0))",
            File {
                params: vec![FixWord::ZERO, FixWord::ONE * 15],
                ..Default::default()
            },
        ),
        (
            kern,
            "(LIGTABLE (KRN C r D 15.0))",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![ligkern::lang::Instruction {
                        next_instruction: None,
                        right_char: 'r'.try_into().unwrap(),
                        operation: ligkern::lang::Operation::Kern(FixWord::ONE * 15),
                    },],
                    right_boundary_char: None,
                    left_boundary_char_entrypoint: None,
                    passthrough: Default::default(),
                },
                ..Default::default()
            },
        ),
        (
            kern_with_stop,
            "(LIGTABLE (KRN C r D 15.0) (STOP) (KRN C t D 15.0))",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![
                        ligkern::lang::Instruction {
                            next_instruction: None,
                            right_char: 'r'.try_into().unwrap(),
                            operation: ligkern::lang::Operation::Kern(FixWord::ONE * 15),
                        },
                        ligkern::lang::Instruction {
                            next_instruction: None,
                            right_char: 't'.try_into().unwrap(),
                            operation: ligkern::lang::Operation::Kern(FixWord::ONE * 15),
                        },
                    ],
                    right_boundary_char: None,
                    left_boundary_char_entrypoint: None,
                    passthrough: Default::default(),
                },
                ..Default::default()
            },
        ),
        (
            kern_with_skip,
            "(LIGTABLE (KRN C r D 15.0) (SKIP D 3))",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![ligkern::lang::Instruction {
                        next_instruction: Some(3),
                        right_char: 'r'.try_into().unwrap(),
                        operation: ligkern::lang::Operation::Kern(FixWord::ONE * 15),
                    },],
                    right_boundary_char: None,
                    left_boundary_char_entrypoint: None,
                    passthrough: Default::default(),
                },
                ..Default::default()
            },
        ),
        (
            lig,
            "(LIGTABLE (LIG/> C r C t))",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![ligkern::lang::Instruction {
                        next_instruction: None,
                        right_char: 'r'.try_into().unwrap(),
                        operation: ligkern::lang::Operation::Ligature {
                            char_to_insert: 't'.try_into().unwrap(),
                            post_lig_operation:
                                ligkern::lang::PostLigOperation::RetainRightMoveToRight,
                            post_lig_tag_invalid: false,
                        },
                    },],
                    right_boundary_char: None,
                    left_boundary_char_entrypoint: None,
                    passthrough: Default::default(),
                },
                ..Default::default()
            },
        ),
        (
            lig_kern_entrypoints,
            "(LIGTABLE (LABEL C e) (KRN C r D 15.0) (LABEL C d))",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![ligkern::lang::Instruction {
                        next_instruction: None,
                        right_char: 'r'.try_into().unwrap(),
                        operation: ligkern::lang::Operation::Kern(FixWord::ONE * 15),
                    },],
                    right_boundary_char: None,
                    left_boundary_char_entrypoint: None,
                    passthrough: Default::default(),
                },
                char_tags: BTreeMap::from([
                    ('e'.try_into().unwrap(), CharTag::Ligature(0),),
                    ('d'.try_into().unwrap(), CharTag::Ligature(1),),
                ]),
                ..Default::default()
            },
        ),
        (
            lig_kern_boundary_char_entrypoint,
            "(LIGTABLE (LABEL BOUNDARYCHAR) (KRN C r D 15.0))",
            File {
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![ligkern::lang::Instruction {
                        next_instruction: None,
                        right_char: 'r'.try_into().unwrap(),
                        operation: ligkern::lang::Operation::Kern(FixWord::ONE * 15),
                    },],
                    right_boundary_char: None,
                    left_boundary_char_entrypoint: Some(0),
                    passthrough: Default::default(),
                },
                ..Default::default()
            },
        ),
        (
            char_width,
            "(CHARACTER C r (CHARWD D 15.0))",
            File {
                char_dimens: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharDimensions {
                        width: Some(FixWord::ONE * 15),
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
                char_dimens: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharDimensions {
                        height: Some(FixWord::ONE * 15),
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
                char_dimens: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharDimensions {
                        depth: Some(FixWord::ONE * 15),
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
                char_dimens: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharDimensions {
                        italic_correction: Some(FixWord::ONE * 15),
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_next_larger,
            "(CHARACTER C A) (CHARACTER C r (NEXTLARGER C A))",
            File {
                char_dimens: BTreeMap::from([
                    (
                        'r'.try_into().unwrap(),
                        CharDimensions {
                            ..Default::default()
                        }
                    ),
                    (
                        'A'.try_into().unwrap(),
                        CharDimensions {
                            ..Default::default()
                        }
                    ),
                ]),
                char_tags: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharTag::List('A'.try_into().unwrap()),
                )]),
                ..Default::default()
            },
        ),
        (
            char_extensible_recipe_empty,
            "(CHARACTER C r (VARCHAR))",
            File {
                char_dimens: BTreeMap::from([
                    (Char(0), Default::default(),),
                    ('r'.try_into().unwrap(), Default::default(),),
                ]),
                char_tags: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharTag::Extension(Default::default()),
                )]),
                ..Default::default()
            },
        ),
        (
            char_extensible_recipe_data,
            "(CHARACTER C r (VARCHAR (TOP O 1) (MID O 2) (BOT O 3) (REP O 4)))",
            File {
                char_dimens: BTreeMap::from([
                    (Char(1), Default::default(),),
                    (Char(2), Default::default(),),
                    (Char(3), Default::default(),),
                    (Char(4), Default::default(),),
                    ('r'.try_into().unwrap(), Default::default(),),
                ]),
                char_tags: BTreeMap::from([(
                    'r'.try_into().unwrap(),
                    CharTag::Extension(ExtensibleRecipe {
                        top: Some(Char(1)),
                        middle: Some(Char(2)),
                        bottom: Some(Char(3)),
                        rep: Char(4),
                    }),
                )]),
                ..Default::default()
            },
        ),
    );

    fn run_from_tfm_file_test(tfm_file: crate::format::File, want: File) {
        let got: File = tfm_file.into();
        assert_eq!(got, want);
    }

    macro_rules! from_tfm_file_tests {
        ( $( ($name: ident, $tfm_file: expr, $pl_file: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let tfm_file = $tfm_file;
                    let want = $pl_file;
                    run_from_tfm_file_test(tfm_file, want);
                }
            )+
        };
    }

    from_tfm_file_tests!((
        gap_in_chars,
        crate::format::File {
            char_dimens: BTreeMap::from([
                (
                    Char('A'.try_into().unwrap()),
                    crate::format::CharDimensions {
                        width_index: WidthIndex::Valid(1.try_into().unwrap()),
                        height_index: 0,
                        depth_index: 0,
                        italic_index: 0,
                    }
                ),
                (
                    Char('C'.try_into().unwrap()),
                    crate::format::CharDimensions {
                        width_index: WidthIndex::Valid(2.try_into().unwrap()),
                        height_index: 0,
                        depth_index: 0,
                        italic_index: 0,
                    }
                ),
                (
                    Char('E'.try_into().unwrap()),
                    crate::format::CharDimensions {
                        width_index: WidthIndex::Invalid,
                        height_index: 0,
                        depth_index: 0,
                        italic_index: 0,
                    }
                ),
            ]),
            widths: vec![FixWord::ZERO, FixWord::ONE, FixWord::ONE * 2],
            ..Default::default()
        },
        File {
            char_dimens: BTreeMap::from([
                (
                    'A'.try_into().unwrap(),
                    CharDimensions {
                        width: Some(FixWord::ONE),
                        ..Default::default()
                    }
                ),
                (
                    'C'.try_into().unwrap(),
                    CharDimensions {
                        width: Some(FixWord::ONE * 2),
                        ..Default::default()
                    }
                ),
                (
                    'E'.try_into().unwrap(),
                    CharDimensions {
                        width: None,
                        ..Default::default()
                    }
                ),
            ]),
            ..<File as From<crate::format::File>>::from(Default::default())
        },
    ),);
}
