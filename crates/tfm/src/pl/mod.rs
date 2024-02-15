/*!
The property list (.pl) file format

|                                     | from .pl source code              | to .pl source code    | from lower level         | to lower level
|-------------------------------------|-----------------------------------|-----------------------|--------------------------|----
| fully parsed .pl file ([`File`])    | [`File::from_pl_source_code`]     | [`File::display`]     | [`File::from_ast`]       | [`File::lower`]
| abstract syntax tree ([`ast::Ast`]) | [`ast::Ast::from_pl_source_code`] | `ast::Ast::display` (TODO) | [`ast::Ast::from_cst`]   | [`ast::Ast::lower`]
| concrete syntax tree ([`cst::Cst`]) | [`cst::Cst::from_pl_source_code`] | [`cst::Cst::display`] | [`cst::Cst::from_lexer`] | N/A
| tokens (vector of [`lexer::Token`]) | [`lexer::Lexer::new`] and [`lexer::Lexer::next`] | N/A    | N/A                      | N/A

*/

use std::collections::HashMap;

use crate::{
    format::{self, ExtensibleRecipe},
    ligkern, Char, Header, NamedParam, Number, Params,
};

pub mod ast;
pub mod cst;
mod error;
pub mod lexer;
pub use error::ParseError;

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
pub struct CharData {
    pub width: Number,
    pub height: Option<Number>,
    pub depth: Option<Number>,
    pub italic_correction: Option<Number>,
    pub tag: CharTag,
}

/// Tag of a character in a .pl file.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum CharTag {
    #[default]
    None,
    Ligature(u16),
    List(Char),
    Extension(format::ExtensibleRecipe),
}

/// Complete contents of a property list (.pl) file.
#[derive(PartialEq, Eq, Debug)]
pub struct File {
    pub header: Header,
    pub design_units: Number,
    pub char_data: HashMap<Char, CharData>,
    pub lig_kern_program: ligkern::lang::Program,
    pub params: Params,
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Header::property_list_default(),
            design_units: Number::UNITY,
            char_data: Default::default(),
            lig_kern_program: Default::default(),
            params: Default::default(),
        }
    }
}

impl File {
    /// Build a File from PL source code.
    pub fn from_pl_source_code(source: &str) -> (File, Vec<ParseError>) {
        let (ast, mut errors) = ast::Ast::from_pl_source_code(source);
        let file = File::from_ast(ast, &mut errors);
        (file, errors)
    }

    /// Return a map from characters to the lig/kern entrypoint for that character.
    pub fn lig_kern_entrypoints(&self) -> HashMap<Char, u16> {
        self.char_data
            .iter()
            .filter_map(|d| match d.1.tag {
                CharTag::Ligature(l) => Some((*d.0, l)),
                _ => None,
            })
            .collect()
    }

    /// Clear all lig/kern data from the file.
    pub fn clear_lig_kern_data(&mut self) {
        for (_, data) in self.char_data.iter_mut() {
            if let CharTag::Ligature(_) = data.tag {
                data.tag = CharTag::None;
            }
        }
        self.lig_kern_program.instructions = vec![];
    }

    /// Build a File from an AST.
    pub fn from_ast(ast: ast::Ast, errors: &mut Vec<error::ParseError>) -> File {
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
                ast::Root::Header(v) => match v.left.0.checked_sub(18) {
                    None => errors.push(ParseError::InvalidHeaderIndex { span: v.left_span }),
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
                                file.params.set(v.left.0 as usize, v.right);
                            }
                            ast::FontDimension::Comment(_) => {}
                        }
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
                                errors.push(error::ParseError::LigTableTooLong { span });
                            }
                        };
                        match node {
                            ast::LigTable::Label(v) => {
                                let u: u16 = file.lig_kern_program.instructions.len().try_into().expect("lig_kern_instructions.len()<= MAX_LIG_KERN_INSTRUCTIONS which is a u16");
                                match v.data {
                                    ast::LigTableLabel::Char(c) => {
                                        let char_data = file.char_data.entry(c).or_default();
                                        // TODO: error if the tag is already set
                                        char_data.tag = CharTag::Ligature(u);
                                    }
                                    ast::LigTableLabel::BoundaryChar => {
                                        file.lig_kern_program.boundary_char_entrypoint = Some(u);
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
                    file.lig_kern_program.boundary_char = Some(v.data);
                }
                ast::Root::Character(b) => {
                    let char_data = file.char_data.entry(b.data).or_default();
                    for node in b.children {
                        match node {
                            ast::Character::Width(v) => {
                                char_data.width = v.data;
                            }
                            ast::Character::Height(v) => {
                                char_data.height = Some(v.data);
                            }
                            ast::Character::Depth(v) => {
                                char_data.depth = Some(v.data);
                            }
                            ast::Character::ItalicCorrection(v) => {
                                char_data.italic_correction = Some(v.data);
                            }
                            ast::Character::NextLarger(c) => {
                                // TODO: warning if tag != CharTag::None
                                char_data.tag = CharTag::List(c.data);
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
                                char_data.tag = CharTag::Extension(recipe)
                            }
                            ast::Character::Comment(_) => {}
                        }
                    }
                }
                ast::Root::Comment(_) => {}
            }
        }

        // PLtoTF.2014.116
        if let Some(final_instruction) = file.lig_kern_program.instructions.last_mut() {
            if final_instruction.next_instruction == Some(0) {
                final_instruction.next_instruction = None;
            }
        }
        file
    }

    /// Convert a TFM file into a PL file.
    pub fn from_tfm_file(tfm_file: crate::format::File) -> File {
        let lig_kern_entrypoints = tfm_file.lig_kern_entrypoints();
        let mut lig_kern_program = tfm_file.lig_kern_program;
        lig_kern_program.pack_kerns(&tfm_file.kerns);
        let lig_kern_entrypoints = lig_kern_program.unpack_entrypoints(lig_kern_entrypoints);

        let char_data = tfm_file
            .char_infos
            .into_iter()
            .map(|(c, info)| {
                (
                    c,
                    CharData {
                        width: tfm_file
                            .widths
                            .get(info.width_index.get() as usize)
                            .copied()
                            .unwrap_or_default(),
                        height: if info.height_index == 0 {
                            None
                        } else {
                            Some(tfm_file.heights[info.height_index as usize])
                        },
                        depth: if info.depth_index == 0 {
                            None
                        } else {
                            Some(tfm_file.depths[info.depth_index as usize])
                        },
                        italic_correction: if info.italic_index == 0 {
                            None
                        } else {
                            Some(tfm_file.italic_corrections[info.italic_index as usize])
                        },
                        tag: match info.tag {
                            format::CharTag::None => CharTag::None,
                            format::CharTag::Ligature(_) => {
                                CharTag::Ligature(*lig_kern_entrypoints.get(&c).unwrap())
                            }
                            format::CharTag::List(c) => CharTag::List(c),
                            format::CharTag::Extension(i) => {
                                // TODO: don't panic
                                // TODO: audit all unwraps in the crate
                                CharTag::Extension(
                                    tfm_file.extensible_chars.get(i as usize).cloned().unwrap(),
                                )
                            }
                        },
                    },
                )
            })
            .collect();
        File {
            header: tfm_file.header,
            design_units: Number::UNITY,
            char_data,
            lig_kern_program,
            params: tfm_file.params,
        }
    }

    /// Lower a File to an AST.
    pub fn lower(&self, char_display_format: CharDisplayFormat) -> ast::Ast {
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
            let i = i.checked_add(18).unwrap();  // TODO: gotta be a warning here
            roots.push(ast::Root::Header((ast::DecimalU8(i), u).into()))
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
        roots.extend([
            ast::Root::CodingScheme(sanitize_string(&self.header.character_coding_scheme).into()),
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
                    (8, FontType::TexMathEx) => NamedParam::DefaultRuleThickness,
                    (9, FontType::TexMathEx) => NamedParam::BigOpSpacing1,
                    (10, FontType::TexMathEx) => NamedParam::BigOpSpacing2,
                    (11, FontType::TexMathEx) => NamedParam::BigOpSpacing3,
                    (12, FontType::TexMathEx) => NamedParam::BigOpSpacing4,
                    (13, FontType::TexMathEx) => NamedParam::BigOpSpacing5,
                    _ => {
                        return ast::FontDimension::IndexedParam((ast::DecimalU8(i), param).into());
                    }
                };
                ast::FontDimension::NamedParam(named_param, param.into())
            })
            .collect();
        if !params.is_empty() {
            roots.push(ast::Root::FontDimension(((), params).into()));
        }

        let ordered_chars = {
            let mut v: Vec<Char> = self.char_data.keys().copied().collect();
            v.sort();
            v
        };

        // Ligtable
        if let Some(boundary_char) = self.lig_kern_program.boundary_char {
            roots.push(ast::Root::BoundaryChar(boundary_char.into()));
        }
        let mut l = Vec::<ast::LigTable>::new();
        let mut index_to_labels = HashMap::<u16, Vec<Char>>::new();
        for c in &ordered_chars {
            if let Some(char_data) = self.char_data.get(c) {
                if let CharTag::Ligature(index) = char_data.tag {
                    index_to_labels.entry(index).or_default().push(*c);
                }
            }
        }
        let build_lig_kern_op = |instruction: &ligkern::lang::Instruction| match instruction
            .operation
        {
            ligkern::lang::Operation::Kern(kern) => {
                ast::LigTable::Kern((instruction.right_char, kern).into())
            }
            ligkern::lang::Operation::KernAtIndex(_) => {
                panic!("tfm::pl::File lig/kern programs cannot contain `KernAtIndex` operations. Use a `Kern` operation instead.");
            }
            ligkern::lang::Operation::EntrypointRedirect(_, _) => {
                panic!("tfm::ligkern::lang::ReachableIter does not return `EntrypointRedirect` operations.");
            }
            ligkern::lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
            } => ast::LigTable::Lig(
                post_lig_operation,
                (instruction.right_char, char_to_insert).into(),
            ),
        };

        // When we fixed the (LIGTABLE (LABEL BOUNDARYCHAR) bug, number of failures went from 26652 -> 12004
        for item in self
            .lig_kern_program
            .reachable_iter(index_to_labels.keys().copied())
        {
            match item {
                ligkern::lang::ReachableIterItem::Reachable {
                    instruction,
                    index,
                    skip_override,
                } => {
                    if let Some(e) = self.lig_kern_program.boundary_char_entrypoint {
                        if e == index {
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
                    let lig_kern_op = build_lig_kern_op(instruction);
                    l.push(lig_kern_op);
                    match skip_override {
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
                ligkern::lang::ReachableIterItem::Unreachable(instructions) => {
                    let mut balanced_elems = vec![cst::BalancedElem::String(
                        "THIS PART OF THE PROGRAM IS NEVER USED!".to_string(),
                    )];
                    for instruction in instructions {
                        let op = build_lig_kern_op(instruction);
                        balanced_elems.push(cst::BalancedElem::Vec(
                            op.into_balanced_elements(char_display_format),
                        ));
                    }
                    l.push(ast::LigTable::Comment(balanced_elems))
                }
            }
        }
        if !self.lig_kern_program.instructions.is_empty() {
            roots.push(ast::Root::LigTable(((), l).into()))
        }

        // Characters
        for c in &ordered_chars {
            let data = match self.char_data.get(c) {
                None => continue,
                Some(data) => data,
            };
            let mut v = vec![];
            v.push(ast::Character::Width(data.width.into()));
            if let Some(height) = data.height {
                v.push(ast::Character::Height(height.into()));
            }
            if let Some(depth) = data.depth {
                v.push(ast::Character::Depth(depth.into()));
            }
            if let Some(italic_correction) = data.italic_correction {
                v.push(ast::Character::ItalicCorrection(italic_correction.into()));
            }

            match &data.tag {
                CharTag::None => {}
                CharTag::Ligature(entrypoint) => {
                    let l: Vec<cst::BalancedElem> = self
                        .lig_kern_program
                        .instructions_for_entrypoint(*entrypoint)
                        .map(build_lig_kern_op)
                        .map(|n| {
                            cst::BalancedElem::Vec(n.into_balanced_elements(char_display_format))
                        })
                        .collect();
                    // TODO: what if l.is_empty()?
                    v.push(ast::Character::Comment(l));
                }
                CharTag::List(c) => v.push(ast::Character::NextLarger((*c).into())),
                CharTag::Extension(recipe) => {
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
                    r.push(ast::ExtensibleCharacter::Replicated(recipe.rep.into()));
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
        let ast = self.pl_file.lower(self.char_display_format);
        let cst = ast.lower(self.char_display_format);
        cst.display(self.indent).fmt(f)
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

#[cfg(test)]
mod tests {
    use crate::Face;

    use self::format::ExtensibleRecipe;

    use super::*;

    fn run_from_pl_source_code_test(source: &str, want: File) {
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
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![],
                    boundary_char: Some('a'.try_into().unwrap()),
                    boundary_char_entrypoint: None,
                },
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
                lig_kern_program: ligkern::lang::Program {
                    instructions: vec![ligkern::lang::Instruction {
                        next_instruction: None,
                        right_char: 'r'.try_into().unwrap(),
                        operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                    },],
                    boundary_char: None,
                    boundary_char_entrypoint: None,
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
                            operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                        },
                        ligkern::lang::Instruction {
                            next_instruction: None,
                            right_char: 't'.try_into().unwrap(),
                            operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                        },
                    ],
                    boundary_char: None,
                    boundary_char_entrypoint: None,
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
                        operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                    },],
                    boundary_char: None,
                    boundary_char_entrypoint: None,
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
                        },
                    },],
                    boundary_char: None,
                    boundary_char_entrypoint: None,
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
                        operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                    },],
                    boundary_char: None,
                    boundary_char_entrypoint: None,
                },
                char_data: HashMap::from([
                    (
                        'e'.try_into().unwrap(),
                        CharData {
                            tag: CharTag::Ligature(0),
                            ..Default::default()
                        }
                    ),
                    (
                        'd'.try_into().unwrap(),
                        CharData {
                            tag: CharTag::Ligature(1),
                            ..Default::default()
                        }
                    ),
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
                        operation: ligkern::lang::Operation::Kern(Number::UNITY * 15),
                    },],
                    boundary_char: None,
                    boundary_char_entrypoint: Some(0),
                },
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
                        height: Some(Number::UNITY * 15),
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
                        depth: Some(Number::UNITY * 15),
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
                        italic_correction: Some(Number::UNITY * 15),
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_next_larger,
            "(CHARACTER C r (NEXTLARGER C A))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        tag: CharTag::List('A'.try_into().unwrap()),
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_extensible_recipe_empty,
            "(CHARACTER C r (VARCHAR))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        tag: CharTag::Extension(Default::default()),
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
        (
            char_extensible_recipe_data,
            "(CHARACTER C r (VARCHAR (TOP O 1) (MID O 2) (BOT O 3) (REP O 4)))",
            File {
                char_data: HashMap::from([(
                    'r'.try_into().unwrap(),
                    CharData {
                        tag: CharTag::Extension(ExtensibleRecipe {
                            top: Some(Char(1)),
                            middle: Some(Char(2)),
                            bottom: Some(Char(3)),
                            rep: Char(4),
                        }),
                        ..Default::default()
                    }
                )]),
                ..Default::default()
            },
        ),
    );

    fn run_from_tfm_file_test(tfm_file: crate::format::File, want: File) {
        let got = File::from_tfm_file(tfm_file);
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
            char_infos: HashMap::from([
                (
                    Char('A'.try_into().unwrap()),
                    crate::format::CharInfo {
                        width_index: 1.try_into().unwrap(),
                        height_index: 0,
                        depth_index: 0,
                        italic_index: 0,
                        tag: crate::format::CharTag::None,
                    }
                ),
                (
                    Char('C'.try_into().unwrap()),
                    crate::format::CharInfo {
                        width_index: 2.try_into().unwrap(),
                        height_index: 0,
                        depth_index: 0,
                        italic_index: 0,
                        tag: crate::format::CharTag::None,
                    }
                ),
            ]),
            widths: vec![Number::ZERO, Number::UNITY, Number::UNITY * 2],
            ..Default::default()
        },
        File {
            char_data: HashMap::from([
                (
                    'A'.try_into().unwrap(),
                    CharData {
                        width: Number::UNITY,
                        ..Default::default()
                    }
                ),
                (
                    'C'.try_into().unwrap(),
                    CharData {
                        width: Number::UNITY * 2,
                        ..Default::default()
                    }
                ),
            ]),
            ..File::from_tfm_file(Default::default())
        },
    ),);
}
