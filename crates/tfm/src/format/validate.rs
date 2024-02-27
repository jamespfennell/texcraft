use super::*;
use crate::ligkern::lang;

pub enum ValidationWarning {
    DesignSizeIsTooSmall,
    DesignSizeIsNegative,
    ParameterIsTooBig(usize),
    /// Unusual number of parameters.
    ///
    /// Math symbol fonts usually contain 22 parameters and math extension fonts 13.
    /// This warning indicates that a different number was in the .tfm file.
    UnusualNumberOfParameters {
        /// True if this is a math symbols font; false if it's a math extension font.
        is_math_symbols_font: bool,
        /// Number of parameters in the .tfm file.
        got: usize,
    },
    InvalidWidthIndex(Char, u8),
    InvalidHeightIndex(Char, u8),
    InvalidDepthIndex(Char, u8),
    InvalidItalicCorrectionIndex(Char, u8),
    InvalidExtensibleRecipeIndex(Char, u8),
    FirstWidthIsNonZero,
    FirstDepthIsNonZero,
    FirstHeightIsNonZero,
    FirstItalicCorrectionIsNonZero,
    WidthIsTooBig(usize),
    HeightIsTooBig(usize),
    DepthIsTooBig(usize),
    ItalicCorrectionIsTooBig(usize),
    KernIsTooBig(usize),
    NextLargerWarning(NextLargerProgramWarning),
    LigKernWarning(lang::ValidationWarning),
}

impl ValidationWarning {
    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tftopl_message(&self) -> String {
        use ValidationWarning::*;
        match self {
            DesignSizeIsTooSmall => "Bad TFM file: Design size too small!\nI've set it to 10 points.".to_string(),
            DesignSizeIsNegative => "Bad TFM file: Design size negative!\nI've set it to 10 points.".to_string(),
            ParameterIsTooBig(i) => format![
                "Bad TFM file: Parameter {} is too big;\nI have set it to zero.",
                i
            ],
            UnusualNumberOfParameters { is_math_symbols_font, got } => {
                let (font_description, expected) = if *is_math_symbols_font {
                    ("a math symbols", 22)
                } else {
                    ("an extension", 13)
                };
                format!["Unusual number of fontdimen parameters for {font_description} font ({got} not {expected})."]
            },
            // The following invalid index warning messages intentionally start with a new line.
            // I think this is a bug in tftopl: in the range_error macro in TFtoPL.2014.47,
            //  the print_ln(` `) invocation should be guarded behind a check on chars_on_line,
            //  like the other macros.
            InvalidWidthIndex(c, _) => format![" \nWidth index for character '{:03o} is too large;\nso I reset it to zero.", c.0],
            InvalidHeightIndex(c, _) =>  format![" \nHeight index for character '{:03o} is too large;\nso I reset it to zero.", c.0],
            InvalidDepthIndex(c, _) =>  format![" \nDepth index for character '{:03o} is too large;\nso I reset it to zero.", c.0],
            InvalidItalicCorrectionIndex(c, _) => format![" \nItalic correction index for character '{:03o} is too large;\nso I reset it to zero.", c.0],
            InvalidExtensibleRecipeIndex(c, _ ) => format![" \nExtensible index for character '{:03o} is too large;\nso I reset it to zero.", c.0],
            FirstWidthIsNonZero => "Bad TFM file: width[0] should be zero.".into(),
            FirstDepthIsNonZero => "Bad TFM file: depth[0] should be zero.".into(),
            FirstHeightIsNonZero => "Bad TFM file: height[0] should be zero.".into(),
            FirstItalicCorrectionIsNonZero => "Bad TFM file: italic[0] should be zero.".into(),
            WidthIsTooBig(i) => format![
                "Bad TFM file: Width {} is too big;\nI have set it to zero.",
                i
            ],
            HeightIsTooBig(i) => format![
                "Bad TFM file: Height {} is too big;\nI have set it to zero.",
                i
            ],
            DepthIsTooBig(i) => format![
                "Bad TFM file: Depth {} is too big;\nI have set it to zero.",
                i
            ],
            ItalicCorrectionIsTooBig(i) => format![
                "Bad TFM file: Italic correction {} is too big;\nI have set it to zero.",
                i
            ],
            KernIsTooBig(i) => format![
                "Bad TFM file: Kern {} is too big;\nI have set it to zero.",
                i
            ],
            NextLargerWarning(warning) => warning.tftopl_message(),
            LigKernWarning(warning) => warning.tftopl_message(),
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> u8 {
        use ValidationWarning::*;
        match self {
            DesignSizeIsNegative | DesignSizeIsTooSmall => 51,
            ParameterIsTooBig(_) => 60,
            UnusualNumberOfParameters { .. } => 59,
            InvalidWidthIndex(_, _) => 79,
            InvalidHeightIndex(_, _) => 80,
            InvalidDepthIndex(_, _) => 81,
            InvalidItalicCorrectionIndex(_, _) => 82,
            InvalidExtensibleRecipeIndex(_, _) => 85,
            FirstWidthIsNonZero
            | FirstDepthIsNonZero
            | FirstHeightIsNonZero
            | FirstItalicCorrectionIsNonZero
            | WidthIsTooBig(_)
            | HeightIsTooBig(_)
            | DepthIsTooBig(_)
            | ItalicCorrectionIsTooBig(_)
            | KernIsTooBig(_) => 62,
            NextLargerWarning(warning) => warning.tftopl_section(),
            LigKernWarning(warning) => warning.tftopl_section(),
        }
    }

    /// Returns true if this warning means the .tfm file was modified.
    pub fn tfm_file_modified(&self) -> bool {
        use ValidationWarning::*;
        match self {
            DesignSizeIsNegative | DesignSizeIsTooSmall | ParameterIsTooBig(_) => true,
            UnusualNumberOfParameters { .. } => false,
            InvalidWidthIndex(_, _)
            | InvalidHeightIndex(_, _)
            | InvalidDepthIndex(_, _)
            | InvalidItalicCorrectionIndex(_, _)
            | InvalidExtensibleRecipeIndex(_, _)
            | FirstWidthIsNonZero
            | FirstDepthIsNonZero
            | FirstHeightIsNonZero
            | FirstItalicCorrectionIsNonZero
            | WidthIsTooBig(_)
            | HeightIsTooBig(_)
            | DepthIsTooBig(_)
            | ItalicCorrectionIsTooBig(_)
            | KernIsTooBig(_)
            | NextLargerWarning(_) => true,
            LigKernWarning(_) => true,
        }
    }
}

pub fn validate_and_fix(file: &mut File) -> Vec<ValidationWarning> {
    let mut warnings = vec![];

    if file.header.design_size.get() < Number::ZERO {
        warnings.push(ValidationWarning::DesignSizeIsNegative);
        file.header.design_size = DesignSize::Invalid;
    }
    if file.header.design_size.get() < Number::UNITY {
        warnings.push(ValidationWarning::DesignSizeIsTooSmall);
        file.header.design_size = DesignSize::Invalid;
    }

    for (i, elem) in file.params.0.iter_mut().enumerate() {
        if i == 0 {
            continue;
        }
        if !elem.is_abs_less_than_16() {
            warnings.push(ValidationWarning::ParameterIsTooBig(i + 1));
            *elem = Number::ZERO
        }
    }

    {
        let scheme = match &file.header.character_coding_scheme {
            None => "".to_string(),
            Some(scheme) => scheme.to_uppercase(),
        };
        let num_params = file.params.0.len();
        if scheme.starts_with("TEX MATH SY") && num_params != 22 {
            warnings.push(ValidationWarning::UnusualNumberOfParameters {
                is_math_symbols_font: true,
                got: num_params,
            })
        }
        if scheme.starts_with("TEX MATH EX") && num_params != 13 {
            warnings.push(ValidationWarning::UnusualNumberOfParameters {
                is_math_symbols_font: false,
                got: num_params,
            })
        }
    }

    for (array, first_dimension_non_zero) in [
        (&mut file.widths, ValidationWarning::FirstWidthIsNonZero),
        (&mut file.heights, ValidationWarning::FirstHeightIsNonZero),
        (&mut file.depths, ValidationWarning::FirstDepthIsNonZero),
        (
            &mut file.italic_corrections,
            ValidationWarning::FirstItalicCorrectionIsNonZero,
        ),
    ] {
        if let Some(first) = array.first_mut() {
            if *first != Number::ZERO {
                warnings.push(first_dimension_non_zero);
                // We zero out the number below because we may still want to issue
                // a warning for number too big.
            }
        }
    }

    for (array, dimension_too_big, zero_first_element) in [
        (
            &mut file.widths,
            ValidationWarning::WidthIsTooBig as fn(usize) -> ValidationWarning,
            true,
        ),
        (&mut file.heights, ValidationWarning::HeightIsTooBig, true),
        (&mut file.depths, ValidationWarning::DepthIsTooBig, true),
        (
            &mut file.italic_corrections,
            ValidationWarning::ItalicCorrectionIsTooBig,
            true,
        ),
        (&mut file.kerns, ValidationWarning::KernIsTooBig, false),
    ] {
        for (i, elem) in array.iter_mut().enumerate() {
            if !elem.is_abs_less_than_16() {
                warnings.push(dimension_too_big(i));
                *elem = Number::ZERO
            }
            if i == 0 && zero_first_element {
                *elem = Number::ZERO
            }
        }
    }

    let (_, next_larger_warnings) = NextLargerProgram::new(
        file.char_tags
            .iter()
            .filter_map(|(c, t)| t.list().map(|l| (*c, l))),
        |c| file.char_dimens.contains_key(&c),
        true,
    );
    let mut next_larger_warnings: HashMap<Char, NextLargerProgramWarning> = next_larger_warnings
        .into_iter()
        .map(|w| (w.bad_char(), w))
        .collect();

    let lig_kern_warnings = file.lig_kern_program.validate_and_fix(
        file.smallest_char,
        file.char_tags
            .iter()
            .filter_map(|(c, t)| t.ligature().map(|l| (*c, l))),
        |c| file.char_dimens.contains_key(&c),
        file.kerns.len(),
    );
    lig_kern_warnings
        .iter()
        .filter_map(|w| match w {
            lang::ValidationWarning::InvalidEntrypoint(c) => Some(c),
            _ => None,
        })
        .for_each(|c| {
            file.char_tags.remove(c);
        });
    // There is a bug in Knuth's tftopl in which the same kern-index-too-big warning is printed
    // more than once. It is printed when the lig/kern instruction is output in the LIGTABLE
    // list, and also each time the instruction appears in a lig/kern COMMENT for a character.
    // The cause of the bug is that the buggy index is never fixed in memory, and so when the
    // instruction is seen multiple times the warning is triggered again. For other lig/kern warnings,
    // the bad data is fixed when the warning is first printed, so the next time the instruction is
    // seen it is valid.
    let kern_index_too_big_indices: HashSet<usize> = lig_kern_warnings
        .iter()
        .filter_map(|warning| match warning {
            lang::ValidationWarning::KernIndexTooBig(u) => Some(*u),
            _ => None,
        })
        .collect();
    warnings.extend(
        lig_kern_warnings
            .into_iter()
            .map(ValidationWarning::LigKernWarning),
    );

    for (c, dimens) in &mut file.char_dimens {
        if dimens.width_index.get() as usize >= file.widths.len() {
            warnings.push(ValidationWarning::InvalidWidthIndex(
                *c,
                dimens.width_index.get(),
            ));
            dimens.width_index = WidthIndex::Invalid;
        }
        if dimens.height_index as usize >= file.heights.len() {
            warnings.push(ValidationWarning::InvalidHeightIndex(
                *c,
                dimens.height_index,
            ));
            dimens.height_index = 0;
        }
        if dimens.depth_index as usize >= file.depths.len() {
            warnings.push(ValidationWarning::InvalidDepthIndex(*c, dimens.depth_index));
            dimens.depth_index = 0;
        }
        if dimens.italic_index as usize >= file.italic_corrections.len() {
            warnings.push(ValidationWarning::InvalidItalicCorrectionIndex(
                *c,
                dimens.italic_index,
            ));
            dimens.italic_index = 0
        }
        match file.char_tags.get(c) {
            Some(CharTag::List(_)) => {
                if let Some(warning) = next_larger_warnings.remove(c) {
                    warnings.push(ValidationWarning::NextLargerWarning(warning));
                    file.char_tags.remove(c);
                }
            }
            Some(CharTag::Extension(e)) => {
                if *e as usize >= file.extensible_chars.len() {
                    warnings.push(ValidationWarning::InvalidExtensibleRecipeIndex(*c, *e));
                    file.char_tags.remove(c);
                }
            }
            Some(CharTag::Ligature(l)) => {
                let entrypoint = file.lig_kern_program.unpack_entrypoint(*l);
                warnings.extend(
                    file.lig_kern_program
                        .instructions_for_entrypoint(entrypoint)
                        .map(|t| t.0)
                        .filter(|u| kern_index_too_big_indices.contains(u))
                        .map(lang::ValidationWarning::KernIndexTooBig)
                        .map(ValidationWarning::LigKernWarning),
                );
            }
            _ => {}
        }
    }

    warnings
}
