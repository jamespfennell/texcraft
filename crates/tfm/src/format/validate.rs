use super::*;
use crate::ligkern::lang;

pub enum ValidationWarning {
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
    LigKernWarning(lang::ValidationWarning),
}

impl ValidationWarning {
    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tftopl_message(&self) -> String {
        use ValidationWarning::*;
        match self {
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
            InvalidWidthIndex(c, _) => format![" \nWidth index for character '{:o} is too large;\nso I reset it to zero.", c.0],
            InvalidHeightIndex(c, _) =>  format![" \nHeight index for character '{:o} is too large;\nso I reset it to zero.", c.0],
            InvalidDepthIndex(c, _) =>  format![" \nDepth index for character '{:o} is too large;\nso I reset it to zero.", c.0],
            InvalidItalicCorrectionIndex(c, _) => format![" \nItalic correction index for character '{:o} is too large;\nso I reset it to zero.", c.0],
            InvalidExtensibleRecipeIndex(c, _ ) => format![" \nExtension index for character '{:o} is too large;\nso I reset it to zero.", c.0],
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
            LigKernWarning(warning) => warning.tftopl_message(),
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> usize {
        use ValidationWarning::*;
        match self {
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
            | ItalicCorrectionIsTooBig(_) => 62,
            LigKernWarning(warning) => warning.tftopl_section(),
        }
    }

    /// Returns true if this warning means the .tfm file was modified.
    pub fn tfm_file_modified(&self) -> bool {
        use ValidationWarning::*;
        match self {
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
            | LigKernWarning(_) => true,
        }
    }
}

pub fn validate_and_fix(file: &mut File) -> Vec<ValidationWarning> {
    let mut warnings = vec![];

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
    }

    for (c, tag) in &mut file.char_tags {
        match tag {
            CharTag::Ligature(_) => {}
            CharTag::List(_) => {}
            CharTag::Extension(e) => {
                if *e as usize >= file.extensible_chars.len() {
                    warnings.push(ValidationWarning::InvalidExtensibleRecipeIndex(*c, *e));
                    continue;
                }
            }
        }
    }

    for (array, first_dimension_non_zero, dimension_too_big) in [
        (
            &mut file.widths,
            ValidationWarning::FirstWidthIsNonZero,
            ValidationWarning::WidthIsTooBig as fn(usize) -> ValidationWarning,
        ),
        (
            &mut file.heights,
            ValidationWarning::FirstHeightIsNonZero,
            ValidationWarning::HeightIsTooBig,
        ),
        (
            &mut file.depths,
            ValidationWarning::FirstDepthIsNonZero,
            ValidationWarning::DepthIsTooBig,
        ),
        (
            &mut file.italic_corrections,
            ValidationWarning::FirstItalicCorrectionIsNonZero,
            ValidationWarning::ItalicCorrectionIsTooBig,
        ),
    ] {
        if let Some(first) = array.first_mut() {
            if *first != Number::ZERO {
                warnings.push(first_dimension_non_zero);
                *first = Number::ZERO;
            }
        }
        for (i, elem) in array.iter_mut().enumerate() {
            // TODO: test the boundary cases when n=-16 and n=+16
            if *elem >= Number::UNITY * 16 || *elem < Number::UNITY * -16 {
                warnings.push(dimension_too_big(i));
                *elem = Number::ZERO
            }
        }
    }

    warnings.extend(
        file.lig_kern_program
            .validate_and_fix()
            .into_iter()
            .map(ValidationWarning::LigKernWarning),
    );

    warnings
}
