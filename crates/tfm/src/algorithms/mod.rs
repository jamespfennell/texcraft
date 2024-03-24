//! The tftopl and pltotf algorithms.

use std::fmt::Write;

/// Output of the tftopl algorithm.
pub struct TfmToPlOutput {
    pub pl_data: Result<String, crate::format::DeserializationError>,
    pub error_messages: Vec<TfmToPlErrorMessage>,
}

/// An error message written by the tftopl algorithm.
pub enum TfmToPlErrorMessage {
    DeserializationWarning(crate::format::DeserializationWarning),
    ValidationWarning(crate::format::ValidationWarning),
}

impl TfmToPlErrorMessage {
    pub fn tftopl_message(&self) -> String {
        match self {
            TfmToPlErrorMessage::DeserializationWarning(warning) => warning.tftopl_message(),
            TfmToPlErrorMessage::ValidationWarning(warning) => warning.tftopl_message(),
        }
    }
}

/// The tftopl algorithm.
///
/// This algorithm converts .tfm bytes to a .pl string.
pub fn tfm_to_pl(
    tfm_data: &[u8],
    indent: usize,
    display_format: &dyn Fn(&crate::pl::File) -> crate::pl::CharDisplayFormat,
) -> Result<TfmToPlOutput, std::fmt::Error> {
    let mut error_messages = Vec::<TfmToPlErrorMessage>::new();
    let (tfm_file_or, warnings) = crate::format::File::deserialize(tfm_data);
    for warning in warnings {
        error_messages.push(TfmToPlErrorMessage::DeserializationWarning(warning));
    }
    let mut tfm_file = match tfm_file_or {
        Ok(tfm_file) => tfm_file,
        Err(err) => {
            return Ok(TfmToPlOutput {
                pl_data: Err(err),
                error_messages,
            });
        }
    };
    let warnings = tfm_file.validate_and_fix();
    let infinite_loop = warnings.iter().any(|w| {
        matches!(
            w,
            crate::format::ValidationWarning::LigKernWarning(
                crate::ligkern::lang::ValidationWarning::InfiniteLoop(_),
            )
        )
    });
    let tfm_modified = warnings
        .iter()
        .map(crate::format::ValidationWarning::tfm_file_modified)
        .any(|t| t);
    for warning in warnings {
        error_messages.push(TfmToPlErrorMessage::ValidationWarning(warning));
    }
    let pl_file: crate::pl::File = tfm_file.into();
    let suffix = if infinite_loop {
        "(INFINITE LIGATURE LOOP MUST BE BROKEN!)"
    } else if tfm_modified {
        "(COMMENT THE TFM FILE WAS BAD, SO THE DATA HAS BEEN CHANGED!)\n"
    } else {
        ""
    };
    let mut s = String::new();
    write![
        &mut s,
        "{}{}",
        pl_file.display(indent, display_format(&pl_file),),
        suffix
    ]?;
    Ok(TfmToPlOutput {
        pl_data: Ok(s),
        error_messages,
    })
}
