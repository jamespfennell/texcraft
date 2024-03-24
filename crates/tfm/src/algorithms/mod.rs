//! The tftopl and pltotf algorithms.

use std::fmt::Write;

/// Output of the tftopl algorithm.
#[derive(Default)]
pub struct TfmToPlOutput {
    pub success: bool,
    pub pl_data: String,
    pub error_messages: Vec<TfmToPlErrorMessage>,
}

/// An error message written by the tftopl algorithm.
pub enum TfmToPlErrorMessage {
    DeserializationWarning(crate::format::DeserializationWarning),
    DeserializationError(crate::format::DeserializationError),
    ValidationWarning(crate::format::ValidationWarning),
}

impl TfmToPlErrorMessage {
    pub fn tftopl_message(&self) -> String {
        match self {
            TfmToPlErrorMessage::DeserializationWarning(warning) => warning.tftopl_message(),
            TfmToPlErrorMessage::DeserializationError(err) => {
                format!(
                    "{}\nSorry, but I can't go on; are you sure this is a TFM?",
                    err.tftopl_message()
                )
            }
            TfmToPlErrorMessage::ValidationWarning(warning) => warning.tftopl_message(),
        }
    }
}
/// The tftopl algorithm.
///
/// This algorithm converts .tfm bytes to a .pl string.
pub fn tfm_to_pl(
    tfm_data: &[u8],
    display_format: &dyn Fn(&crate::pl::File) -> crate::pl::CharDisplayFormat,
) -> Result<TfmToPlOutput, std::fmt::Error> {
    let mut output: TfmToPlOutput = Default::default();
    let (tfm_file_or, warnings) = crate::format::File::deserialize(tfm_data);
    for warning in warnings {
        output
            .error_messages
            .push(TfmToPlErrorMessage::DeserializationWarning(warning));
    }
    let mut tfm_file = match tfm_file_or {
        Ok(tfm_file) => tfm_file,
        Err(err) => {
            output
                .error_messages
                .push(TfmToPlErrorMessage::DeserializationError(err));
            return Ok(output);
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
        output
            .error_messages
            .push(TfmToPlErrorMessage::ValidationWarning(warning));
    }
    let pl_file: crate::pl::File = tfm_file.into();
    let suffix = if infinite_loop {
        "(INFINITE LIGATURE LOOP MUST BE BROKEN!)"
    } else if tfm_modified {
        "(COMMENT THE TFM FILE WAS BAD, SO THE DATA HAS BEEN CHANGED!)\n"
    } else {
        ""
    };
    write![
        &mut output.pl_data,
        "{}{}",
        pl_file.display(3, display_format(&pl_file),),
        suffix
    ]?;
    output.success = true;
    Ok(output)
}
