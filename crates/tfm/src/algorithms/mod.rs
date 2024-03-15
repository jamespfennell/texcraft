//! Algorithms like tftopl and pltotf.

use std::fmt::Write;

/// Output of the TFM to PL algorithm.
#[derive(Default)]
pub struct TfmToPlOutput {
    pub success: bool,
    pub pl_data: String,
    pub error_messages: String,
}

/// Convert .tfm bytes to a .pl string.
pub fn tfm_to_pl(
    tfm_data: &[u8],
    display_format: &dyn Fn(&crate::pl::File) -> crate::pl::CharDisplayFormat,
) -> Result<TfmToPlOutput, std::fmt::Error> {
    let mut output: TfmToPlOutput = Default::default();
    let (tfm_file_or, warnings) = crate::format::File::deserialize(tfm_data);
    for warning in &warnings {
        writeln!(&mut output.error_messages, "{}", warning.tftopl_message())?;
    }
    let tfm_file = match tfm_file_or {
        Ok(tfm_file) => tfm_file,
        Err(err) => {
            writeln!(
                &mut output.error_messages,
                "{}\nSorry, but I can't go on; are you sure this is a TFM?",
                err.tftopl_message()
            )?;
            return Ok(output);
        }
    };
    let pl_file = crate::pl::File::from_tfm_file(tfm_file);
    let infinite_loop = warnings.iter().any(|w| {
        matches!(
            w,
            crate::format::Warning::ValidationWarning(
                crate::format::ValidationWarning::LigKernWarning(
                    crate::ligkern::lang::ValidationWarning::InfiniteLoop(_),
                )
            )
        )
    });
    let tfm_modified = warnings
        .iter()
        .map(crate::format::Warning::tfm_file_modified)
        .any(|t| t);
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
