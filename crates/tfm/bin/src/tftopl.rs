use clap::Parser;

mod common;

fn main() {
    if let Err(err) = Cli::parse().run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

/// Convert a TeX font metric (.tfm) file to a human-readable property list (.pl) file.
///
/// This is an alternative implementation of tftopl, built as part of the Texcraft project.
/// It is designed to have the same functionality and API as Knuth's original tftopl.
/// For additional conversion options, batch conversions, and other features,
///     consider using the Texcraft tfmtools binary.
#[derive(Debug, clap::Parser)]
#[command(
    name = "TFtoPL (Texcraft version)",
    author = "The Texcraft Project",
    version = "0.1",
    about,
    long_about,
    max_term_width(100)
)]
struct Cli {
    /// Path to the TeX font metric (.tfm) file to convert.
    ///
    /// A file extension is optional, and will be set to .tfm if missing.
    tfm_file_path: std::path::PathBuf,

    /// Output path for the property list (.pl) file.
    ///
    /// A file extension is optional, and will be set to .pl if missing.
    ///
    /// If the output path is not specified,
    ///     the property list file is printed to standard out.
    pl_file_path: Option<std::path::PathBuf>,

    /// Write some additional information to standard error while converting.
    ///
    /// This option just prints a banner on startup.
    /// It primarily exists for compatibility with Knuth's tftopl.
    #[arg(short, long)]
    verbose: bool,

    /// Specification for how to output characters.
    #[arg(short, long, default_value = "default")]
    charcode_format: common::CharcodeFormat,
}

impl Cli {
    fn run(self) -> Result<(), String> {
        if self.verbose {
            eprintln!("This is the Texcraft implementation of TFtoPL, version 0.1")
        }

        // Input
        let tfm_file_path = with_default_file_extension(self.tfm_file_path, "tfm");
        let tfm_data = match std::fs::read(&tfm_file_path) {
            Ok(tfm_data) => tfm_data,
            Err(err) => {
                return Err(format!(
                    "Failed to read `{}`: {}",
                    tfm_file_path.display(),
                    err
                ))
            }
        };

        // Conversion
        let (tfm_file_or, warnings) = tfm::format::File::deserialize(&tfm_data);
        for warning in &warnings {
            eprintln!("{}", warning.tftopl_message())
        }
        let tfm_file = match tfm_file_or {
            Ok(tfm_file) => tfm_file,
            Err(err) => {
                return Err(format![
                    "{}\nSorry, but I can't go on; are you sure this is a TFM?",
                    err.tftopl_message()
                ])
            }
        };
        let pl_file = tfm::pl::File::from_tfm_file(tfm_file);
        let tfm_modified = warnings
            .iter()
            .map(tfm::format::DeserializeWarning::tfm_file_modified)
            .any(|t| t);
        let suffix = if tfm_modified {
            "(COMMENT THE TFM FILE WAS BAD, SO THE DATA HAS BEEN CHANGED!)\n"
        } else {
            ""
        };
        let pl_output = format![
            "{}{}",
            pl_file.display(
                3,
                self.charcode_format
                    .to_display_format(&pl_file.header.character_coding_scheme)
            ),
            suffix
        ];

        // Output
        match self.pl_file_path {
            None => print!("{pl_output}"),
            Some(pl_file_path) => {
                let pl_file_path = with_default_file_extension(pl_file_path, "pl");
                if let Err(err) = std::fs::write(&pl_file_path, pl_output) {
                    return Err(format!(
                        "Failed to write file `{}`: {}",
                        pl_file_path.display(),
                        err
                    ));
                }
            }
        }
        Ok(())
    }
}

fn with_default_file_extension(
    mut path: std::path::PathBuf,
    extension: &'static str,
) -> std::path::PathBuf {
    if path.extension().is_none() {
        path.set_extension(extension);
    }
    path
}
