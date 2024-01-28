use clap::Parser;

mod argtypes;

fn main() {
    let args = Cli::parse();
    if args.verbose {
        eprintln!("This is the Texcraft implementation of TFtoPL, version 0.1")
    }
    if let Err(err) = run(args) {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run(args: Cli) -> Result<(), String> {
    let tfm_file_path = with_default_file_extension(args.tfm_file_path, "tfm");
    // TODO: use argtypes
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
    let (tfm_file, warnings) = match tfm::deserialize_tfm(&tfm_data) {
        Ok(t) => t,
        Err(err) => return Err(err.tf_to_pl_message()),
    };
    for warning in warnings {
        eprintln!("{}", warning.tf_to_pl_message())
    }
    let pl_file = tfm::pl::File::from_tfm_file(tfm_file);
    let char_display_format = match args.charcode_format {
        argtypes::CharcodeFormat::Default => tfm::pl::CharDisplayFormat::Default,
        // TODO: handle the Tex fontex
        argtypes::CharcodeFormat::Ascii => tfm::pl::CharDisplayFormat::Ascii,
        argtypes::CharcodeFormat::Octal => tfm::pl::CharDisplayFormat::Octal,
    };
    let pl_output = format!["{}", pl_file.display(3, char_display_format)];
    match args.pl_file_path {
        None => print!("{pl_output}"),
        Some(pl_file_path) => {
            let pl_file_path = with_default_file_extension(pl_file_path, "pl");
            if let Err(err) = std::fs::write(&pl_file_path, pl_output) {
                return Err(format!(
                    "Failed to write `{}`: {}",
                    pl_file_path.display(),
                    err
                ));
            }
        }
    }
    Ok(())
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

/// Convert a TeX font metric (.tfm) file to a human-readable property list (.pl) file.
///
/// This is an alternative implementation of tftopl, built as part of the Texcraft project.
/// It is designed to have the same functionality and API as Knuth's original tftopl.
/// For additional conversion options, batch conversions, and other features,
///     consider using the Texcraft tfmtools binary.
#[derive(Debug, clap::Parser)]
#[command(
    name = "tftopl",
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
    /// If not provided, the property list file is printed to standard out.
    ///
    /// A file extension is optional, and will be set to .pl if missing.
    pl_file_path: Option<std::path::PathBuf>,

    /// Write some additional information to standard error while converting.
    ///
    /// This option just prints a banner on startup.
    /// It primarily exists for compatibility with Knuth's tftopl.
    #[arg(short, long)]
    verbose: bool,

    /// Specification for how to output characters.
    #[arg(short, long, default_value = "default")]
    charcode_format: argtypes::CharcodeFormat,
}
