use clap::Parser;

mod common;

fn main() {
    if let Err(err) = Cli::parse().run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

/// Convert a human-readable property list (.pl) file to a TeX font metric (.tfm) file.
///
/// This is an alternative implementation of pltotf, built as part of the Texcraft project.
/// It is designed to have the same functionality and API as Knuth's original pltotf.
/// For additional conversion options, batch conversions, and other features,
///     consider using the Texcraft tfmtools binary.
#[derive(Debug, clap::Parser)]
#[command(
    name = "PLtoTF (Texcraft version)",
    author = "The Texcraft Project",
    version = "0.1",
    about,
    long_about,
    max_term_width(100)
)]
struct Cli {
    /// Path to the property list (.pl) file to convert.
    ///
    /// A file extension is optional, and will be set to .pl if missing.
    pl_file_path: std::path::PathBuf,

    /// Output path for the TeX font metric (.tfm) file.
    ///
    /// A file extension is optional, and will be set to .tfm if missing.
    ///
    /// If the output path is not specified,
    ///     the property list file path will be used with a .tfm extension set.
    tfm_file_path: Option<std::path::PathBuf>,

    /// Write some additional information to standard error while converting.
    ///
    /// This option just prints a banner on startup.
    /// It primarily exists for compatibility with Knuth's tftopl.
    #[arg(short, long)]
    verbose: bool,
}

impl Cli {
    fn run(self) -> Result<(), String> {
        if self.verbose {
            eprintln!("This is the Texcraft implementation of PLtoTF, version 0.1")
        }

        // Input
        let pl_file_path = with_default_file_extension(self.pl_file_path, "pl");
        let pl_data = match std::fs::read_to_string(&pl_file_path) {
            Ok(pl_data) => pl_data,
            Err(err) => {
                return Err(format!(
                    "Failed to read `{}`: {}",
                    pl_file_path.display(),
                    err
                ))
            }
        };

        // Conversion
        let (pl_file, warnings) = tfm::pl::File::from_pl_source_code(&pl_data);
        for warning in warnings {
            eprintln!("{}", warning.pltotf_message(&pl_data));
        }
        let tfm_file = tfm::format::File::from_pl_file(&pl_file);
        let tfm_output: Vec<u8> = tfm_file.serialize();

        // Output
        let tfm_file_path = match self.tfm_file_path {
            None => {
                let mut path: std::path::PathBuf = pl_file_path
                    .file_name()
                    .expect("the path points at a .pl file we've successfully opened")
                    .into();
                path.set_extension("tfm");
                path
            }
            Some(tfm_file_path) => with_default_file_extension(tfm_file_path, "tfm"),
        };
        if let Err(err) = std::fs::write(&tfm_file_path, tfm_output) {
            return Err(format!(
                "Failed to write file `{}`: {}",
                tfm_file_path.display(),
                err
            ));
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
