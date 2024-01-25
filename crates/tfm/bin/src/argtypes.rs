#[derive(Debug, Default, Clone, clap::ValueEnum)]
pub enum CharcodeFormat {
    /// Letters and numbers are output in PL ASCII format (e.g. `C A`), and
    /// other characters are output in octal (e.g. `O 14`).
    /// However if the font coding scheme starts with "TeX math sy" or "TeX math ex"
    /// then all characters are output in octal.
    #[default]
    Default,
    /// Visible ASCII characters except ( and ) are output in PL ASCII format (e.g. `C A`), and
    /// other characters are output in octal (e.g. `O 14`).
    Ascii,
    /// All characters are output in octal (e.g. `O 14`)
    Octal,
}

impl TfOrPlPath {
    pub fn parse(input: &str) -> Result<Self, InvalidExtension> {
        let path_buf: std::path::PathBuf = input.into();
        match path_buf.extension().and_then(std::ffi::OsStr::to_str) {
            Some("pl") => Ok(TfOrPlPath::Pl(PlPath(path_buf))),
            Some("tfm") => Ok(TfOrPlPath::Tf(TfPath(path_buf))),
            extension => Err(InvalidExtension {
                provided: extension.map(str::to_string),
                allowed: vec!["pl", "tfm"],
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TfOrPlPath {
    Tf(TfPath),
    Pl(PlPath),
}

impl clap::builder::ValueParserFactory for TfOrPlPath {
    type Parser = clap::builder::ValueParser;

    fn value_parser() -> Self::Parser {
        clap::builder::ValueParser::new(TfOrPlPath::parse)
    }
}

#[derive(Clone, Debug)]
pub struct TfPath(pub std::path::PathBuf);

impl TfPath {
    #[allow(dead_code)]
    pub fn read(&self) -> Result<(tfm::Font, Vec<tfm::DeserializeTfmWarning>), String> {
        let data = match std::fs::read(&self.0) {
            Ok(data) => data,
            Err(err) => return Err(format!("Failed to read `{}`: {}", self.0.display(), err)),
        };
        let (tfm_file, warnings) = match tfm::deserialize_tfm(&data) {
            Ok(t) => t,
            Err(err) => return Err(err.tf_to_pl_message()),
        };
        for warning in &warnings {
            eprintln!("{}", warning.tf_to_pl_message())
        }
        Ok((tfm_file, warnings))
    }
}

#[derive(Clone, Debug)]
pub struct PlPath(pub std::path::PathBuf);

impl PlPath {
    #[allow(dead_code)]
    pub fn write(&self, content: &str) -> Result<(), String> {
        match std::fs::write(&self.0, content) {
            Ok(_) => Ok(()),
            Err(err) => {
            Err(format!(
                "Failed to write `{}`: {}",
                self.0.display(),
                err
            ))
            }
        }
    }
}

#[derive(Debug)]
pub struct InvalidExtension {
    pub provided: Option<String>,
    pub allowed: Vec<&'static str>,
}

impl std::fmt::Display for InvalidExtension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let allowed = self
            .allowed
            .iter()
            .map(|s| format![".{s}"])
            .collect::<Vec<String>>()
            .join(" or ");
        match &self.provided {
            None => write!(
                f,
                "the file extension must be {} but it is missing",
                allowed
            ),
            Some(extension) => write!(
                f,
                "the file extension must be {} but it is .{}",
                allowed, extension
            ),
        }
    }
}

impl std::error::Error for InvalidExtension {}
