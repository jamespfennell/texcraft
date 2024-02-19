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

impl CharcodeFormat {
    #[allow(dead_code)]
    pub fn to_display_format(
        &self,
        character_coding_scheme: &Option<String>,
    ) -> tfm::pl::CharDisplayFormat {
        match self {
            Self::Default => {
                let scheme = match character_coding_scheme {
                    None => String::new(),
                    Some(scheme) => scheme.to_uppercase(),
                };
                if scheme.starts_with("TEX MATH SY") || scheme.starts_with("TEX MATH EX") {
                    tfm::pl::CharDisplayFormat::Octal
                } else {
                    tfm::pl::CharDisplayFormat::Default
                }
            }
            Self::Ascii => tfm::pl::CharDisplayFormat::Ascii,
            Self::Octal => tfm::pl::CharDisplayFormat::Octal,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TfOrPlPath {
    Tf(TfPath),
    Pl(PlPath),
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
    pub fn read(&self) -> Result<(Vec<u8>, tfm::format::File, Vec<tfm::format::Warning>), String> {
        let data = match std::fs::read(&self.0) {
            Ok(data) => data,
            Err(err) => return Err(format!("Failed to read `{}`: {}", self.0.display(), err)),
        };
        let (tfm_file_or, warnings) = tfm::format::File::deserialize(&data);
        for warning in &warnings {
            eprintln!("{}", warning.tftopl_message())
        }
        let tfm_file = match tfm_file_or {
            Ok(t) => t,
            Err(err) => return Err(err.tftopl_message()),
        };
        Ok((data, tfm_file, warnings))
    }
    fn parse(input: &str) -> Result<Self, InvalidExtension> {
        let path_buf: std::path::PathBuf = input.into();
        match path_buf.extension().and_then(std::ffi::OsStr::to_str) {
            Some("tfm") => Ok(TfPath(path_buf)),
            extension => Err(InvalidExtension {
                provided: extension.map(str::to_string),
                allowed: vec!["tfm"],
            }),
        }
    }
    #[allow(dead_code)]
    pub fn write(&self, content: &[u8]) -> Result<(), String> {
        match std::fs::write(&self.0, content) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Failed to write `{}`: {}", self.0.display(), err)),
        }
    }
}

impl clap::builder::ValueParserFactory for TfPath {
    type Parser = clap::builder::ValueParser;

    fn value_parser() -> Self::Parser {
        clap::builder::ValueParser::new(TfPath::parse)
    }
}

#[derive(Clone, Debug)]
pub struct PlPath(pub std::path::PathBuf);

impl PlPath {
    #[allow(dead_code)]
    pub fn read(&self) -> Result<tfm::pl::File, String> {
        let data = match std::fs::read_to_string(&self.0) {
            Ok(data) => data,
            Err(err) => return Err(format!("Failed to read `{}`: {}", self.0.display(), err)),
        };
        let (pl_file, warnings) = tfm::pl::File::from_pl_source_code(&data);
        let source = AriadneSource {
            path: self.0.clone(),
            source: data.into(),
        };
        for warning in &warnings {
            warning.ariadne_report().eprint(&source).unwrap();
        }
        Ok(pl_file)
    }
    #[allow(dead_code)]
    pub fn write(&self, content: &str) -> Result<(), String> {
        match std::fs::write(&self.0, content) {
            Ok(_) => Ok(()),
            Err(err) => Err(format!("Failed to write `{}`: {}", self.0.display(), err)),
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

pub struct AriadneSource {
    pub path: std::path::PathBuf,
    pub source: ariadne::Source,
}

impl ariadne::Cache<()> for &AriadneSource {
    fn fetch(&mut self, _: &()) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.source)
    }

    fn display<'a>(&self, _: &'a ()) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(format!["{}", self.path.display()]))
    }
}
