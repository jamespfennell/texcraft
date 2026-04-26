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
