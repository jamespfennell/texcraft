use clap::{ArgEnum, Parser};

/// Format a property list (.pl) file
#[derive(Debug, Parser)]
struct Cli {
    /// Path to the property list file
    #[clap(parse(from_os_str))]
    pl_path: std::path::PathBuf,

    /// Overwrite the file with the formatted contents instead of writing to stdout
    #[clap(short, long)]
    overwrite: bool,

    /// Check that the file is a valid propertly list file
    #[clap(short, long)]
    validate: bool,

    /// Number of spaces to indent property lists by (default 3)
    #[clap(short, long)]
    indent: Option<usize>,

    /// Style to apply to the closing parentheses of property lists
    #[clap(short, long, arg_enum)]
    closing_brace_style: Option<ClosingBraceStyle>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, ArgEnum)]
enum ClosingBraceStyle {
    SameLine,
    MatchingOpening,
    ExtraIndent,
}

fn main() {
    let args = Cli::parse();
    let pl_path = if args.pl_path.extension().is_none() {
        let mut pl_path = args.pl_path;
        pl_path.set_extension("pl");
        pl_path
    } else {
        args.pl_path
    };
    let input = match std::fs::read_to_string(&pl_path) {
        Ok(pl_contents) => pl_contents,
        Err(err) => {
            println!("Failed to open {:?}: {}", pl_path, err);
            std::process::exit(1);
        }
    };
    let file_name = match pl_path.to_str() {
        None => "",
        Some(s) => s,
    };
    if args.validate {
        if let Err(err) = tfm::pl::parse(file_name, &input) {
            println!["{}", err];
            std::process::exit(1);
        }
    }
    let style = tfm::pl::PlStyle {
        indent: args.indent.unwrap_or(3),
        closing_brace_style: match args.closing_brace_style {
            None => Default::default(),
            Some(ClosingBraceStyle::SameLine) => tfm::pl::ClosingBraceStyle::SameLine,
            Some(ClosingBraceStyle::MatchingOpening) => tfm::pl::ClosingBraceStyle::MatchingOpening,
            Some(ClosingBraceStyle::ExtraIndent) => tfm::pl::ClosingBraceStyle::ExtraIndent,
        },
    };
    let formatted = tfm::pl::format(file_name, &input, &style);
    println!["{}", formatted];
}
