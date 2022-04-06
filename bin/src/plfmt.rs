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
    if args.validate {
        if let Err(err) = tfm::parse_pl(&input) {
            println!["{:?}", err];
            std::process::exit(1);
        }
    }
    let style = tfm::PlStyle {
        indent: args.indent.unwrap_or(3),
        closing_brace_style: match args.closing_brace_style {
            None => Default::default(),
            Some(ClosingBraceStyle::SameLine) => tfm::ClosingBraceStyle::SameLine,
            Some(ClosingBraceStyle::MatchingOpening) => tfm::ClosingBraceStyle::MatchingOpening,
            Some(ClosingBraceStyle::ExtraIndent) => tfm::ClosingBraceStyle::ExtraIndent,
        },
    };
    let formatted = tfm::format_pl(&input, &style);
    println!["{}", formatted];
}
