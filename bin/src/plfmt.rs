use std::os;

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

    /// Number of spaces to indent property lists by (default 3)
    #[clap(short, long)]
    indent: Option<usize>,

    /// Style to apply to the closing parentheses of property lists
    #[clap(short, long, arg_enum)]
    closing_style: Option<ClosingStyle>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, ArgEnum)]
enum ClosingStyle {
    SameLine,
    Balanced,
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
    let pl_contents = match std::fs::read(&pl_path) {
        Ok(pl_contents) => pl_contents,
        Err(err) => {
            println!("Failed to open {:?}: {}", pl_path, err);
            std::process::exit(1);
        }
    };
    let indent = args.indent.unwrap_or(3);
    let closing_style = args.closing_style.unwrap_or(ClosingStyle::ExtraIndent);
}
