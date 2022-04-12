use clap::Parser;

#[derive(Parser)]
#[clap(version, name = "tftopl")]
struct Cli {
    /// Path to the TeX font metric (tfm) file
    #[clap(parse(from_os_str))]
    tfm_path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();
    let tfm_path = if args.tfm_path.extension().is_none() {
        let mut tfm_path = args.tfm_path;
        tfm_path.set_extension("tfm");
        tfm_path
    } else {
        args.tfm_path
    };
    let tfm_bytes = match std::fs::read(&tfm_path) {
        Ok(contents) => contents,
        Err(err) => {
            println!("Failed to open {:?}: {}", tfm_path, err);
            std::process::exit(1);
        }
    };
    let file = tfm::format::deserialize(&tfm_bytes);
    let pl_str = tfm::pl::write(&file, tfm::pl::Style::default());
    println!("{}", pl_str);
}
