use clap::Parser;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::process::Command;

#[derive(Parser)]
#[command(about = "Hyphenate words using the plain TeX en-US patterns")]
struct Args {
    words: Vec<String>,

    #[arg(long, value_name = "FILE")]
    words_file: Option<String>,

    #[arg(long)]
    /// Suppress output. This is for when the tool is being used in performance testing,
    /// in which case printing should be skipped.
    no_output: bool,

    #[arg(long)]
    /// Checks the output of the tool is the same as in TeX. Requires `tex` to be available.
    validate: bool,

    #[arg(long)]
    /// Prints the time it took per hyphenation.
    time: bool,
}

fn main() {
    let args = Args::parse();
    let hyphenator = hyphenate::Hyphenator::plain_tex_en_us();

    let mut words: Vec<String> = args.words;

    if let Some(path) = args.words_file {
        let content = fs::read_to_string(&path).unwrap_or_else(|e| {
            eprintln!("error reading {path}: {e}");
            std::process::exit(1);
        });
        words.extend(content.split_whitespace().map(str::to_owned));
    }

    let tex_results = if args.validate {
        Some(tex_hyphenate(&words))
    } else {
        None
    };

    let mut errors = 0_usize;
    let start = std::time::Instant::now();
    let mut hyphenated = String::new();
    for word in &words {
        hyphenated.clear();
        hyphenator.hypthenate(word, &mut hyphenated);
        if !args.no_output {
            println!("{}", hyphenated);
        }
        if let Some(tex_results) = &tex_results {
            let Some(tex_result) = tex_results.get(word) else {
                eprintln!(
                    "Warning: failed to find {word} in TeX results, will skip validating this word"
                );
                continue;
            };
            if &hyphenated != tex_result {
                eprintln!("Validation error when hyphenating {word}: hyphenate={hyphenated} != tex={tex_result}");
                errors += 1;
            }
        }
    }
    if args.time {
        let elapsed: usize = start.elapsed().as_nanos().try_into().unwrap_or(usize::MAX);
        eprintln!("Took {:?}ns/hyphenation", elapsed / words.len());
    }
    if errors > 0 {
        eprintln!("{errors} errors");
        std::process::exit(1);
    }
}

fn tex_hyphenate(words: &[String]) -> HashMap<String, String> {
    let dir = tempfile::tempdir().unwrap_or_else(|e| {
        eprintln!("failed to create temp dir: {e}");
        std::process::exit(1);
    });
    // We need to use a TFM file without ligatures as ligatures can intefere with
    // hyphenation. An example is the word `chaffless`, where the ffl ligature
    // makes TeX ignore the hyphen after the second f.
    //
    // This TFM file was created by running:
    // cargo run --bin tfmtools -- convert crates/tfm/corpus/computer-modern/cmr10.plst --output cmr10-no-lig.tfm --remove-lig-kern-program
    let tfm_bytes = include_bytes!("cmr10-no-lig.tfm");
    fs::write(dir.path().join("cmr10-no-lig.tfm"), tfm_bytes).unwrap_or_else(|e| {
        eprintln!("failed to write tfm file: {e}");
        std::process::exit(1);
    });

    let tex_path = dir.path().join("input.tex");
    let mut file = fs::File::create(&tex_path).unwrap_or_else(|e| {
        eprintln!("failed to create temp file: {e}");
        std::process::exit(1);
    });
    writeln!(file, "\\font\\tenrm=cmr10-no-lig").unwrap();
    for word in words {
        writeln!(file, "\\showhyphens{{{word}}}").unwrap();
    }
    writeln!(file, "\\end").unwrap();
    drop(file);

    let output = Command::new("tex")
        .arg(&tex_path)
        .current_dir(dir.path())
        .output()
        .unwrap_or_else(|e| {
            eprintln!("failed to run tex: {e}");
            std::process::exit(1);
        });

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .lines()
        .filter_map(|line| line.strip_prefix("[] \\tenrm "))
        .map(|word| (hyphenate::strip_hyphens(word), word.to_string()))
        .collect()
}
