use clap::Parser;
use rand::SeedableRng;

/// Generate random no-op TeX files.
///
/// This program generates random TeX files that do nothing.
/// Specifically, the TeX files repeatedly redefine a macro \macro
///   that has 3 parameters and a replacement text with random
///   tokens of different types (control sequences, letters, parameters, etc).
/// The program was developed in order to compare the speeds of the
///   lexers in Texcraft and pdfTeX.
///
/// This program is a part of the Texcraft project: `<https://texcraft.dev>`.
#[derive(Parser)]
#[clap(version, name = "randtex")]
struct Cli {
    /// Seed to initialize the random number generator.
    ///
    /// Use this to generate output that is deterministic across runs.
    /// If not specified the random number generator is seeded from the operating system's
    /// random number generator.
    #[clap(short, long)]
    seed: Option<u64>,

    /// Number of lines of output to generate. Default: 20.
    #[clap(short, long)]
    num_lines: Option<usize>,

    /// Minimum width of lines within the macro definition in the output. Default: 80.
    #[clap(long)]
    line_length_min: Option<usize>,

    /// Maximum width of lines in the output. Default: 100.
    #[clap(long)]
    line_length_max: Option<usize>,

    /// Minimum number of lines in each macro definition. Default: 10.
    #[clap(long)]
    macro_length_min: Option<usize>,

    /// Maximum number of lines in each macro definition. Default: 30.
    #[clap(long)]
    macro_length_max: Option<usize>,

    /// Number of distinct control sequences. Default: 100.
    #[clap(long)]
    num_cs_names: Option<usize>,
}

fn main() {
    let args: Cli = Cli::parse();
    let weights = Default::default();
    let mut rng = match args.seed {
        None => rand::prelude::StdRng::from_entropy(),
        Some(seed) => rand::prelude::StdRng::seed_from_u64(seed),
    };
    print![
        "{}",
        performance::generate_random_tex_document(
            &mut rng,
            args.num_lines.unwrap_or(20),
            (
                args.macro_length_min.unwrap_or(10),
                args.macro_length_max.unwrap_or(30)
            ),
            (
                args.line_length_min.unwrap_or(80),
                args.line_length_max.unwrap_or(100)
            ),
            args.num_cs_names.unwrap_or(100),
            &weights
        )
    ];
}
