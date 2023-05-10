use rand::Rng;
use std::process::Command;
use std::process::Stdio;
use texlang_core::token::catcode;
use texlang_core::vm::VM;
use texlang_stdlib::script;
use texlang_stdlib::tracingmacros;
use texlang_stdlib::StdLibState;

pub fn run_in_texcraft(input: &str) {
    let mut initial_built_ins = StdLibState::all_initial_built_ins();
    initial_built_ins.insert("par", script::get_par());
    initial_built_ins.insert("end", script::get_newline());
    let mut vm = VM::<StdLibState>::new(
        catcode::CatCodeMap::new_with_tex_defaults(),
        initial_built_ins,
        Default::default(),
        Some(tracingmacros::hook),
    );
    vm.push_source("".to_string(), input.to_string()).unwrap();
    script::run(&mut vm, true).unwrap();
}

pub fn host_has_pdftex() -> bool {
    if let Ok(val) = std::env::var("SKIP_PDFTEX") {
        match val.parse::<usize>() {
            Ok(u) => {
                if u != 0 {
                    println!("Skipping pdftex benchmark because SKIP_PDFTEX={val}");
                    return false;
                }
            }
            Err(_) => {
                panic!("cannot parse SKIP_PDFTEX environment variable value {val} as an integer",)
            }
        }
    };
    Command::new("which")
        .arg("pdftex")
        .spawn()
        .expect("`which pdftex` command failed to start")
        .wait()
        .expect("failed to run `which pdftex`")
        .success()
}

pub fn run_in_pdftex(input: &str) {
    let mut input_path = std::env::temp_dir();
    input_path.push("pdftex-input");
    input_path.set_extension("tex");
    std::fs::write(&input_path, input).expect("Unable to write file");

    Command::new("pdftex")
        .arg(&input_path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .spawn()
        .expect("pdftex command failed to start")
        .wait()
        .expect("Failed to run pdfTeX");
}

use rand::prelude::Distribution;

pub struct Weights {
    pub begin_group: u32,
    pub end_group: u32,
    pub parameter: u32,
    pub space: u32,
    pub comment: u32,
    pub letter: u32,
    pub other: u32,
    pub control_sequence: u32,
}

impl Default for Weights {
    fn default() -> Self {
        Self {
            begin_group: 10,
            end_group: 10,
            parameter: 20,
            space: 20,
            comment: 5,
            letter: 200,
            other: 100,
            control_sequence: 100,
        }
    }
}

pub fn generate_random_tex_document(
    rng: &mut rand::prelude::StdRng,
    num_lines: usize,
    macro_length_bounds: (usize, usize),
    line_length_bounds: (usize, usize),
    num_cs_names: usize,
    weights: &Weights,
) -> String {
    let mut result = String::new();
    result.push_str("% This TeX document was randomly genenerated by randtex, a program of the Texcraft project.\n");
    result.push_str(
        "% Running the document is a no-op except that \\macro will be defined at the end.\n",
    );

    let cs_names = generate_random_cs_names(rng, num_cs_names, (5, 15));
    // 2 lines of commments and the final \end are always included
    let mut num_lines_generated: usize = 3;
    loop {
        let range = if macro_length_bounds.1 < macro_length_bounds.0 {
            macro_length_bounds.1..macro_length_bounds.1 + 1
        } else {
            macro_length_bounds.0..macro_length_bounds.1 + 1
        };
        // There is a risk that we generate too many lines.
        let range = if num_lines_generated + macro_length_bounds.1 + 2 > num_lines {
            if num_lines_generated + macro_length_bounds.0 + 2 >= num_lines {
                break;
            }
            // The range is non empty because of the inverse of the previous conditional.
            macro_length_bounds.0..num_lines - num_lines_generated - 2
        } else {
            range
        };
        let macro_length = rng.gen_range(range);
        result.push_str(&generate_random_tex_macro(
            rng,
            &cs_names,
            line_length_bounds,
            macro_length,
            weights,
        ));
        num_lines_generated += macro_length + 2;
    }
    result.push_str("\\end\n");
    result
}

pub fn generate_random_tex_macro(
    rng: &mut rand::prelude::StdRng,
    cs_names: &[String],
    line_length_bounds: (usize, usize),
    num_lines: usize,
    weights: &Weights,
) -> String {
    let dist = rand::distributions::WeightedIndex::new([
        weights.begin_group,
        weights.end_group,
        weights.parameter,
        weights.space,
        weights.comment,
        weights.letter,
        weights.other,
        weights.control_sequence,
    ])
    .unwrap();

    let mut result = String::with_capacity(num_lines * line_length_bounds.1 + 100);
    result.push_str("\\def\\macro#1#2#3{\n");
    for _ in 0..num_lines {
        result.push_str("  ");
        let mut commenting = false;
        let mut group_depth: u32 = 0;
        let line_length = if line_length_bounds.1 <= line_length_bounds.0 {
            line_length_bounds.1
        } else {
            rng.gen_range(line_length_bounds.0..line_length_bounds.1 + 1)
        };
        let mut i = 0;
        while i < line_length {
            let temp;
            let s = match dist.sample(rng) {
                0 => {
                    if !commenting {
                        group_depth += 1;
                    }
                    "{"
                }
                1 => {
                    if !commenting && group_depth == 0 {
                        continue;
                    }
                    if !commenting && group_depth > 0 {
                        group_depth -= 1;
                    }
                    "}"
                }
                2 => match rng.gen_range(0..4) {
                    0 => "#1",
                    1 => "#2",
                    2 => "#3",
                    _ => "##",
                },
                3 => " ",
                4 => {
                    for _ in 0..group_depth {
                        result.push('}');
                    }
                    group_depth = 0;
                    commenting = true;
                    "%"
                }
                5 => {
                    let ascii_offset = match rng.gen_range(0..4) {
                        0 => 65, // uppercase
                        _ => 97, // lowercase
                    };
                    temp = char::from_u32(ascii_offset + rng.gen_range(0..26))
                        .unwrap()
                        .to_string();
                    &temp
                }
                6 => match rng.gen_range(0..14) {
                    0 => "0",
                    1 => "1",
                    2 => "2",
                    3 => "3",
                    4 => "4",
                    5 => "5",
                    6 => "6",
                    7 => "7",
                    8 => "8",
                    9 => "9",
                    10 => ".",
                    11 => ",",
                    12 => ";",
                    _ => ":",
                },
                _ => {
                    temp = format!["\\{} ", cs_names[rng.gen_range(0..cs_names.len())]];
                    &temp
                }
            };
            i += s.len();
            result.push_str(s);
        }
        for _ in 0..group_depth {
            result.push('}');
        }
        result.push('\n');
    }
    result.push_str("}\n");
    result
}

pub fn generate_random_cs_names(
    rng: &mut rand::prelude::StdRng,
    n: usize,
    length_range: (usize, usize),
) -> Vec<String> {
    let mut names = Vec::with_capacity(n);
    for _ in 0..n {
        let len = rng.gen_range(length_range.0..length_range.1 + 1);
        let cs_name: String = rng
            .sample_iter(rand::distributions::Uniform::new_inclusive('a', 'z'))
            .take(len)
            .map(char::from)
            .collect();
        names.push(cs_name);
    }
    names
}
