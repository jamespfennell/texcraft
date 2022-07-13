use rand::Rng;
use std::io::Write;
use std::process::Command;
use std::process::Stdio;
use texlang_stdlib::script;
use texlang_stdlib::StdLibState;

pub fn run_in_texcraft(input: &str) {
    let mut env = StdLibState::new();
    env.set_command("par", script::get_par());
    env.set_command("end", script::get_newline());
    env.push_source("".to_string(), input.to_string()).unwrap();
    script::run(&mut env, true).unwrap();
}

pub fn host_has_pdftex() -> bool {
    Command::new("which")
        .arg("pdftex")
        .spawn()
        .expect("`which pdftex` command failed to start")
        .wait()
        .expect("failed to run `which pdftex`")
        .success()
}

pub fn run_in_pdftex(input: &str) {
    let mut child = Command::new("pdftex")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .spawn()
        .expect("pdftex command failed to start");
    let child_stdin = child.stdin.as_mut().unwrap();
    child_stdin
        .write_all(input.as_bytes())
        .expect("failed to write to pdfTeX");
    child.wait().expect("Failed to run pdfTeX");
}

use rand::prelude::Distribution;

static RANDOM_CS_NAMES: [&str; 19] = [
    "def", "gdef", "edef", "xdef", "let", "ifcase", "ifnum", "iftrue", "iffalse", "else", "fi",
    "advance", "multiply", "divide", "time", "day", "month", "year", "end",
];

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
    weights: &Weights,
) -> String {
    let mut result = String::new();
    result.push_str("% This TeX document was randomly genenerated by randtex, a program of the Texcraft project.\n");
    result.push_str(
        "% Running the document is a no-op except that \\macro will be defined at the end.\n",
    );

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
    line_length_bounds: (usize, usize),
    num_lines: usize,
    weights: &Weights,
) -> String {
    let dist = rand::distributions::WeightedIndex::new(&[
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
                    temp = format![
                        "\\{} ",
                        RANDOM_CS_NAMES[rng.gen_range(0..RANDOM_CS_NAMES.len())]
                    ];
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
