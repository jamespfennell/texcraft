use texlang::*;
use texlang_stdlib::*;

fn main() {
    println!("# All Texlang errors\n");
    for case in ErrorCase::all_error_cases() {
        let result = run(case);
        println!(
            "## {}\n\nInput:\n```\n{}\n```\nOutput:\n```\n{}```\n\n",
            result.case.description, result.case.source_code, result.err,
        )
    }
}

struct RunResult {
    case: ErrorCase,
    err: Box<error::TracedTexError>,
}

fn run(case: ErrorCase) -> RunResult {
    let mut vm = vm::VM::<StdLibState>::new();
    vm.push_source("input.tex", case.source_code).unwrap();
    let err = match vm.run::<vm::DefaultHandlers>() {
        Ok(_) => panic!(
            "successfully ran {} (`{}`) but expected an error",
            case.description, case.source_code
        ),
        Err(err) => err,
    };
    RunResult { case, err }
}
