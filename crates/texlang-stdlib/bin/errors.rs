use texlang_core::*;
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
    err: Box<error::Error>,
}

fn run(case: ErrorCase) -> RunResult {
    let mut vm =
        vm::VM::<StdLibState>::new(StdLibState::all_initial_built_ins(), Default::default());
    vm.push_source("input.tex", case.source_code).unwrap();
    let err = match vm::run::<StdLibState, vm::DefaultHandlers>(&mut vm) {
        Ok(_) => panic!(
            "successfully ran {} (`{}`) but expected an error",
            case.description, case.source_code
        ),
        Err(err) => err,
    };
    RunResult { case, err }
}
