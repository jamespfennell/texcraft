use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

macro_rules! include_str_from_corpus {
    ( $p: expr ) => {
        include_str![concat!["../../corpus/", $p]]
    };
}

macro_rules! include_bytes_from_corpus {
    ( $p: expr ) => {
        include_bytes![concat!["../../corpus/", $p]]
    };
}

#[derive(Default)]
struct Options {
    charcode_format: Option<&'static str>,
}

enum Binary {
    KnuthReImplementation,
    Tfmtools,
}

fn run_tfm_to_pl_test(
    tfm: &[u8],
    pl: &str,
    binary: Binary,
    options: Options,
    want_stderr: Option<&str>,
) {
    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    let (command, args) = match binary {
        Binary::KnuthReImplementation => {
            let mut v = vec![tfm_file_path.to_str().unwrap()];
            if let Some(charcode_format) = options.charcode_format {
                v.extend(["--charcode-format", charcode_format]);
            }
            ("tftopl", v)
        }
        Binary::Tfmtools => {
            let mut v = vec![];
            if let Some(charcode_format) = options.charcode_format {
                v.extend(["--pl-charcode-format", charcode_format]);
            }
            v.extend(["convert", tfm_file_path.to_str().unwrap()]);
            ("tfmtools", v)
        }
    };
    let mut cmd = Command::cargo_bin(command).unwrap();
    cmd.args(args);

    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    let want_success = !pl.is_empty();
    assert_eq!(
        want_success,
        output.status.success(),
        "want_success={want_success}, got_success={}, stderr={}",
        output.status.success(),
        stderr
    );
    if let Some(want_stderr) = want_stderr {
        similar_asserts::assert_eq!(texcraft: stderr, knuth: want_stderr.replace("\r\n", "\n"));
    }
    let got = String::from_utf8(output.stdout).unwrap();
    similar_asserts::assert_eq!(texcraft: got, knuth: pl.replace("\r\n", "\n"));
}

type PlToTfmArgsFn = for<'a> fn(pl_file_path: &'a str, tfm_file_path: &'a str) -> Vec<&'a str>;

fn run_pl_to_tfm_test(
    tfm: &[u8],
    pl: &str,
    command: &str,
    args_fn: PlToTfmArgsFn,
    want_stderr: Option<&str>,
) {
    let dir = tempfile::TempDir::new().unwrap();
    let pl_file_path = dir.path().join("input.plst");
    let mut pl_file = std::fs::File::create(&pl_file_path).unwrap();
    pl_file.write_all(pl.as_bytes()).unwrap();
    let tfm_file_path = dir.path().join("output.tfm");

    let mut cmd = Command::cargo_bin(command).unwrap();
    let args = args_fn(
        pl_file_path.to_str().unwrap(),
        tfm_file_path.to_str().unwrap(),
    );
    cmd.args(args);

    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(
        output.status.success(),
        "failed to run Texcraft command: {}",
        stderr
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    similar_asserts::assert_eq!(texcraft: stdout, knuth: "");
    if let Some(want_stderr) = want_stderr {
        similar_asserts::assert_eq!(texcraft: stderr, knuth: want_stderr.replace("\r\n", "\n"));
    }

    let got = std::fs::read(&tfm_file_path).unwrap();
    if got == tfm {
        return;
    }
    let want_tfm_file_path = dir.path().join("want.tfm");
    let mut want_tfm_file = std::fs::File::create(&want_tfm_file_path).unwrap();
    want_tfm_file.write_all(tfm).unwrap();

    similar_asserts::assert_eq!(texcraft: debug_tfm(&tfm_file_path), knuth: debug_tfm(&want_tfm_file_path));
    // It's possible that there is a bug in `tfmtools debug` such that different files have
    // the same debug output. For this case, we assert the bytes are equal too.
    assert_eq!(got, tfm);
}

fn debug_tfm(tfm_file_path: &std::path::PathBuf) -> String {
    let mut cmd = Command::cargo_bin("tfmtools").unwrap();
    cmd.args(vec!["debug", tfm_file_path.to_str().unwrap()]);
    let output = cmd.output().unwrap();
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap()
}

macro_rules! convert_tests {
    ( ($name: ident, roundtrip($base_path: expr) $(,)? ) ) => {
        convert_tests![($name, roundtrip($base_path, ".tfm", ".plst"))];
    };
    ( ($name: ident, roundtrip($base_path: expr, $tfm_path: expr, $pl_path: expr $(,)? ) $(,)? ) ) => {
        convert_tests![($name, roundtrip($base_path, $tfm_path, $pl_path, Default::default()))];
    };
    ( ($name: ident, roundtrip($base_path: expr, $tfm_path: expr, $pl_path: expr, $options: expr $(,)? ) $(,)? ) ) => {
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes_from_corpus!(concat![$base_path, $tfm_path]);
                const PL: &'static str = include_str_from_corpus!(concat![$base_path, $pl_path]);

                #[test]
                fn tfmtools_convert_tfm_to_pl() {
                    run_tfm_to_pl_test(TFM, PL, Binary::Tfmtools, $options, Some(""));
                }

                #[test]
                fn tftopl() {
                    run_tfm_to_pl_test(TFM, PL, Binary::KnuthReImplementation, $options, Some(""));
                }

                #[test]
                fn tfmtools_convert_pl_to_tfm() {
                    run_pl_to_tfm_test(TFM, PL, "tfmtools", |pl_file_path, tfm_file_path| {
                        vec!["convert", "-o", tfm_file_path, pl_file_path]
                    }, Some(""));
                }

                #[test]
                fn pltotf() {
                    run_pl_to_tfm_test(TFM, PL, "pltotf", |pl_file_path, tfm_file_path|{
                        vec![pl_file_path, tfm_file_path]
                    }, Some(""));
                }
            }
    };
    ( ($name: ident, pltotf($base_path: expr ) $(,)? ) ) => {
        convert_tests![($name, pltotf($base_path, ".plst", ".tfm"))];
    };
    ( ($name: ident, pltotf($base_path: expr, $pl_path: expr, $tfm_path: expr $(,)? ) $(,)? ) ) => {
        convert_tests![($name, __pltotf_internal($base_path, $pl_path, $tfm_path, ""))];
    };
    ( ($name: ident, pltotf_with_stderr($base_path: expr) $(,)? ) ) => {
        convert_tests![($name, pltotf_with_stderr($base_path, ".plst", ".tfm", ".stderr.txt"))];
    };
    ( ($name: ident, pltotf_with_stderr($base_path: expr, $pl_path: expr, $tfm_path: expr, $stderr_path: expr $(,)? ) $(,)? ) ) => {
        convert_tests![($name, __pltotf_internal($base_path, $pl_path, $tfm_path, include_str_from_corpus!(concat![$base_path, $stderr_path])))];
    };
    ( ($name: ident, __pltotf_internal($base_path: expr, $pl_path: expr, $tfm_path: expr, $stderr: expr ) ) ) => {
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes_from_corpus!(concat![$base_path, $tfm_path]);
                const PL: &'static str = include_str_from_corpus!(concat![$base_path, $pl_path]);
                const STDERR: &'static str = $stderr;

                #[test]
                fn tfmtools_convert_pl_to_tfm() {
                    run_pl_to_tfm_test(TFM, PL, "tfmtools", |pl_file_path, tfm_file_path| {
                        vec!["convert", "-o", tfm_file_path, pl_file_path]
                    }, None);
                }

                #[test]
                fn pltotf() {
                    run_pl_to_tfm_test(TFM, PL, "pltotf", |pl_file_path, tfm_file_path|{
                        vec![pl_file_path, tfm_file_path]
                    }, Some(STDERR));
                }
            }
    };
    ( ($name: ident, tftopl($base_path: expr ) $(,)? ) ) => {
        convert_tests![($name, tftopl($base_path, ".tfm", ".plst"))];
    };
    ( ($name: ident, tftopl($base_path: expr, $tfm_path: expr, $pl_path: expr $(,)? ) $(,)? ) ) => {
        convert_tests![($name, __tftopl_internal($base_path, $tfm_path, $pl_path, ""))];
    };
    ( ($name: ident, tftopl_with_stderr($base_path: expr) $(,)? ) ) => {
        convert_tests![($name, tftopl_with_stderr($base_path, ".tfm", ".plst", ".stderr.txt"))];
    };
    ( ($name: ident, tftopl_with_stderr($base_path: expr, $tfm_path: expr, $pl_path: expr, $stderr_path: expr $(,)? ) $(,)? ) ) => {
        convert_tests![($name, __tftopl_internal($base_path, $tfm_path, $pl_path, include_str_from_corpus!(concat![$base_path, $stderr_path])))];
    };
    ( ($name: ident, tftopl_fuzz($hash: expr) $(,)? ) ) => {
        convert_tests![($name, tftopl_with_stderr(concat!["fuzz/fuzz_tftopl_", $hash]))];
    };
    ( ($name: ident, __tftopl_internal($base_path: expr, $tfm_path: expr, $pl_path: expr, $stderr: expr ) ) ) => {
        mod $name {
            use super::*;
            const TFM: &'static [u8] = include_bytes_from_corpus!(concat![$base_path, $tfm_path]);
            const PL: &'static str = include_str_from_corpus!(concat![$base_path, $pl_path]);
            const WANT_STDERR: &'static str = $stderr;

            #[test]
            fn tfmtools_convert_tfm_to_pl() {
                run_tfm_to_pl_test(TFM, PL, Binary::Tfmtools, Default::default(), None);
            }

            #[test]
            fn tftopl() {
                run_tfm_to_pl_test(TFM, PL, Binary::KnuthReImplementation, Default::default(), Some(WANT_STDERR));
            }
        }
};
    ( $( $piece: tt , )+ ) => {
        $(
            convert_tests!($piece);
        )+
    };
}

convert_tests!(
    // TODO: make sure all of the files in the corpus are here
    // Maybe order them alphabetically
    // E.g. lig-kern-redirect-254.plst is not here
    (cmr10, roundtrip("computer-modern/cmr10")),
    (
        cmr10_ascii,
        roundtrip(
            "computer-modern/cmr10",
            ".tfm",
            "_ascii.plst",
            Options {
                charcode_format: Some("ascii")
            },
        ),
    ),
    (
        cmr10_octal,
        roundtrip(
            "computer-modern/cmr10",
            ".tfm",
            "_octal.plst",
            Options {
                charcode_format: Some("octal")
            },
        ),
    ),
    (cmss8, roundtrip("computer-modern/cmss8")),
    (cmex10, roundtrip("computer-modern/cmex10")),
    (cminch, roundtrip("computer-modern/cminch")),
    (cmsy7, roundtrip("computer-modern/cmsy7")),
    (many_ligatures, roundtrip("originals/many-ligatures")),
    (params, roundtrip("originals/font-dimen")),
    (boundary_char, roundtrip("originals/boundarychar")),
    (
        boundary_char_unspecified,
        roundtrip("originals/boundarychar-unspecified"),
    ),
    (
        boundary_char_no_entrypoint,
        roundtrip("originals/boundarychar-noentrypoint"),
    ),
    (many_entrypoints, roundtrip("originals/many-entrypoints")),
    (
        theano_old_style,
        roundtrip("ctan/TheanoOldStyle-Bold-tlf-t1--base"),
    ),
    (
        orphan_lig_kerns_1_2,
        pltotf("originals/orphan-lig-kerns-", "1.plst", "2.tfm"),
    ),
    (
        orphan_lig_kerns_2_3,
        tftopl("originals/orphan-lig-kerns-", "2.tfm", "3.plst"),
    ),
    (
        orphan_lig_kerns_3_4,
        pltotf("originals/orphan-lig-kerns-", "3.plst", "4.tfm"),
    ),
    (
        orphan_lig_kerns_4_5,
        roundtrip("originals/orphan-lig-kerns-", "4.tfm", "5.plst"),
    ),
    (
        arev_sans_1_2,
        tftopl_with_stderr(
            "ctan/ArevSans-BoldOblique-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        arev_sans_2_3,
        pltotf("ctan/ArevSans-BoldOblique-", "2.plst", "3.tfm")
    ),
    (
        arev_sans_3_4,
        roundtrip("ctan/ArevSans-BoldOblique-", "3.tfm", "4.plst"),
    ),
    (
        dimen_index_out_of_bounds_1_2,
        tftopl_with_stderr(
            "originals/dimen-index-out-of-bounds-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        dimen_index_out_of_bounds_3_4,
        roundtrip("originals/dimen-index-out-of-bounds-", "3.tfm", "4.plst"),
    ),
    (smfebsl10_1_2, tftopl("ctan/smfebsl10-", "1.tfm", "2.plst")),
    (smfebsl10_2_3, pltotf("ctan/smfebsl10-", "2.plst", "3.tfm")),
    (
        smfebsl10_3_4,
        roundtrip("ctan/smfebsl10-", "3.tfm", "4.plst"),
    ),
    (cprbn8t, roundtrip("ctan/cprbn8t")),
    (rashii2_1_2, tftopl("ctan/rashii2-", "1.tfm", "2.plst")),
    (rashii2_2_3, pltotf("ctan/rashii2-", "2.plst", "3.tfm")),
    (rashii2_3_4, roundtrip("ctan/rashii2-", "3.tfm", "4.plst")),
    (veracruz_6vcr8r, roundtrip("ctan/6vcr8r")),
    (empty_plst_file, pltotf("originals/empty", ".plst", ".tfm")),
    (empty_varchar, pltotf("originals/empty-varchar")),
    (zero_width_chars, pltotf("originals/zero-width-char")),
    (ligature_loop, pltotf_with_stderr("originals/ligature-loop")),
    (
        next_larger_loop_pl,
        pltotf_with_stderr("originals/next-larger-loop-pl"),
    ),
    (
        theano_old_style_bold_tlf_lgr,
        pltotf("ctan/TheanoOldStyle-Bold-tlf-lgr"),
    ),
    /* TODO: fix the bug and enable
    (
        dimen_index_out_of_bounds_2_3,
        "originals/dimen-index-out-of-bounds-3.tfm",
        "originals/dimen-index-out-of-bounds-2.plst",
        include_str_from_corpus!["originals/dimen-index-out-of-bounds-3.stderr.txt"],
    ),
     */
    (
        bxjatoucs_jis,
        tftopl("ctan/bxjatoucs-jis-", "1.tfm", "2.plst"),
    ),
    /*
    The problem in this one is that pltotf imposes max params of 254.
    (It's not 255 because in the CST -> AST step, number bigger than this become 255
    due to how Knuth does the get_byte method. So with 254 we can still check for this condition)
    Do we want to support that?
        (
        bxjatoucs_jis_roundtrip,
        "ctan/bxjatoucs-jis-3.tfm",
        "ctan/bxjatoucs-jis-2.plst",
    ),
     */
    (aebkri, pltotf("ctan/aebkri")),
    (mt2exa, pltotf("ctan/mt2exa")),
    (copti, pltotf("ctan/copti")),
    (bxjatoucs_cid, pltotf_with_stderr("ctan/bxjatoucs-cid")),
    (
        xcharter_bolditalic_tlf_ot1g,
        pltotf("ctan/XCharter-BoldItalic-tlf-ot1G"),
    ),
    (empty_coding_scheme, tftopl("ctan/md-utree")),
    (txbmi, tftopl("ctan/txbmi")),
    (md_grbr7m, tftopl("ctan/md-grbr7m")),
    (xcyeuat12, tftopl("ctan/xyeuat12")),
    (
        non_zero_first_dimens,
        tftopl_with_stderr("originals/non-zero-first-dimens"),
    ),
    (
        duplicate_lig_stop_warning,
        tftopl_with_stderr("originals/duplicate-lig-stop-warnings"),
    ),
    (
        duplicate_lig_kern_warning,
        tftopl_with_stderr(
            "originals/duplicate-lig-kern-warnings-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        quicspool_wwfonts_i_1_2,
        tftopl_with_stderr(
            "ctan/quicspool_wwfonts_i-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        quicspool_wwfonts_b_1_2,
        tftopl_with_stderr(
            "ctan/quicspool_wwfonts_b-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        quicspool_wwfonts_cw_1_2,
        tftopl_with_stderr(
            "ctan/quicspool_wwfonts_cw-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        quicspool_wwfonts_r_1_2,
        tftopl_with_stderr(
            "ctan/quicspool_wwfonts_r-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        number_limit_16,
        tftopl_with_stderr(
            "originals/number-limit-16-",
            "1.tfm",
            "2.plst",
            "2.stderr.txt"
        ),
    ),
    (
        next_larger_loop_tfm,
        tftopl_with_stderr("originals/next-larger-loop-tfm"),
    ),
    (zero_design_size, tftopl_fuzz("8512ff447bf762fe")),
    (lig_kern_out_of_bounds, tftopl_fuzz("794bb506a827c3f")),
    (truncated_string_in_header, tftopl_fuzz("f89546ae5b0f1d5d")),
    (
        header_size_too_big_a,
        tftopl("originals/large-string-length-a"),
    ),
    (
        header_size_too_big_b,
        tftopl_with_stderr("originals/large-string-length-b"),
    ),
    (
        bad_chars_in_string,
        tftopl_with_stderr("originals/bad-chars-in-string"),
    ),
    (
        lig_kern_instruction_references_non_existent_character,
        tftopl_fuzz("10e324e0595b2934"),
    ),
    (
        extensible_character_references_non_existent_character,
        tftopl_fuzz("e1f1c0de87caa4a0"),
    ),
    (
        lig_kern_empty_invisible_section,
        tftopl_fuzz("764173f545c18b72"),
    ),
    (
        lig_kern_stop_address_too_big,
        tftopl_fuzz("8ab6f071335a4abc"),
    ),
    (infinite_ligature_loop, tftopl_fuzz("273fbef691836675")),
    (
        invalid_boundary_char_entrypoint,
        tftopl_fuzz("f49074da35aa2807"),
    ),
    (
        invalid_rep_in_extensible_recipe,
        tftopl_fuzz("1c72e451cd0e25e7"),
    ),
    (
        non_existent_lig_kern_duplicate_warning,
        tftopl_fuzz("e87827104f9cc5c4"),
    ),
    (
        lig_kern_unused_part_of_program_edge_case,
        tftopl_fuzz("622eb29085ef8389"),
    ),
    (header_warnings_ordering, tftopl_fuzz("b38d3def70d7a026")),
    (string_with_non_ascii_chars, tftopl_fuzz("d35c51fe3046ea8f")),
    (
        lig_kern_label_boundary_char,
        tftopl_fuzz("e604027d3275b06e"),
    ),
    (slant_param_is_i32_min, tftopl_fuzz("ef0432d46c78f8a")),
    (phantom_ligature_bug, tftopl_fuzz("90cbce0c1d734f4e")),
    (
        phantom_ligature_bug_minimal_repro_1,
        tftopl_with_stderr("originals/phantom-ligature-bug-minimal-repro-1"),
    ),
    (
        phantom_ligature_bug_minimal_repro_2,
        tftopl_with_stderr("originals/phantom-ligature-bug-minimal-repro-2"),
    ),
    (
        left_boundary_char_infinite_loop,
        tftopl_with_stderr("originals/left-boundary-char-infinite-loop"),
    ),
    (
        right_boundary_char_creates_infinite_loop_a,
        tftopl_with_stderr("originals/right-boundary-char-creates-infinite-loop-a"),
    ),
    (
        right_boundary_char_creates_infinite_loop_b,
        tftopl_with_stderr("originals/right-boundary-char-creates-infinite-loop-b"),
    ),
    (
        right_boundary_char_breaks_infinite_loop_a,
        tftopl("originals/right-boundary-char-breaks-infinite-loop-a"),
    ),
    (
        right_boundary_char_breaks_infinite_loop_b,
        tftopl_with_stderr("originals/right-boundary-char-breaks-infinite-loop-b"),
    ),
    (gk256g, tftopl_with_stderr("ctan/gk256g")),
    (
        unusual_num_params,
        tftopl_with_stderr("originals/unusual-num-params"),
    ),
    (
        infinite_loop_error_ordering_a,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-a")
    ),
    (
        infinite_loop_error_ordering_b,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-b")
    ),
    (
        infinite_loop_error_ordering_c,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-c")
    ),
    (
        infinite_loop_error_ordering_d,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-d")
    ),
    (
        infinite_loop_error_ordering_e,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-e")
    ),
    (
        infinite_loop_error_ordering_f,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-f")
    ),
    (
        infinite_loop_error_ordering_g,
        tftopl_with_stderr("originals/infinite-loop-error-ordering-g")
    ),
    (
        infinite_loop_error_ordering_fuzz_1,
        tftopl_fuzz("43c85ba5be9a35ef")
    ),
    (
        infinite_loop_error_ordering_fuzz_2,
        tftopl_fuzz("6915a258ab846ae4")
    ),
    (duplicate_kern_warning, tftopl_fuzz("d1cf983ef16e54f3")),
);
