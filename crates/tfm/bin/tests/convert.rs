use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

type TfmToPlArgsFn = for<'a> fn(tfm_file_path: &'a str) -> Vec<&'a str>;

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

fn run_tfm_to_pl_test(
    tfm: &[u8],
    pl: &str,
    command: &str,
    args_fn: TfmToPlArgsFn,
    want_stderr: Option<&str>,
    want_success: bool,
) {
    let dir = tempfile::TempDir::new().unwrap();
    let tfm_file_path = dir.path().join("input.tfm");
    let mut tfm_file = std::fs::File::create(&tfm_file_path).unwrap();
    tfm_file.write_all(tfm).unwrap();

    let mut cmd = Command::cargo_bin(command).unwrap();
    let args = args_fn(tfm_file_path.to_str().unwrap());
    cmd.args(args);

    let output = cmd.output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    if want_success {
        assert!(
            output.status.success(),
            "failed to run Texcraft command: {}",
            stderr
        );
    }
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
    ( $( ($name: ident, $tfm_path: expr, $pl_path: expr $(, $charcode_format: expr, )? $(,)? ), )+ ) => {
        $(
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes_from_corpus!($tfm_path);
                const PL: &'static str = include_str_from_corpus!($pl_path);

                #[test]
                fn tfmtools_convert_tfm_to_pl() {
                    run_tfm_to_pl_test(TFM, PL, "tfmtools", |tfm_file_path| {
                        let mut v: Vec<&str> = vec![];
                        $(
                            v.extend(["--pl-charcode-format", $charcode_format]);
                        )?
                        v.extend(["convert", tfm_file_path]);
                        v
                    }, Some(""), true);
                }

                #[test]
                fn tftopl() {
                    run_tfm_to_pl_test(TFM, PL, "tftopl", |tfm_file_path|{
                        let mut v: Vec<&str> = vec![];
                        v.extend([tfm_file_path]);
                        $(
                            v.extend(["--charcode-format", $charcode_format]);
                        )?
                        v
                    }, Some(""), true);
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
        )+
    };
}

macro_rules! convert_pltotf_tests {
    ( $( ($name: ident, $tfm_path: expr, $pl_path: expr, $stderr: expr $(,)? ), )+ ) => {
        $(
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes_from_corpus!($tfm_path);
                const PL: &'static str = include_str_from_corpus!($pl_path);
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
        )+
    };
}

macro_rules! convert_tftopl_tests {
    ( $( ($name: ident, $tfm_path: expr, $pl: expr, $stderr: expr, $want_success: expr $(,)? ), )+ ) => {
        $(
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes_from_corpus!($tfm_path);
                const PL: &'static str = $pl;
                const WANT_STDERR: &'static str = $stderr;
                const WANT_SUCCESS: bool = $want_success;

                #[test]
                fn tfmtools_convert_tfm_to_pl() {
                    run_tfm_to_pl_test(TFM, PL, "tfmtools", |tfm_file_path| {
                            vec!["convert", tfm_file_path]
                    }, None, WANT_SUCCESS);
                }

                #[test]
                fn tftopl() {
                    run_tfm_to_pl_test(TFM, PL, "tftopl", |tfm_file_path|{
                            vec![tfm_file_path]
                    },Some(WANT_STDERR), WANT_SUCCESS);
                }
            }
        )+
    };
}

convert_tests!(
    (
        cmr10,
        "computer-modern/cmr10.tfm",
        "computer-modern/cmr10.plst"
    ),
    (
        cmr10_ascii,
        "computer-modern/cmr10.tfm",
        "computer-modern/cmr10_ascii.plst",
        "ascii",
    ),
    (
        cmr10_octal,
        "computer-modern/cmr10.tfm",
        "computer-modern/cmr10_octal.plst",
        "octal",
    ),
    (
        cmss8,
        "computer-modern/cmss8.tfm",
        "computer-modern/cmss8.plst"
    ),
    (
        cmex10,
        "computer-modern/cmex10.tfm",
        "computer-modern/cmex10.plst"
    ),
    (
        cminch,
        "computer-modern/cminch.tfm",
        "computer-modern/cminch.plst"
    ),
    (
        cmsy7,
        "computer-modern/cmsy7.tfm",
        "computer-modern/cmsy7.plst"
    ),
    (
        many_ligatures,
        "originals/many-ligatures.tfm",
        "originals/many-ligatures.plst",
    ),
    (
        params,
        "originals/font-dimen.tfm",
        "originals/font-dimen.plst",
    ),
    (
        boundary_char,
        "originals/boundarychar.tfm",
        "originals/boundarychar.plst",
    ),
    (
        boundary_char_unspecified,
        "originals/boundarychar-unspecified.tfm",
        "originals/boundarychar-unspecified.plst",
    ),
    (
        boundary_char_no_entrypoint,
        "originals/boundarychar-noentrypoint.tfm",
        "originals/boundarychar-noentrypoint.plst",
    ),
    (
        many_entrypoints,
        "originals/many-entrypoints.tfm",
        "originals/many-entrypoints.plst",
    ),
    (
        theano_old_style,
        "ctan/TheanoOldStyle-Bold-tlf-t1--base.tfm",
        "ctan/TheanoOldStyle-Bold-tlf-t1--base.plst",
    ),
    (
        orphan_lig_kerns_4_5,
        "originals/orphan-lig-kerns-4.tfm",
        "originals/orphan-lig-kerns-5.plst",
    ),
    (
        arev_sans_3_4,
        "ctan/ArevSans-BoldOblique-3.tfm",
        "ctan/ArevSans-BoldOblique-4.plst",
    ),
    (
        dimen_index_out_of_bounds_3_4,
        "originals/dimen-index-out-of-bounds-3.tfm",
        "originals/dimen-index-out-of-bounds-4.plst",
    ),
    (
        smfebsl10_3_4,
        "ctan/smfebsl10-3.tfm",
        "ctan/smfebsl10-4.plst",
    ),
    (cprbn8t, "ctan/cprbn8t.tfm", "ctan/cprbn8t.plst",),
    (rashii2_3_4, "ctan/rashii2-3.tfm", "ctan/rashii2-4.plst",),
    (veracruz_6vcr8r, "ctan/6vcr8r.tfm", "ctan/6vcr8r.plst",),
);

convert_pltotf_tests!(
    (rashii2_2_3, "ctan/rashii2-3.tfm", "ctan/rashii2-2.plst", ""),
    (empty, "originals/empty.tfm", "originals/empty.plst", "",),
    (
        empty_varchar,
        "originals/empty-varchar.tfm",
        "originals/empty-varchar.plst",
        "",
    ),
    (
        zero_width_chars,
        "originals/zero-width-char.tfm",
        "originals/zero-width-char.plst",
        "",
    ),
    (
        ligature_loop,
        "originals/ligature-loop.tfm",
        "originals/ligature-loop.plst",
        include_str_from_corpus!["originals/ligature-loop.stderr.txt"],
    ),
    (
        next_larger_loop_pl,
        "originals/next-larger-loop-pl.tfm",
        "originals/next-larger-loop-pl.plst",
        include_str_from_corpus!["originals/next-larger-loop-pl.stderr.txt"],
    ),
    (
        orphan_lig_kerns_1_2,
        "originals/orphan-lig-kerns-2.tfm",
        "originals/orphan-lig-kerns-1.plst",
        "",
    ),
    (
        orphan_lig_kerns_3_4,
        "originals/orphan-lig-kerns-4.tfm",
        "originals/orphan-lig-kerns-3.plst",
        "",
    ),
    (
        arev_sans_2_3,
        "ctan/ArevSans-BoldOblique-3.tfm",
        "ctan/ArevSans-BoldOblique-2.plst",
        "",
    ),
    (
        theano_old_style_bold_tlf_lgr,
        "ctan/TheanoOldStyle-Bold-tlf-lgr.tfm",
        "ctan/TheanoOldStyle-Bold-tlf-lgr.plst",
        "",
    ),
    /* TODO: fix the bug and enable
    (
        dimen_index_out_of_bounds_2_3,
        "originals/dimen-index-out-of-bounds-3.tfm",
        "originals/dimen-index-out-of-bounds-2.plst",
        include_str_from_corpus!["originals/dimen-index-out-of-bounds-3.stderr.txt"],
    ),
     */

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
    (
        smfebsl10_2_3,
        "ctan/smfebsl10-3.tfm",
        "ctan/smfebsl10-2.plst",
        "",
    ),
    (aebkri, "ctan/aebkri.tfm", "ctan/aebkri.plst", "",),
    (mt2exa, "ctan/mt2exa.tfm", "ctan/mt2exa.plst", "",),
    (copti, "ctan/copti.tfm", "ctan/copti.plst", "",),
    (
        bxjatoucs_cid,
        "ctan/bxjatoucs-cid.tfm",
        "ctan/bxjatoucs-cid.plst",
        include_str_from_corpus!["ctan/bxjatoucs-cid.stderr.txt"],
    ),
    (
        xcharter_bolditalic_tlf_ot1g,
        "ctan/XCharter-BoldItalic-tlf-ot1G.tfm",
        "ctan/XCharter-BoldItalic-tlf-ot1G.plst",
        "",
    ),
);

convert_tftopl_tests!(
    (
        rashii2_1_2,
        "ctan/rashii2-1.tfm",
        include_str_from_corpus!("ctan/rashii2-2.plst"),
        "",
        true,
    ),
    (
        gk256g,
        "ctan/gk256g.tfm",
        "",
        include_str_from_corpus!["ctan/gk256g.stderr.txt"],
        false,
    ),
    (
        orphan_lig_kerns_2_3,
        "originals/orphan-lig-kerns-2.tfm",
        include_str_from_corpus!("originals/orphan-lig-kerns-3.plst"),
        "",
        true,
    ),
    (
        txbmi,
        "ctan/txbmi.tfm",
        include_str_from_corpus!("ctan/txbmi.plst"),
        "",
        true,
    ),
    (
        unusual_num_params,
        "originals/unusual-num-params.tfm",
        include_str_from_corpus!("originals/unusual-num-params.plst"),
        "Unusual number of fontdimen parameters for a math symbols font (0 not 22).\n",
        true,
    ),
    (
        empty_coding_scheme,
        "ctan/md-utree.tfm",
        include_str_from_corpus!("ctan/md-utree.plst"),
        "",
        true,
    ),
    (
        md_grbr7m,
        "ctan/md-grbr7m.tfm",
        include_str_from_corpus!("ctan/md-grbr7m.plst"),
        "",
        true,
    ),
    (
        xcyeuat12,
        "ctan/xyeuat12.tfm",
        include_str_from_corpus!("ctan/xyeuat12.plst"),
        "",
        true,
    ),
    (
        arev_sans_1_2,
        "ctan/ArevSans-BoldOblique-1.tfm",
        include_str_from_corpus!("ctan/ArevSans-BoldOblique-2.plst"),
        include_str_from_corpus!("ctan/ArevSans-BoldOblique-2.stderr.txt"),
        true,
    ),
    (
        non_zero_first_dimens,
        "originals/non-zero-first-dimens.tfm",
        include_str_from_corpus!("originals/non-zero-first-dimens.plst"),
        include_str_from_corpus!("originals/non-zero-first-dimens.stderr.txt"),
        true,
    ),
    (
        dimen_index_out_of_bounds_1_2,
        "originals/dimen-index-out-of-bounds-1.tfm",
        include_str_from_corpus!("originals/dimen-index-out-of-bounds-2.plst"),
        include_str_from_corpus!("originals/dimen-index-out-of-bounds-2.stderr.txt"),
        true,
    ),
    (
        duplicate_lig_stop_warning,
        "originals/duplicate-lig-stop-warnings.tfm",
        include_str_from_corpus!("originals/duplicate-lig-stop-warnings.plst"),
        include_str_from_corpus!("originals/duplicate-lig-stop-warnings.stderr.txt"),
        true,
    ),
    (
        duplicate_lig_kern_warning,
        "originals/duplicate-lig-kern-warnings-1.tfm",
        include_str_from_corpus!("originals/duplicate-lig-kern-warnings-2.plst"),
        include_str_from_corpus!("originals/duplicate-lig-kern-warnings-2.stderr.txt"),
        true,
    ),
    (
        smfebsl10_1_2,
        "ctan/smfebsl10-1.tfm",
        include_str_from_corpus!("ctan/smfebsl10-2.plst"),
        "",
        true,
    ),
    (
        bxjatoucs_jis,
        "ctan/bxjatoucs-jis-1.tfm",
        include_str_from_corpus!("ctan/bxjatoucs-jis-2.plst"),
        "",
        true,
    ),
    (
        quicspool_wwfonts_i_1_2,
        "ctan/quicspool_wwfonts_i-1.tfm",
        include_str_from_corpus!("ctan/quicspool_wwfonts_i-2.plst"),
        include_str_from_corpus!("ctan/quicspool_wwfonts_i-2.stderr.txt"),
        true,
    ),
    (
        quicspool_wwfonts_b_1_2,
        "ctan/quicspool_wwfonts_b-1.tfm",
        include_str_from_corpus!("ctan/quicspool_wwfonts_b-2.plst"),
        include_str_from_corpus!("ctan/quicspool_wwfonts_b-2.stderr.txt"),
        true,
    ),
    (
        quicspool_wwfonts_cw_1_2,
        "ctan/quicspool_wwfonts_cw-1.tfm",
        include_str_from_corpus!("ctan/quicspool_wwfonts_cw-2.plst"),
        include_str_from_corpus!("ctan/quicspool_wwfonts_cw-2.stderr.txt"),
        true,
    ),
    (
        quicspool_wwfonts_r_1_2,
        "ctan/quicspool_wwfonts_r-1.tfm",
        include_str_from_corpus!("ctan/quicspool_wwfonts_r-2.plst"),
        include_str_from_corpus!("ctan/quicspool_wwfonts_r-2.stderr.txt"),
        true,
    ),
    (
        number_limit_16,
        "originals/number-limit-16-1.tfm",
        include_str_from_corpus!("originals/number-limit-16-2.plst"),
        include_str_from_corpus!("originals/number-limit-16-2.stderr.txt"),
        true,
    ),
    (
        next_larger_loop_tfm,
        "originals/next-larger-loop-tfm.tfm",
        include_str_from_corpus!("originals/next-larger-loop-tfm.plst"),
        include_str_from_corpus!["originals/next-larger-loop-tfm.stderr.txt"],
        true,
    ),
    (
        zero_design_size,
        "fuzz/fuzz_tftopl_8512ff447bf762fe.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_8512ff447bf762fe.plst"),
        include_str_from_corpus!["fuzz/fuzz_tftopl_8512ff447bf762fe.stderr.txt"],
        true,
    ),
    (
        lig_kern_out_of_bounds,
        "fuzz/fuzz_tftopl_794bb506a827c3f.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_794bb506a827c3f.plst"),
        include_str_from_corpus!["fuzz/fuzz_tftopl_794bb506a827c3f.stderr.txt"],
        true,
    ),
    (
        truncated_string_in_header,
        "fuzz/fuzz_tftopl_f89546ae5b0f1d5d.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_f89546ae5b0f1d5d.plst"),
        "",
        true,
    ),
    (
        header_size_too_big_a,
        "originals/large-string-length-a.tfm",
        include_str_from_corpus!("originals/large-string-length-a.plst"),
        "",
        true,
    ),
    (
        header_size_too_big_b,
        "originals/large-string-length-b.tfm",
        include_str_from_corpus!("originals/large-string-length-b.plst"),
        include_str_from_corpus!("originals/large-string-length-b.stderr.txt"),
        true,
    ),
    (
        bad_chars_in_string,
        "originals/bad-chars-in-string.tfm",
        include_str_from_corpus!("originals/bad-chars-in-string.plst"),
        include_str_from_corpus!("originals/bad-chars-in-string.stderr.txt"),
        true,
    ),
    (
        lig_kern_instruction_references_non_existent_character,
        "fuzz/fuzz_tftopl_10e324e0595b2934.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_10e324e0595b2934.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_10e324e0595b2934.stderr.txt"),
        true,
    ),
    (
        extensible_character_references_non_existent_character,
        "fuzz/fuzz_tftopl_e1f1c0de87caa4a0.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_e1f1c0de87caa4a0.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_e1f1c0de87caa4a0.stderr.txt"),
        true,
    ),
    (
        lig_kern_empty_invisible_section,
        "fuzz/fuzz_tftopl_764173f545c18b72.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_764173f545c18b72.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_764173f545c18b72.stderr.txt"),
        true,
    ),
    (
        lig_kern_stop_address_too_big,
        "fuzz/fuzz_tftopl_8ab6f071335a4abc.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_8ab6f071335a4abc.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_8ab6f071335a4abc.stderr.txt"),
        true,
    ),
    (
        infinite_ligature_loop,
        "fuzz/fuzz_tftopl_273fbef691836675.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_273fbef691836675.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_273fbef691836675.stderr.txt"),
        true,
    ),
    (
        invalid_boundary_char_entrypoint,
        "fuzz/fuzz_tftopl_f49074da35aa2807.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_f49074da35aa2807.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_f49074da35aa2807.stderr.txt"),
        true,
    ),
    (
        invalid_rep_in_extensible_recipe,
        "fuzz/fuzz_tftopl_1c72e451cd0e25e7.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_1c72e451cd0e25e7.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_1c72e451cd0e25e7.stderr.txt"),
        true,
    ),
    (
        non_existent_lig_kern_duplicate_warning,
        "fuzz/fuzz_tftopl_e87827104f9cc5c4.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_e87827104f9cc5c4.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_e87827104f9cc5c4.stderr.txt"),
        true,
    ),
    (
        lig_kern_unused_part_of_program_edge_case,
        "fuzz/fuzz_tftopl_622eb29085ef8389.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_622eb29085ef8389.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_622eb29085ef8389.stderr.txt"),
        true,
    ),
    (
        header_warnings_ordering,
        "fuzz/fuzz_tftopl_b38d3def70d7a026.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_b38d3def70d7a026.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_b38d3def70d7a026.stderr.txt"),
        true,
    ),
    (
        string_with_non_ascii_chars,
        "fuzz/fuzz_tftopl_d35c51fe3046ea8f.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_d35c51fe3046ea8f.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_d35c51fe3046ea8f.stderr.txt"),
        true,
    ),
    (
        lig_kern_label_boundary_char,
        "fuzz/fuzz_tftopl_e604027d3275b06e.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_e604027d3275b06e.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_e604027d3275b06e.stderr.txt"),
        true,
    ),
    (
        slant_param_is_i32_min,
        "fuzz/fuzz_tftopl_ef0432d46c78f8a.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_ef0432d46c78f8a.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_ef0432d46c78f8a.stderr.txt"),
        true,
    ),
    (
        phantom_ligature_bug,
        "fuzz/fuzz_tftopl_90cbce0c1d734f4e.tfm",
        include_str_from_corpus!("fuzz/fuzz_tftopl_90cbce0c1d734f4e.plst"),
        include_str_from_corpus!("fuzz/fuzz_tftopl_90cbce0c1d734f4e.stderr.txt"),
        true,
    ),
    (
        phantom_ligature_bug_minimal_repro_1,
        "originals/phantom-ligature-bug-minimal-repro-1.tfm",
        include_str_from_corpus!("originals/phantom-ligature-bug-minimal-repro-1.plst"),
        include_str_from_corpus!("originals/phantom-ligature-bug-minimal-repro-1.stderr.txt"),
        true,
    ),
    (
        phantom_ligature_bug_minimal_repro_2,
        "originals/phantom-ligature-bug-minimal-repro-2.tfm",
        include_str_from_corpus!("originals/phantom-ligature-bug-minimal-repro-2.plst"),
        include_str_from_corpus!("originals/phantom-ligature-bug-minimal-repro-2.stderr.txt"),
        true,
    ),
    (
        left_boundary_char_infinite_loop,
        "originals/left-boundary-char-infinite-loop.tfm",
        include_str_from_corpus!("originals/left-boundary-char-infinite-loop.plst"),
        include_str_from_corpus!("originals/left-boundary-char-infinite-loop.stderr.txt"),
        true,
    ),
    (
        right_boundary_char_creates_infinite_loop_a,
        "originals/right-boundary-char-creates-infinite-loop-a.tfm",
        include_str_from_corpus!("originals/right-boundary-char-creates-infinite-loop-a.plst"),
        include_str_from_corpus!(
            "originals/right-boundary-char-creates-infinite-loop-a.stderr.txt"
        ),
        true,
    ),
    (
        right_boundary_char_creates_infinite_loop_b,
        "originals/right-boundary-char-creates-infinite-loop-b.tfm",
        include_str_from_corpus!("originals/right-boundary-char-creates-infinite-loop-b.plst"),
        include_str_from_corpus!(
            "originals/right-boundary-char-creates-infinite-loop-b.stderr.txt"
        ),
        true,
    ),
    (
        right_boundary_char_breaks_infinite_loop_a,
        "originals/right-boundary-char-breaks-infinite-loop-a.tfm",
        include_str_from_corpus!("originals/right-boundary-char-breaks-infinite-loop-a.plst"),
        include_str_from_corpus!("originals/right-boundary-char-breaks-infinite-loop-a.stderr.txt"),
        true,
    ),
    (
        right_boundary_char_breaks_infinite_loop_b,
        "originals/right-boundary-char-breaks-infinite-loop-b.tfm",
        include_str_from_corpus!("originals/right-boundary-char-breaks-infinite-loop-b.plst"),
        include_str_from_corpus!("originals/right-boundary-char-breaks-infinite-loop-b.stderr.txt"),
        true,
    ),
);
