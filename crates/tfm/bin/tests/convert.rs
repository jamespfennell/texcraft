use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

type TfmToPlArgsFn = for<'a> fn(tfm_file_path: &'a str) -> Vec<&'a str>;

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
    let pl_file_path = dir.path().join("input.pl");
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
    ( $( ($name: ident, $tfm_path: expr, $pl_path: expr $(, $tftopl_args: expr )? $(,)? ), )+ ) => {
        $(
            mod $name {
                use super::*;
                const TFM: &'static [u8] = include_bytes!($tfm_path);
                const PL: &'static str = include_str!($pl_path);

                #[test]
                fn tfmtools_convert_tfm_to_pl() {
                    run_tfm_to_pl_test(TFM, PL, "tfmtools", |tfm_file_path| {
                        let mut v: Vec<&str> = vec![];
                        v.extend(["convert", tfm_file_path]);
                        $(
                            v.extend( $tftopl_args );
                        )?
                        v
                    }, Some(""), true);
                }

                #[test]
                fn tftopl() {
                    run_tfm_to_pl_test(TFM, PL, "tftopl", |tfm_file_path|{
                        let mut v: Vec<&str> = vec![];
                        v.extend([tfm_file_path]);
                        $(
                            v.extend( $tftopl_args );
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
                const TFM: &'static [u8] = include_bytes!($tfm_path);
                const PL: &'static str = include_str!($pl_path);
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
                const TFM: &'static [u8] = include_bytes!($tfm_path);
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
        "data/computer-modern/cmr10.tfm",
        "data/computer-modern/cmr10.pl"
    ),
    (
        cmr10_ascii,
        "data/computer-modern/cmr10.tfm",
        "data/computer-modern/cmr10_ascii.pl",
        vec!["--charcode-format", "ascii"]
    ),
    (
        cmr10_octal,
        "data/computer-modern/cmr10.tfm",
        "data/computer-modern/cmr10_octal.pl",
        vec!["--charcode-format", "octal"]
    ),
    (
        cmss8,
        "data/computer-modern/cmss8.tfm",
        "data/computer-modern/cmss8.pl"
    ),
    (
        cmex10,
        "data/computer-modern/cmex10.tfm",
        "data/computer-modern/cmex10.pl"
    ),
    (
        cminch,
        "data/computer-modern/cminch.tfm",
        "data/computer-modern/cminch.pl"
    ),
    (
        cmsy7,
        "data/computer-modern/cmsy7.tfm",
        "data/computer-modern/cmsy7.pl"
    ),
    (
        many_ligatures,
        "data/originals/many-ligatures.tfm",
        "data/originals/many-ligatures.pl",
    ),
    (
        params,
        "data/originals/font-dimen.tfm",
        "data/originals/font-dimen.pl",
    ),
    (
        boundary_char,
        "data/originals/boundarychar.tfm",
        "data/originals/boundarychar.pl",
    ),
    (
        boundary_char_unspecified,
        "data/originals/boundarychar-unspecified.tfm",
        "data/originals/boundarychar-unspecified.pl",
    ),
    (
        boundary_char_no_entrypoint,
        "data/originals/boundarychar-noentrypoint.tfm",
        "data/originals/boundarychar-noentrypoint.pl",
    ),
    (
        many_entrypoints,
        "data/originals/many-entrypoints.tfm",
        "data/originals/many-entrypoints.pl",
    ),
    (
        theano_old_style,
        "data/ctan/TheanoOldStyle-Bold-tlf-t1--base.tfm",
        "data/ctan/TheanoOldStyle-Bold-tlf-t1--base.pl",
    ),
    (
        orphan_lig_kerns_4_5,
        "data/originals/orphan-lig-kerns-4.tfm",
        "data/originals/orphan-lig-kerns-5.pl",
    ),
    (
        arev_sans_3_4,
        "data/ctan/ArevSans-BoldOblique-3.tfm",
        "data/ctan/ArevSans-BoldOblique-4.pl",
    ),
    (
        dimen_index_out_of_bounds_3_4,
        "data/originals/dimen-index-out-of-bounds-3.tfm",
        "data/originals/dimen-index-out-of-bounds-4.pl",
    ),
    (
        smfebsl10_3_4,
        "data/ctan/smfebsl10-3.tfm",
        "data/ctan/smfebsl10-4.pl",
    ),
    (cprbn8t, "data/ctan/cprbn8t.tfm", "data/ctan/cprbn8t.pl",),
    (
        rashii2_3_4,
        "data/ctan/rashii2-3.tfm",
        "data/ctan/rashii2-4.pl",
    ),
    (
        veracruz_6vcr8r,
        "data/ctan/6vcr8r.tfm",
        "data/ctan/6vcr8r.pl",
    ),
);

convert_pltotf_tests!(
    (
        rashii2_2_3,
        "data/ctan/rashii2-3.tfm",
        "data/ctan/rashii2-2.pl",
        ""
    ),
    (
        empty,
        "data/originals/empty.tfm",
        "data/originals/empty.pl",
        "",
    ),
    (
        empty_varchar,
        "data/originals/empty-varchar.tfm",
        "data/originals/empty-varchar.pl",
        "",
    ),
    (
        zero_width_chars,
        "data/originals/zero-width-char.tfm",
        "data/originals/zero-width-char.pl",
        "",
    ),
    (
        ligature_loop,
        "data/originals/ligature-loop.tfm",
        "data/originals/ligature-loop.pl",
        include_str!["data/originals/ligature-loop.stderr.txt"],
    ),
    (
        next_larger_loop_pl,
        "data/originals/next-larger-loop-pl.tfm",
        "data/originals/next-larger-loop-pl.pl",
        include_str!["data/originals/next-larger-loop-pl.stderr.txt"],
    ),
    (
        orphan_lig_kerns_1_2,
        "data/originals/orphan-lig-kerns-2.tfm",
        "data/originals/orphan-lig-kerns-1.pl",
        "",
    ),
    (
        orphan_lig_kerns_3_4,
        "data/originals/orphan-lig-kerns-4.tfm",
        "data/originals/orphan-lig-kerns-3.pl",
        "",
    ),
    (
        arev_sans_2_3,
        "data/ctan/ArevSans-BoldOblique-3.tfm",
        "data/ctan/ArevSans-BoldOblique-2.pl",
        "",
    ),
    (
        theano_old_style_bold_tlf_lgr,
        "data/ctan/TheanoOldStyle-Bold-tlf-lgr.tfm",
        "data/ctan/TheanoOldStyle-Bold-tlf-lgr.pl",
        "",
    ),
    /* TODO: fix the bug and enable
    (
        dimen_index_out_of_bounds_2_3,
        "data/originals/dimen-index-out-of-bounds-3.tfm",
        "data/originals/dimen-index-out-of-bounds-2.pl",
        include_str!["data/originals/dimen-index-out-of-bounds-3.stderr.txt"],
    ),
     */

    /*
    The problem in this one is that pltotf imposes max params of 254.
    (It's not 255 because in the CST -> AST step, number bigger than this become 255
    due to how Knuth does the get_byte method. So with 254 we can still check for this condition)
    Do we want to support that?
        (
        bxjatoucs_jis_roundtrip,
        "data/ctan/bxjatoucs-jis-3.tfm",
        "data/ctan/bxjatoucs-jis-2.pl",
    ),
     */
    (
        smfebsl10_2_3,
        "data/ctan/smfebsl10-3.tfm",
        "data/ctan/smfebsl10-2.pl",
        "",
    ),
    (aebkri, "data/ctan/aebkri.tfm", "data/ctan/aebkri.pl", "",),
    (mt2exa, "data/ctan/mt2exa.tfm", "data/ctan/mt2exa.pl", "",),
    (copti, "data/ctan/copti.tfm", "data/ctan/copti.pl", "",),
    (
        bxjatoucs_cid,
        "data/ctan/bxjatoucs-cid.tfm",
        "data/ctan/bxjatoucs-cid.pl",
        include_str!["data/ctan/bxjatoucs-cid.stderr.txt"],
    ),
    (
        xcharter_bolditalic_tlf_ot1g,
        "data/ctan/XCharter-BoldItalic-tlf-ot1G.tfm",
        "data/ctan/XCharter-BoldItalic-tlf-ot1G.pl",
        "",
    ),
);

convert_tftopl_tests!(
    (
        rashii2_1_2,
        "data/ctan/rashii2-1.tfm",
        include_str!("data/ctan/rashii2-2.pl"),
        "",
        true,
    ),
    (
        gk256g,
        "data/ctan/gk256g.tfm",
        "",
        include_str!["data/ctan/gk256g.stderr.txt"],
        false,
    ),
    (
        orphan_lig_kerns_2_3,
        "data/originals/orphan-lig-kerns-2.tfm",
        include_str!("data/originals/orphan-lig-kerns-3.pl"),
        "",
        true,
    ),
    (
        txbmi,
        "data/ctan/txbmi.tfm",
        include_str!("data/ctan/txbmi.pl"),
        "",
        true,
    ),
    (
        unusual_num_params,
        "data/originals/unusual-num-params.tfm",
        include_str!("data/originals/unusual-num-params.pl"),
        "Unusual number of fontdimen parameters for a math symbols font (0 not 22).\n",
        true,
    ),
    (
        empty_coding_scheme,
        "data/ctan/md-utree.tfm",
        include_str!("data/ctan/md-utree.pl"),
        "",
        true,
    ),
    (
        md_grbr7m,
        "data/ctan/md-grbr7m.tfm",
        include_str!("data/ctan/md-grbr7m.pl"),
        "",
        true,
    ),
    (
        xcyeuat12,
        "data/ctan/xyeuat12.tfm",
        include_str!("data/ctan/xyeuat12.pl"),
        "",
        true,
    ),
    (
        arev_sans_1_2,
        "data/ctan/ArevSans-BoldOblique-1.tfm",
        include_str!("data/ctan/ArevSans-BoldOblique-2.pl"),
        include_str!("data/ctan/ArevSans-BoldOblique-2.stderr.txt"),
        true,
    ),
    (
        non_zero_first_dimens,
        "data/originals/non-zero-first-dimens.tfm",
        include_str!("data/originals/non-zero-first-dimens.pl"),
        include_str!("data/originals/non-zero-first-dimens.stderr.txt"),
        true,
    ),
    (
        dimen_index_out_of_bounds_1_2,
        "data/originals/dimen-index-out-of-bounds-1.tfm",
        include_str!("data/originals/dimen-index-out-of-bounds-2.pl"),
        include_str!("data/originals/dimen-index-out-of-bounds-2.stderr.txt"),
        true,
    ),
    (
        duplicate_lig_stop_warning,
        "data/originals/duplicate-lig-stop-warnings.tfm",
        include_str!("data/originals/duplicate-lig-stop-warnings.pl"),
        include_str!("data/originals/duplicate-lig-stop-warnings.stderr.txt"),
        true,
    ),
    (
        duplicate_lig_kern_warning,
        "data/originals/duplicate-lig-kern-warnings-1.tfm",
        include_str!("data/originals/duplicate-lig-kern-warnings-2.pl"),
        include_str!("data/originals/duplicate-lig-kern-warnings-2.stderr.txt"),
        true,
    ),
    (
        smfebsl10_1_2,
        "data/ctan/smfebsl10-1.tfm",
        include_str!("data/ctan/smfebsl10-2.pl"),
        "",
        true,
    ),
    (
        bxjatoucs_jis,
        "data/ctan/bxjatoucs-jis-1.tfm",
        include_str!("data/ctan/bxjatoucs-jis-2.pl"),
        "",
        true,
    ),
    (
        quicspool_wwfonts_i_1_2,
        "data/ctan/quicspool_wwfonts_i-1.tfm",
        include_str!("data/ctan/quicspool_wwfonts_i-2.pl"),
        include_str!("data/ctan/quicspool_wwfonts_i-2.stderr.txt"),
        true,
    ),
    (
        quicspool_wwfonts_b_1_2,
        "data/ctan/quicspool_wwfonts_b-1.tfm",
        include_str!("data/ctan/quicspool_wwfonts_b-2.pl"),
        include_str!("data/ctan/quicspool_wwfonts_b-2.stderr.txt"),
        true,
    ),
    (
        quicspool_wwfonts_cw_1_2,
        "data/ctan/quicspool_wwfonts_cw-1.tfm",
        include_str!("data/ctan/quicspool_wwfonts_cw-2.pl"),
        include_str!("data/ctan/quicspool_wwfonts_cw-2.stderr.txt"),
        true,
    ),
    (
        quicspool_wwfonts_r_1_2,
        "data/ctan/quicspool_wwfonts_r-1.tfm",
        include_str!("data/ctan/quicspool_wwfonts_r-2.pl"),
        include_str!("data/ctan/quicspool_wwfonts_r-2.stderr.txt"),
        true,
    ),
    (
        number_limit_16,
        "data/originals/number-limit-16-1.tfm",
        include_str!("data/originals/number-limit-16-2.pl"),
        include_str!("data/originals/number-limit-16-2.stderr.txt"),
        true,
    ),
    (
        next_larger_loop_tfm,
        "data/originals/next-larger-loop-tfm.tfm",
        include_str!("data/originals/next-larger-loop-tfm.pl"),
        include_str!["data/originals/next-larger-loop-tfm.stderr.txt"],
        true,
    ),
    (
        zero_design_size,
        "data/fuzz/fuzz_tftopl_8512ff447bf762fe.tfm",
        include_str!("data/fuzz/fuzz_tftopl_8512ff447bf762fe.pl"),
        include_str!["data/fuzz/fuzz_tftopl_8512ff447bf762fe.stderr.txt"],
        true,
    ),
    (
        lig_kern_out_of_bounds,
        "data/fuzz/fuzz_tftopl_794bb506a827c3f.tfm",
        include_str!("data/fuzz/fuzz_tftopl_794bb506a827c3f.pl"),
        include_str!["data/fuzz/fuzz_tftopl_794bb506a827c3f.stderr.txt"],
        true,
    ),
    (
        truncated_string_in_header,
        "data/fuzz/fuzz_tftopl_f89546ae5b0f1d5d.tfm",
        include_str!("data/fuzz/fuzz_tftopl_f89546ae5b0f1d5d.pl"),
        "",
        true,
    ),
    (
        header_size_too_big_a,
        "data/originals/large-string-length-a.tfm",
        include_str!("data/originals/large-string-length-a.pl"),
        "",
        true,
    ),
    (
        header_size_too_big_b,
        "data/originals/large-string-length-b.tfm",
        include_str!("data/originals/large-string-length-b.pl"),
        include_str!("data/originals/large-string-length-b.stderr.txt"),
        true,
    ),
    (
        bad_chars_in_string,
        "data/originals/bad-chars-in-string.tfm",
        include_str!("data/originals/bad-chars-in-string.pl"),
        include_str!("data/originals/bad-chars-in-string.stderr.txt"),
        true,
    ),
    (
        lig_kern_instruction_references_non_existent_character,
        "data/fuzz/fuzz_tftopl_10e324e0595b2934.tfm",
        include_str!("data/fuzz/fuzz_tftopl_10e324e0595b2934.pl"),
        include_str!("data/fuzz/fuzz_tftopl_10e324e0595b2934.stderr.txt"),
        true,
    ),
    (
        extensible_character_references_non_existent_character,
        "data/fuzz/fuzz_tftopl_e1f1c0de87caa4a0.tfm",
        include_str!("data/fuzz/fuzz_tftopl_e1f1c0de87caa4a0.pl"),
        include_str!("data/fuzz/fuzz_tftopl_e1f1c0de87caa4a0.stderr.txt"),
        true,
    ),
    (
        lig_kern_empty_invisible_section,
        "data/fuzz/fuzz_tftopl_764173f545c18b72.tfm",
        include_str!("data/fuzz/fuzz_tftopl_764173f545c18b72.pl"),
        include_str!("data/fuzz/fuzz_tftopl_764173f545c18b72.stderr.txt"),
        true,
    ),
    (
        lig_kern_stop_address_too_big,
        "data/fuzz/fuzz_tftopl_8ab6f071335a4abc.tfm",
        include_str!("data/fuzz/fuzz_tftopl_8ab6f071335a4abc.pl"),
        include_str!("data/fuzz/fuzz_tftopl_8ab6f071335a4abc.stderr.txt"),
        true,
    ),
);
