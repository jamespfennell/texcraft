//! Examples of TFM and PL files
//!
//! ```bash
//! wget "https://mirrors.ctan.org/fonts/cm/tfm.zip" -O tfm.zip && unzip tfm.zip && rm tfm.zip
//! ```

// TODO: delete all this
pub struct TfmFile {
    pub path: &'static str,
    pub data: &'static [u8],
}

macro_rules! include_tfm {
    ( $( $file: expr, )+ ) => {
        vec!( $(
            TfmFile {
                path: $file,
                data: include_bytes![$file],
            },
        )+)
    };
}

pub fn tfm_files() -> Vec<TfmFile> {
    include_tfm!(
        "tfm/cmss8.tfm",
        "tfm/cmbxsl10.tfm",
        "tfm/cmssqi8.tfm",
        "tfm/cmcsc10.tfm",
        "tfm/cmbx9.tfm",
        "tfm/cmr10.tfm",
        "tfm/cmmi6.tfm",
        "tfm/cmss12.tfm",
        "tfm/cmss10.tfm",
        "tfm/cmtcsc10.tfm",
        "tfm/cmbx6.tfm",
        "tfm/cmmi9.tfm",
        "tfm/cmr8.tfm",
        "tfm/cmsy9.tfm",
        "tfm/cmtex9.tfm",
        "tfm/cmr12.tfm",
        "tfm/cmvtt10.tfm",
        "tfm/cmssi10.tfm",
        "tfm/cmbx10.tfm",
        "tfm/cmmib10.tfm",
        "tfm/cmssdc10.tfm",
        "tfm/cmsl8.tfm",
        "tfm/cmtt8.tfm",
        "tfm/cmsy8.tfm",
        "tfm/cmssi9.tfm",
        "tfm/cmsy10.tfm",
        "tfm/cmss17.tfm",
        "tfm/cmti10.tfm",
        "tfm/cmtt12.tfm",
        "tfm/cmmi8.tfm",
        "tfm/cmsy6.tfm",
        "tfm/cmmi10.tfm",
        "tfm/cmssq8.tfm",
        "tfm/cmdunh10.tfm",
        "tfm/cmitt10.tfm",
        "tfm/cmbx5.tfm",
        "tfm/cmbx12.tfm",
        "tfm/cmr9.tfm",
        "tfm/cmbsy10.tfm",
        "tfm/cmsltt10.tfm",
        "tfm/cmtt9.tfm",
        "tfm/cmssi12.tfm",
        "tfm/cmsl9.tfm",
        "tfm/cmss9.tfm",
        "tfm/cmr5.tfm",
        "tfm/cmtt10.tfm",
        "tfm/cmtex10.tfm",
        "tfm/cmb10.tfm",
        "tfm/cmti9.tfm",
        "tfm/cmti12.tfm",
        "tfm/cmbx8.tfm",
        "tfm/cmfi10.tfm",
        "tfm/cmsy7.tfm",
        "tfm/cmmi12.tfm",
        "tfm/cminch.tfm",
        "tfm/cmssi8.tfm",
        "tfm/cmr17.tfm",
        "tfm/cmmi5.tfm",
        "tfm/cmex10.tfm",
        "tfm/cmu10.tfm",
        "tfm/cmbxti10.tfm",
        "tfm/cmsy5.tfm",
        "tfm/cmsl12.tfm",
        "tfm/cmbx7.tfm",
        "tfm/cmtex8.tfm",
        "tfm/cmmi7.tfm",
        "tfm/cmr6.tfm",
        "tfm/cmff10.tfm",
        "tfm/cmfib8.tfm",
        "tfm/cmr7.tfm",
        "tfm/cmti7.tfm",
        "tfm/cmssbx10.tfm",
        "tfm/cmti8.tfm",
        "tfm/cmsl10.tfm",
        "tfm/cmssi17.tfm",
    )
}
