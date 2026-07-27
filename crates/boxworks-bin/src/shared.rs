use boxworks::tex as bwt;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

pub fn load_font_metrics(font_metrics: Option<PathBuf>) -> Result<(Vec<u8>, PathBuf), String> {
    let Some(path) = font_metrics else {
        let bytes = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm").to_vec();
        return Ok((bytes, PathBuf::from("cmr10.tfm")));
    };
    let bytes = match path.extension().and_then(|s| s.to_str()) {
        Some("pl" | "plst") => {
            let pl_data = match fs::read_to_string(&path) {
                Ok(source) => source,
                Err(err) => return Err(format!["failed to open file {:?}: {err}", &path]),
            };
            tfm::algorithms::pl_to_tfm(&pl_data).0
        }
        Some("tfm") => match fs::read(&path) {
            Ok(source) => source,
            Err(err) => return Err(format!["failed to open file {:?}: {err}", &path]),
        },
        _ => {
            return Err(format![
                "unsupported font metrics file extension: must be pl, plst or tfm, is {:?}",
                path.extension()
            ])
        }
    };
    let mut file_name: PathBuf = path.file_name().unwrap().into();
    file_name.set_extension("tfm");
    Ok((bytes, file_name))
}

pub fn build_tex_context(
    font_metrics: Option<PathBuf>,
) -> Result<(HashMap<PathBuf, Vec<u8>>, String), String> {
    let mut auxiliary_files: HashMap<PathBuf, Vec<u8>> = Default::default();
    let (source, file_name) = load_font_metrics(font_metrics)?;
    let file_stem = file_name.file_stem().unwrap().to_string_lossy();
    let preamble = bwt::diagnostic_preamble(&file_stem);
    auxiliary_files.insert(file_name, source);
    Ok((auxiliary_files, preamble))
}
