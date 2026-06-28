use tfm::ligkern::{CompiledProgram, Emitter, Ligature};
use wasm_bindgen::prelude::*;

const CMR10_TFM: &[u8] = include_bytes!("../../../../crates/tfm/corpus/computer-modern/cmr10.tfm");

#[wasm_bindgen]
pub fn list_bundled_fonts() -> String {
    r#"[{"name":"cmr10","display_name":"Computer Modern Roman 10pt (cmr10)"}]"#.into()
}

#[wasm_bindgen]
pub fn run_bundled(font_name: &str, text: &str) -> String {
    match font_name {
        "cmr10" => run_tfm_bytes(CMR10_TFM, text),
        _ => make_error("unknown font"),
    }
}

#[wasm_bindgen]
pub fn run_compact(program_str: &str, text: &str) -> String {
    let (lang_program, entrypoints) = match tfm::ligkern::lang::Program::parse_compact(program_str)
    {
        Ok(p) => p,
        Err(e) => return make_error(&format!("{e:?}")),
    };
    // Use 10pt design size to match cmr10.
    let design_size = tfm::FixWord::ONE * 10;
    let (program, _) = CompiledProgram::compile(&lang_program, design_size, &[], entrypoints);
    run_program(&program, text)
}

#[wasm_bindgen]
pub fn run_pl(program_str: &str, text: &str) -> String {
    let (pl_file, _warnings) = tfm::pl::File::from_pl_source_code(program_str);
    let (program, _) = CompiledProgram::compile_from_pl_file(&pl_file);
    run_program(&program, text)
}

fn run_tfm_bytes(bytes: &[u8], text: &str) -> String {
    let (result, _warnings) = tfm::File::deserialize(bytes);
    let mut file = match result {
        Ok(f) => f,
        Err(e) => return make_error(&format!("{e:?}")),
    };
    let (program, _) = CompiledProgram::compile_from_tfm_file(&mut file);
    run_program(&program, text)
}

fn run_program(program: &CompiledProgram, text: &str) -> String {
    let mut out = JsonOutput::default();
    program.run(text, &mut out);
    out.finish()
}

fn make_error(msg: &str) -> String {
    format!(r#"{{"error":"{}"}}"#, escape_json(msg))
}

fn escape_json(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out
}

#[derive(Default)]
struct JsonOutput {
    elements: Vec<String>,
}

impl JsonOutput {
    fn finish(self) -> String {
        format!(r#"{{"elements":[{}]}}"#, self.elements.join(","))
    }
}

impl Emitter for JsonOutput {
    fn emit_character(&mut self, c: char) {
        self.elements.push(format!(
            r#"{{"type":"char","char":"{}"}}"#,
            escape_json(&c.to_string())
        ));
    }

    fn emit_kern(&mut self, kern: common::Scaled) {
        let value_pt = kern.0 as f64 / 65536.0;
        self.elements.push(format!(
            r#"{{"type":"kern","value_scaled":{},"value_pt":{:.4}}}"#,
            kern.0, value_pt
        ));
    }

    fn emit_ligature(&mut self, ligature: Ligature) {
        self.elements.push(format!(
            r#"{{"type":"ligature","char_hex":"{:04X}","original":"{}"}}"#,
            ligature.c as u32,
            escape_json(&ligature.original)
        ));
    }
}
