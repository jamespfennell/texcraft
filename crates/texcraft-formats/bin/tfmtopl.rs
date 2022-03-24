fn main() {
    let args: Vec<String> = std::env::args().collect();

    let tfm_bytes = std::fs::read(&args[1]).unwrap();
    texcraft_formats::read(&tfm_bytes);
}
