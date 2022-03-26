fn main() {
    let args: Vec<String> = std::env::args().collect();
    let tfm_bytes = std::fs::read(&args[1]).unwrap();
    let raw_file = texcraft_formats::tfm::deserialize_tfm_bla(&tfm_bytes);
    let pl_str = texcraft_formats::tfm::pl::serialize(&raw_file);
    println!("{}", pl_str);
}
