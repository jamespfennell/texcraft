fn main() {
    let args: Vec<String> = std::env::args().collect();
    let tfm_bytes = std::fs::read(&args[1]).unwrap();
    let file = tfm::parse_tfm(&tfm_bytes);
    let pl_str = tfm::serialize_pl(&file, tfm::PlStyle::default());
    println!("{}", pl_str);
}
