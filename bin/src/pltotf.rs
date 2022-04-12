fn main() {
    tfm::pl::parse("todo.pl", CMR10_PL);
}

static CMR10_PL: &str = include_str!("../../crates/tfm/src/cmr10.pl");
