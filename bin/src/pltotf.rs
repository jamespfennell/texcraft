fn main() {
    tfm::parse_pl(CMR10_PL);
}

static CMR10_PL: &str = include_str!("../../crates/tfm/src/cmr10.pl");
