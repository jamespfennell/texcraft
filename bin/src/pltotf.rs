fn main() {
    tfm::pl::run(CMR10_PL);
}

static CMR10_PL: &'static str = include_str!("../../crates/tfm/src/cmr10.pl");
