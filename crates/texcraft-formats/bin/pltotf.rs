fn main() {
    texcraft_formats::tfm::pl::run(CMR10_PL);
}

static CMR10_PL: &'static str = include_str!("../src/tfm/cmr10.pl");
