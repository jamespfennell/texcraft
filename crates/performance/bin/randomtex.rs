use rand::SeedableRng;

fn main() {
    let weights = Default::default();
    let mut rng = rand::prelude::StdRng::seed_from_u64(43);
    print![
        "{}",
        performance::generate_random_tex_document(&mut rng, 250, (20, 50), (80, 100), &weights)
    ];
}
