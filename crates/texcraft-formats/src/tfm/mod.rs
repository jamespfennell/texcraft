use std::panic;
mod pl;
mod tfm;

pub use tfm::deserialize_tfm_bla;

#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    pub checksum: u32,
    pub design_size: FixWord,
    pub character_coding_scheme: Option<String>,
    pub font_family: Option<String>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    pub trailing_words: Vec<u32>,
}

#[derive(Debug, PartialEq, Eq)]
enum Tag {
    None,
    Ligature(usize),
    List(usize),
    Extension(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Weight {
    Medium,
    Bold,
    Light,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Slope {
    Roman,
    Italic,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expansion {
    Regular,
    Condensed,
    Expanded,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Face {
    pub weight: Weight,
    pub slope: Slope,
    pub expansion: Expansion,
}

impl Face {
    fn serialize(&self) -> u8 {
        let a = match self.slope {
            Slope::Roman => 0_u8,
            Slope::Italic => 1_u8,
        };
        let b = match self.weight {
            Weight::Medium => 0_u8,
            Weight::Bold => 2_u8,
            Weight::Light => 4_u8,
        };
        let c = match self.expansion {
            Expansion::Regular => 0_u8,
            Expansion::Condensed => 6_u8,
            Expansion::Expanded => 12_u8,
        };
        a + b + c
    }

    fn deserialize(mut raw: u8) -> Option<Face> {
        if raw >= 18 {
            None
        } else {
            let slope = if raw % 2 == 0 {
                Slope::Roman
            } else {
                Slope::Italic
            };
            raw /= 2;
            let weight = match raw % 3 {
                0 => Weight::Medium,
                1 => Weight::Bold,
                _ => Weight::Light,
            };
            raw /= 3;
            let expansion = match raw % 3 {
                0 => Expansion::Regular,
                1 => Expansion::Condensed,
                _ => Expansion::Expanded,
            };
            Some(Face {
                weight,
                slope,
                expansion,
            })
        }
    }
}

pub struct File {
    pub header: Header,
    pub char_infos: Vec<CharInfo>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExtensibleChar {
    top: u8,
    middle: u8,
    bottom: u8,
    rep: u8,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Params {
    slant: FixWord,
    space: FixWord,
    space_stretch: FixWord,
    space_shrink: FixWord,
    x_height: FixWord,
    quad: FixWord,
    extra_space: FixWord,
    additional_params: Vec<u32>,
}

pub struct CharInfo {
    pub width: (u32, FixWord),
}

#[derive(PartialEq, Eq, Debug)]
pub struct FixWord(i32);

impl FixWord {
    const UNITY: i32 = 1 << 20;
}

impl std::fmt::Display for FixWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pl::serialize_pl(self))
    }
}
