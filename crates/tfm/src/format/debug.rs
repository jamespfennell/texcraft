use super::*;

static SECTION_DIVIDER: &str = "============================[";
static INDEX_DIVIDER: char = '|';

pub fn parse(s: &str) -> Result<Vec<u8>, (String, usize)> {
    let mut section_to_bytes: HashMap<Section, (Vec<u8>, usize)> = Default::default();
    let mut current_buffer: Option<&mut Vec<u8>> = None;
    for (n, line) in s.lines().enumerate() {
        let line = line.trim();

        match (&mut current_buffer, line.starts_with(SECTION_DIVIDER)) {
            (None, false) => continue,
            (Some(buffer), false) => {
                if line.starts_with("//") || line.is_empty() {
                    continue;
                }
                let data = match line.rfind(INDEX_DIVIDER) {
                    None => line,
                    Some(i) => &line[i + INDEX_DIVIDER.len_utf8()..],
                };

                let start = buffer.len();
                for byte in data.split_whitespace() {
                    let b = match byte.parse() {
                        Ok(b) => b,
                        Err(err) => {
                            return Err((
                                format!["failed to parse the word '{byte}' as a byte value: {err}"],
                                n,
                            ))
                        }
                    };
                    buffer.push(b);
                }
                if buffer.len() - start != 4 {
                    return Err((
                        format!(
                            "each line must contain exactly 4 bytes; found {} bytes",
                            buffer.len() - start
                        ),
                        n,
                    ));
                }
                continue;
            }
            (_, true) => {
                current_buffer = None;
                let i = match line.find("/raw") {
                    None => continue,
                    Some(i) => i,
                };
                let section: Section = match line[SECTION_DIVIDER.len()..i].try_into() {
                    Ok(section) => section,
                    Err(_) => continue,
                };
                use std::collections::hash_map::Entry::*;
                current_buffer = match section_to_bytes.entry(section) {
                    Occupied(_) => {
                        return Err((format!["section {} appears more than once", section], n))
                    }
                    Vacant(v) => {
                        let r = v.insert((vec![], n));
                        Some(&mut r.0)
                    }
                };
            }
        }
    }

    // Clippy wants us to use the entry API here.
    // But we can't because calculating the value to insert requires accessing the map
    // and the entry API uses a mutable reference to the map.
    #[allow(clippy::map_entry)]
    if !section_to_bytes.contains_key(&Section::SubFileSizes) {
        let num_words = |section| -> Result<i16, (String, usize)> {
            let (l, n) = section_to_bytes
                .get(&section)
                .map(|(s, n)| (s.len() / 4, *n))
                .unwrap_or((0, 0));
            l.try_into().map_err(|_| {
                (
                    format![
                        "section {section} contains {l} lines but the max is {}",
                        i16::MAX
                    ],
                    n,
                )
            })
        };
        let mut s = SubFileSizes {
            lf: 0,
            lh: num_words(Section::Header)?,
            bc: if num_words(Section::CharInfos)? == 0 {
                1
            } else {
                0
            },
            ec: {
                let n = num_words(Section::CharInfos)?;
                if n == 0 {
                    0
                } else {
                    n - 1
                }
            },
            nw: num_words(Section::Widths)?,
            nh: num_words(Section::Heights)?,
            nd: num_words(Section::Depths)?,
            ni: num_words(Section::ItalicCorrections)?,
            nl: num_words(Section::LigKern)?,
            nk: num_words(Section::Kerns)?,
            ne: num_words(Section::ExtensibleRecipes)?,
            np: num_words(Section::Params)?,
        };
        s.lf = s.valid_lf();
        let b: [u8; 24] = s.clone().into();
        section_to_bytes.insert(Section::SubFileSizes, (b.into(), 0));
    };

    let mut buffer = vec![];
    for section in Section::ALL_SECTIONS {
        buffer.extend_from_slice(
            section_to_bytes
                .get(&section)
                .map(|v| v.0.as_slice())
                .unwrap_or_default(),
        );
    }
    Ok(buffer)
}

pub struct Debug<'a> {
    pub sub_file_sizes: SubFileSizes,
    pub path: Option<&'a str>,
    pub raw_file: Option<&'a RawFile<'a>>,
    pub tfm_file: Option<&'a File>,
    pub sections: Vec<Section>,
}

impl<'a> std::fmt::Display for Debug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "tfmtools debug")?;
        if let Some(path) = self.path {
            writeln!(f, ".tfm file: {}", path)?;
        }
        writeln!(f, "outputting sections: {:#?}", self.sections)?;
        for section in self.sections.iter().copied() {
            if let Some(raw_file) = self.raw_file {
                writeln!(f, "{}{}/raw]", SECTION_DIVIDER, section)?;
                let raw_data = match section {
                    Section::SubFileSizes => raw_file.raw_sub_file_sizes,
                    Section::Header => raw_file.header,
                    Section::CharInfos => raw_file.char_infos,
                    Section::Widths => raw_file.widths,
                    Section::Heights => raw_file.heights,
                    Section::Depths => raw_file.depths,
                    Section::ItalicCorrections => raw_file.italic_corrections,
                    Section::LigKern => raw_file.lig_kern_instructions,
                    Section::Kerns => raw_file.kerns,
                    Section::ExtensibleRecipes => raw_file.extensible_recipes,
                    Section::Params => raw_file.params,
                };
                if raw_data.is_empty() {
                    writeln!(f, "// empty")?;
                }
                for (j, word) in WordIter(raw_data).enumerate() {
                    writeln!(
                        f,
                        "{} {} {:4}{:4}{:4}{:4}",
                        index(self.sub_file_sizes.bc, section, j),
                        INDEX_DIVIDER,
                        word[0],
                        word[1],
                        word[2],
                        word[3]
                    )?;
                }
            }
            if let Some(tfm_file) = self.tfm_file {
                writeln!(f, "{}{}/parsed]", SECTION_DIVIDER, section)?;
                match section {
                    Section::SubFileSizes => writeln!(f, "{:#?}", self.sub_file_sizes),
                    Section::Header => writeln!(f, "{:#?}", tfm_file.header),
                    Section::CharInfos => {
                        for u in 0_u8..=255 {
                            if (u as i16) < self.sub_file_sizes.bc {
                                continue;
                            }
                            if (u as i16) > self.sub_file_sizes.ec {
                                continue;
                            }
                            writeln!(f, "{}:", format_char(Char(u)))?;
                            if let Some(dimens) = tfm_file.char_dimens.get(&Char(u)) {
                                writeln![f, "      dimens={:?}", dimens]?;
                            }
                            if let Some(tag) = tfm_file.char_tags.get(&Char(u)) {
                                writeln![f, "      tag={:?}", tag]?;
                            }
                        }
                        Ok(())
                    }
                    Section::Widths => writeln!(f, "{:#?}", tfm_file.widths),
                    Section::Heights => writeln!(f, "{:#?}", tfm_file.heights),
                    Section::Depths => writeln!(f, "{:#?}", tfm_file.depths),
                    Section::ItalicCorrections => writeln!(f, "{:#?}", tfm_file.italic_corrections),
                    Section::LigKern => writeln!(f, "{:#?}", tfm_file.lig_kern_program),
                    Section::Kerns => writeln!(f, "{:#?}", tfm_file.kerns),
                    Section::ExtensibleRecipes => writeln!(f, "{:#?}", tfm_file.extensible_chars),
                    Section::Params => writeln!(f, "{:#?}", tfm_file.params),
                }?;
            }
        }
        Ok(())
    }
}

struct WordIter<'a>(&'a [u8]);

impl<'a> Iterator for WordIter<'a> {
    type Item = [u8; 4];

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.get(..4) {
            None => None,
            Some(head) => {
                self.0 = &self.0[4..];
                Some([head[0], head[1], head[2], head[3]])
            }
        }
    }
}

fn index(bc: i16, section: Section, j: usize) -> String {
    let default = format!("{:5}", j);
    if section != Section::CharInfos {
        return default;
    }
    let j = j + bc as usize;
    let raw: u8 = match j.try_into() {
        Ok(raw) => raw,
        Err(_) => return default,
    };
    format_char(Char(raw))
}

fn format_char(c: Char) -> String {
    let ch = c.0 as char;
    if ch.is_ascii_alphanumeric() || ch.is_ascii_graphic() {
        format!("   {}", ch)
    } else {
        format!("0x{:02x}", c.0)
    }
}
