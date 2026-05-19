use boxworks::ds;

#[derive(Default)]
pub struct Hyphenator {
    pub hyphenator: hyphenate::Hyphenator,
    pub left_hyphen_min: i32,
    pub right_hyphen_min: i32,
}

impl Hyphenator {
    pub fn plain_tex_en_us() -> Self {
        Self {
            hyphenator: hyphenate::Hyphenator::plain_tex_en_us(),
            left_hyphen_min: 2,
            right_hyphen_min: 3,
        }
    }
}

impl boxworks::Hyphenator for Hyphenator {
    fn hyphenate(&self, list: &mut Vec<ds::Horizontal>) {
        let out = hyphenate_impl(self, list);
        *list = out;
    }
}

fn hyphenate_impl(hyphenater: &Hyphenator, list: &[ds::Horizontal]) -> Vec<ds::Horizontal> {
    let lower_caser = hyphenate::AsciiLowerCaser::default();
    let mut out = vec![];
    let mut i = 0;
    while let Some(elem) = list.get(i) {
        i += 1;
        out.push(elem.clone());
        // Hyphenation starts only at glue nodes, as per TeX.2021.866.
        if !matches!(elem, ds::Horizontal::Glue(_)) {
            continue;
        }

        // TODO: implement \lefthyphenmin and \righthyphenmin,
        // we assume they are 2 and 3 in the hyphenate package
        // TODO: hyf_char needs to be read from somewhere using the font numbers
        // in char and lig nodes.
        let hyf_char = 3_i32;

        // Find the place to start hyphenating
        // TeX.2021.896
        let hyphenation_font: Option<u32> = loop {
            let Some(elem) = list.get(i) else { break None };
            enum Action {
                Start { font: u32 },
                Continue,
                // Equivalent to done1 in Knuth's TeX.
                Abort,
            }
            use ds::Horizontal::*;
            let action = match elem {
                Char(char) => {
                    // The hyf_char logic runs in the label done2.
                    // TODO: this code assumes \uchyph=true (uppercase letters are hyphenated)
                    // and that \lccode has its PlainTeX values (has lower case character iff
                    // ASCII alphabetic). We should remove these assumptions by plumbing in some
                    // parameters.
                    if char.char.is_ascii_alphabetic() {
                        if hyf_char > 0 && hyf_char <= 255 {
                            Action::Start { font: char.font }
                        } else {
                            Action::Abort
                        }
                    } else {
                        Action::Continue
                    }
                }
                Ligature(ligature) => {
                    match ligature.original_chars.chars().next() {
                        None => Action::Continue,
                        Some(char) => {
                            // TODO: all of the TODOs in the Char arm.
                            if char.is_ascii_alphabetic() {
                                if hyf_char > 0 && hyf_char <= 255 {
                                    Action::Start {
                                        font: ligature.font,
                                    }
                                } else {
                                    Action::Abort
                                }
                            } else {
                                Action::Continue
                            }
                        }
                    }
                }
                Whatsit(whatsit) => {
                    // TeX.2021.1363 but given how we've architected the code, the logic in TeX.2021.1382
                    // (which changes the current language) should run here.
                    whatsit.hyphenation_hook();
                    Action::Continue
                }
                Kern(kern) => match kern.kind {
                    ds::KernKind::Normal => Action::Continue,
                    _ => Action::Abort,
                },
                _ => Action::Abort,
            };
            match action {
                Action::Start { font } => {
                    break Some(font);
                }
                Action::Continue => {
                    i += 1;
                    out.push(elem.clone());
                }
                Action::Abort => {
                    i += 1;
                    out.push(elem.clone());
                    break None;
                }
            }
        };
        let Some(hyphenation_font) = hyphenation_font else {
            continue;
        };
        // It's still possible we won't hyphenate based on the node that ends the
        // string of characters. So we save this `i` value here; if hyphenation is skipped
        // we set `i` back to this.
        let hyphenation_start_i = i;

        let mut s = String::new();
        // Accumulate the word to be hyphenated.
        // TeX.2021.897
        while let Some(elem) = list.get(i) {
            use ds::Horizontal::*;
            match elem {
                Char(char) => {
                    if char.font != hyphenation_font {
                        break;
                    }
                    // TODO: plumb in \lccode and change this check.
                    if !char.char.is_ascii_alphabetic() {
                        break;
                    }
                    if s.len() + char.char.len_utf8() >= 64 {
                        // TeX only hyphenates words up to 64 bytes.
                        // TODO: this check is not quite right: unicode values in the range [128, 255)
                        // should count as 1 only.
                        break;
                    }
                    s.push(char.char);
                }
                Ligature(ligature) => {
                    // TeX.2021.898
                    if ligature.font != hyphenation_font {
                        break;
                    }
                    if !ligature
                        .original_chars
                        .chars()
                        .all(|c| c.is_ascii_alphabetic())
                    {
                        break;
                    }
                    if s.len() + ligature.original_chars.len() >= 64 {
                        // TeX only hyphenates words up to 64 bytes.
                        // TODO: this check is not quite right: unicode values in the range [128, 255)
                        // should count as 1 only.
                        break;
                    }
                    s.push_str(&ligature.original_chars);
                }
                Kern(kern) => match kern.kind {
                    ds::KernKind::Normal => {
                        // TODO: set up the lig/kern program correctly.
                    }
                    _ => break,
                },
                _ => break,
            }
            // Consume the node whose characters have just been placed in s (or the normal kern).
            i += 1;
        }
        // The first char node that triggered the word search will have been put in s.
        assert!(!s.is_empty());

        // Check if the word can be hyphenated based on the terminating node.
        // TeX.2021.899
        // We use a different index to iterate as all of the elements here still need to be
        // copied to the output list.
        let mut j = i;
        let should_hyphenate = loop {
            let Some(elem) = list.get(j) else { break true };
            use ds::Horizontal::*;
            match elem {
                Char(_) | Ligature(_) => {
                    // This can happen if the font is different, or the 64 byte limit is already
                    // reached.
                }
                Kern(kern) => match kern.kind {
                    ds::KernKind::Normal => {
                        // TODO: set up the lig/kern program correctly.
                    }
                    _ => break true,
                },
                Whatsit(_) | Glue(_) | Penalty(_) | Insertion(_) | Adjust(_) | Mark(_) => {
                    break true;
                }
                HBox(_) | VBox(_) | Rule(_) | Discretionary(_) | Math(_) => {
                    // done1 in Knuth's TeX.f
                    break false;
                }
            }
            j += 1;
        };
        if !should_hyphenate {
            i = hyphenation_start_i;
            continue;
        }

        let l = s.chars().count();
        // TeX.2021.1200
        let left_hyphen_min = if hyphenater.left_hyphen_min < 1 {
            1
        } else {
            hyphenater.left_hyphen_min as usize
        };
        let right_hyphen_min = if hyphenater.right_hyphen_min < 1 {
            1
        } else {
            hyphenater.right_hyphen_min as usize
        };

        let mut indices = hyphenater.hyphenator.calculate_indices(&lower_caser, &s);
        let mut next_or = indices.next();

        let mut j = hyphenation_start_i;
        let mut chars_pushed = 0;
        while j < i {
            if let Some(next) = next_or {
                if next == chars_pushed
                    && next >= left_hyphen_min
                    && next <= l.saturating_sub(right_hyphen_min)
                {
                    let mut replace_count = 0_u32;
                    while j + (replace_count as usize) < i
                        && matches!(&list[j + (replace_count as usize)], ds::Horizontal::Kern(_))
                    {
                        replace_count += 1;
                    }
                    let mut pre_break = vec![];
                    let put_back = if replace_count > 0 {
                        let last_char = out.pop().expect("char character was just written");
                        pre_break.push(
                            last_char
                                .clone()
                                .try_into()
                                .expect("this must be a char item"),
                        );
                        replace_count += 1;
                        Some(last_char)
                    } else {
                        None
                    };
                    pre_break.push(ds::DiscretionaryElem::Char(ds::Char {
                        char: '-',
                        font: hyphenation_font,
                    }));
                    out.push(ds::Horizontal::Discretionary(ds::Discretionary {
                        pre_break,
                        post_break: vec![],
                        replace_count,
                    }));
                    if let Some(put_back) = put_back {
                        out.push(put_back);
                    }
                }
                if next <= chars_pushed {
                    next_or = indices.next();
                }
            }
            let elem = &list[j];
            out.push(elem.clone());
            j += 1;
            use ds::Horizontal::*;
            chars_pushed += match elem {
                Char(_) => 1,
                Ligature(ligature) => ligature.original_chars.chars().count(),
                Kern(kern) => match kern.kind {
                    ds::KernKind::Normal => 0,
                    _ => panic!("unexpected node in hyphenated word"),
                },
                _ => panic!("unexpected node in hyphenated word"),
            };
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use boxworks::TextPreprocessor;
    use boxworks_testing::assert_box_eq;
    use boxworks_text as bwt;

    const TFM_CMR10: &'static [u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");
    #[test]
    fn first() {
        let input = "a-b";

        let want = r#"
            chars("a")
            disc(
              pre_break=[
                chars("-", font=0)
              ],
            )
            chars("b")
            "#;

        let unhyphenated: String = input.chars().filter(|c| *c != '-').collect();

        // TeX does not hyphenate the first word of a paragraph so we need to out
        // another word before the word of interest
        let tex_input = format!["x {unhyphenated}"];

        let mut tfm_file = tfm::File::deserialize(TFM_CMR10).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
        let mut tp: bwt::TextPreprocessorImpl = Default::default();
        tp.register_font(0, &tfm_file, lig_kern_program);
        tp.activate_font(0);
        let mut list = vec![];
        for word in tex_input.split_ascii_whitespace() {
            tp.add_word(word.trim_matches(' '), &mut list);
            tp.add_space(&mut list);
        }
        list.pop();

        let mut font_repo: bwt::TfmFontRepo = Default::default();
        font_repo.register_font(0, tfm_file);

        let mut hyphenator = Hyphenator::plain_tex_en_us();
        hyphenator.hyphenator.insert_exception(input);
        hyphenator.left_hyphen_min = 1;
        hyphenator.right_hyphen_min = 1;
        {
            use boxworks::Hyphenator;
            hyphenator.hyphenate(&mut list)
        }

        let tex_got: Vec<boxworks::ds::Horizontal> = list[2..].iter().cloned().collect();
        assert_box_eq!(want, tex_got);

        if std::env::var("TEXCRAFT_VERIFY").unwrap_or("".to_string()) != "tex" {
            return;
        }

        let preamble = format![
            r"
            \hyphenation{{{input}}}
            \lefthyphenmin=0
            \righthyphenmin=0
        "
        ];
        let tex_engine = boxworks::tex::new_tex_engine_binary("tex".to_string()).unwrap();
        let (_, tex_got) = boxworks::tex::build_horizontal_lists(
            tex_engine.as_ref(),
            &Default::default(),
            &preamble,
            &mut vec![tex_input].iter(),
            /*hyphenate=*/ true,
        );
        let tex_got: Vec<boxworks::ds::Horizontal> = tex_got[0].list[2..].iter().cloned().collect();

        assert_box_eq!(want, tex_got);
    }
}
