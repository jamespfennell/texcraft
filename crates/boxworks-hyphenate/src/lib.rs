use boxworks::ds;

pub struct Hyphenator {
    // TODOs:
    // (1) Don't depend on the tfm crate. There should be kind of abstraction here
    // so that this works with all font types. Maybe the solution is to have a ligkern
    // crate that contains the non-tfm logic for lig/kern programs. Or some boxworks
    // abstractions.
    // (2) Support changing the font!
    pub lig_kern_program: tfm::ligkern::CompiledProgram,
    pub hyphenator: hyphenate::Hyphenator,
    pub left_hyphen_min: i32,
    pub right_hyphen_min: i32,
}

impl Hyphenator {
    /// Creates a hyphenator with plain TeX's English patterns and defaults.
    ///
    /// The lig/kern program of the font being hyphenated is required because
    /// hyphenation breaks ligatures apart to insert discretionaries, and the
    /// program is needed to reconstitute ligatures and kerns in the result.
    /// Passing the wrong program (e.g. an empty one) silently produces
    /// un-ligatured output.
    pub fn plain_tex_en_us(lig_kern_program: tfm::ligkern::CompiledProgram) -> Self {
        Self {
            lig_kern_program,
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
        let right_boundary: Option<char> =
            loop {
                let Some(elem) = list.get(i) else { break None };
                use ds::Horizontal::*;
                match elem {
                    Char(char) => {
                        if char.font != hyphenation_font {
                            // TODO: add tests for this case including left/right boundary behaviour.
                            break None;
                        }
                        // TODO: plumb in \lccode and change this check.
                        if !char.char.is_ascii_alphabetic() {
                            break Some(char.char);
                        }
                        if s.len() + char.char.len_utf8() >= 64 {
                            // TeX only hyphenates words up to 64 bytes.
                            // TODO: this check is not quite right: unicode values in the range [128, 255)
                            // should count as 1 only.
                            break Some(char.char);
                        }
                        s.push(char.char);
                    }
                    Ligature(ligature) => {
                        // TeX.2021.898
                        if ligature.font != hyphenation_font {
                            // TODO: add tests for this case including left/right boundary behaviour.
                            break None;
                        }
                        if !ligature
                            .original_chars
                            .chars()
                            .all(|c| c.is_ascii_alphabetic())
                        {
                            break Some(ligature.original_chars.chars().next().expect(
                                "there must be at least one char for this branch to execute",
                            ));
                        }
                        if s.len() + ligature.original_chars.len() >= 64 {
                            // TeX only hyphenates words up to 64 bytes.
                            // TODO: this check is not quite right: unicode values in the range [128, 255)
                            // should count as 1 only.
                            break Some(ligature.original_chars.chars().next().expect(
                                "there must be at least one char for this branch to execute",
                            ));
                        }
                        s.push_str(&ligature.original_chars);
                    }
                    Kern(kern) => match kern.kind {
                        ds::KernKind::Normal => {
                            // TODO: set up the lig/kern program correctly.
                        }
                        _ => break None,
                    },
                    _ => break None,
                }
                // Consume the node whose characters have just been placed in s (or the normal kern).
                i += 1;
            };
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

        let mut indices = {
            let indices = hyphenater.hyphenator.calculate_indices(&lower_caser, &s);
            // TeX.2021.1200
            let left_hyphen_min: usize = match hyphenater.left_hyphen_min.try_into() {
                Ok(0) | Err(_) => 1,
                Ok(i) => i,
            };
            let right_hyphen_min: usize = match hyphenater.right_hyphen_min.try_into() {
                Ok(0) | Err(_) => 1,
                Ok(i) => i,
            };
            let hyph_max = l.saturating_sub(right_hyphen_min);
            IndexIter::new(indices, left_hyphen_min, hyph_max)
        };
        let mut next_or = indices.next();

        let mut main_iter = hyphenater.lig_kern_program.run_iter(&s, right_boundary);
        use tfm::ligkern::RunItem;
        let mut chars_pushed = 0;
        // This corresponds to the loop in TeX.2021.913 but not 1-1.
        //
        // TeX's loop is across all cut prefixes whereas this loop is over each
        // individual lig/kern element that is emitted.
        //
        // Knuth's reconstitute method sets hyphen_passed>0 if either the main lig/kern
        // program ran over a hyphen while processing the cut prefix,
        // or if the hyphen lig/kern program is non-trivial and
        // thus needs to run. His reconstitute method does *not* consider the regular case where the
        // hyphen does not interact with the lig/kern program; this case is handled in the
        // body of section 913.
        while let Some(elem) = main_iter.next() {
            let prev_chars_pushed = chars_pushed;
            // TODO: don't have 3 matches...
            chars_pushed += match &elem {
                RunItem::Char(_) => 1,
                RunItem::Ligature(ligature) => ligature.original.chars().count(),
                RunItem::Kern(_) => 0,
            };
            let last_char = match &elem {
                RunItem::Char(c) => Some(*c),
                RunItem::Ligature(ligature) => ligature.original.chars().last(),
                RunItem::Kern(_) => None,
            };
            let original_elem: ds::Horizontal = match elem {
                RunItem::Char(c) => ds::Char {
                    char: c,
                    font: hyphenation_font,
                }
                .into(),
                RunItem::Kern(scaled) => ds::Kern {
                    width: scaled,
                    kind: ds::KernKind::Normal,
                }
                .into(),
                RunItem::Ligature(ligature) => ds::Ligature {
                    included_left_boundary: false,
                    included_right_boundary: false,
                    char: ligature.c,
                    font: hyphenation_font,
                    original_chars: ligature.original,
                }
                .into(),
            };
            // TODO: debug assert this is equal to the original element in the list
            out.push(original_elem);

            let hyph_next = next_or.unwrap_or(usize::MAX);
            if hyph_next > chars_pushed {
                // No hyphen here.
                continue;
            }

            // TODO: I'm sure we can remove this expect by piping the char from the matches above.
            let last_char =
                last_char.expect("we are seeing this hyphen because we have passed a character");
            let is_hyphen_rule = hyphenater
                .lig_kern_program
                .has_replacement(Some(last_char), Some('-'));

            let (mut pop_before_disc, mut pre_break_text) = if hyph_next == chars_pushed
                && main_iter.is_separation_point()
                && !is_hyphen_rule
            {
                // regime 1: the hyphen is exactly on a lig/kern atom boundary, there is no hyphen lig/kern program.
                // In this case we put the element now, and then insert the discretionary.
                (0, "-".into())
            } else {
                // regime 2: either the hyphen was in the middle of the lig element OR there is a hyphen lig/kern
                // program. In this case we need to put the element in afterwards.
                // TODO: is this enough or do we need to reverse the atom entirely?
                // TODO: we are indexing string using chars bad bad bad.
                // Some simple unicode tests will show this :)
                (1, format!["{}-", &s[prev_chars_pushed..hyph_next]])
            };

            // This is the loop in TeX.2021.344.
            loop {
                let pre_break: Vec<ds::DiscretionaryElem> = hyphenater
                    .lig_kern_program
                    .run_iter(&pre_break_text, None)
                    .map(|elem| {
                        let d: ds::DiscretionaryElem = match elem {
                            RunItem::Char(c) => ds::Char {
                                char: c,
                                font: hyphenation_font,
                            }
                            .into(),
                            RunItem::Kern(scaled) => ds::Kern {
                                width: scaled,
                                kind: ds::KernKind::Normal,
                            }
                            .into(),
                            RunItem::Ligature(ligature) => ds::Ligature {
                                char: ligature.c,
                                font: hyphenation_font,
                                included_left_boundary: false,  // TODO
                                included_right_boundary: false, // TODO
                                original_chars: ligature.original.clone(),
                            }
                            .into(),
                        };
                        d
                    })
                    .collect();

                let hyph_next = next_or.unwrap_or(usize::MAX);
                let post_break_text = &s[hyph_next..];
                let mut post_break_iter = hyphenater
                    .lig_kern_program
                    .run_iter(post_break_text, right_boundary);
                let mut post_break: Vec<ds::DiscretionaryElem> = vec![];
                let mut post_chars_pushed = hyph_next;

                let mut post_disc: Vec<ds::Horizontal> = vec![];
                // TODO: if pop_before_disc is only ever in [0,1], change it to a boolean and simplify this code.
                // This all hinges on the other TODO about whether we need to actually add all the elements
                // since the last synchronization pt.
                for _ in 0..pop_before_disc {
                    post_disc.push(out.pop().unwrap());
                    post_disc.reverse();
                }

                // This is the loop in TeX.2021.916.
                // We want to achieve synchronization for the two iterators.
                loop {
                    if post_chars_pushed == chars_pushed
                        && post_break_iter.is_separation_point()
                        && main_iter.is_separation_point()
                    {
                        break;
                    }
                    if post_chars_pushed < chars_pushed {
                        while let Some(elem) = post_break_iter.next() {
                            post_chars_pushed += match &elem {
                                RunItem::Char(_) => 1,
                                RunItem::Kern(_) => 0,
                                RunItem::Ligature(ligature) => ligature.original.chars().count(),
                            };
                            post_break.push(match elem {
                                RunItem::Char(c) => ds::Char {
                                    char: c,
                                    font: hyphenation_font,
                                }
                                .into(),
                                RunItem::Kern(scaled) => ds::Kern {
                                    width: scaled,
                                    kind: ds::KernKind::Normal,
                                }
                                .into(),
                                RunItem::Ligature(ligature) => ds::Ligature {
                                    char: ligature.c,
                                    font: hyphenation_font,
                                    included_left_boundary: false, // TODO
                                    included_right_boundary: false, // TODO
                                    original_chars: ligature.original.clone(),
                                }
                                .into(),
                            });
                            if post_break_iter.is_separation_point() {
                                break;
                            }
                        }
                    } else {
                        while let Some(elem) = main_iter.next() {
                            chars_pushed += match &elem {
                                RunItem::Char(_) => 1,
                                RunItem::Kern(_) => 0,
                                RunItem::Ligature(ligature) => ligature.original.chars().count(),
                            };
                            post_disc.push(match elem {
                                RunItem::Char(c) => ds::Char {
                                    char: c,
                                    font: hyphenation_font,
                                }
                                .into(),
                                RunItem::Kern(scaled) => ds::Kern {
                                    width: scaled,
                                    kind: ds::KernKind::Normal,
                                }
                                .into(),
                                RunItem::Ligature(ligature) => ds::Ligature {
                                    char: ligature.c,
                                    font: hyphenation_font,
                                    included_left_boundary: false, // TODO
                                    included_right_boundary: false, // TODO
                                    original_chars: ligature.original,
                                }
                                .into(),
                            });
                            if main_iter.is_separation_point() {
                                break;
                            }
                        }
                    }
                }
                out.push(
                    ds::Discretionary {
                        pre_break,
                        post_break,
                        replace_count: post_disc.len().try_into().unwrap(),
                    }
                    .into(),
                );
                out.append(&mut post_disc);

                // We increment the hyphen index at least once to account for the hyphen we have just inserted.
                next_or = indices.next();
                // When performing synchronization we may have passed over some hyphens.
                while next_or.unwrap_or(usize::MAX) < chars_pushed {
                    next_or = indices.next();
                }
                if next_or.unwrap_or(usize::MAX) > chars_pushed {
                    // We need to consume more characters before trying more hyphens
                    break;
                }
                pop_before_disc = 0;
                pre_break_text = "-".into();
            }
        }
    }
    out
}

struct IndexIter<I> {
    inner: I,
    min: usize,
    max: usize,
}

impl<I: Iterator<Item = usize>> IndexIter<I> {
    fn new(inner: I, min: usize, max: usize) -> Self {
        Self { inner, min, max }
    }
}

impl<I: Iterator<Item = usize>> Iterator for IndexIter<I> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let n = self.inner.next()?;
        if n >= self.min && n <= self.max {
            return Some(n);
        }
        self.next()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::path::PathBuf;

    use super::*;
    use boxworks::TextPreprocessor;
    use boxworks_testing::assert_box_eq;
    use boxworks_text as bwt;

    const TFM_CMR10: &'static [u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");

    fn run_hyphenation_test(tc: TestCase) {
        let unhyphenated: String = tc.input.chars().filter(|c| *c != '-').collect();
        let hyphenation_patterns = tc.hyphenation_patterns.unwrap_or(&tc.input);

        // TeX does not hyphenate the first word of a paragraph so we need to put
        // another word before the word of interest
        let tex_input = format!["x {unhyphenated}"];

        let mut tfm_file = tfm::File::deserialize(TFM_CMR10).0.unwrap();
        {
            let (p, e) = tfm::ligkern::lang::Program::parse_compact(tc.lig_kern_program).unwrap();
            tfm_file.replace_lig_kern_program(p, e);
        }

        if std::env::var("TEXCRAFT_VERIFY").unwrap_or("".to_string()) == "tex" {
            let tfm_bytes = tfm_file.serialize();
            let mut auxiliary_files: HashMap<PathBuf, Vec<u8>> = Default::default();
            auxiliary_files.insert("specialFont.tfm".into(), tfm_bytes);

            let preamble = format![
                r"
                    \font \customFont specialFont
                    
                    \hyphenation{{{}}}
                    \lefthyphenmin={}
                    \righthyphenmin=0
                    
                    \customFont
                ",
                hyphenation_patterns,
                tc.left_hyphen_min.unwrap_or(1),
            ];
            let mut tex_engine = boxworks::tex::new_tex_engine_binary("tex".to_string()).unwrap();
            let (_, tex_got) = boxworks::tex::build_horizontal_lists(
                tex_engine.as_mut(),
                &auxiliary_files,
                &preamble,
                &mut vec![tex_input.clone()].iter(),
                /*hyphenate=*/ true,
            );
            let tex_got: Vec<boxworks::ds::Horizontal> =
                tex_got[0].list[2..].iter().cloned().collect();

            assert_box_eq!(tc.want, tex_got);
            return;
        }

        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
        let mut tp = bwt::TextPreprocessorImpl::new(bwt::Params::plain_tex_defaults());
        tp.register_font(0, &tfm_file, lig_kern_program.clone());
        tp.activate_font(0);
        let mut list = vec![];
        for word in tex_input.split_ascii_whitespace() {
            tp.add_word(word.trim_matches(' '), &mut list);
            tp.add_space(&mut list);
        }
        list.pop();

        let mut font_repo: bwt::TfmFontRepo = Default::default();
        font_repo.register_font(0, tfm_file);

        let mut hyphenator = Hyphenator::plain_tex_en_us(lig_kern_program);
        hyphenator
            .hyphenator
            .insert_exceptions(hyphenation_patterns);
        hyphenator.left_hyphen_min = tc.left_hyphen_min.unwrap_or(1);
        hyphenator.right_hyphen_min = 1;
        {
            use boxworks::Hyphenator;
            hyphenator.hyphenate(&mut list)
        }

        let tex_got: Vec<boxworks::ds::Horizontal> = list[2..].iter().cloned().collect();
        assert_box_eq!(tc.want, tex_got);
    }

    macro_rules! hyphenation_tests {
        ( $( {
            $name: ident,
            TestCase {
                $( $field: ident: $value: expr, )*
            }
        }, )* ) => {
            $(
                #[test]
                fn $name() {
                    let test_case = TestCase {
                        $( $field: $value, )*
                        ..Default::default()
                    };
                    run_hyphenation_test(test_case);
                }
            )*
        };
    }

    #[derive(Default)]
    struct TestCase {
        input: &'static str,
        lig_kern_program: &'static str,
        want: &'static str,
        hyphenation_patterns: Option<&'static str>,
        left_hyphen_min: Option<i32>,
    }

    hyphenation_tests![
        {
            no_hyphens,
            TestCase {
                input: "mint",
                lig_kern_program: "",
                want: r#"
                    chars("mint")
                "#,
            }
        },
        {
            most_simple_case,
            TestCase {
                input: "a-b",
                lig_kern_program: "",
                want: r#"
                    chars("a")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("b")
                "#,
            }
        },
        {
            lig_1,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    ab -> axb^
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a-")
                      ],
                      replace_count=2,
                    )
                    chars("a")
                    lig("x", "")
                    chars("b")
                "#,
            }
        },
        {
            lig_with_hyphen,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    a- -> ax-^
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        lig("x", "")
                        chars("-")
                      ],
                      replace_count=1,
                    )
                    chars("a")
                    chars("b")
                "#,
            }
        },
        {
            lig_with_hyphen_and_letters,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    a- -> ax-^
                    ab -> ac^_
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        lig("x", "")
                        chars("-")
                      ],
                      post_break=[
                        chars("b")
                      ],
                      replace_count=2,
                    )
                    chars("a")
                    lig("c", "b")
                "#,
            }
        },
        /* TODO: fix the bug
        {
            // BUG: handled in TeX.2021.916. If there is a left boundary char
            // synchronization cannot be a no-op as it is currently.
            left_boundary_char,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    |b -> |c^_
                ",
                want: r#"
                    chars("a")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                      post_break=[
                        chars("c")
                      ],
                      replace_count=1,
                    )
                    chars("b")
                "#,
            }
        },
        {
            // BUG: we're running the hyphen lig kern program without disabling
            // left char behaviour. Need to change run_iter to factor this in.
            left_boundary_char_does_not_change_hyphen,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    |- -> |c^_
                ",
                want: r#"
                    chars("a")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("b")
                "#,
            }
        },
        */
        {
            right_boundary_char_after_hyphen,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    -| -> -c^|
                ",
                want: r#"
                    chars("a")
                    disc(
                      pre_break=[
                        chars("-")
                        lig("c", "")
                      ],
                    )
                    chars("b")
                "#,
            }
        },
        {
            big_lig_1,
            TestCase {
                input: "a-bc",
                lig_kern_program: "
                    ab -> _x^_
                    xc -> _y^_
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        chars("-")
                      ],
                      post_break=[
                        chars("b")
                        chars("c")
                      ],
                      replace_count=1,
                    )
                    lig("y", "abc")
                "#,
            }
        },
        {
            big_lig_2,
            TestCase {
                input: "a-bc",
                lig_kern_program: "
                    ab -> _x^_
                    xc -> _y^_
                    bc -> _z^_
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        chars("-")
                      ],
                      post_break=[
                        lig("z", "bc")
                      ],
                      replace_count=1,
                    )
                    lig("y", "abc")
                "#,
            }
        },
        {
            big_lig_3,
            TestCase {
                input: "ab-c",
                lig_kern_program: "
                    ab -> _x^_
                    xc -> _y^_
                ",
                want: r#"
                    disc(
                      pre_break=[
                        lig("x", "ab")
                        chars("-")
                      ],
                      post_break=[
                        chars("c")
                      ],
                      replace_count=1,
                    )
                    lig("y", "abc")
                "#,
            }
        },
        {
            big_lig_4,
            TestCase {
                input: "ab-c",
                lig_kern_program: "
                    ab -> ax^_
                ",
                want: r#"
                    chars("a")
                    lig("x", "b")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                      replace_count=0,
                    )
                    chars("c")
                "#,
            }
        },
        /*
         * TODO: fix the bug
        {
            // BUG: probably how the simple case looks for the lig rule "b-" rather than "x-" but
            // I'm not sure.
            big_lig_with_hyphen,
            TestCase {
                input: "ab-c",
                lig_kern_program: "
                    ab -> ax^_
                    x- -> xy^-
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("axy")
                        chars("-")
                      ],
                      replace_count=2,
                    )
                    chars("a")
                    lig("x", "b")
                    chars("c")
                "#,
            }
        },
        */
        {
            big_lig_with_hyphen_2,
            TestCase {
                input: "ab-c",
                lig_kern_program: "
                    ab -> ax^b
                    x- -> xy^-
                ",
                want: r#"
                    chars("a")
                    lig("x", "")
                    chars("b")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                      replace_count=0,
                    )
                    chars("c")
                "#,
            }
        },
        {
            empty_lig_before,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    ab -> ax^b
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        chars("-")
                      ],
                      replace_count=2,
                    )
                    chars("a")
                    lig("x", "")
                    chars("b")
                "#,
            }
        },
        {
            simple_kern,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    ab -> a[100]b
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        chars("-")
                      ],
                      replace_count=2,
                    )
                    chars("a")
                    kern(0.00095pt)
                    chars("b")
                "#,
            }
        },
        {
            same_kern,
            TestCase {
                input: "a-b",
                lig_kern_program: "
                    ab -> a[100]b
                    a- -> a[100]-
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a")
                        kern(0.00095pt)
                        chars("-")
                      ],
                      replace_count=2,
                    )
                    chars("a")
                    kern(0.00095pt)
                    chars("b")
                "#,
            }
        },
        {
            synchronization_1,
            TestCase {
                input: "a-bcdefgh",
                lig_kern_program: "
                    ab -> _x^_
                    bc -> _y^_
                    cd -> _z^_
                    de -> _w^_
                    ef -> _v^_
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a-")
                      ],
                      post_break=[
                        lig("y", "bc")
                        lig("w", "de")
                        chars("f")
                      ],
                      replace_count=3,
                    )
                    lig("x", "ab")
                    lig("z", "cd")
                    lig("v", "ef")
                    # synchronization point
                    chars("gh", font=0)
                "#,
            }
        },
        {
            synchronization_2,
            TestCase {
                input: "a-bcd-ef-gh",
                lig_kern_program: "
                    ab -> _x^_
                    bc -> _y^_
                    cd -> _z^_
                    de -> _w^_
                    ef -> _v^_
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a-")
                      ],
                      post_break=[
                        lig("y", "bc")
                        lig("w", "de")
                        chars("f")
                      ],
                      replace_count=3,
                    )
                    lig("x", "ab")
                    lig("z", "cd")
                    # the hyphen here is skipped
                    lig("v", "ef")
                    # synchronization point
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                      post_break=[
                      ],
                    )
                    chars("gh", font=0)
                "#,
            }
        },
        {
            synchronization_3,
            TestCase {
                input: "a-bcde",
                lig_kern_program: "
                    ab -> _x^_
                    bc -> _y^_
                    xc -> _y^_
                    yd -> yzd^
                ",
                want: r#"
                    disc(
                      pre_break=[
                        chars("a-")
                      ],
                      post_break=[
                        lig("y", "bc")
                        lig("z", "")
                      ],
                      replace_count=2,
                    )
                    lig("y", "abc")
                    lig("z", "")
                    chars("de", font=0)
                "#,
            }
        },
        {
            word_ends_in_comma_1,
            TestCase {
                input: "baby,",
                lig_kern_program: "
                    y, -> y[100],
                    y| -> y[200]|
                ",
                want: r#"
                    chars("baby")
                    kern(0.00095pt)
                    chars(",")
                "#,
                hyphenation_patterns: Some("baby"),
            }
        },
        {
            word_ends_in_comma_2,
            TestCase {
                input: "baby,",
                lig_kern_program: "
                    y, -> y[100],
                    y| -> y[200]|
                ",
                want: r#"
                    chars("ba")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("by")
                    kern(0.00095pt)
                    chars(",")
                "#,
                hyphenation_patterns: Some("ba-by"),
            }
        },
        /* TODO: fix the bugs
        {
            right_boundary_char_override_1,
            TestCase {
                input: "ba-by",
                lig_kern_program: "
                    y| -> y.^|
                ",
                want: r#"
                    chars("ba")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("by")
                    lig(".", "|")
                "#,
            }
        },
        {
            right_boundary_char_override_2,
            TestCase {
                input: "ab.",
                lig_kern_program: "
                    |b -> |c^_
                    c. -> c,^_
                ",
                want: r#"
                    chars("a")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                      post_break=[
                        chars("c,")
                      ],
                      replace_count=1,
                    )
                    chars("b.")
                "#,
                hyphenation_patterns: Some("a-b"),
            }
        },
        {
            right_boundary_char_override_3,
            TestCase {
                input: "journey.",
                lig_kern_program: "
                    y. -> y^,_
                    ,| -> ,?^|
                ",
                want: r#"
                    chars("jour")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("ney")
                    lig(",", "|")
                    lig(",", ".")
                    lig("?", "|")
                "#,
                hyphenation_patterns: Some(""),
            }
        },
        */
        {
            right_boundary_char_override_4,
            TestCase {
                input: "journey.",
                lig_kern_program: "
                    y. -> y^,_
                    y, -> y^?_
                ",
                want: r#"
                    chars("jour")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("ney")
                    lig("?", "|")
                    lig("?", ".")
                "#,
                hyphenation_patterns: Some(""),
            }
        },

        {
            right_boundary_char_override_5,
            TestCase {
                input: "journey.",
                lig_kern_program: "
                    y. -> y,^_
                ",
                want: r#"
                    chars("jour")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("ney")
                    lig(",", "|")
                    lig(",", ".")
                "#,
                hyphenation_patterns: Some(""),
            }
        },
        {
            right_boundary_char_override_6,
            TestCase {
                input: "journey.",
                lig_kern_program: "
                    y. -> y^,_
                    y, -> y^?,
                ",
                want: r#"
                    chars("jour")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("ney")
                    lig("?", "")
                    lig(",", "|")
                    lig(",", ".")
                "#,
                hyphenation_patterns: Some(""),
            }
        },
        {
            sneezing,
            TestCase {
                input: "sneezing",
                lig_kern_program: "
                    y. -> y^,_
                    y, -> y^?,
                ",
                want: r#"
                    chars("sneez")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("ing")
                "#,
                hyphenation_patterns: Some(""),
                left_hyphen_min: Some(3),
            }
        },
        {
            difficult,
            TestCase {
                input: "d-if-fi-cult",
                lig_kern_program: "
                    ff -> _0^_
                    0i -> _1^_
                ",
                want: r#"
                    chars("di")
                    disc(
                      pre_break=[
                       chars("f-")
                      ],
                      post_break=[
                        chars("fi")
                      ],
                      replace_count=1,
                    )
                    lig("1", "ffi")
                    disc(
                      pre_break=[
                        chars("-")
                      ],
                    )
                    chars("cult")
                "#,
                hyphenation_patterns: Some(""),
                left_hyphen_min: Some(3),
            }
        },
    ];
}
