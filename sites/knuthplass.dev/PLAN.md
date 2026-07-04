# knuthplass.dev — Design Plan

A website that demonstrates the Knuth-Plass line breaking algorithm from the
`boxworks-knuthplass` crate. Layout is split-screen like the Rust playground /
Compiler Explorer: input paragraph and algorithm parameters on the left, the
typeset paragraph on the right.

## Status (2026-07-03)

**Milestone 1 — done (committed).** `knuthplass-wasm` crate exporting
`break_paragraph(text, params_json)`. Mirrors the `box linebreak` pipeline;
all positions computed in Rust. 7 native unit tests. Implementation notes
that go beyond the original schema sketch:

- Each line carries `baseline_pt` / `height_pt` / `depth_pt` (vertical
  placement follows TeX's `\baselineskip`=12pt / `\lineskip`=1pt rule), plus
  `badness` (approximated as 100·|glue ratio|³; 1000000 for overfull).
- Char/lig elements carry a `unicode` field mapped through an OT1→Unicode
  table (f-ligatures at U+FB00–FB04, `` `` ``→U+201C, `''`→U+201D,
  `--`/`---`→en/em dash, etc.).

**Milestone 2 — done (uncommitted).** The site itself:

- `index.html` single-page app per the layout below; parameter inputs are
  generated from a JS table; modified params highlighted; state in the URL
  (only non-default values encoded).
- SVG rendering as designed. A fourth overlay toggle was added: per-line
  **badness** labels in the right margin.
- Font: `fonts/lmroman10-regular.otf` from CTAN (plain OTF, not WOFF2 —
  browsers handle OTF fine and it saved a conversion step). Its cmap was
  verified to contain all the OT1-mapped codepoints. Rendering disables
  browser shaping (`font-kerning: none`, `liga`/`clig` off).
- Default text is Alice paragraph 1 with TeX-style `` `` `` quoting so the
  quote ligatures fire.
- Deployment files mirror ligkern.dev: Caddyfile, Caddyfile.local (port
  8767), Dockerfile, package.json; 5 puppeteer smoke tests pass.
- Local dev: `caddy start --config Caddyfile.local` then `npm test`.

**Upstream bugs found while building (in the crates, not the site):**

1. **The hyphenation pass drops the final line.** Whenever pass 2 runs and
   selects a solution, the last line is missing; a single-line paragraph
   (e.g. one word) produces nothing at all. Repro:
   `box linebreak --width 300pt --pre-tolerance=-1 --output-text "hello world this is a test"`
   → empty. Curiously `--tolerance=10000` produces complete output.
   `post_line_break` is innocent (it emits every breakpoint given), so the
   chain returned by `break_line_single_attempt` is missing the final
   break. Prime suspect: best-node selection at TeX.2021.874 combined with
   the artificial-demerits path not deactivating the old node (ties on
   total_demerits + strict `<` keep the non-final node). The UI shows a
   "known bug" message when zero lines come back.
2. **Hyphenation destroys ligatures** (`boxworks-hyphenate`): ligs are split
   into plain chars to insert discretionaries and never reconstituted, so
   pass 2 loses ligatures (and lig kerns) even in words that aren't broken.
   TeX re-ligatures the fragments. Surfaced on the site as "ligatures for
   difficult don't work".
3. The "need emergency attempt" panic is effectively unreachable: the second
   pass runs with `force_solution=true`, so infeasible tolerances produce
   overfull lines (badness 1000000) rather than panicking. The UI keeps a
   try/catch anyway.

## Core interaction

1. User types or pastes a paragraph of text on the left
2. User adjusts the paragraph width and any of the Knuth-Plass parameters
3. The right panel re-renders the typeset paragraph on every change,
   using the exact output of the algorithm (positions from TFM metrics,
   not browser text layout)

## Left panel: inputs

- **Paragraph text** — a textarea, prefilled with a demo paragraph
  (e.g. the first paragraph of Alice in Wonderland, already used in
  `boxworks-knuthplass/testdata`)
- **Width** (`\hsize`) — the main knob; a slider + numeric input in pt.
  Dragging the slider and watching breaks reflow is the signature demo.
- **Parameters** — the fields of `boxworks_knuthplass::Params`, grouped:
  - *Penalties*: `hyphen_penalty`, `ex_hyphen_penalty`, `broken_penalty`,
    `club_penalty`, `final_widow_penalty`, `inter_line_penalty`, `line_penalty`
  - *Demerits*: `adj_demerits`, `double_hyphen_demerits`, `final_hyphen_demerits`
  - *Tolerance*: `pre_tolerance`, `tolerance`, `looseness`
  - *Glue*: `left_skip`, `right_skip`, `par_fill_skip` — text inputs parsed
    in TeX glue syntax ("0pt plus 1fil"), same parsing as `box linebreak`
  - A "reset to plain TeX defaults" button (`Params::plain_tex_defaults`)
- Numeric params are number inputs; consider sliders for the high-impact ones
  (tolerance, hyphen_penalty, adj_demerits).

## Right panel: typeset output

The paragraph rendered exactly as the algorithm broke it. Rendering is **SVG**:

- The WASM layer returns, per line, a list of positioned elements
  (x offset + width in pt, both computed in Rust with `Scaled` arithmetic
  from the TFM metrics and the glue set ratio). JS only places elements —
  it never measures text itself.
- Each char/ligature is an SVG `<text>` element at its exact x position.
  SVG viewBox is in pt units, so zoom/resize is free.
- Glyphs come from a **Latin Modern webfont** (WOFF2, self-hosted). We use it
  only for glyph *shapes*; all positioning comes from cmr10's TFM data.
  Requires an OT1 → Unicode mapping for display (most of ASCII maps directly;
  ligatures ff/fi/fl/ffi/ffl are U+FB00–FB04; quotes and a few others differ).
- v1 font: cmr10 only. The `.tfm` is embedded in the WASM binary
  (same approach as ligkern.dev).

### Kern & ligature visibility

Kerns and ligatures are invisible in correct typeset output, so they are shown
via toggleable overlays (checkboxes above the output):

- **Show kerns** — a thin colored marker in the kern's gap (color-coded
  positive vs negative), hover tooltip with the value in pt
- **Show ligatures** — colored underline/background on ligature glyphs,
  hover tooltip showing original chars (e.g. "ffi → U+FB03")
- **Show glue** — shade inter-word spaces; tooltip with natural/stretch/shrink
  and the set width for that line
- Per-line badness/demerits shown in the margin (cheap, and a preview of v2)

## Pipeline (mirrors `box linebreak`, see `boxworks-bin/src/box.rs`)

```
text ──boxworks-text──▶ h-list (chars, ligs, kerns, glue)
     ──LineBreaker::break_line_all_attempts──▶ breakpoints
     ──▶ lines with glue set ratios ──▶ positioned elements JSON
```

Hyphenation: use the same `Hyphenator` the CLI uses (English patterns),
embedded in WASM.

## WASM API (knuthplass-wasm crate)

One main export, returning a JSON string:

```
break_paragraph(text: &str, params_json: &str) -> String
```

Input `params_json`: all `Params` fields plus `width_pt`. Omitted fields use
plain TeX defaults.

### Output JSON schema (sketch)

```json
{
  "lines": [
    {
      "width_pt": 300.0,
      "badness": 23,
      "elements": [
        { "type": "char", "x_pt": 0.0, "width_pt": 5.5, "unicode": "T" },
        { "type": "lig",  "x_pt": 5.5, "width_pt": 8.1, "unicode": "ﬁ",
          "original": "fi" },
        { "type": "kern", "x_pt": 13.6, "width_pt": -0.83 },
        { "type": "glue", "x_pt": 20.0, "width_pt": 3.9,
          "natural_pt": 3.33, "stretch_pt": 1.66, "shrink_pt": 1.11 }
      ]
    }
  ]
}
```

### Error JSON schema

```json
{ "error": "description of the error" }
```

## Page layout

```
knuthplass.dev                                    ← h1, clickable to reset
┌──────────────────────────┬──────────────────────────────┐
│ [paragraph textarea]     │ ☑ kerns ☑ ligatures ☐ glue   │
│                          │                              │
│ width ────●──── 300pt    │   Alice was beginning to     │
│                          │   get very tired of sit-     │
│ ▸ Penalties              │   ting by her sister on      │
│ ▸ Demerits               │   the bank, …                │
│ ▸ Tolerance              │                              │
│ ▸ Glue                   │                              │
│ [reset to defaults]      │                              │
└──────────────────────────┴──────────────────────────────┘
```

Parameter groups are collapsible so the common case (text + width) is clean.
Stacks vertically on mobile. State saved in the URL bar (same mechanism as
ligkern.dev).

## File structure (mirrors ligkern.dev)

```
sites/knuthplass.dev/
  index.html          ← single-page app, all CSS + JS inline
  knuthplass-wasm/
    Cargo.toml
    src/lib.rs
  fonts/              ← Latin Modern OTF (self-hosted)
  Caddyfile
  Caddyfile.local
  Dockerfile
  package.json
  smoke.test.js
  PLAN.md
```

## Decided

- Webfont: **Latin Modern** (canonical Computer Modern successor,
  MathJax-proven; used for glyph shapes only)
- Positions computed in **Rust** (Scaled arithmetic already exists there);
  JS only places elements
- v1 width model: **single width** (+ maybe `\parindent`); shaped paragraphs
  via `line_widths`/`line_indents` arrays are future work
- Debounce: none needed — the WASM call runs on every input/slider event
  with no visible lag

## v1.1 — planned improvements (from review, 2026-07-03)

### Layout & look

- ~~**Full-height split layout**~~ — done (2026-07-03): 50/50 split filling
  the viewport, panels scroll internally, attribution moved into the header,
  mobile stacks with normal page scrolling. Draggable divider still a
  nice-to-have for later.
- **New color scheme** — keep the current visual language but change the
  palette; the current one is inherited from ligkern.dev and the sites
  should be distinguishable. Proposal: teal/green accent replacing the
  indigo, warmer overlay tones. Exact values at implementation time.

### Parameters panel

- **Slider + number inputs** — every numeric param gets a slider with a
  tuned range plus a small editable number, like the width control:
  tolerances 0–10000 (log-scale), penalties −10000–10000, demerits
  0–100000 (log-scale), looseness −3–+3. Glue params stay text inputs.
- **Explanatory tooltips on every parameter** — one- or two-sentence
  TeXbook-derived explanation, stored in the JS parameter table.
- **Force-hyphenation checkbox** (Tolerance group) — sets
  `pre_tolerance = -1` so the first (no-hyphenation) pass always fails and
  the hyphenated pass always runs, matching the TeX idiom.

### Output panel

- **Custom tooltips on every element** — replace the native `<title>`
  tooltips with a styled tooltip that appears instantly. Invisible hover
  target rects exist for *all* elements regardless of overlay toggles
  (overlays only add color). Content:
  - glue: ideal (natural) width, stretch/shrink spec, actual set width and
    the delta ("3.33pt, stretched +1.42pt → 4.76pt")
  - kern: value in pt
  - ligature: original chars → glyph, width
  - char: glyph, OT1 code, width
- **Glue-set margin annotation** — toggle (next to badness) showing how much
  each line stretched or shrank, e.g. "+1.9pt (38% of stretch)"; the value
  is the line's glue ratio.
- **Summary statistics strip** — above the output: number of lines,
  hyphenated lines, total badness, worst-line badness. Total demerits once
  the v2 introspection data is available.
- **Boxworks language tab** (ambitious) — tabs on the output panel:
  *Typeset* | *Boxworks*. A second WASM export returns the broken paragraph
  as Boxworks language text via `bwl::convert::ToBoxLang` + `Display`,
  exactly like `box linebreak`'s default output. Adds a `boxworks-lang`
  dependency to the wasm crate.

### Input

- **Multiple paragraphs** — split the input on blank lines (TeX convention),
  run the breaker per paragraph, stack results with `\parskip`
  (plain TeX: `0pt plus 1pt`). JSON schema grows a `paragraphs` level.
  This also motivates adding a `\parindent` param (plain TeX: 20pt) via the
  `LineBreaker::line_indents` field — verify upstream support when
  implementing.

## Future work (v2+)

- **Introspection mode** — hovering/clicking any feasible breakpoint
  highlights it plus the chain of previous breakpoints that optimally lead
  to it. The data is the passive node linked list (`previous_node_index`);
  the WASM layer ships each breakpoint with its predecessor index and JS
  walks the chain. All feasible breakpoints get markers (not just chosen
  ones — a candidate's optimal chain generally passes mid-line through the
  rendered paragraph). Also show per-break demerits and fitness class.
  Requires capturing passive nodes via `debug::Logger` before they're
  discarded — verify/extend the logger to record predecessor indices.
  This is the Compiler-Explorer-style payoff.
- Show pass number (pre_tolerance pass vs hyphenation pass) and what
  hyphenation points were considered
- More fonts (cmbx10, cmti10, …)
- Side-by-side comparison of two parameter sets
- First-fit / greedy breaking comparison to show why Knuth-Plass is better
