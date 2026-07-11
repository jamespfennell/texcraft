# knuthplass.dev — Design Plan

A website that demonstrates the Knuth-Plass line breaking algorithm from the
`boxworks-knuthplass` crate. Layout is split-screen like the Rust playground /
Compiler Explorer: input paragraph and algorithm parameters on the left, the
typeset paragraph on the right.

## Status (updated 2026-07-10)

- **v1 done and committed**: `knuthplass-wasm` crate
  (`break_paragraph(text, params_json)` → JSON lines/elements, all positions
  computed in Rust), `index.html` single-page app, deployment files
  mirroring ligkern.dev, CI workflow pushing `jamespfennell/knuthplass.dev`.
- **v1.1 done** (two review rounds, 2026-07-04/05 — details in git
  history): full-height split with draggable divider, teal-on-paper
  color scheme, sliders + scales + tooltips on all params,
  force-hyphenation checkbox, element info row, badness/glue-set margin
  columns with headers, kern hover dots, stats strip.
- **v1.2 design overhaul done** (2026-07-09/10, iterated with James —
  details in git history): flush layout with connected teal frame, dark
  teal masthead, full-bleed white galley (line numbers, aligned annotation
  columns, width annotation, draggable right margin, overfull slugs, chip
  toggles, vivid overlay colors), plus a full bug/cleanliness audit pass.
  Wasm grew a `passes` field (stats say "hyphenation skipped" when the
  first pass wins) and glue parse errors name their parameter.
- Remaining work: two v1.1 feature items and v2 introspection, below.
- Local dev: `caddy start --config Caddyfile.local` (port 8767), then
  `npm test` (7 puppeteer smoke tests).

## Architecture (short reference — the code is the truth)

```
text ──boxworks-text──▶ h-list (chars, ligs, kerns, glue)
     ──LineBreaker::break_line──▶ lines with glue set ratios
     ──▶ positioned elements JSON ──▶ JS places SVG elements
```

Mirrors the `box linebreak` pipeline (`boxworks-bin/src/box.rs`), including
the same English-pattern `Hyphenator`. The wasm export returns
`{ lines: [{ baseline_pt, badness, glue_ratio, glue_order, elements: [...] }] }`
or `{ error }`; element types are char/lig/kern/glue with `x_pt`/`width_pt`.

## Design decisions (rationale not recoverable from the code)

- **SVG rendering, positions computed in Rust.** Chosen over HTML/CSS
  specifically because v2 introspection is a diagram (connector paths,
  breakpoint chain highlights). JS never measures text; all positions come
  from `Scaled` arithmetic on the embedded cmr10 TFM.
- **Latin Modern OTF for glyph shapes only** (plain OTF from CTAN, not
  WOFF2 — browsers handle OTF fine). Metrics come from the TFM; browser
  shaping is disabled (`font-kerning: none`, `liga`/`clig` off). Display
  needs an OT1→Unicode mapping (f-ligatures U+FB00–FB04, TeX quotes, en/em
  dashes).
- **Badness** is approximated in the wasm layer as 100·|glue ratio|³, with
  1000000 marking overfull lines (detected geometrically, since overfull
  glue ratios are clamped to unity upstream, per TeX.2021.664). The UI
  never shows the sentinel: overfull lines read "inf" in the badness
  column and make the stats total "infinite (N lines overfull)".
- **The overfull slug is presentation-only**: the 5pt black rule is drawn
  in the SVG from the geometric detection; appending TeX's real
  \overfullrule in `HBox::pack` (TeX.2021.666) remains an upstream TODO.
- **Latin Modern is galley-only** — it is the one font Boxworks supports,
  not branding; UI text is Inter, data roles (parameter names, stats,
  annotations) are JetBrains Mono, both from Google Fonts.
- **URL is the state store**, only non-default values encoded. Two
  re-encodings to be aware of: overlay defaults flipped 2026-07-05
  (kerns/ligs now default off, badness/glue-set on — old bookmarked overlay
  params are reinterpreted), and the force-hyphenation checkbox encodes
  `forcehyph=1` plus the value pre_tolerance will *revert to*, not −1.
- **Tolerance sliders range from −1** (meaningful in TeX: the pass always
  fails; −1 pre_tolerance is the standard force-hyphenation idiom).
- **Output leading is not exact TeX**: `LINE_EXTRA_PT = 4` in `render()`
  adds presentational leading so the kern hover dots have a lane between
  lines. Set it to 0 to restore exact `\baselineskip` geometry. Breaks,
  badness, and glue values are unaffected.
- **Single width for v1** (+ maybe `\parindent`); shaped paragraphs via
  `line_widths`/`line_indents` are future work.
- **No debounce**: the wasm call runs on every input event with no visible
  lag.
- Default text is the opening paragraph of *A Farewell to Arms*,
  ASCII-ified ("cafes", straight apostrophes) — the site rejects characters
  the font lacks.

## Remaining v1.1 work

- **Boxworks language tab** (ambitious) — tabs on the output panel:
  *Typeset* | *Boxworks*. A second WASM export returns the broken paragraph
  as Boxworks language text via `bwl::convert::ToBoxLang` + `Display`,
  exactly like `box linebreak`'s default output. Adds a `boxworks-lang`
  dependency to the wasm crate.
- **Multiple paragraphs** — split the input on blank lines (TeX convention),
  run the breaker per paragraph, stack results with `\parskip`
  (plain TeX: `0pt plus 1pt`). JSON schema grows a `paragraphs` level.
  This also motivates adding a `\parindent` param (plain TeX: 20pt) via the
  `LineBreaker::line_indents` field — verify upstream support when
  implementing.

## Follow-ups (James's review, 2026-07-10)

- **Max width for the left panel's content.** On very wide viewports the
  input box and controls stretch too wide; cap their width.
- **The page-breaking penalties don't visibly do anything.** The
  penalties that only affect page breaking (broken_penalty, club_penalty,
  final_widow_penalty, inter_line_penalty) are configurable but change
  nothing in the output. Either remove them, or add a penalties column to
  the galley showing the penalty attached to each line.
- **Deduplicate the glue parsing code** shared by `knuthplass-wasm` and
  `box linebreak` (`crates/boxworks-bin/src/box.rs`) — the copies carry a
  "keep in sync" comment today.
- **Per-line widths and indents**, once implemented in
  boxworks-knuthplass (`line_widths`/`line_indents`): expose them in the
  UI — there is probably something nice to do in the galley (e.g. drag
  individual line margins, shaped-paragraph presets).
- **An "about"/documentation popup** explaining what the site shows and
  how to read the galley.

## Upstream bugs found while building (in the crates, not the site)

1. ~~**The hyphenation pass drops the final line**~~ — fixed by James
   2026-07-04: the artificial-demerits branch (TeX.2021.854) didn't
   deactivate the old active node, which tied and won the strict `<`
   best-node scan (TeX.2021.874). Fallout fixed with it: overfull glue
   ratios now clamp to unity (TeX.2021.664); TeX's \overfullrule appending
   (TeX.2021.666) is **still TODO** — suppressed in TeX-generated test data
   via `\hfuzz=\maxdimen`. Regression test `wolf_hall_2in` in
   boxworks-knuthplass; its expectation is certified against real TeX by
   the `TEXCRAFT_VERIFY=tex` test mode (needs `max_print_line` raised).
2. ~~**Hyphenation destroys ligatures**~~ — fixed 2026-07-04 (James's
   diagnosis): call sites constructed the `Hyphenator` without its lig/kern
   program, so reconstitution ran empty. The footgun is closed upstream:
   `plain_tex_en_us` now *requires* the lig/kern program as an argument.
3. The "need emergency attempt" panic is effectively unreachable (the
   second pass forces a solution → overfull lines, not a panic). The UI
   keeps a try/catch anyway.
4. **OPEN: wasm allocator crash on some non-ASCII inputs.** Passing certain
   texts containing non-ASCII characters (e.g. "és and artillery up", but
   not "és and" — heap-state dependent, non-monotonic in length) aborts the
   wasm call with `dlmalloc-0.2.11 ... assertion failed: psize <= size +
   max_overhead`. That is the allocator vendored in the Rust toolchain's
   std (rustc 1.94.0 locally), likely tripped by wasm-bindgen's
   grow-then-shrink realloc when passing non-ASCII strings across the JS
   boundary. Our code is not at fault: the same inputs work natively, and
   the font-character validation returns a proper error whenever the call
   survives. When it aborts, the UI now shows a generic "internal error"
   message. Candidate fixes to investigate: a newer toolchain (does
   rust:1.95 in the Dockerfile vendor a fixed dlmalloc?); overriding
   `#[global_allocator]` in the wasm crate with a fixed allocator crate;
   or an upstream report. Note ligkern.dev and hyphenate.dev pass user
   text across the same boundary and may be equally affected.

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
  This is the Compiler-Explorer-style payoff. (The stats strip's "total
  demerits" also waits for this data.)
- Show pass number (pre_tolerance pass vs hyphenation pass) and what
  hyphenation points were considered
- More fonts (cmbx10, cmti10, …)
- Side-by-side comparison of two parameter sets
- First-fit / greedy breaking comparison to show why Knuth-Plass is better
