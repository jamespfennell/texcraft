# ligkern.dev — Design Plan

A website that visualizes the lig/kern algorithm from the `tfm` crate,
analogous to how hyphenate.dev visualizes the Liang hyphenation algorithm.

## Core interaction

1. User selects a lig/kern program (a bundled font or a custom program they paste in)
2. User types text into an input box
3. The output of running the lig/kern program on that text is shown as a
   horizontal sequence of typed elements

## Output element types

The `CompiledProgram::run` method produces three kinds of output via the
`Emitter` trait. Each maps to a distinct visual element:

- **Char** — a plain character, unchanged by the program. Shows the character itself.
- **Kern** — a spacing adjustment. Shows the kern value (e.g. `−0.083pt`).
  The element takes up visible horizontal space but is not trying to be
  typeset-correct — it is wide enough to show its label. Positive kerns are
  colored differently from negative ones.
- **Ligature** — a replacement character produced by a ligature rule. Shows
  the Unicode hex code of the replacement (e.g. `U+FB01`) and the original
  consumed characters underneath (e.g. `fi → U+FB01`). Actual glyph rendering
  is a future concern (v1: hex only).

## Program sources

Two modes, selectable via a dropdown/tab:

### Bundled fonts
Pre-compiled programs loaded from real `.tfm` files embedded in the WASM binary.
Initial set:
- `cmr10` — Computer Modern Roman 10pt (canonical example; has fi/fl/ff/ffi/ffl
  ligatures and extensive kerning pairs like A/V, T/a, etc.)

More fonts can be added later.

### Custom program
User pastes in a program in one of two formats (toggle between them):
- **Compact format** — the `Operation::parse_compact` / `Program::parse_compact`
  format already in the codebase: `ab -> _x^_`, `av -> a[-83]v`, etc.
- **Property list format** — standard TeX `.pl` format: `(LIG C f C fi)`, etc.

Errors in the custom program are shown inline below the editor.

## WASM API (ligkern-wasm crate)

Three exported functions, all returning JSON strings:

```
run_bundled(font_name: &str, text: &str) -> String
run_compact(program: &str, text: &str) -> String
run_pl(program: &str, text: &str) -> String
list_bundled_fonts() -> String   // returns [{name, display_name}, ...]
```

### Output JSON schema

```json
{
  "elements": [
    { "type": "char",      "char": "f" },
    { "type": "kern",      "value_scaled": -5462, "value_pt": -0.083 },
    { "type": "ligature",  "char_hex": "FB01", "original": "fi",
      "includes_left_boundary": false, "includes_right_boundary": false }
  ]
}
```

`value_scaled` is the raw `Scaled` integer; `value_pt` is the float in points.

### Error JSON schema (for custom program modes)

```json
{ "error": "description of parse or compile error" }
```

## Page layout

```
ligkern.dev                    ← h1, clickable to reset

[ dropdown: cmr10 ▾ ]         ← program selector
                               ← custom program editor (hidden when bundled)

[ input text box ]

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  ← separator
[f][i][ fi→U+FB01 ][-0.08pt][V]…       ← output elements row

[how it works]                 ← collapsible/scrollable explanation section
[links]
```

The output row is a single horizontal flex row that wraps on overflow.
Each element type gets a distinct visual treatment (background color / border).

## File structure (mirrors hyphenate.dev)

```
sites/ligkern.dev/
  index.html          ← single-page app, all CSS + JS inline
  ligkern-wasm/
    Cargo.toml
    src/
      lib.rs
  Caddyfile
  Caddyfile.local
  Dockerfile
  package.json
  smoke.test.js
```

## Decided

- Demo strings for cmr10: "office", "difficult", "AVAST", "Typeface"
- Kerns displayed in points (cmr10 is 10pt, so scaled values divide by 65536 × 10)
- No "how it works" section for v1

## Future work

- v2: render actual glyphs for ligatures (requires webfont with the right codepoints,
  or embedding a subset of the font as WOFF2)
- v2: "explain" mode showing which rule in the program fired for each pair
  (analogous to hyphenate.dev's pattern grid)
