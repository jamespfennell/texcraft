const puppeteer = require("puppeteer");

const PORT = 8767;

async function main() {
  const browser = await puppeteer.launch({
    args: ["--no-sandbox", "--disable-setuid-sandbox"],
  });
  try {
    // Test 1: the default paragraph renders as multiple justified lines of
    // SVG text with ligature and kern overlays.
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);
      await page.waitForSelector("#output-svg text");

      const counts = await page.evaluate(() => ({
        texts: document.querySelectorAll("#output-svg text:not(.badness-label)").length,
        ligs: document.querySelectorAll("#output-svg .lig-rect").length,
        kerns: document.querySelectorAll("#output-svg .kern-rect").length,
        labels: document.querySelectorAll("#output-svg .badness-label").length,
        hovers: document.querySelectorAll("#output-svg .hover-target").length,
        stats: document.getElementById("stats").textContent,
        baselines: new Set(
          [...document.querySelectorAll("#output-svg text:not(.badness-label)")].map(
            (t) => t.getAttribute("y"),
          ),
        ).size,
      }));

      if (counts.texts < 100) {
        throw new Error(`expected many glyphs, got ${counts.texts}`);
      }
      if (counts.baselines < 4) {
        throw new Error(`expected several lines, got ${counts.baselines} baselines`);
      }
      if (counts.kerns !== 0 || counts.ligs !== 0) {
        throw new Error("kern/ligature overlays should be off by default");
      }
      if (counts.labels < 2 * counts.baselines) {
        throw new Error(
          `badness + glue-set labels should be on by default, got ${counts.labels}`,
        );
      }
      if (counts.hovers < counts.texts) {
        throw new Error(
          `expected a hover target per element, got ${counts.hovers} for ${counts.texts} glyphs`,
        );
      }
      const statsRe =
        /^\d+ lines? · (hyphenation skipped|\d+ hyphenated)( · \d+ lines? overfull)? · total demerits -?\d+$/;
      if (!statsRe.test(counts.stats)) {
        throw new Error(`unexpected stats: "${counts.stats}"`);
      }
      console.log(`PASS test 1: default render: ${JSON.stringify(counts)}`);
      await page.close();
    }

    // Test 1b: a paragraph with f-ligatures gets ligature overlays showing
    // the combined glyph.
    {
      const page = await browser.newPage();
      await page.setViewport({ width: 1200, height: 800 });
      await page.goto(
        `http://localhost:${PORT}/?text=it+is+difficult+to+fly+away+to+the+office&width=200&ligs=1&kerns=1`,
      );
      await page.waitForSelector("#output-svg .lig-rect");

      const ligs = await page.evaluate(() =>
        [...document.querySelectorAll("#output-svg .hover-target")]
          .map((r) => r.dataset.info)
          .filter((t) => t.startsWith("ligature")),
      );
      if (!ligs.some((l) => l.includes("ffi") && l.includes("ﬃ"))) {
        throw new Error(`expected an ffi ligature, got: ${JSON.stringify(ligs)}`);
      }

      // Hovering an element shows its details in the info row.
      const target = await page.evaluate(() => {
        const rect = [...document.querySelectorAll("#output-svg .hover-target")]
          .find((r) => r.dataset.info.startsWith("ligature"))
          .getBoundingClientRect();
        return { x: rect.x + rect.width / 2, y: rect.y + rect.height / 2 };
      });
      await page.mouse.move(target.x, target.y);
      const info = await page.evaluate(
        () => document.getElementById("element-info").textContent,
      );
      if (!info.startsWith("ligature") || !info.includes("width")) {
        throw new Error(`expected ligature details in the info row, got: "${info}"`);
      }

      // Kerns get a hoverable dot below the line.
      const kernDot = await page.evaluate(() => {
        const d = document.querySelector("#output-svg .kern-dot");
        if (!d) return null;
        const r = d.getBoundingClientRect();
        return { x: r.x + r.width / 2, y: r.y + r.height / 2 };
      });
      if (!kernDot) {
        throw new Error("expected kern dots under the lines");
      }
      await page.mouse.move(kernDot.x, kernDot.y);
      const kernInfo = await page.evaluate(
        () => document.getElementById("element-info").textContent,
      );
      if (!kernInfo.startsWith("kern")) {
        throw new Error(`expected kern details in the info row, got: "${kernInfo}"`);
      }
      console.log(
        `PASS test 1b: ligature overlays + info row: "${info}", kern dot: "${kernInfo}"`,
      );
      await page.close();
    }

    // Test 2: narrowing the width produces more lines.
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);
      await page.waitForSelector("#output-svg text");

      const lineCount = () =>
        page.evaluate(
          () =>
            new Set(
              [...document.querySelectorAll("#output-svg text")].map((t) =>
                t.getAttribute("y"),
              ),
            ).size,
        );

      const before = await lineCount();
      await page.evaluate(() => {
        const n = document.getElementById("width-number");
        n.value = "150";
        n.dispatchEvent(new Event("input"));
      });
      const after = await lineCount();
      if (after <= before) {
        throw new Error(`expected more lines at 150pt: before=${before} after=${after}`);
      }
      console.log(`PASS test 2: width 300pt -> ${before} lines, 150pt -> ${after} lines`);
      await page.close();
    }

    // Test 3: state round-trips through the URL.
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}/?text=hello+world&width=200&glue=1`);
      await page.waitForSelector("#output-svg text");

      const state = await page.evaluate(() => ({
        text: document.getElementById("text-input").value,
        width: document.getElementById("width-number").value,
        glueRects: document.querySelectorAll("#output-svg .glue-rect").length,
      }));
      if (state.text !== "hello world" || state.width !== "200") {
        throw new Error(`URL state not loaded: ${JSON.stringify(state)}`);
      }
      if (state.glueRects === 0) {
        throw new Error("expected glue overlays to be enabled from the URL");
      }
      console.log(`PASS test 3: URL state: ${JSON.stringify(state)}`);
      await page.close();
    }

    // Test 4: an impossible tolerance still renders (the second pass forces
    // a solution, producing overfull lines) rather than crashing the page.
    {
      const page = await browser.newPage();
      await page.goto(
        `http://localhost:${PORT}/?tolerance=1&pre_tolerance=1&width=100&badness=1`,
      );
      await page.waitForSelector("#output-svg text");

      const labels = await page.evaluate(() =>
        [...document.querySelectorAll("#output-svg .badness-label")].map(
          (t) => t.textContent,
        ),
      );
      if (labels.length === 0) {
        throw new Error("expected badness labels");
      }
      // Overfull lines show "inf" in the badness column, and the stats line
      // reports the total as infinite.
      if (!labels.includes("inf")) {
        throw new Error(
          `expected overfull (inf) lines at tolerance=1, got: ${JSON.stringify(labels)}`,
        );
      }
      const stats = await page.evaluate(
        () => document.getElementById("stats").textContent,
      );
      if (!/\d+ lines? overfull/.test(stats)) {
        throw new Error(`expected an overfull-lines count, got: "${stats}"`);
      }
      console.log(`PASS test 4: tolerance=1 renders with overfull lines: "${stats}"`);
      await page.close();
    }

    // Test 5: parameter sliders and the force-hyphenation checkbox work.
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}/?forcehyph=1`);
      await page.waitForSelector("#output-svg text");

      const state = await page.evaluate(() => ({
        preTolDisabled: document.getElementById("param-pre_tolerance").disabled,
        preTolValue: document.getElementById("param-pre_tolerance").value,
        checked: document.getElementById("force-hyphenation").checked,
        sliders: document.querySelectorAll(".param-slider").length,
        tooltips: document.querySelectorAll(".param-grid [data-tooltip]").length,
      }));
      if (!state.checked || !state.preTolDisabled || state.preTolValue !== "-1") {
        throw new Error(`force hyphenation not applied from URL: ${JSON.stringify(state)}`);
      }

      // Unchecking the box reverts pre_tolerance to its previous value
      // (the plain TeX default here, since the URL didn't override it).
      const reverted = await page.evaluate(() => {
        const cb = document.getElementById("force-hyphenation");
        cb.checked = false;
        cb.dispatchEvent(new Event("change"));
        return document.getElementById("param-pre_tolerance").value;
      });
      if (reverted !== "100") {
        throw new Error(`expected pre_tolerance to revert to 100, got ${reverted}`);
      }
      if (state.sliders < 12) {
        throw new Error(`expected a slider per numeric param, got ${state.sliders}`);
      }
      if (state.tooltips < 16) {
        throw new Error(`expected tooltips on all params, got ${state.tooltips}`);
      }

      // Dragging the tolerance slider to its minimum changes the layout
      // (tolerance 0 forces overfull-ish solutions) and syncs the number.
      const lineCount = () =>
        page.evaluate(
          () =>
            new Set(
              [...document.querySelectorAll("#output-svg text")].map((t) =>
                t.getAttribute("y"),
              ),
            ).size,
        );
      const before = await lineCount();
      await page.evaluate(() => {
        const slider = document.querySelector("#param-tolerance")
          .previousElementSibling;
        slider.value = slider.max;
        slider.dispatchEvent(new Event("input"));
      });
      const toleranceValue = await page.evaluate(
        () => document.getElementById("param-tolerance").value,
      );
      if (Number(toleranceValue) !== 10000) {
        throw new Error(`slider did not sync number input: ${toleranceValue}`);
      }
      const after = await lineCount();

      // The looseness chip row (it has no slider): clicking +1 syncs the
      // hidden input, encodes the URL, and makes the paragraph one line
      // longer (achievable here, with tolerance at max from the drag above).
      const looseness = await page.evaluate(() => {
        const radio = document.querySelector(
          'input[name="chips-looseness"][value="1"]',
        );
        radio.checked = true;
        radio.dispatchEvent(new Event("change"));
        return {
          value: document.getElementById("param-looseness").value,
          url: new URLSearchParams(location.search).get("looseness"),
        };
      });
      if (looseness.value !== "1" || looseness.url !== "1") {
        throw new Error(`looseness chip did not sync: ${JSON.stringify(looseness)}`);
      }
      const afterLoose = await lineCount();
      if (afterLoose !== after + 1) {
        throw new Error(
          `looseness=1 should add a line: ${after} -> ${afterLoose}`,
        );
      }

      console.log(
        `PASS test 5: sliders + force hyphenation: ${JSON.stringify(state)}, ` +
          `tolerance slider max -> ${toleranceValue}, lines ${before} -> ${after}, ` +
          `looseness=1 -> ${afterLoose}`,
      );
      await page.close();
    }

    // Test 6: the divider between the panels can be dragged to resize the
    // split, and double-clicking it re-fits the output panel to the sheet
    // (the same fit chosen on first load, so the width returns to it).
    {
      const page = await browser.newPage();
      // The default 800px viewport triggers the mobile layout, which stacks
      // the panels and hides the divider.
      await page.setViewport({ width: 1200, height: 800 });
      await page.goto(`http://localhost:${PORT}`);
      await page.waitForSelector("#output-svg text");

      const controlsWidth = () =>
        page.evaluate(
          () => document.getElementById("controls").getBoundingClientRect().width,
        );
      const before = await controlsWidth();

      const d = await page.evaluate(() => {
        const r = document.getElementById("divider").getBoundingClientRect();
        return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
      });
      // Drag outward: the auto-fit initial split already sits near the 20%
      // minimum at this viewport, so there is little room to shrink.
      await page.mouse.move(d.x, d.y);
      await page.mouse.down();
      await page.mouse.move(d.x + 150, d.y, { steps: 5 });
      await page.mouse.up();

      const after = await controlsWidth();
      if (after < before + 100) {
        throw new Error(
          `expected dragging the divider to grow the controls panel: before=${before} after=${after}`,
        );
      }

      const d2 = await page.evaluate(() => {
        const r = document.getElementById("divider").getBoundingClientRect();
        return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
      });
      await page.mouse.click(d2.x, d2.y, { clickCount: 2 });
      const reset = await controlsWidth();
      if (Math.abs(reset - before) > 2) {
        throw new Error(
          `expected double-click to reset the split: before=${before} reset=${reset}`,
        );
      }
      console.log(
        `PASS test 6: divider drag ${Math.round(before)}px -> ${Math.round(after)}px, dblclick reset -> ${Math.round(reset)}px`,
      );
      await page.close();
    }

    console.log("all smoke tests passed");
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
