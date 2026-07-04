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
        texts: document.querySelectorAll("#output-svg text").length,
        ligs: document.querySelectorAll("#output-svg .lig-rect").length,
        kerns: document.querySelectorAll("#output-svg .kern-rect").length,
        baselines: new Set(
          [...document.querySelectorAll("#output-svg text")].map((t) =>
            t.getAttribute("y"),
          ),
        ).size,
      }));

      if (counts.texts < 100) {
        throw new Error(`expected many glyphs, got ${counts.texts}`);
      }
      if (counts.baselines < 4) {
        throw new Error(`expected several lines, got ${counts.baselines} baselines`);
      }
      if (counts.kerns === 0) {
        throw new Error("expected kern overlays in the default paragraph");
      }
      console.log(`PASS test 1: default render: ${JSON.stringify(counts)}`);
      await page.close();
    }

    // Test 1b: a paragraph with f-ligatures gets ligature overlays showing
    // the combined glyph.
    {
      const page = await browser.newPage();
      await page.goto(
        `http://localhost:${PORT}/?text=it+is+difficult+to+fly+to+the+office&width=200`,
      );
      await page.waitForSelector("#output-svg .lig-rect");

      const ligs = await page.evaluate(() =>
        [...document.querySelectorAll("#output-svg .lig-rect title")].map(
          (t) => t.textContent,
        ),
      );
      if (!ligs.some((l) => l.includes("ffi") && l.includes("ﬃ"))) {
        throw new Error(`expected an ffi ligature, got: ${JSON.stringify(ligs)}`);
      }
      console.log(`PASS test 1b: ligature overlays: ${JSON.stringify(ligs)}`);
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

      const badnesses = await page.evaluate(() =>
        [...document.querySelectorAll("#output-svg .badness-label")].map((t) =>
          // childNodes[0] is the label text; a <title> tooltip follows it.
          Number(t.childNodes[0].nodeValue),
        ),
      );
      if (badnesses.length === 0) {
        throw new Error("expected badness labels");
      }
      if (!badnesses.some((b) => b >= 10000)) {
        throw new Error(
          `expected some very bad lines at tolerance=1, got: ${JSON.stringify(badnesses)}`,
        );
      }
      console.log(`PASS test 4: tolerance=1 renders, line badnesses: ${JSON.stringify(badnesses)}`);
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
