const puppeteer = require("puppeteer");

const PORT = 8766;

async function main() {
  const browser = await puppeteer.launch({
    args: ["--no-sandbox", "--disable-setuid-sandbox"],
  });
  try {
    // Test 1: typing "office" with cmr10 produces a ligature chip
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.evaluate(() => {
        const input = document.getElementById("text-input");
        input.value = "office";
        input.dispatchEvent(new Event("input"));
      });

      await page.waitForSelector(".chip-lig");

      const chips = await page.$$eval(".chip", (els) =>
        els.map((el) => ({
          type: el.classList.contains("chip-char")
            ? "char"
            : el.classList.contains("chip-kern")
              ? "kern"
              : "lig",
          text: el.textContent.trim(),
        })),
      );

      const hasLig = chips.some((c) => c.type === "lig");
      if (!hasLig) {
        throw new Error(`expected a ligature chip for "office", got: ${JSON.stringify(chips)}`);
      }

      console.log(`PASS test 1: "office" chips: ${JSON.stringify(chips)}`);
      await page.close();
    }

    // Test 2: "AVAST" with cmr10 produces kern chips
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.evaluate(() => {
        const input = document.getElementById("text-input");
        input.value = "AVAST";
        input.dispatchEvent(new Event("input"));
      });

      await page.waitForSelector(".chip-kern");

      const kernCount = await page.$$eval(".chip-kern", (els) => els.length);
      if (kernCount === 0) {
        throw new Error(`expected kern chips for "AVAST"`);
      }

      console.log(`PASS test 2: "AVAST" produced ${kernCount} kern chip(s)`);
      await page.close();
    }

    // Test 3: clearing input removes has-output class
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.evaluate(() => {
        const input = document.getElementById("text-input");
        input.value = "";
        input.dispatchEvent(new Event("input"));
      });

      const hasOutput = await page.$eval("body", (el) =>
        el.classList.contains("has-output"),
      );
      if (hasOutput) {
        throw new Error("expected has-output to be absent after clearing input");
      }

      console.log("PASS test 3: has-output absent after clearing input");
      await page.close();
    }

    // Test 4: state is encoded in URL after typing
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.evaluate(() => {
        const input = document.getElementById("text-input");
        input.value = "AVAST";
        input.dispatchEvent(new Event("input"));
      });

      const params = await page.evaluate(() => Object.fromEntries(new URLSearchParams(location.search)));
      if (params.font !== "cmr10") throw new Error(`expected font=cmr10 in URL, got: ${JSON.stringify(params)}`);
      if (params.text !== "AVAST") throw new Error(`expected text=AVAST in URL, got: ${JSON.stringify(params)}`);

      console.log("PASS test 4: URL encodes builtin state");
      await page.close();
    }

    // Test 5: loading a URL restores builtin state and renders output
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}?font=cmr10&text=AVAST`);

      const textVal = await page.$eval("#text-input", (el) => el.value);
      if (textVal !== "AVAST") throw new Error(`expected text input = "AVAST", got: "${textVal}"`);

      await page.waitForSelector(".chip-kern");
      const kernCount = await page.$$eval(".chip-kern", (els) => els.length);
      if (kernCount === 0) throw new Error("expected kern chips to be rendered from URL state");

      console.log("PASS test 5: URL restores builtin state and renders output");
      await page.close();
    }

    // Test 6: loading a URL restores custom compact state
    {
      const program = "ab -> fi";
      const url = `http://localhost:${PORT}?type=custom&format=compact&program=${encodeURIComponent(program)}&text=ab`;
      const page = await browser.newPage();
      await page.goto(url);

      const customVisible = await page.$eval("#custom-panel", (el) => el.style.display !== "none");
      if (!customVisible) throw new Error("expected custom panel to be visible");

      const programVal = await page.$eval("#compact-input", (el) => el.value);
      if (programVal !== program) throw new Error(`expected compact program = "${program}", got: "${programVal}"`);

      const textVal = await page.$eval("#text-input", (el) => el.value);
      if (textVal !== "ab") throw new Error(`expected text input = "ab", got: "${textVal}"`);

      console.log("PASS test 6: URL restores custom compact state");
      await page.close();
    }
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error("FAIL:", err.message);
  process.exit(1);
});
