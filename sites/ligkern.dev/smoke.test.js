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
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error("FAIL:", err.message);
  process.exit(1);
});
