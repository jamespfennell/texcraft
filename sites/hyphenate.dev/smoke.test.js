const puppeteer = require('puppeteer');

const PORT = 8765;
const WORD = 'concatenation';
const EXPECTED = 'con-cate-na-tion';

async function main() {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--disable-setuid-sandbox'],
  });
  try {
    const page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}`);

    // Wait for WASM to initialize — hyphens only appear after init() resolves
    await page.waitForSelector('#result .hyphen');

    // Set the input value and fire the input event
    await page.evaluate((word) => {
      const input = document.getElementById('word-input');
      input.value = word;
      input.dispatchEvent(new Event('input'));
    }, WORD);

    await page.waitForFunction(
      (expected) => document.getElementById('result').textContent === expected,
      {},
      EXPECTED,
    );

    const result = await page.$eval('#result', (el) => el.textContent);
    console.log(`PASS: "${result}"`);
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error('FAIL:', err.message);
  process.exit(1);
});
