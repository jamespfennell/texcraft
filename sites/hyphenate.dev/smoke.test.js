const puppeteer = require('puppeteer');

const PORT = 8765;

async function main() {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--disable-setuid-sandbox'],
  });
  try {
    // Test 1: typing a word hyphenates it
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.waitForSelector('#result .hyphen');

      await page.evaluate((word) => {
        const input = document.getElementById('word-input');
        input.value = word;
        input.dispatchEvent(new Event('input'));
      }, 'concatenation');

      await page.waitForFunction(
        (expected) => document.getElementById('result').textContent === expected,
        {},
        'con-cate-na-tion',
      );

      const result = await page.$eval('#result', (el) => el.textContent);
      console.log(`PASS test 1: "${result}"`);
      await page.close();
    }

    // Test 2: typing updates the URL
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.waitForSelector('#result .hyphen');

      await page.evaluate(() => {
        const input = document.getElementById('word-input');
        input.value = 'hyphenation';
        input.dispatchEvent(new Event('input'));
      });

      const url = page.url();
      if (!url.endsWith('/hyphenation')) {
        throw new Error(`expected URL to end with "/hyphenation", got "${url}"`);
      }

      console.log(`PASS test 2: URL updated to "${url}"`);
      await page.close();
    }

    // Test 3: navigating to /<word> pre-fills and hyphenates the word
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}/interesting`);

      await page.waitForSelector('#result .hyphen');

      const inputValue = await page.$eval('#word-input', (el) => el.value);
      if (inputValue !== 'interesting') {
        throw new Error(`expected input value "interesting", got "${inputValue}"`);
      }

      const result = await page.$eval('#result', (el) => el.textContent);
      console.log(`PASS test 3: "${result}"`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
}

main().catch((err) => {
  console.error('FAIL:', err.message);
  process.exit(1);
});
