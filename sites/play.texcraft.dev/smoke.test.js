const puppeteer = require('puppeteer');

const PORT = process.env.PORT || 8080;

async function setTextarea(page, id, value) {
  await page.evaluate((id, value) => {
    const el = document.getElementById(id);
    // Use the native setter to bypass React's overridden setter, which
    // allows the synthetic onChange handler to fire correctly.
    const setter = Object.getOwnPropertyDescriptor(window.HTMLTextAreaElement.prototype, 'value').set;
    setter.call(el, value);
    el.dispatchEvent(new Event('input', { bubbles: true }));
  }, id, value);
}

async function main() {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--disable-setuid-sandbox'],
  });
  try {
    // Test 1: welcome message appears on page load
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.waitForFunction(
        () => document.getElementById('output')?.textContent?.includes('Welcome to the Texcraft Playground!'),
        { timeout: 10000 },
      );

      const output = await page.$eval('#output', el => el.textContent);
      console.log(`PASS test 1: "${output.split('\n')[0]}"`);
      await page.close();
    }

    // Test 2: editing the input and clicking Run updates the output
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.waitForFunction(
        () => document.getElementById('output')?.textContent?.includes('Welcome'),
        { timeout: 10000 },
      );

      await setTextarea(page, 'inputtex', '\\def\\greeting{hello world}\\greeting');
      await page.click('#runButton');

      await page.waitForFunction(
        () => document.getElementById('output')?.textContent?.trim() === 'hello world',
        { timeout: 5000 },
      );

      const output = await page.$eval('#output', el => el.textContent);
      console.log(`PASS test 2: "${output}"`);
      await page.close();
    }

    // Test 3: an undefined control sequence produces an error message
    {
      const page = await browser.newPage();
      await page.goto(`http://localhost:${PORT}`);

      await page.waitForFunction(
        () => document.getElementById('output')?.textContent?.includes('Welcome'),
        { timeout: 10000 },
      );

      await setTextarea(page, 'inputtex', '\\undefinedcommand');
      await page.click('#runButton');

      await page.waitForFunction(
        () => document.getElementById('output')?.textContent?.includes('undefinedcommand'),
        { timeout: 5000 },
      );

      const output = await page.$eval('#output', el => el.textContent);
      console.log(`PASS test 3: error reported for undefined control sequence`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
}

main().catch(err => {
  console.error('FAIL:', err.message);
  process.exit(1);
});
