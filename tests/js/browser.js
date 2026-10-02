/* Open the shipped controls in Chrome, for what happy-dom cannot check:
 * where a control lands on the page, and how it reads there.
 *
 * Each test gets a fresh page, controls.html: the stylesheets and scripts
 * controls_dep() attaches, in its order, and no controls. The icons, which
 * controls_dep() writes into the head, are set ahead of the page's scripts
 * from the icons' files (icons.js). The test builds the controls it needs
 * in the page, and fails on any script error the page throws.
 *
 * Playwright drives the Chrome installed on the machine, which GitHub's
 * Ubuntu runners have, so nothing downloads a browser. Set CHROME_BIN to
 * use another Chrome or Chromium.
 *
 * Usage:
 *   const { test } = require('./browser');
 *   test('what it does', async (page) => {
 *     await page.evaluate(() => { ... Blockr ... });
 *     ...
 *   }, { viewport: { width: 400, height: 600 } });
 */
'use strict';

const assert = require('node:assert');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const nodeTest = require('node:test');
const { chromium } = require('playwright-core');
const icons = require('./icons');

const PAGE = pathToFileURL(path.join(__dirname, 'controls.html')).href;

/** @type {Promise<import('playwright-core').Browser> | null} */
let browser = null;

const launch = () => chromium.launch(
  process.env.CHROME_BIN ? { executablePath: process.env.CHROME_BIN } : { channel: 'chrome' }
).catch((e) => {
  throw new Error('No Chrome for the browser tests: install Google Chrome, or set CHROME_BIN ' +
    `to a Chrome or Chromium binary.\n${e.message}`);
});

nodeTest.after(async () => {
  // A launch that failed has failed every test already.
  const launched = browser && await browser.catch(() => null);
  if (launched) await launched.close();
});

/**
 * Register `name`; `fn` receives a Playwright page with controls.html open.
 * @param {string} name
 * @param {(page: import('playwright-core').Page) => Promise<void>} fn
 * @param {{ viewport?: { width: number, height: number } }} [opts]
 */
const test = (name, fn, opts = {}) => nodeTest(name, async () => {
  browser = browser || launch();
  const page = await (await browser).newPage({ viewport: opts.viewport || { width: 800, height: 600 } });
  /** @type {string[]} */
  const errors = [];
  page.on('pageerror', (e) => errors.push(e.message));
  try {
    await page.addInitScript(icons.script);
    await page.goto(PAGE);
    await fn(page);
    assert.deepStrictEqual(errors, [], 'the page threw');
  } finally {
    await page.close();
  }
});

module.exports = { test };
