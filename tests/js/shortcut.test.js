/* Blockr.keys(): a keyboard hint for JavaScript-built UI. R's shortcut()
 * writes the same hints into markup, from its own table of key names, so
 * both are held to one list of cases, tests/testthat/fixtures/shortcuts.tsv,
 * which test-shortcut.R reads too.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { test } = require('node:test');
const { Window } = require('happy-dom');

const ui = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'blockr-ui.js'), 'utf8');
const cases = fs.readFileSync(
  path.join(__dirname, '..', 'testthat', 'fixtures', 'shortcuts.tsv'), 'utf8')
  .trim().split('\n').slice(1).map((line) => line.split('\t'));

/** blockr-ui.js loaded on a page whose platform is `platform`. */
const onPlatform = (platform) => {
  const win = new Window({ url: 'http://localhost/' });
  Object.defineProperty(win.navigator, 'platform', { value: platform, configurable: true });
  win.eval(ui);
  return win;
};

test('Blockr.keys() writes the Mac form on a Mac, as shortcut() does', () => {
  const win = onPlatform('MacIntel');
  assert.ok(win.document.documentElement.classList.contains('blockr-mac'));
  for (const [keys, mac] of cases) assert.strictEqual(win.Blockr.keys(keys), mac, keys);
  win.close();
});

test('Blockr.keys() writes the other form elsewhere, as shortcut() does', () => {
  const win = onPlatform('Linux x86_64');
  assert.ok(!win.document.documentElement.classList.contains('blockr-mac'));
  for (const [keys, , other] of cases) assert.strictEqual(win.Blockr.keys(keys), other, keys);
  win.close();
});
