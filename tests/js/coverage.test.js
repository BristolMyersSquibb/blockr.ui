/* Every script the package ships is loaded by a test here, or by the R
 * test named below. The table preview's script once went out with 45 lines
 * of tooltip timing and no test at all.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { test } = require('node:test');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'assets', 'js');

// Scripts whose tests are R tests, and where.
const elsewhere = {
  'shiny-has-perf.js': 'tests/testthat/test-shiny-has-perf.R',
  'shiny-input-batch.js': 'tests/testthat/test-shiny-input-batch.R'
};

test('every shipped script has a test', () => {
  const tests = fs.readdirSync(__dirname)
    .filter((f) => f.endsWith('.js') && f !== path.basename(__filename))
    .map((f) => fs.readFileSync(path.join(__dirname, f), 'utf8'))
    .join('\n');
  const untested = fs.readdirSync(JS_DIR)
    .filter((f) => f.endsWith('.js') && !(f in elsewhere) && !tests.includes(f));
  assert.deepStrictEqual(untested, []);
});
