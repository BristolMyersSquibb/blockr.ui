/* blockr-table-preview.js: the sort and page clicks, seen from Shiny.
 *
 * What the server sees: a click on a sortable header sends the column and
 * the next direction in the cycle (ascending, descending, missing values
 * first, off); a click on a page arrow sends the page it leads to. Shiny is
 * a stub that records what it is sent; jQuery, which the scroll restore
 * hooks, is a stub too.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { test } = require('./select-impls');

const preview = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'blockr-table-preview.js'),
  'utf8'
);

/** A window with the preview script and a Shiny that records its inputs,
 * as JSON, the form they go to the server in (and out of the window's
 * realm, whose Object prototype deepStrictEqual would count as different). */
const load = (newWindow) => {
  const win = newWindow();
  const sent = [];
  win.$ = () => ({ on: () => {} });
  win.Shiny = {
    setInputValue: (id, value) => sent.push([id, JSON.parse(JSON.stringify(value))])
  };
  win.eval(preview);
  return { win, sent };
};

/** What build_html_table() draws, reduced to what the clicks read. */
const draw = (win, { dir = 'none', page = 1, maxPage = 3 } = {}) => {
  const box = win.document.createElement('div');
  box.className = 'blockr-table-container';
  box.dataset.sortInput = 'out_table_sort';
  box.dataset.pageInput = 'out_table_page';
  box.dataset.currentPage = String(page);
  box.dataset.maxPage = String(maxPage);
  const sorted = dir === 'none' ? '' : ` blockr-sort-${dir}`;
  box.innerHTML = `
    <table class="blockr-table"><thead><tr>
      <th class="blockr-sortable${sorted}" data-column="cyl">
        <span class="blockr-col-head"><span class="blockr-col-name">cyl</span></span>
        <span class="blockr-type-row"><span class="blockr-type-label">&lt;dbl&gt;</span></span>
      </th>
    </tr></thead></table>
    <button class="blockr-nav-btn${page === 1 ? ' disabled' : ''}" data-direction="prev"></button>
    <button class="blockr-nav-btn${page === maxPage ? ' disabled' : ''}" data-direction="next"></button>`;
  win.document.body.appendChild(box);
  return {
    box,
    name: box.querySelector('.blockr-col-name'),
    cue: box.querySelector('.blockr-type-row'),
    prev: box.querySelector('[data-direction="prev"]'),
    next: box.querySelector('[data-direction="next"]')
  };
};

const click = (win, el) =>
  el.dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true }));

test('a header click sends the next direction in the cycle', (newWindow) => {
  const { win, sent } = load(newWindow);
  const cycle = [['none', 'asc'], ['asc', 'desc'], ['desc', 'na'], ['na', 'none']];
  for (const [dir, next] of cycle) {
    const t = draw(win, { dir });
    click(win, t.cue);
    t.box.remove();
    assert.deepStrictEqual(sent.pop(), ['out_table_sort', { col: 'cyl', dir: next }], dir);
  }
  win.close();
});

test('a click on the column name does not sort', (newWindow) => {
  const { win, sent } = load(newWindow);
  click(win, draw(win).name);
  assert.deepStrictEqual(sent, []);
  win.close();
});

test('a page arrow sends the page it leads to, and a disabled one nothing', (newWindow) => {
  const { win, sent } = load(newWindow);
  let t = draw(win, { page: 2 });
  click(win, t.next);
  click(win, t.prev);
  assert.deepStrictEqual(sent, [['out_table_page', 3], ['out_table_page', 1]]);
  t.box.remove();
  t = draw(win, { page: 1 });
  click(win, t.prev);
  assert.strictEqual(sent.length, 2, 'the first page has no previous one');
  win.close();
});
