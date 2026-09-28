/* blockr-table-preview.js: the sorted header's tooltip.
 *
 * What a user can see: after a sort click, the redrawn header shows its
 * tooltip while the pointer still rests on it, and no later redraw of the
 * table (the next page) brings it back where the pointer no longer is.
 * happy-dom has no layout, so the page stands in for elementFromPoint();
 * Shiny and jQuery are stubs.
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

const wait = (ms) => new Promise((r) => setTimeout(r, ms));
const card = (win) => {
  const c = win.document.querySelector('.blockr-tooltip');
  return c && c.isConnected ? c.textContent : null;
};

/** A window with the preview script, the stubs it needs, and `under`, the
 * element the pointer is on as far as elementFromPoint() is concerned. */
const load = (newWindow) => {
  const win = newWindow();
  const at = { under: null };
  win.$ = () => ({ on: () => {} });
  win.Shiny = { setInputValue: () => {} };
  win.document.elementFromPoint = () => at.under;
  win.eval(preview);
  return { win, at };
};

/** What build_html_table() draws for a page, sorted on `cyl` or not. */
const draw = (win, sorted) => {
  const old = win.document.querySelector('.blockr-table-container');
  if (old) old.remove();
  const box = win.document.createElement('div');
  box.className = 'blockr-table-container';
  box.dataset.sortInput = 'out_table_sort';
  const tip = sorted ? ' data-sort-tip="Sorted ascending, missing values last"' : '';
  box.innerHTML = `
    <table class="blockr-table"><thead><tr>
      <th class="blockr-sortable${sorted ? ' blockr-sort-asc' : ''}" data-column="cyl"${tip}>
        <span class="blockr-col-head"><span class="blockr-col-name">cyl</span></span>
        <span class="blockr-type-row"><span class="blockr-type-label">&lt;dbl&gt;</span></span>
      </th>
    </tr></thead></table>
    <button class="blockr-nav-btn" data-direction="next">Next</button>`;
  win.document.body.appendChild(box);
  return {
    th: box.querySelector('th'),
    cue: box.querySelector('.blockr-type-row'),
    next: box.querySelector('.blockr-nav-btn')
  };
};

const click = (win, el) => el.dispatchEvent(
  new win.MouseEvent('click', { bubbles: true, cancelable: true, clientX: 10, clientY: 10 })
);

test('the sorted header shows its tooltip under a pointer that stayed', async (newWindow) => {
  const { win, at } = load(newWindow);
  const before = draw(win, false);
  at.under = before.cue;
  click(win, before.cue);
  const after = draw(win, true);
  at.under = after.cue;
  await wait(600);
  assert.strictEqual(card(win), 'Sorted ascending, missing values last');
  win.close();
});

test('a later redraw of the table leaves the tooltip where the pointer went', async (newWindow) => {
  const { win, at } = load(newWindow);
  const before = draw(win, false);
  at.under = before.cue;
  click(win, before.cue);
  let now = draw(win, true);
  // The pointer went to Next and pressed it; the header still sits at the
  // point of the sort click.
  at.under = now.cue;
  await wait(600);
  now.next.dispatchEvent(new win.Event('pointerdown', { bubbles: true }));
  assert.strictEqual(card(win), null, 'the press hides it');
  now = draw(win, true);
  at.under = now.cue;
  await wait(600);
  assert.strictEqual(card(win), null, 'the next page does not bring it back');
  win.close();
});

test('a sort click on one table does not show the tooltip on another', async (newWindow) => {
  const { win, at } = load(newWindow);
  const before = draw(win, false);
  at.under = before.cue;
  click(win, before.cue);
  const other = draw(win, true);
  other.th.closest('.blockr-table-container').dataset.sortInput = 'other_table_sort';
  at.under = other.cue;
  await wait(600);
  assert.strictEqual(card(win), null);
  win.close();
});
