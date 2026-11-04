/* The dropdown on its own: `Blockr.Select.menu`.
 *
 * A word in a block's sentence that IS one of its settings opens the list
 * directly. Before this the caller built a popover holding a single select
 * and clicked the control for the user, which was two boxes deep for a list
 * of six columns. The menu is the same widget with no control of its own, so
 * these tests are about the differences: it opens itself, it hangs off an
 * anchor the caller owns, it can lead with the label instead of the name, and
 * it tears itself down when it closes.
 *
 * happy-dom has no layout engine, so nothing here measures. Position is the
 * one thing these tests cannot check.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const anchorIn = (win, text) => {
  const a = win.document.createElement('span');
  a.textContent = text || 'AVAL';
  win.document.body.appendChild(a);
  return a;
};

const COLS = [
  '(none)',
  { value: 'AVAL', label: 'Analysis Value' },
  { value: 'CHG', label: 'Change from Baseline' },
  'BASE'
];

const dropdown = (win) =>
  win.document.querySelector('.blockr-select__dropdown');
const optionTexts = (win) =>
  [...win.document.querySelectorAll('.blockr-select__option')]
    .map((e) => e.textContent);
const ticked = (win) =>
  [...win.document.querySelectorAll('.blockr-select__option')]
    .filter((e) => e.querySelector('.blockr-select__tick svg'))
    .map((e) => e.getAttribute('data-value'));

test('the menu opens itself: no control to click first', (newWindow) => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win), { options: COLS, selected: 'AVAL' });
  assert.ok(dropdown(win), 'a dropdown exists');
  assert.strictEqual(dropdown(win).style.display, 'block');
  // The control is never mounted, so there is no second box on screen.
  assert.strictEqual(win.document.querySelectorAll('.blockr-select__control').length, 0);
  m.close();
  win.close();
});

test('the title says which setting the word fills', (newWindow) => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win), {
    options: COLS, selected: 'AVAL', title: 'Colour'
  });
  const t = win.document.querySelector('.blockr-select__menu-title');
  assert.strictEqual(t.textContent, 'Colour');
  m.close();
  win.close();
});

test('the option leads with the half the sentence printed', (newWindow) => {
  const byName = newWindow();
  const a = byName.Blockr.Select.menu(anchorIn(byName), { options: COLS, selected: 'AVAL' });
  assert.deepStrictEqual(optionTexts(byName), [
    '(none)', 'AVALAnalysis Value', 'CHGChange from Baseline', 'BASE'
  ]);
  a.close();
  byName.close();

  const byLabel = newWindow();
  const b = byLabel.Blockr.Select.menu(anchorIn(byLabel), {
    options: COLS, selected: 'AVAL', labelFirst: true
  });
  assert.deepStrictEqual(optionTexts(byLabel), [
    '(none)', 'Analysis ValueAVAL', 'Change from BaselineCHG', 'BASE'
  ]);
  b.close();
  byLabel.close();
});

test('title, filter box and the count with All and None share one head, in that order, before the rows', (newWindow) => {
  const win = newWindow();
  const many = Array.from({ length: 12 }, (_, i) => `COL${i}`);
  const m = win.Blockr.Select.menu(anchorIn(win), {
    mode: 'multi', title: 'Rows', options: many, selected: ['COL1']
  });
  const head = dropdown(win).querySelector('.blockr-select__head');
  assert.ok(head, 'one element to make sticky');
  assert.strictEqual(head.parentElement, dropdown(win));
  assert.ok(dropdown(win).classList.contains('blockr-select__dropdown--menu'),
    'the panel is marked, so CSS can hand its top padding to the head');
  assert.deepStrictEqual(
    [...head.children].map((c) => c.className.split(' ')[0]),
    ['blockr-select__menu-title', 'blockr-select__search', 'blockr-select__bulk']
  );
  // The rows come after the head, never inside it: a re-render rebuilds only
  // what follows, so the box being typed into is never touched.
  const first = dropdown(win).querySelector('.blockr-select__option');
  assert.ok(head.compareDocumentPosition(first) & win.Node.DOCUMENT_POSITION_FOLLOWING);
  assert.ok(!head.contains(first));
  m.close();
  win.close();
});

test('the current option is marked, the same way every select marks it', (newWindow) => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win), { options: COLS, selected: 'CHG' });
  const on = win.document.querySelectorAll('.blockr-select__option--selected');
  assert.strictEqual(on.length, 1);
  assert.strictEqual(on[0].getAttribute('data-value'), 'CHG');
  m.close();
  win.close();
});

test('the filter box appears past the threshold, and holds focus below it', (newWindow) => {
  const short = newWindow();
  const a = short.Blockr.Select.menu(anchorIn(short), { options: COLS, selected: 'AVAL' });
  const hidden = short.document.querySelector('.blockr-select__search--menu');
  assert.ok(hidden, 'the input is always mounted: it is what the arrows type into');
  assert.ok(hidden.className.includes('blockr-select__search--offscreen'));
  a.close();
  short.close();

  const long = newWindow();
  const many = Array.from({ length: 12 }, (_, i) => `COL${i}`);
  const b = long.Blockr.Select.menu(anchorIn(long), { options: many, selected: 'COL0' });
  const shown = long.document.querySelector('.blockr-select__search--menu');
  assert.ok(!shown.className.includes('blockr-select__search--offscreen'));
  b.close();
  long.close();
});

test('typing filters without losing the input', (newWindow) => {
  const win = newWindow();
  const many = Array.from({ length: 12 }, (_, i) => `COL${i}`);
  const m = win.Blockr.Select.menu(anchorIn(win), { options: many, selected: 'COL0' });
  const input = win.document.querySelector('.blockr-select__search--menu');
  input.value = 'COL1';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
  // COL1, COL10, COL11 -- and the input is still in the panel, which is what
  // clearing only the options below the head buys.
  assert.deepStrictEqual(optionTexts(win), ['COL1', 'COL10', 'COL11']);
  assert.ok(dropdown(win).contains(input));
  m.close();
  win.close();
});

test('a pick reports the value and takes the menu away', (newWindow) => {
  const win = newWindow();
  const picked = [];
  let closed = 0;
  win.Blockr.Select.menu(anchorIn(win), {
    options: COLS,
    selected: 'AVAL',
    onChange: (v) => picked.push(v),
    onClose: () => { closed++; }
  });
  const chg = [...win.document.querySelectorAll('.blockr-select__option')]
    .find((e) => e.getAttribute('data-value') === 'CHG');
  chg.dispatchEvent(new win.Event('click', { bubbles: true }));
  assert.deepStrictEqual(picked, ['CHG']);
  return new Promise((resolve) => {
    // Teardown is deferred by a tick on purpose: close() runs before the
    // option's own onChange, and destroying synchronously would pull the DOM
    // out from under it.
    win.setTimeout(() => {
      assert.strictEqual(closed, 1);
      assert.strictEqual(win.document.querySelectorAll('.blockr-select__dropdown').length, 0);
      assert.strictEqual(win.document.querySelectorAll('.blockr-select-menu-host').length, 0);
      win.close();
      resolve();
    }, 0);
  });
});

/* A click as a browser delivers it: the pointerdown, which the dismiss stack
 * reads as landing inside a layer or outside it, then the click. */
const click = (win, el) => {
  el.dispatchEvent(new win.PointerEvent('pointerdown', { bubbles: true }));
  el.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
};
const tick = (win) => new Promise((resolve) => win.setTimeout(resolve, 0));

test('a click on the anchor is the caller\'s toggle, not an outside click', async (newWindow) => {
  const win = newWindow();
  const anchor = anchorIn(win);
  let closed = 0;
  const m = win.Blockr.Select.menu(anchor, {
    options: COLS, selected: 'AVAL', onClose: () => { closed++; }
  });
  click(win, anchor);
  // Teardown is deferred by a tick, as above.
  await tick(win);
  assert.strictEqual(closed, 0, 'the menu stays: closing here would have it reopen on the same click');
  assert.ok(dropdown(win), 'and it is still on screen');
  // Anywhere else does close it.
  const elsewhere = win.document.createElement('div');
  win.document.body.appendChild(elsewhere);
  click(win, elsewhere);
  await tick(win);
  assert.strictEqual(closed, 1);
  m.close();
  win.close();
});

/* --- multi mode -----------------------------------------------------------
 *
 * The same panel for a setting that takes several values. What changes: the
 * picks are on top of the list, under "Selected", with the count and All and
 * None in the head, and a pick leaves the panel open, because adding three
 * columns has to be one gesture rather than three round trips through the
 * word. The picks are reported once, when the menu closes.
 */

test('a multi menu has its picks on top and ticked, and no control', (newWindow) => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(anchorIn(win, 'AESOC, AEDECOD'), {
    mode: 'multi', title: 'Rows', options: ['AESOC', 'AEDECOD', 'AETOXGR'],
    selected: ['AEDECOD', 'AESOC']
  });
  // One box: the panel, with no field beside it.
  assert.strictEqual(win.document.querySelectorAll('.blockr-select__control').length, 0);
  assert.deepStrictEqual(optionTexts(win), ['AEDECOD', 'AESOC', 'AETOXGR'], 'the picks first, in pick order');
  assert.deepStrictEqual(ticked(win), ['AEDECOD', 'AESOC']);
  assert.strictEqual(dropdown(win).querySelector('.blockr-select__count').textContent, '2 of 3 selected');
  m.close();
  win.close();
});

test('a pick keeps the multi menu open, ticks the row, and the menu reports on close', (newWindow) => {
  const win = newWindow();
  const picked = [];
  let closed = 0;
  const m = win.Blockr.Select.menu(anchorIn(win), {
    mode: 'multi', options: ['AESOC', 'AEDECOD', 'AETOXGR'], selected: ['AESOC'],
    onChange: (v) => picked.push(v), onClose: () => { closed++; }
  });
  const opt = [...win.document.querySelectorAll('.blockr-select__option')]
    .find((e) => e.getAttribute('data-value') === 'AETOXGR');
  opt.dispatchEvent(new win.Event('click', { bubbles: true }));
  assert.deepStrictEqual(picked, [], 'nothing reported while open');
  assert.strictEqual(closed, 0, 'the panel stays: the next pick is one click away');
  assert.ok(dropdown(win), 'still on screen');
  assert.deepStrictEqual(optionTexts(win), ['AESOC', 'AETOXGR', 'AEDECOD']);
  assert.deepStrictEqual(ticked(win), ['AESOC', 'AETOXGR']);
  m.close();
  win.close();
});

test('None empties a multi menu and it reports [] on close', async (newWindow) => {
  const win = newWindow();
  const picked = [];
  const m = win.Blockr.Select.menu(anchorIn(win), {
    mode: 'multi', options: ['AESOC', 'AEDECOD'], selected: ['AESOC', 'AEDECOD'],
    onChange: (v) => picked.push(v)
  });
  const none = [...dropdown(win).querySelectorAll('.blockr-select__bulk-btn')]
    .find((b) => b.textContent === 'None');
  none.dispatchEvent(new win.Event('click', { bubbles: true }));
  assert.deepStrictEqual(ticked(win), []);
  m.close();
  await new Promise((r) => win.setTimeout(r, 5));
  // [], not [''] -- the slot is unset.
  assert.deepStrictEqual(JSON.parse(JSON.stringify(picked)), [[]]);
  win.close();
});

test('the filter prompt survives a pick', (newWindow) => {
  const win = newWindow();
  const opts = Array.from({ length: 12 }, (_, i) => `COL${i + 1}`);
  const m = win.Blockr.Select.menu(anchorIn(win), {
    mode: 'multi', options: opts, selected: [], searchPlaceholder: 'Filter columns'
  });
  const input = dropdown(win).querySelector('.blockr-select__search');
  assert.strictEqual(input.getAttribute('placeholder'), 'Filter columns');
  [...win.document.querySelectorAll('.blockr-select__option')][0]
    .dispatchEvent(new win.Event('click', { bubbles: true }));
  assert.strictEqual(input.getAttribute('placeholder'), 'Filter columns');
  assert.ok(dropdown(win).contains(input), 'and it stays in the head');
  m.close();
  win.close();
});
