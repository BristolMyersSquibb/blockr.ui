/* Blockr.dropdown: a panel under its toggle that stays open while it is
 * worked in. What a caller can see: the toggle opens and closes it; a click
 * inside leaves it open, a click outside, Escape or another dropdown closes
 * it, through the dismiss stack; the wrapper fires blockr:dropdown-shown and
 * blockr:dropdown-hidden; open, the panel is on <body>, and closed, back
 * beside its toggle. Where the panel lands is tested in Chrome, at the
 * end. Every happy-dom test closes its window, as an open panel keeps
 * Blockr.place's observers running, and compares elements with `===`:
 * assert.strictEqual() on two happy-dom elements spends minutes printing
 * them when it fails.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const build = (win, id) => {
  const doc = win.document;
  const host = doc.createElement('div');
  host.innerHTML = `
    <div id="${id}" class="blockr-dropdown" data-align="end">
      <button class="blockr-dropdown__toggle" type="button" aria-expanded="false">
        <span class="label">Views</span>
      </button>
      <div class="blockr-dropdown__panel">
        <input class="search" type="text">
        <button class="row" type="button">Row</button>
      </div>
    </div>`;
  doc.body.appendChild(host);
  const wrap = host.querySelector('.blockr-dropdown');
  return {
    wrap,
    toggle: wrap.querySelector('.blockr-dropdown__toggle'),
    label: wrap.querySelector('.label'),
    panel: wrap.querySelector('.blockr-dropdown__panel'),
    row: wrap.querySelector('.row')
  };
};

// A click as the browser sends it: the pointerdown, which the dismiss stack
// reads, then the click.
const click = (win, el) => {
  el.dispatchEvent(new win.PointerEvent('pointerdown', { bubbles: true, cancelable: true }));
  el.dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true }));
};
const key = (win, el, k) =>
  el.dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));

test('the toggle opens the panel on <body> and closes it back in place', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { wrap, toggle, label, panel } = build(win, 'a');

  click(win, label);
  assert.ok(wrap.classList.contains('is-open'));
  assert.strictEqual(toggle.getAttribute('aria-expanded'), 'true');
  assert.ok(panel.parentElement === win.document.body, 'on <body> while open');

  click(win, toggle);
  assert.ok(!wrap.classList.contains('is-open'));
  assert.strictEqual(toggle.getAttribute('aria-expanded'), 'false');
  assert.ok(panel.parentElement === wrap, 'back beside its toggle once closed');
});

test('a click inside leaves it open, a click outside closes it', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { wrap, toggle, row } = build(win, 'a');

  click(win, toggle);
  click(win, row);
  assert.ok(wrap.classList.contains('is-open'));

  click(win, win.document.body);
  assert.ok(!wrap.classList.contains('is-open'));
});

test('a row removed by its own click does not close the panel', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { wrap, toggle, row } = build(win, 'a');

  row.addEventListener('click', () => row.remove());
  click(win, toggle);
  click(win, row);
  assert.ok(wrap.classList.contains('is-open'));
});

test('Escape closes and returns focus to the toggle', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { wrap, toggle, panel } = build(win, 'a');

  click(win, toggle);
  key(win, panel.querySelector('.search'), 'Escape');
  assert.ok(!wrap.classList.contains('is-open'));
  assert.ok(win.document.activeElement === toggle, 'the focus is back on the toggle');
});

test('opening one dropdown closes the other', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const a = build(win, 'a');
  const b = build(win, 'b');

  click(win, a.toggle);
  click(win, b.toggle);
  assert.ok(!a.wrap.classList.contains('is-open'));
  assert.ok(b.wrap.classList.contains('is-open'));
});

test('shown and hidden events bubble from the wrapper', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { toggle, row } = build(win, 'a');
  const seen = [];
  win.document.addEventListener('blockr:dropdown-shown', (e) => seen.push(['shown', e.target.id]));
  win.document.addEventListener('blockr:dropdown-hidden', (e) => seen.push(['hidden', e.target.id]));

  click(win, toggle);
  win.Blockr.dropdown.hide(row);
  assert.deepStrictEqual(seen, [['shown', 'a'], ['hidden', 'a']]);
  assert.ok(win.Blockr.dropdown.current() === null, 'none is open');
});

test('the open dropdown is one layer on the dismiss stack', (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { toggle } = build(win, 'a');
  const before = win.Blockr.layer.count();

  click(win, toggle);
  assert.strictEqual(win.Blockr.layer.count(), before + 1);

  click(win, toggle);
  assert.strictEqual(win.Blockr.layer.count(), before);
});

test('a dropdown that leaves the page while open closes and drops its panel', async (newWindow, t) => {
  const win = newWindow();
  t.after(() => win.close());
  const { wrap, toggle, panel } = build(win, 'a');
  const before = win.Blockr.layer.count();

  click(win, toggle);
  wrap.parentElement.remove();
  await new Promise((resolve) => win.setTimeout(resolve, 0));

  assert.ok(win.Blockr.dropdown.current() === null, 'none is open');
  assert.ok(!panel.isConnected, 'the panel is gone from <body>');
  assert.strictEqual(win.Blockr.layer.count(), before);
});

const chrome = require('./browser').test;

// A dropdown as dropdown() builds it, in `parent`.
const dropdown = (id, align) => `
  <div id="${id}" class="blockr-dropdown" data-align="${align}">
    <button class="blockr-dropdown__toggle" type="button" aria-expanded="false">Menu</button>
    <div class="blockr-dropdown__panel blockr-menu">
      <input type="text" style="width: 200px">
      <div style="height: 120px">Rows</div>
    </div>
  </div>`;

const placement = (page, id) => page.evaluate((id) => {
  const wrap = document.getElementById(id);
  const t = wrap.querySelector('.blockr-dropdown__toggle').getBoundingClientRect();
  const panel = document.querySelector(`#${id}-panel`) ||
    Array.from(document.querySelectorAll('.blockr-dropdown__panel'))
      .find((p) => p.dataset.owner === id);
  const p = panel.getBoundingClientRect();
  // The panel is on top wherever it is drawn: at its middle and both ends.
  const seen = [p.top + 4, p.top + p.height / 2, p.bottom - 4].every((y) =>
    panel.contains(document.elementFromPoint(p.left + p.width / 2, y)));
  return {
    toggle: { top: t.top, bottom: t.bottom, left: t.left, right: t.right },
    panel: { top: p.top, bottom: p.bottom, left: p.left, right: p.right },
    inWindow: p.left >= 0 && p.top >= 0 && p.right <= innerWidth && p.bottom <= innerHeight,
    seen
  };
}, id);

const mark = (page) => page.evaluate(() => {
  for (const w of document.querySelectorAll('.blockr-dropdown')) {
    w.querySelector('.blockr-dropdown__panel').dataset.owner = w.id;
  }
});

chrome('in Chrome, the panel opens under its toggle, at its right edge with data-align="end"', async (page) => {
  await page.evaluate((html) => {
    const bar = document.createElement('div');
    bar.style.cssText = 'display: flex; justify-content: space-between; padding: 8px;';
    bar.innerHTML = html;
    document.body.appendChild(bar);
  }, dropdown('start', 'start') + dropdown('end', 'end'));
  await mark(page);

  await page.click('#start > .blockr-dropdown__toggle');
  const start = await placement(page, 'start');
  assert.strictEqual(start.panel.top, start.toggle.bottom + 4);
  assert.strictEqual(start.panel.left, start.toggle.left);
  assert.ok(start.seen && start.inWindow);

  await page.click('#end > .blockr-dropdown__toggle');
  const end = await placement(page, 'end');
  assert.strictEqual(end.panel.top, end.toggle.bottom + 4);
  assert.strictEqual(end.panel.right, end.toggle.right);
  assert.ok(end.seen && end.inWindow);
});

chrome('in Chrome, a dropdown in a clipping box at the bottom opens upward, whole', async (page) => {
  await page.evaluate((html) => {
    const box = document.createElement('div');
    box.style.cssText = 'position: fixed; left: 8px; bottom: 8px; height: 60px; ' +
      'overflow: hidden; z-index: 1;';
    box.innerHTML = html;
    document.body.appendChild(box);
    // A panel stacked above the box, where the dropdown's panel opens.
    const over = document.createElement('div');
    over.style.cssText = 'position: fixed; left: 0; right: 0; bottom: 70px; ' +
      'height: 200px; z-index: 2; background: #fde68a;';
    document.body.appendChild(over);
  }, dropdown('clipped', 'start'));
  await mark(page);

  await page.click('#clipped > .blockr-dropdown__toggle');
  const r = await placement(page, 'clipped');
  assert.ok(r.panel.bottom <= r.toggle.top, 'above its toggle');
  assert.ok(r.seen, 'not clipped by the box, nor under the panel above it');
  assert.ok(r.inWindow);

  await page.keyboard.press('Escape');
  const back = await page.evaluate(() =>
    document.querySelector('[data-owner="clipped"]').parentElement.id);
  assert.strictEqual(back, 'clipped', 'closed, the panel is back in the box');
});
