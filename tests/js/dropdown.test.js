/* Blockr.dropdown: a panel under its toggle that stays open while it is
 * worked in. What a caller can see: the toggle opens and closes it; a click
 * inside leaves it open, a click outside, Escape or another dropdown closes
 * it, through the dismiss stack; the wrapper fires blockr:dropdown-shown and
 * blockr:dropdown-hidden; the panel never leaves its place in the page.
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

test('the toggle opens and closes the panel in place', (newWindow) => {
  const win = newWindow();
  const { wrap, toggle, label, panel } = build(win, 'a');

  click(win, label);
  assert.ok(wrap.classList.contains('is-open'));
  assert.strictEqual(toggle.getAttribute('aria-expanded'), 'true');
  assert.strictEqual(panel.parentElement, wrap, 'the panel stays in place');

  click(win, toggle);
  assert.ok(!wrap.classList.contains('is-open'));
  assert.strictEqual(toggle.getAttribute('aria-expanded'), 'false');
});

test('a click inside leaves it open, a click outside closes it', (newWindow) => {
  const win = newWindow();
  const { wrap, toggle, row } = build(win, 'a');

  click(win, toggle);
  click(win, row);
  assert.ok(wrap.classList.contains('is-open'));

  click(win, win.document.body);
  assert.ok(!wrap.classList.contains('is-open'));
});

test('a row removed by its own click does not close the panel', (newWindow) => {
  const win = newWindow();
  const { wrap, toggle, row } = build(win, 'a');

  row.addEventListener('click', () => row.remove());
  click(win, toggle);
  click(win, row);
  assert.ok(wrap.classList.contains('is-open'));
});

test('Escape closes and returns focus to the toggle', (newWindow) => {
  const win = newWindow();
  const { wrap, toggle, panel } = build(win, 'a');

  click(win, toggle);
  key(win, panel.querySelector('.search'), 'Escape');
  assert.ok(!wrap.classList.contains('is-open'));
  assert.strictEqual(win.document.activeElement, toggle);
});

test('opening one dropdown closes the other', (newWindow) => {
  const win = newWindow();
  const a = build(win, 'a');
  const b = build(win, 'b');

  click(win, a.toggle);
  click(win, b.toggle);
  assert.ok(!a.wrap.classList.contains('is-open'));
  assert.ok(b.wrap.classList.contains('is-open'));
});

test('shown and hidden events bubble from the wrapper', (newWindow) => {
  const win = newWindow();
  const { wrap, toggle } = build(win, 'a');
  const seen = [];
  win.document.addEventListener('blockr:dropdown-shown', (e) => seen.push(['shown', e.target.id]));
  win.document.addEventListener('blockr:dropdown-hidden', (e) => seen.push(['hidden', e.target.id]));

  click(win, toggle);
  win.Blockr.dropdown.hide(wrap.querySelector('.row'));
  assert.deepStrictEqual(seen, [['shown', 'a'], ['hidden', 'a']]);
  assert.strictEqual(win.Blockr.dropdown.current(), null);
});

test('the open dropdown is one layer on the dismiss stack', (newWindow) => {
  const win = newWindow();
  const { toggle } = build(win, 'a');
  const before = win.Blockr.layer.count();

  click(win, toggle);
  assert.strictEqual(win.Blockr.layer.count(), before + 1);

  click(win, toggle);
  assert.strictEqual(win.Blockr.layer.count(), before);
});
