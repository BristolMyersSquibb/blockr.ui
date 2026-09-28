/* Blockr.Input, the code field with completions, seen from a caller: what it
 * offers while typing, what a pick inserts, and what Enter does.
 *
 * happy-dom has no layout engine, so the popup's position is not checked
 * here; the placement is the same portal pattern as Blockr.Select.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const nodeTest = require('node:test');
const { newWindow } = require('./select-impls');

const input = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'blockr-input.js'), 'utf8');

const test = (name, fn) => nodeTest(name, () => {
  const win = newWindow();
  win.eval(input);
  try { fn(win); } finally { win.close(); }
});

const mount = (win, config) => {
  const host = win.document.createElement('div');
  win.document.body.appendChild(host);
  const h = win.Blockr.Input.create(host, config);
  const field = h.el.querySelector('input, textarea');
  return { h, field };
};

/* Type `text` at the end of the field and fire the input event. */
const type = (win, field, text) => {
  field.value += text;
  field.setSelectionRange(field.value.length, field.value.length);
  field.dispatchEvent(new win.Event('input', { bubbles: true }));
};

const key = (win, field, k) =>
  field.dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));

const offered = (win) =>
  [...win.document.querySelectorAll('.blockr-input__item-text')].map((e) => e.textContent);

test('Input: typing offers the columns and functions that start with the word', (win) => {
  const { field } = mount(win, {
    columns: ['mpg', 'cyl', 'my col'],
    categories: { Math: ['mean', 'max'] }
  });
  type(win, field, 'm');
  // Columns rank above functions; a non-syntactic name comes backticked.
  assert.deepStrictEqual(offered(win), ['`my col`', 'mpg', 'max', 'mean']);
  type(win, field, 'e');
  assert.deepStrictEqual(offered(win), ['mean']);
});

test('Input: Enter on an open list inserts the pick; a function gets its parens', (win) => {
  const changes = [];
  const { h, field } = mount(win, {
    columns: ['mpg'],
    categories: { Math: ['mean'] },
    onChange: () => changes.push(h.getValue())
  });
  type(win, field, 'me');
  key(win, field, 'Enter');
  assert.strictEqual(h.getValue(), 'mean()');
  // The cursor lands between the parens, ready for the argument.
  assert.strictEqual(field.selectionStart, 'mean('.length);
  assert.strictEqual(offered(win).length, 0);
  assert.ok(changes.includes('mean()'));
});

test('Input: Enter with the list closed confirms the trimmed value', (win) => {
  const confirmed = [];
  const { field } = mount(win, { onConfirm: (v) => confirmed.push(v) });
  type(win, field, 'mpg > 20  ');
  key(win, field, 'Enter');
  assert.deepStrictEqual(confirmed, ['mpg > 20']);
});

test('Input: a multiline field never confirms on Enter', (win) => {
  const confirmed = [];
  const { field } = mount(win, { multiline: true, onConfirm: (v) => confirmed.push(v) });
  assert.strictEqual(field.tagName, 'TEXTAREA');
  type(win, field, 'x');
  key(win, field, 'Enter');
  assert.deepStrictEqual(confirmed, []);
});

test('Input: setColumns replaces the columns offered; setValue does not fire onChange', (win) => {
  let changes = 0;
  const { h, field } = mount(win, { columns: ['mpg'], onChange: () => changes++ });
  h.setValue('restored');
  assert.strictEqual(h.getValue(), 'restored');
  assert.strictEqual(changes, 0);
  h.setValue('');
  h.setColumns(['hp', 'hwy']);
  type(win, field, 'h');
  assert.deepStrictEqual(offered(win), ['hp', 'hwy']);
});

test('Input: Escape closes the list and destroy removes the popup', (win) => {
  const { h, field } = mount(win, { columns: ['mpg'] });
  type(win, field, 'm');
  assert.strictEqual(offered(win).length, 1);
  key(win, field, 'Escape');
  assert.strictEqual(offered(win).length, 0);
  type(win, field, 'p');
  h.destroy();
  assert.strictEqual(win.document.querySelectorAll('.blockr-input__popup').length, 0);
});

test('Input: Escape on an open list closes the list and stops there', (win) => {
  const doc = win.document;
  const band = doc.createElement('div');
  const gear = doc.createElement('button');
  doc.body.append(band, gear);
  const tray = win.Blockr.gearTray(band, gear);
  const h = win.Blockr.Input.create(band, { columns: ['AGE', 'AGEGR1'] });
  const field = h.el.querySelector('input');
  tray.set(true);
  type(win, field, 'AG');
  const open = () => h.el.classList.contains('blockr-input--popup-open');
  assert.ok(open(), 'the list is open');
  key(win, field, 'Escape');
  assert.ok(!open(), 'the list closed');
  assert.ok(tray.isOpen(), 'the tray did not see the Escape');
  key(win, field, 'Escape');
  assert.ok(!tray.isOpen(), 'a closed list lets the next Escape through');
});
