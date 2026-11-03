/* Blockr.Select's public options and handle, as the other packages see them.
 *
 * These pin the surface that moves to blockr.ui: `bordered` for a standalone
 * field, `search: false` for a short fixed list, `setValue` for an owner that
 * sets the value without a user pick, and a menu handle that offers `close`
 * and nothing else.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const host = (win) => {
  const el = win.document.createElement('div');
  win.document.body.appendChild(el);
  return el;
};

const MANY = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'];

test('bordered marks a standalone field; left out, the select stays bare', (newWindow) => {
  const win = newWindow();
  const field = win.Blockr.Select.single(host(win), { options: MANY, bordered: true });
  const bare = win.Blockr.Select.single(host(win), { options: MANY });
  assert.ok(field.el.classList.contains('blockr-select--bordered'));
  assert.ok(!bare.el.classList.contains('blockr-select--bordered'));
  win.close();
});

test('the filter box shows past eight options, and never with search: false', (newWindow) => {
  const win = newWindow();
  const filtered = (m) => {
    // A closed menu leaves on the next tick, so take the newest one.
    const boxes = win.document.querySelectorAll('.blockr-select__search--menu');
    const box = boxes[boxes.length - 1];
    const shown = !!box && !box.classList.contains('blockr-select__search--offscreen');
    m.close();
    return shown;
  };
  const anchor = host(win);
  assert.strictEqual(filtered(win.Blockr.Select.menu(anchor, { options: MANY })), true);
  assert.strictEqual(
    filtered(win.Blockr.Select.menu(anchor, { options: MANY, search: false })),
    false
  );
  win.close();
});

test('setValue changes the pick without reporting it', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const one = win.Blockr.Select.single(host(win), {
    options: MANY, selected: 'a', onChange: (v) => seen.push(v)
  });
  one.setValue('c');
  assert.strictEqual(one.getValue(), 'c');

  const many = win.Blockr.Select.multi(host(win), {
    options: MANY, selected: ['a'], onChange: (v) => seen.push(v)
  });
  many.setValue(['b', 'not-an-option', 'd']);
  assert.deepStrictEqual(many.getValue(), ['b', 'd']);

  assert.deepStrictEqual(seen, []);
  win.close();
});

test('disabled: marked, not a tab stop, does not open, and setDisabled flips it', (newWindow) => {
  const win = newWindow();
  const click = (el) => el.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
  const sel = win.Blockr.Select.single(host(win), { options: MANY, bordered: true, disabled: true });
  const input = sel.el.querySelector('.blockr-select__search');
  assert.ok(sel.el.classList.contains('blockr-select--disabled'));
  assert.ok(input.disabled, 'the combobox input is disabled, so Tab skips it');
  input.focus();
  assert.notStrictEqual(win.document.activeElement, input);
  click(sel.el.querySelector('.blockr-select__control'));
  assert.ok(!sel.el.classList.contains('blockr-select--open'), 'a click does not open it');

  sel.setDisabled(false);
  assert.ok(!sel.el.classList.contains('blockr-select--disabled'));
  assert.ok(!input.disabled);
  click(sel.el.querySelector('.blockr-select__control'));
  assert.ok(sel.el.classList.contains('blockr-select--open'));
  // Disabling an open select closes it.
  sel.setDisabled(true);
  assert.ok(!sel.el.classList.contains('blockr-select--open'));
  assert.ok(sel.el.classList.contains('blockr-select--disabled'));
  win.close();
});

test('disabled multi: never opens, nothing drags, and setDisabled flips it', (newWindow) => {
  const win = newWindow();
  const seen = [];
  const sel = win.Blockr.Select.multi(host(win), {
    options: MANY, selected: ['a', 'b'], disabled: true, onChange: (v) => seen.push(v)
  });
  const control = sel.el.querySelector('.blockr-select__control');
  assert.strictEqual(control.tabIndex, -1, 'not a tab stop');
  assert.strictEqual(sel.el.querySelector('.blockr-pick__face').textContent, 'a, b', 'the picks stay');
  control.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
  assert.ok(!sel.el.classList.contains('blockr-select--open'));
  sel.setDisabled(false);
  assert.strictEqual(control.tabIndex, 0);
  control.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
  assert.ok(sel.el.classList.contains('blockr-select--open'));
  assert.strictEqual(win.document.querySelectorAll('.blockr-select__option[draggable="true"]').length, 2,
    'the picks drag once it is enabled');
  assert.deepStrictEqual(seen, []);
  win.close();
});

test('a menu hands back close and setOptions, nothing else', (newWindow) => {
  const win = newWindow();
  const m = win.Blockr.Select.menu(host(win), { options: MANY });
  assert.deepStrictEqual(Object.keys(m), ['close', 'setOptions']);
  m.close();
  win.close();
});
