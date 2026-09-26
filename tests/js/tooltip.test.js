/* Blockr.tooltip: the light card that replaces native `title` tooltips.
 *
 * What a caller can see: the card appears (role tooltip, aria-describedby on
 * the element), after the delay on hover and at once on focus or while warm,
 * leaves with Escape, and stays away from an `overflow` element that is not
 * cut off. happy-dom has no layout, so nothing is ever cut off here; that
 * case is the browser check's.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const wait = (ms) => new Promise((r) => setTimeout(r, ms));
const card = (win) => win.document.querySelector('.blockr-tooltip');

const button = (win, text) => {
  const b = win.document.createElement('button');
  if (text) b.textContent = text;
  win.document.body.appendChild(b);
  return b;
};

const over = (win, el) => el.dispatchEvent(new win.Event('pointerover', { bubbles: true }));
const out = (win, el) => el.dispatchEvent(new win.Event('pointerout', { bubbles: true }));

test('keyboard focus shows the card at once, and Escape hides it', (newWindow) => {
  const win = newWindow();
  const b = button(win);
  win.Blockr.tooltip.set(b, 'Settings');
  b.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  const c = card(win);
  assert.ok(c, 'shown on focus');
  assert.strictEqual(c.getAttribute('role'), 'tooltip');
  assert.strictEqual(c.textContent, 'Settings');
  assert.strictEqual(b.getAttribute('aria-describedby'), c.id);
  win.document.dispatchEvent(new win.KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
  assert.ok(!card(win), 'Escape hides it');
  assert.strictEqual(b.getAttribute('aria-describedby'), null);
  win.close();
});

test('on hover the card waits 300ms; once one has shown, the next is at once', async (newWindow) => {
  const win = newWindow();
  const a = button(win);
  const b = button(win);
  win.Blockr.tooltip.set(a, 'First');
  win.Blockr.tooltip.set(b, 'Second');
  over(win, a);
  await wait(150);
  assert.ok(!card(win), 'not yet at 150ms');
  await wait(250);
  assert.strictEqual(card(win).textContent, 'First');
  out(win, a);
  assert.ok(!card(win), 'leaves with the pointer');
  over(win, b);
  assert.strictEqual(card(win).textContent, 'Second', 'warm: no wait');
  win.close();
});

test('an overflow tooltip stays away while nothing is cut off', async (newWindow) => {
  const win = newWindow();
  const b = button(win, 'AGE');
  win.Blockr.tooltip.set(b, 'AGE', { overflow: true });
  b.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  assert.ok(!card(win));
  win.close();
});

test('a column shows its name, then its label muted; a list shows one per line', (newWindow) => {
  const win = newWindow();
  const b = button(win);
  win.Blockr.tooltip.set(b, [{ name: 'AGE', label: 'Age' }, { name: 'SEX', label: '' }, 'more']);
  assert.strictEqual(win.Blockr.tooltip.text(b), 'AGE · Age\nSEX\nmore');
  b.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  const lines = [...card(win).querySelectorAll('.blockr-tooltip__line')];
  assert.strictEqual(lines.length, 3);
  assert.strictEqual(lines[0].querySelector('.blockr-tooltip__meta').textContent, 'Age');
  assert.strictEqual(lines[1].querySelector('.blockr-tooltip__meta'), null);
  win.close();
});

test('the controls set no native title', (newWindow) => {
  const win = newWindow();
  const doc = win.document;
  const host = () => { const d = doc.createElement('div'); doc.body.appendChild(d); return d; };
  const opts = [{ value: 'AGE', label: 'Age' }, { value: 'SEX', label: 'Sex' }];
  const one = win.Blockr.Select.single(host(), { options: opts, selected: 'AGE' });
  win.Blockr.Select.multi(host(), { options: opts, selected: ['AGE', 'SEX'], maxTagChars: 2 });
  one.el.querySelector('.blockr-select__control').click();

  const input = doc.createElement('input');
  host().appendChild(input);
  const commit = win.Blockr.textCommit(input, { onCommit: () => {} });

  const seg = win.Blockr.segmented([{ value: 'a', label: 'Asc', title: 'Sort ascending' }], 'a', () => {});
  host().appendChild(seg.el);

  const gear = doc.createElement('button');
  const band = doc.createElement('div');
  host().append(gear, band);
  win.Blockr.gearTray(band, gear);

  assert.deepStrictEqual([...doc.querySelectorAll('[title]')].map((e) => e.className), []);
  assert.strictEqual(win.Blockr.tooltip.text(gear), 'Settings', 'the gear is icon-only');
  assert.strictEqual(win.Blockr.tooltip.text(commit.chip), '', 'the Enter button says its name');
  assert.strictEqual(win.Blockr.tooltip.text(seg.el.querySelector('button')), '', 'so does a segment');
  win.close();
});

test('a line can carry a badge after the name', (newWindow) => {
  const win = newWindow();
  const b = button(win);
  win.Blockr.tooltip.set(b, { name: 'dataset block', badge: 'blockr.core' });
  b.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  const badge = card(win).querySelector('.blockr-tooltip__badge');
  assert.ok(badge, 'badge drawn');
  assert.strictEqual(badge.textContent, 'blockr.core');
  assert.strictEqual(win.Blockr.tooltip.text(b), 'dataset block · blockr.core');
  win.close();
});

test('markup from R carries its tooltip as an attribute', (newWindow) => {
  const win = newWindow();
  const b = button(win);
  b.setAttribute('data-blockr-tooltip', 'Download');
  const icon = win.document.createElement('span');
  b.appendChild(icon);
  icon.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  assert.strictEqual(card(win).textContent, 'Download', 'found from a child');
  assert.strictEqual(win.Blockr.tooltip.text(b), 'Download');
  win.document.dispatchEvent(new win.KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
  win.Blockr.tooltip.set(b, 'Set wins');
  b.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  assert.strictEqual(card(win).textContent, 'Set wins', 'set() wins over the attribute');
  win.close();
});

test('an empty attribute shows nothing', (newWindow) => {
  const win = newWindow();
  const b = button(win);
  b.setAttribute('data-blockr-tooltip', '');
  b.dispatchEvent(new win.FocusEvent('focusin', { bubbles: true }));
  assert.ok(!card(win));
  win.close();
});

test('editable text names its gesture, the default or its own', (newWindow) => {
  const win = newWindow();
  const a = win.document.createElement('span');
  a.textContent = 'Filter rows';
  a.setAttribute('data-blockr-editable', '');
  const b = win.document.createElement('span');
  b.textContent = 'Overview';
  b.setAttribute('data-blockr-editable', 'Click to rename');
  win.document.body.append(a, b);
  over(win, a);
  assert.strictEqual(win.Blockr.tooltip.text(a), 'Double-click to edit');
  over(win, b);
  assert.strictEqual(win.Blockr.tooltip.text(b), 'Click to rename');
  b.removeAttribute('data-blockr-editable');
  assert.strictEqual(win.Blockr.tooltip.text(b), '', 'none once the attribute goes');
  win.close();
});
