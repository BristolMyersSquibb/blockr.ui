/* The small controls in blockr-ui.js, seen from a caller: where Blockr.place
 * puts a panel, what the Enter button does with Escape, when the gear tray
 * closes.
 *
 * happy-dom has no layout engine, so the placement tests hand the anchor a
 * rect and the panel a width and check the arithmetic; the real thing is
 * checked in a browser.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const host = (win) => {
  const el = win.document.createElement('div');
  win.document.body.appendChild(el);
  return el;
};

/* --- Blockr.place --------------------------------------------------------- */

/* A viewport 800px wide, an anchor at a given rect, a panel of a given
 * natural width. Returns the panel's left and width as numbers. */
const placed = (win, rect, panelWidth, opts) => {
  Object.defineProperty(win.document.documentElement, 'clientWidth',
    { value: 800, configurable: true });
  const anchor = host(win);
  anchor.getBoundingClientRect = () => Object.assign(
    { top: 100, bottom: 130, height: 30, right: rect.left + rect.width }, rect);
  const panel = host(win);
  Object.defineProperty(panel, 'offsetWidth', { value: panelWidth, configurable: true });
  const h = win.Blockr.place(panel, anchor, opts);
  h.stop();
  return { left: parseFloat(panel.style.left), width: parseFloat(panel.style.width) || panelWidth,
           top: parseFloat(panel.style.top) };
};

test('place: a field dropdown takes the control\'s width, at least minWidth', (newWindow) => {
  const win = newWindow();
  const wide = placed(win, { left: 40, width: 300 }, 0, { minWidth: 190 });
  assert.deepStrictEqual([wide.left, wide.width], [40, 300]);
  const narrow = placed(win, { left: 40, width: 120 }, 0, { minWidth: 190 });
  assert.deepStrictEqual([narrow.left, narrow.width], [40, 190]);
  assert.strictEqual(narrow.top, 134, '4px under the anchor');
  win.close();
});

test('place: both width modes stay 8px inside the viewport', (newWindow) => {
  const win = newWindow();
  // A 120px control at the right edge, widened to 190px: pulled back.
  const field = placed(win, { left: 700, width: 120 }, 0, { minWidth: 190 });
  assert.strictEqual(field.left, 800 - 190 - 8);
  // A word near the edge with a content-sized menu.
  const word = placed(win, { left: 760, width: 30 }, 250, { width: { min: 180, max: 320 } });
  assert.strictEqual(word.left, 800 - 250 - 8);
  // And never past the left edge either.
  const edge = placed(win, { left: 2, width: 30 }, 250, { width: { min: 180, max: 320 } });
  assert.strictEqual(edge.left, 8);
  win.close();
});

test('place: align end lines the panel up with the anchor\'s right edge', (newWindow) => {
  const win = newWindow();
  const start = placed(win, { left: 500, width: 30 }, 250, { width: { min: 180, max: 320 } });
  assert.strictEqual(start.left, 500);
  const end = placed(win, { left: 500, width: 30 }, 250,
    { width: { min: 180, max: 320 }, align: 'end' });
  assert.strictEqual(end.left, 530 - 250);
  // A field dropdown widened past its control also aligns on the right.
  const field = placed(win, { left: 500, width: 120 }, 0, { minWidth: 190, align: 'end' });
  assert.strictEqual(field.left, 620 - 190);
  win.close();
});

/* --- Blockr.textCommit and the gear tray ---------------------------------- */

const press = (win, el, key) => {
  const e = new win.KeyboardEvent('keydown', { key, bubbles: true, cancelable: true });
  el.dispatchEvent(e);
  return e;
};

/* A field inside a gear tray, with the tray listening for Escape. Returns
 * what a caller can observe: the input, the chip, and how often Escape
 * reached the tray. */
const fieldInTray = (win) => {
  const band = host(win);
  band.className = 'blockr-settings';
  const gear = win.document.createElement('button');
  win.document.body.appendChild(gear);
  const input = win.document.createElement('input');
  input.value = 'AGE';
  band.appendChild(input);
  const committed = [];
  const h = win.Blockr.textCommit(input, { onCommit: (v) => committed.push(v) });
  const tray = win.Blockr.gearTray(band, gear);
  return { band, gear, input, chip: h.chip, tray, committed };
};

const type = (win, input, text) => {
  input.value = text;
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
};

test('textCommit: Escape on a dirty field reverts it and stops there', (newWindow) => {
  const win = newWindow();
  const f = fieldInTray(win);
  f.tray.set(true);
  type(win, f.input, 'SEX');
  assert.strictEqual(f.chip.style.display, '', 'armed');
  press(win, f.input, 'Escape');
  assert.strictEqual(f.input.value, 'AGE', 'reverted');
  assert.strictEqual(f.chip.style.display, 'none', 'disarmed');
  assert.ok(f.tray.isOpen(), 'the tray did not see the Escape');
  assert.deepStrictEqual(f.committed, []);
  win.close();
});

test('textCommit: Escape on a clean field goes through, and closes the tray', (newWindow) => {
  const win = newWindow();
  const f = fieldInTray(win);
  f.tray.set(true);
  press(win, f.input, 'Escape');
  assert.ok(!f.tray.isOpen(), 'a clean field has nothing to revert, so the tray closes');
  assert.strictEqual(win.document.activeElement, f.gear, 'focus returns to the gear');
  win.close();
});

test('textCommit: the Enter button reads "Enter" while dirty and is marked confirmed after', (newWindow) => {
  const win = newWindow();
  const f = fieldInTray(win);
  type(win, f.input, 'SEX');
  assert.ok(f.chip.textContent.startsWith('Enter'));
  assert.ok(!f.chip.classList.contains('confirmed'));
  press(win, f.input, 'Enter');
  assert.deepStrictEqual(f.committed, ['SEX']);
  assert.ok(f.chip.classList.contains('confirmed'));
  assert.strictEqual(f.chip.style.display, '', 'stays, as the confirmed mark');
  win.close();
});

test('gear tray: the gear toggles it and Escape on the gear closes it', (newWindow) => {
  const win = newWindow();
  const f = fieldInTray(win);
  assert.strictEqual(f.gear.getAttribute('aria-expanded'), 'false');
  f.gear.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
  assert.ok(f.tray.isOpen());
  assert.ok(f.band.classList.contains('blockr-settings--open'));
  assert.ok(f.gear.classList.contains('blockr-gear-active'));
  assert.strictEqual(f.gear.getAttribute('aria-expanded'), 'true');
  // After the click, focus is on the gear, not in the band.
  let reached = 0;
  win.document.addEventListener('keydown', () => { reached++; });
  f.gear.focus();
  press(win, f.gear, 'Escape');
  assert.ok(!f.tray.isOpen());
  assert.strictEqual(reached, 0, 'consumed: nothing above the gear sees it');
  assert.ok(!f.gear.classList.contains('blockr-gear-active'));
  // Escape on a closed tray does nothing and is not consumed.
  press(win, f.gear, 'Escape');
  assert.strictEqual(reached, 1);
  win.close();
});
