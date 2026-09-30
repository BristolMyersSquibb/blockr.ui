/* Blockr.menu: the action menu.
 *
 * What a caller can see: the panel opens under its trigger with role menu,
 * draws rows, dividers, titles and a head; arrows move the keyboard row past
 * disabled rows; Enter picks, runs onSelect and closes; Escape closes and
 * hands focus back to the trigger; a click outside closes; bind() toggles.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const panel = (win) => win.document.querySelector('.blockr-menu');
const key = (win, el, k) =>
  el.dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true }));

const trigger = (win) => {
  const b = win.document.createElement('button');
  b.textContent = '…';
  win.document.body.appendChild(b);
  return b;
};

test('draws rows, a divider, a title and a head', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, {
    head: { title: 'Filter rows', badge: 'blockr.dplyr', text: 'Keep rows that match.' },
    items: [
      { title: 'Block' },
      { label: 'Rename' },
      { label: 'Copy block ID', meta: 'filter_1', mono: true },
      { divider: true },
      { label: 'Remove', danger: true }
    ]
  });
  const p = panel(win);
  assert.ok(p, 'open');
  const list = p.querySelector('[role="menu"]');
  assert.ok(list && !p.hasAttribute('role'), 'the menu role is on the list of rows');
  assert.ok(!list.contains(p.querySelector('.blockr-menu__head')), 'the head sits above it');
  assert.strictEqual(t.getAttribute('aria-controls'), list.id);
  assert.strictEqual(p.parentElement, win.document.body, 'portalled to body');
  assert.strictEqual(p.querySelector('.blockr-menu__badge').textContent, 'blockr.dplyr');
  assert.strictEqual(p.querySelector('.blockr-menu__head-text').textContent, 'Keep rows that match.');
  assert.strictEqual(p.querySelector('.blockr-menu__title').textContent, 'Block');
  const rows = p.querySelectorAll('.blockr-menu__item');
  assert.strictEqual(rows.length, 3);
  assert.strictEqual(rows[1].querySelector('.blockr-menu__meta--mono').textContent, 'filter_1');
  assert.ok(rows[2].classList.contains('blockr-menu__item--danger'));
  assert.ok(p.querySelector('.blockr-menu__divider'));
  assert.strictEqual(t.getAttribute('aria-expanded'), 'true');
  win.close();
});

test('arrows skip disabled rows, Enter picks and closes', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  const picked = [];
  win.Blockr.menu(t, {
    items: [
      { label: 'One', onSelect: () => picked.push('one') },
      { label: 'Two', disabled: true, reason: 'Not now', onSelect: () => picked.push('two') },
      { label: 'Three', onSelect: () => picked.push('three') }
    ]
  });
  const p = panel(win);
  key(win, p, 'ArrowDown');
  key(win, p, 'ArrowDown');
  const active = p.querySelector('.blockr-menu__item--active');
  assert.strictEqual(active.textContent, 'Three', 'skipped the disabled row');
  key(win, p, 'Enter');
  assert.deepStrictEqual(picked, ['three']);
  assert.ok(!panel(win), 'closed after the pick');
  assert.strictEqual(t.getAttribute('aria-expanded'), 'false');
  win.close();
});

test('a disabled row does nothing on click', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  let ran = false;
  win.Blockr.menu(t, { items: [{ label: 'Two', disabled: true, onSelect: () => { ran = true; } }] });
  panel(win).querySelector('.blockr-menu__item').click();
  assert.strictEqual(ran, false);
  assert.ok(panel(win), 'still open');
  win.close();
});

test('Escape closes and hands focus back to the trigger', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  let closed = 0;
  win.Blockr.menu(t, { items: [{ label: 'One' }], onClose: () => { closed++; } });
  key(win, panel(win), 'Escape');
  assert.ok(!panel(win));
  assert.strictEqual(win.document.activeElement, t);
  assert.strictEqual(closed, 1);
  win.close();
});

test('Tab closes and leaves focus on the trigger, for the browser to move on', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, { filter: true, items: [{ label: 'One' }] });
  const box = panel(win).querySelector('.blockr-menu__filter-input');
  assert.strictEqual(win.document.activeElement, box);
  key(win, box, 'Tab');
  assert.ok(!panel(win));
  assert.strictEqual(win.document.activeElement, t);
  win.close();
});

test('the keyboard row is the active descendant of what holds focus', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, { items: [{ label: 'One' }, { label: 'Two' }] });
  const list = panel(win).querySelector('[role="menu"]');
  assert.strictEqual(win.document.activeElement, list, 'without a filter box, the list');
  key(win, list, 'ArrowDown');
  key(win, list, 'ArrowDown');
  const two = panel(win).querySelectorAll('.blockr-menu__item')[1];
  assert.strictEqual(list.getAttribute('aria-activedescendant'), two.id);
  key(win, list, 'Escape');
  assert.ok(!t.hasAttribute('aria-controls'), 'no reference to a panel that is gone');

  win.Blockr.menu(t, { filter: true, items: [{ label: 'One' }, { label: 'Two' }] });
  const box = panel(win).querySelector('.blockr-menu__filter-input');
  assert.strictEqual(box.getAttribute('aria-controls'), panel(win).querySelector('[role="menu"]').id);
  key(win, box, 'ArrowDown');
  const one = panel(win).querySelector('.blockr-menu__item');
  assert.strictEqual(box.getAttribute('aria-activedescendant'), one.id, 'with one, the box');
  win.close();
});

test('a click outside closes it', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, { items: [{ label: 'One' }] });
  // The pointerdown decides, as it comes first.
  const tap = (el) => el.dispatchEvent(new win.PointerEvent('pointerdown', { bubbles: true }));
  tap(panel(win).querySelector('.blockr-menu__item'));
  assert.ok(panel(win), 'a row is inside');
  const other = win.document.createElement('div');
  win.document.body.appendChild(other);
  tap(other);
  assert.ok(!panel(win));
  win.close();
});

test('bind() opens on click, closes on a second click, reads a function config', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  let reads = 0;
  win.Blockr.menu.bind(t, () => { reads++; return { items: [{ label: 'One' }] }; });
  assert.strictEqual(t.getAttribute('aria-haspopup'), 'menu');
  t.click();
  assert.ok(panel(win), 'open after the first click');
  t.click();
  assert.ok(!panel(win), 'closed after the second');
  t.click();
  assert.strictEqual(reads, 2, 'config read on each open');
  win.close();
});

test('bind(): a click from the keyboard opens on the first row, one from the pointer on none', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu.bind(t, { items: [{ label: 'One' }, { label: 'Two' }] });
  // A click the keyboard made (Enter or Space on the button) has detail 0.
  const click = (detail) =>
    t.dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true, detail }));
  const list = () => panel(win).querySelector('[role="menu"]');
  click(1);
  assert.strictEqual(list().getAttribute('aria-activedescendant'), null);
  click(1);
  click(0);
  const one = panel(win).querySelector('.blockr-menu__item');
  assert.strictEqual(list().getAttribute('aria-activedescendant'), one.id);
  win.close();
});

test('opening a second menu closes the first', (newWindow) => {
  const win = newWindow();
  const a = trigger(win);
  const b = trigger(win);
  win.Blockr.menu(a, { items: [{ label: 'A' }] });
  win.Blockr.menu(b, { items: [{ label: 'B' }] });
  const open = win.document.querySelectorAll('.blockr-menu');
  assert.strictEqual(open.length, 1);
  assert.strictEqual(open[0].textContent, 'B');
  win.close();
});

test('a gap separates groups, the current item carries a check, quiet rows are marked', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, {
    items: [
      { label: 'Overview', current: true, meta: '3 blocks' },
      { label: 'Labs' },
      { gap: true },
      { label: 'Manage pages', quiet: true, icon: 'sliders' }
    ]
  });
  const p = panel(win);
  assert.ok(p.querySelector('.blockr-menu__gap'), 'gap drawn');
  const rows = p.querySelectorAll('.blockr-menu__item');
  assert.ok(rows[0].querySelector('.blockr-menu__check'), 'check on the current item');
  assert.ok(rows[0].lastElementChild.classList.contains('blockr-menu__check'),
    'at the end of the row, after its meta text');
  assert.ok(!rows[1].querySelector('.blockr-menu__check'));
  assert.ok(!rows[1].querySelector('.blockr-menu__icon'), 'no icon unless given');
  assert.ok(rows[2].classList.contains('blockr-menu__item--quiet'));
  assert.ok(rows[2].querySelector('.blockr-menu__icon svg'), 'icon by name from Blockr.icons');
  win.close();
});

test('a row\'s mark is the block\'s mark, in its category\'s colour or one of its own', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, {
    items: [
      { label: 'Chart', mark: { icon: '<svg viewBox="0 0 16 16"></svg>', category: 'plot' } },
      { label: 'Stack', mark: { icon: '<svg></svg>', color: '#7c3aed' } },
      { label: 'Rename' }
    ]
  });
  const rows = panel(win).querySelectorAll('.blockr-menu__item');
  const chart = rows[0].querySelector('.blockr-mark');
  assert.strictEqual(rows[0].firstElementChild, chart, 'before the label');
  assert.strictEqual(chart.className, 'blockr-mark', 'the 24px size, no modifier');
  assert.strictEqual(chart.dataset.category, 'plot');
  assert.strictEqual(chart.style.color, '', 'the stylesheet colours it');
  assert.ok(chart.querySelector('svg'), 'the glyph');
  const stack = rows[1].querySelector('.blockr-mark');
  assert.strictEqual(stack.dataset.category, undefined);
  assert.notStrictEqual(stack.style.color, '', 'its own colour');
  assert.strictEqual(rows[2].querySelector('.blockr-mark'), null, 'no mark unless given');
  win.close();
});

test('a filter box narrows the rows, hides empty groups and Enter takes the first match', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  const picked = [];
  win.Blockr.menu(t, {
    caption: 'Add a block',
    filter: 'Search blocks',
    items: [
      { title: 'Transform' },
      { label: 'Filter rows', badge: 'blockr.dplyr', keywords: 'subset', onSelect: () => picked.push('filter') },
      { label: 'Arrange rows', badge: 'blockr.dplyr', onSelect: () => picked.push('arrange') },
      { title: 'Plot' },
      { label: 'Chart', badge: 'blockr.viz', mark: { icon: '<svg></svg>', category: 'plot' }, onSelect: () => picked.push('chart') }
    ]
  });
  const p = panel(win);
  assert.strictEqual(p.querySelector('.blockr-menu__caption').textContent, 'Add a block');
  const input = p.querySelector('.blockr-menu__filter-input');
  assert.strictEqual(win.document.activeElement, input, 'the filter holds the focus');
  assert.ok(p.querySelector('.blockr-mark'), 'mark drawn');

  input.value = 'subset';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
  const visible = [...p.querySelectorAll('.blockr-menu__item')].filter((r) => !r.hidden);
  assert.deepStrictEqual(visible.map((r) => r.querySelector('.blockr-menu__label').textContent), ['Filter rows']);
  const titles = [...p.querySelectorAll('.blockr-menu__title')].map((x) => x.hidden);
  assert.deepStrictEqual(titles, [false, true], 'the Plot group hides');

  input.value = 'zzz';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
  assert.ok(!p.querySelector('.blockr-menu__empty').hidden, 'no matches shown');

  input.value = 'viz';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
  key(win, input, 'Enter');
  assert.deepStrictEqual(picked, ['chart']);
  assert.ok(!panel(win), 'closed after the pick');
  win.close();
});

test('Enter takes the first row whose label matches before a keyword match', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  const picked = [];
  win.Blockr.menu(t, {
    filter: true,
    items: [
      { label: 'Heatmap', keywords: 'filter by colour', onSelect: () => picked.push('heatmap') },
      { label: 'Filter rows', onSelect: () => picked.push('filter') }
    ]
  });
  const input = panel(win).querySelector('.blockr-menu__filter-input');
  input.value = 'filter';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
  key(win, input, 'Enter');
  assert.deepStrictEqual(picked, ['filter']);
  win.close();
});

test('a checked row carries a check and its state, without the current weight', (newWindow) => {
  const win = newWindow();
  const t = trigger(win);
  win.Blockr.menu(t, { items: [{ label: 'Controls', checked: true }, { label: 'Preview', checked: false }] });
  const rows = panel(win).querySelectorAll('.blockr-menu__item');
  assert.strictEqual(rows[0].getAttribute('role'), 'menuitemcheckbox');
  assert.strictEqual(rows[0].getAttribute('aria-checked'), 'true');
  assert.ok(rows[0].querySelector('.blockr-menu__check'));
  assert.ok(!rows[0].classList.contains('blockr-menu__item--current'));
  assert.strictEqual(rows[1].getAttribute('aria-checked'), 'false');
  assert.ok(!rows[1].querySelector('.blockr-menu__check'));
  win.close();
});
