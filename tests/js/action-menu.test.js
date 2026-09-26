/* Blockr.actionMenu: the menus action_menu() builds in R.
 *
 * The markup here is what action_menu() renders (tests/testthat pins that
 * side). What a caller can see: a click on the trigger opens the list on
 * <body> and a second click, a pick, Escape, Tab or a click outside closes it
 * and puts it back beside the trigger; the keyboard moves over the usable
 * rows only; a disabled row does nothing; a block removed while its menu is
 * open takes the menu with it.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./select-impls');

const wait = (ms) => new Promise((r) => setTimeout(r, ms));

/** The markup action_menu() renders, with a click counter on each row. */
const build = (win) => {
  const doc = win.document;
  const host = doc.createElement('div');
  host.innerHTML = `
    <span class="blockr-action-menu" data-align="end">
      <button class="blockr-tool blockr-action-menu__trigger" type="button"
              aria-haspopup="menu" aria-expanded="false"
              data-blockr-tooltip="Download">D</button>
      <div class="blockr-menu" role="menu" tabindex="-1" hidden>
        <div class="blockr-menu__title" role="presentation">This patient</div>
        <a id="pptx" class="blockr-menu__item" role="menuitem" tabindex="-1" href="#">PowerPoint</a>
        <a id="xlsx" class="blockr-menu__item blockr-menu__item--disabled" role="menuitem"
           tabindex="-1" aria-disabled="true" href="#">Excel</a>
        <a id="html" class="blockr-menu__item" role="menuitem" tabindex="-1" href="#">Web page</a>
        <div class="blockr-menu__divider" role="separator"></div>
        <button id="rm" class="blockr-menu__item blockr-menu__item--danger" role="menuitem"
                tabindex="-1" type="button">Remove</button>
      </div>
    </span>
    <span id="after">after</span>`;
  doc.body.appendChild(host);
  const wrap = host.querySelector('.blockr-action-menu');
  const trigger = host.querySelector('.blockr-action-menu__trigger');
  const panel = host.querySelector('.blockr-menu');
  const clicks = {};
  for (const row of panel.querySelectorAll('.blockr-menu__item')) {
    clicks[row.id] = 0;
    row.addEventListener('click', (e) => { e.preventDefault(); clicks[row.id]++; });
  }
  return { host, wrap, trigger, panel, clicks };
};

const click = (win, el, detail = 1) =>
  el.dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true, detail }));
const key = (win, el, k) =>
  el.dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));

test('a click opens the list on the body; a second click puts it back', (newWindow) => {
  const win = newWindow();
  const { wrap, trigger, panel } = build(win);

  click(win, trigger);
  assert.strictEqual(panel.parentNode, win.document.body, 'portalled to <body>');
  assert.strictEqual(panel.hidden, false);
  assert.strictEqual(trigger.getAttribute('aria-expanded'), 'true');
  assert.strictEqual(trigger.getAttribute('aria-controls'), panel.id);
  assert.strictEqual(win.document.activeElement, panel, 'a mouse open focuses the list, no row');
  assert.strictEqual(win.Blockr.actionMenu.current(), trigger);

  click(win, trigger);
  assert.strictEqual(panel.parentNode, wrap, 'back beside its trigger');
  assert.strictEqual(panel.previousElementSibling, trigger);
  assert.strictEqual(panel.hidden, true);
  assert.strictEqual(trigger.getAttribute('aria-expanded'), 'false');
  assert.strictEqual(win.Blockr.actionMenu.current(), null);
  win.close();
});

test('the keyboard opens on the first row and moves over usable rows only', (newWindow) => {
  const win = newWindow();
  const { trigger, panel } = build(win);
  const id = () => win.document.activeElement.id;

  click(win, trigger, 0);
  assert.strictEqual(id(), 'pptx', 'a keyboard open focuses the first row');
  key(win, win.document.activeElement, 'ArrowDown');
  assert.strictEqual(id(), 'html', 'the disabled row is skipped');
  key(win, win.document.activeElement, 'ArrowDown');
  assert.strictEqual(id(), 'rm');
  key(win, win.document.activeElement, 'ArrowDown');
  assert.strictEqual(id(), 'pptx', 'wraps round');
  key(win, win.document.activeElement, 'ArrowUp');
  assert.strictEqual(id(), 'rm');
  key(win, win.document.activeElement, 'Home');
  assert.strictEqual(id(), 'pptx');
  key(win, win.document.activeElement, 'End');
  assert.strictEqual(id(), 'rm');

  key(win, win.document.activeElement, 'Escape');
  assert.strictEqual(panel.hidden, true, 'Escape closes');
  assert.strictEqual(win.document.activeElement, trigger, 'and hands focus back');
  win.close();
});

test('arrows from a mouse open start at the ends', (newWindow) => {
  const win = newWindow();
  const { trigger, panel } = build(win);
  click(win, trigger);
  key(win, panel, 'ArrowDown');
  assert.strictEqual(win.document.activeElement.id, 'pptx');
  key(win, win.document.activeElement, 'Escape');
  click(win, trigger);
  key(win, panel, 'ArrowUp');
  assert.strictEqual(win.document.activeElement.id, 'rm');
  win.close();
});

test('a pick runs the row, then the menu closes', async (newWindow) => {
  const win = newWindow();
  const { wrap, trigger, panel, clicks } = build(win);

  click(win, trigger);
  click(win, panel.querySelector('#html'));
  assert.strictEqual(clicks.html, 1, 'the row did its job');
  await wait(5);
  assert.strictEqual(panel.parentNode, wrap);
  assert.strictEqual(win.document.activeElement, trigger);

  click(win, trigger, 0);
  key(win, win.document.activeElement, 'End');
  key(win, win.document.activeElement, ' ');
  assert.strictEqual(clicks.rm, 1, 'Space runs the keyboard row');
  await wait(5);
  assert.strictEqual(panel.hidden, true);
  win.close();
});

test('a disabled row does nothing and keeps the menu open', async (newWindow) => {
  const win = newWindow();
  const { trigger, panel, clicks } = build(win);
  click(win, trigger);
  const ev = new win.MouseEvent('click', { bubbles: true, cancelable: true, detail: 1 });
  panel.querySelector('#xlsx').dispatchEvent(ev);
  assert.strictEqual(clicks.xlsx, 0);
  assert.strictEqual(ev.defaultPrevented, true);
  await wait(5);
  assert.strictEqual(panel.hidden, false);
  win.close();
});

test('a click outside or Tab closes; opening another menu closes the first', (newWindow) => {
  const win = newWindow();
  const a = build(win);
  const b = build(win);

  click(win, a.trigger);
  click(win, win.document.getElementById('after'));
  assert.strictEqual(a.panel.hidden, true, 'outside click');

  click(win, a.trigger);
  key(win, a.panel, 'Tab');
  assert.strictEqual(a.panel.hidden, true, 'Tab');

  click(win, a.trigger);
  click(win, b.trigger);
  assert.strictEqual(a.panel.hidden, true, 'one menu at a time');
  assert.strictEqual(a.panel.parentNode, a.wrap);
  assert.strictEqual(b.panel.hidden, false);
  assert.strictEqual(win.Blockr.actionMenu.current(), b.trigger);
  win.close();
});

test('a block removed while its menu is open takes the menu with it', async (newWindow) => {
  const win = newWindow();
  const { host, trigger, panel } = build(win);
  click(win, trigger);
  host.remove();
  await wait(5);
  assert.strictEqual(panel.isConnected, false, 'not left behind on <body>');
  assert.strictEqual(win.Blockr.actionMenu.current(), null);
  win.close();
});
