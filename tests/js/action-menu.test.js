/* Blockr.actionMenu: the menus action_menu() builds in R.
 *
 * The markup here is what action_menu() renders (tests/testthat pins that
 * side). What a caller can see: a click on the trigger opens the list on
 * <body> and a second click, a pick, Escape, Tab or a click outside closes it
 * and puts it back beside the trigger; the keyboard moves over the usable
 * rows only, with the focus on the list and the keyboard row its active
 * descendant, as in Blockr.menu; a disabled row does nothing; a block
 * removed while its menu is open takes the menu with it.
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
/* A pointer's click: the pointerdown first, which the dismiss stack reads as
 * landing inside the menu or outside it. */
const tap = (win, el) => {
  el.dispatchEvent(new win.PointerEvent('pointerdown', { bubbles: true }));
  click(win, el);
};
const key = (win, el, k) =>
  el.dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));
/** The keyboard row: the list's active descendant. */
const row = (panel) => panel.getAttribute('aria-activedescendant');

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

  click(win, trigger, 0);
  assert.strictEqual(win.document.activeElement, panel, 'the list holds the focus');
  assert.strictEqual(row(panel), 'pptx', 'a keyboard open starts on the first row');
  assert.strictEqual(panel.querySelector('.blockr-menu__item--active').id, 'pptx');
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'html', 'the disabled row is passed over');
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'rm');
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'pptx', 'wraps round');
  key(win, panel, 'ArrowUp');
  assert.strictEqual(row(panel), 'rm');
  key(win, panel, 'Home');
  assert.strictEqual(row(panel), 'pptx');
  key(win, panel, 'End');
  assert.strictEqual(row(panel), 'rm');

  key(win, panel, 'Escape');
  assert.strictEqual(panel.hidden, true, 'Escape closes');
  assert.strictEqual(win.document.activeElement, trigger, 'and hands focus back');
  assert.strictEqual(row(panel), null, 'with no keyboard row left behind');
  assert.strictEqual(panel.querySelector('.blockr-menu__item--active'), null);
  win.close();
});

test('a download Shiny has not bound yet keeps its place for the keyboard', (newWindow) => {
  const win = newWindow();
  const { trigger, panel, clicks } = build(win);
  // A downloadLink() before its handler binds, as Shiny marks it; on the
  // first open every download is still in this state.
  for (const id of ['pptx', 'html']) {
    const r = panel.querySelector(`#${id}`);
    r.classList.add('shiny-download-link', 'disabled');
    r.setAttribute('aria-disabled', 'true');
  }
  click(win, trigger, 0);
  assert.strictEqual(row(panel), 'pptx', 'the first row, not Remove');
  key(win, panel, ' ');
  assert.strictEqual(clicks.pptx, 0, 'inert until Shiny binds it');
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'html', 'the row its author disabled is still passed over');

  // Shiny binds the handler.
  const pptx = panel.querySelector('#pptx');
  pptx.classList.remove('disabled');
  pptx.removeAttribute('aria-disabled');
  key(win, panel, 'ArrowUp');
  key(win, panel, ' ');
  assert.strictEqual(clicks.pptx, 1);
  win.close();
});

test('arrows from a mouse open start at the ends', (newWindow) => {
  const win = newWindow();
  const { trigger, panel } = build(win);
  click(win, trigger);
  assert.strictEqual(row(panel), null, 'a mouse open marks no row');
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'pptx');
  key(win, panel, 'Escape');
  click(win, trigger);
  key(win, panel, 'ArrowUp');
  assert.strictEqual(row(panel), 'rm');
  win.close();
});

test('Enter clicks the keyboard row, as the pointer would', async (newWindow) => {
  const win = newWindow();
  const { wrap, trigger, panel, clicks } = build(win);
  click(win, trigger, 0);
  key(win, panel, 'ArrowDown');
  key(win, panel, 'Enter');
  assert.strictEqual(clicks.html, 1);
  await wait(5);
  assert.strictEqual(panel.parentNode, wrap, 'then the menu closes');
  win.close();
});

test('the pointer moves the keyboard row, over usable rows only', (newWindow) => {
  const win = newWindow();
  const { trigger, panel } = build(win);
  const move = (el) => el.dispatchEvent(new win.MouseEvent('mousemove', { bubbles: true }));
  click(win, trigger);
  move(panel.querySelector('#html'));
  assert.strictEqual(row(panel), 'html');
  move(panel.querySelector('#xlsx'));
  assert.strictEqual(row(panel), 'html', 'a disabled row does not take it');
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'rm', 'the keys go on from where the pointer left it');
  panel.dispatchEvent(new win.MouseEvent('mouseleave'));
  assert.strictEqual(row(panel), null);
  win.close();
});

test('the down arrow on the trigger opens the menu on its first row', (newWindow) => {
  const win = newWindow();
  const { trigger, panel } = build(win);
  trigger.focus();
  key(win, trigger, 'ArrowDown');
  assert.strictEqual(panel.hidden, false);
  assert.strictEqual(win.document.activeElement, panel);
  assert.strictEqual(row(panel), 'pptx');
  win.close();
});

test('one menu of either kind is open at a time', (newWindow) => {
  const win = newWindow();
  const { wrap, trigger, panel } = build(win);
  const views = win.document.createElement('button');
  win.document.body.appendChild(views);
  click(win, trigger);
  win.Blockr.menu(views, { items: [{ label: 'Page 1' }] });
  assert.strictEqual(panel.parentNode, wrap, 'Blockr.menu closed the action menu');
  assert.strictEqual(win.Blockr.actionMenu.current(), null);
  click(win, trigger);
  assert.strictEqual(win.document.querySelector('.blockr-menu__list'), null,
    'and the action menu closed it in turn');
  assert.strictEqual(win.Blockr.actionMenu.current(), trigger);
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

  tap(win, a.trigger);
  tap(win, a.panel.querySelector('.blockr-menu__title'));
  assert.strictEqual(a.panel.hidden, false, 'a click in the list is inside');
  tap(win, win.document.getElementById('after'));
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

test('a row the page has hidden is skipped by the keyboard', (newWindow) => {
  const win = newWindow();
  const { trigger, panel } = build(win);
  panel.querySelector('#html').hidden = true;
  click(win, trigger, 0);
  key(win, panel, 'ArrowDown');
  assert.strictEqual(row(panel), 'rm');
  win.close();
});

/* --- In Chrome ------------------------------------------------------------ */

/* Real keys (browser.js): a click the keyboard makes reaches the page with
 * detail 0, which is how the menu tells a keyboard open from a pointer's. */
const chrome = require('./browser').test;

chrome('in Chrome, Enter on the trigger opens on the first row, and Enter runs the keyboard row', async (page) => {
  await page.evaluate(() => {
    const host = document.createElement('div');
    host.innerHTML = `
      <span class="blockr-action-menu" data-align="start">
        <button class="blockr-tool blockr-action-menu__trigger" type="button" aria-label="Actions">…</button>
        <div class="blockr-menu" role="menu" tabindex="-1" hidden>
          <button id="rename" class="blockr-menu__item" role="menuitem" tabindex="-1" type="button">Rename</button>
          <button id="remove" class="blockr-menu__item" role="menuitem" tabindex="-1" type="button">Remove</button>
        </div>
      </span>`;
    document.body.appendChild(host);
    window.ran = [];
    for (const b of host.querySelectorAll('.blockr-menu__item')) {
      b.addEventListener('click', () => window.ran.push(b.id));
    }
  });
  const state = () => page.evaluate(() => {
    const panel = document.querySelector('.blockr-menu');
    return {
      open: !panel.hidden,
      row: panel.getAttribute('aria-activedescendant'),
      focus: document.activeElement.className,
      ran: window.ran
    };
  });
  await page.focus('.blockr-action-menu__trigger');
  await page.keyboard.press('Enter');
  assert.deepStrictEqual(await state(), { open: true, row: 'rename', focus: 'blockr-menu', ran: [] });
  await page.keyboard.press('ArrowDown');
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('.blockr-menu').hidden);
  assert.deepStrictEqual(await state(), {
    open: false, row: null, focus: 'blockr-tool blockr-action-menu__trigger', ran: ['remove']
  });
});
