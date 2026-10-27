/* blockr-shiny.js: the gear's binding, driven the way Shiny drives it
 * (find the element, initialize, subscribe, read the value), and Shiny's
 * selects on the dismiss stack. Shiny is a stub that records the binding;
 * the markup is what the R functions write.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { test } = require('./select-impls');

const shinyJs = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'blockr-shiny.js'), 'utf8');

/* A window with Blockr, a Shiny stub and blockr-shiny.js. */
const shinyWindow = (newWindow) => {
  const win = newWindow();
  const bindings = {};
  class InputBinding {
    getId(el) { return el.getAttribute('data-input-id') || el.id; }
  }
  win.Shiny = {
    InputBinding,
    inputBindings: { register: (b, name) => { bindings[name] = b; } }
  };
  win.jQuery = (scope) => ({ find: (sel) => Array.from(scope.querySelectorAll(sel)) });
  win.eval(shinyJs);
  return { win, bindings };
};

/* Put R's markup on the page and bind it as Shiny would. Returns the bound
 * element and a count of the callbacks it made. */
const bind = (ctx, name, html) => {
  const { win, bindings } = ctx;
  const host = win.document.createElement('div');
  host.innerHTML = html;
  win.document.body.appendChild(host);
  const b = bindings['blockr.ui.' + name];
  const [el] = b.find(host);
  assert.ok(el, `${name}: the binding finds its element`);
  b.initialize(el);
  const calls = { n: 0 };
  b.subscribe(el, () => { calls.n++; });
  return { el, b, calls, value: () => JSON.parse(JSON.stringify(b.getValue(el))) };
};

const click = (win, el) => el.dispatchEvent(new win.MouseEvent('click', { bubbles: true }));
const key = (win, el, k) =>
  el.dispatchEvent(new win.KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));

test('shiny: the gear registers a binding', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  assert.deepStrictEqual(Object.keys(ctx.bindings).sort(), ['blockr.ui.gear']);
  ctx.win.close();
});

test('selectize: an open dropdown is a layer, so Escape closes it first', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const host = win.document.createElement('div');
  host.innerHTML = `<select id="sep" class="selectized"></select>
    <div class="selectize-control"><div class="selectize-input"><input type="text"></div>
      <div class="selectize-dropdown"></div></div>`;
  win.document.body.appendChild(host);

  // A stand-in for the selectize instance Shiny puts on the <select>.
  const handlers = {};
  let closed = 0;
  const control = host.querySelector('.selectize-control');
  win.document.getElementById('sep').selectize = {
    $wrapper: [control],
    $dropdown: [control.querySelector('.selectize-dropdown')],
    on: (name, fn) => { handlers[name] = fn; },
    close: () => { closed++; handlers.dropdown_close(); }
  };

  const input = control.querySelector('input');
  input.dispatchEvent(new win.Event('focusin', { bubbles: true }));
  assert.ok(handlers.dropdown_open, 'hooked when the control takes the focus');

  const before = win.Blockr.layer.count();
  handlers.dropdown_open();
  assert.strictEqual(win.Blockr.layer.count(), before + 1);
  key(win, input, 'Escape');
  assert.strictEqual(closed, 1, 'Escape closes the dropdown');
  assert.strictEqual(win.Blockr.layer.count(), before);
  win.close();
});

test('gear: opens its tray, reports the state, and stays open when drawn again', async (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const markup = `<div class="blockr-gear-header"><button id="g" type="button"
      class="blockr-gear-btn blockr-ui-gear" aria-controls="g_tray"
      aria-expanded="false"></button></div>
    <div id="g_tray" class="blockr-settings blockr-settings--beak" aria-label="Read settings"></div>`;
  const g = bind(ctx, 'gear', markup);
  const tray = () => win.document.getElementById('g_tray');
  const tick = () => new Promise((r) => win.setTimeout(r, 0));

  assert.strictEqual(tray().getAttribute('aria-label'), 'Read settings');
  assert.strictEqual(g.value(), false);

  click(win, g.el);
  await tick();
  assert.strictEqual(g.value(), true);
  assert.ok(tray().classList.contains('blockr-settings--open'));
  assert.ok(g.el.classList.contains('blockr-gear-active'));
  assert.strictEqual(g.calls.n, 1);

  // The block draws its UI again: the new gear starts with the tray open,
  // without the slide.
  g.el.parentElement.parentElement.remove();
  const again = bind(ctx, 'gear', markup);
  assert.strictEqual(again.value(), true);
  assert.ok(tray().classList.contains('blockr-settings--open'));

  key(win, again.el, 'Escape');
  await tick();
  assert.strictEqual(again.value(), false);
  assert.strictEqual(again.calls.n, 1);
  win.close();
});
