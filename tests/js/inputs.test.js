/* The Shiny input bindings of blockr-inputs.js, driven the way Shiny drives
 * them: find the element, initialize, subscribe, read the value, apply a
 * server update. Shiny is a stub that records what the bindings
 * register; the markup is what the R functions write.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { test } = require('./select-impls');

const inputsJs = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'blockr-inputs.js'), 'utf8');

/* A window with Blockr, a Shiny stub and blockr-inputs.js. */
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
  win.eval(inputsJs);
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

const esc = (x) => JSON.stringify(x).replace(/"/g, '&quot;');

test('inputs: every control registers a binding', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  assert.deepStrictEqual(Object.keys(ctx.bindings).sort(), [
    'blockr.ui.gear', 'blockr.ui.select'
  ]);
  ctx.win.close();
});

test('select: mounts a bordered Blockr.Select and reports picks and pushes', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const s = bind(ctx, 'select',
    `<label id="col-label" class="blockr-label">Column</label>
    <div id="col" class="blockr-ui-select" data-multiple="false"
      data-options="${esc(['a', { value: 'b', label: 'Bee' }, 'c'])}"
      data-selected="${esc(['b'])}"></div>`);
  const name = () => s.el.querySelector('[role="combobox"]').getAttribute('aria-label');

  assert.ok(s.el.querySelector('.blockr-select--bordered'), 'bordered');
  assert.strictEqual(name(), 'Column', 'the combobox is named by its label');
  assert.strictEqual(s.b.getType(s.el), 'blockr.ui.select');
  assert.strictEqual(s.value(), 'b');

  // A pick is reported.
  click(win, s.el.querySelector('.blockr-select__control'));
  const row = [...win.document.querySelectorAll('.blockr-select__option')]
    .find((r) => r.getAttribute('data-value') === 'c');
  click(win, row);
  assert.strictEqual(s.value(), 'c');
  assert.strictEqual(s.calls.n, 1);

  // A push from the server is shown and sent back, as Shiny's own inputs
  // send theirs.
  s.b.receiveMessage(s.el, { choices: ['x', 'y'], selected: ['y'] });
  assert.strictEqual(s.value(), 'y');
  assert.strictEqual(s.calls.n, 2);

  // New choices alone keep the pick while the list has it; without it the
  // select falls back to the first choice, and that is sent back too. One
  // choice may arrive unboxed.
  s.b.receiveMessage(s.el, { choices: ['y', 'z'] });
  assert.strictEqual(s.value(), 'y');
  s.b.receiveMessage(s.el, { choices: 'z' });
  assert.strictEqual(s.value(), 'z');
  assert.strictEqual(s.calls.n, 4);

  // A new label renames the field and the combobox.
  s.b.receiveMessage(s.el, { label: 'Columns' });
  assert.strictEqual(win.document.getElementById('col-label').textContent, 'Columns');
  assert.strictEqual(name(), 'Columns');
  win.close();
});

test('select: a placeholder holds until a pick; multi reports an array', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const single = bind(ctx, 'select',
    `<div id="one" class="blockr-ui-select" data-multiple="false"
      data-options="${esc(['a', 'b'])}" data-selected="[]" data-placeholder="Pick"></div>`);
  assert.strictEqual(single.value(), '');

  const plain = bind(ctx, 'select',
    `<div id="two" class="blockr-ui-select" data-multiple="false"
      data-options="${esc(['a', 'b'])}" data-selected="[]"></div>`);
  assert.strictEqual(plain.value(), 'a', 'no placeholder: the first choice');

  const multi = bind(ctx, 'select',
    `<div id="cols" class="blockr-ui-select" data-multiple="true"
      data-options="${esc(['a', 'b', 'c'])}" data-selected="${esc(['c', 'a'])}"></div>`);
  assert.deepStrictEqual(multi.value(), ['c', 'a']);
  multi.b.receiveMessage(multi.el, { selected: 'b' });
  assert.deepStrictEqual(multi.value(), ['b']);
  multi.b.receiveMessage(multi.el, { selected: [] });
  assert.deepStrictEqual(multi.value(), []);
  assert.strictEqual(multi.calls.n, 2);
  ctx.win.close();
});

test('select: binding twice keeps one control', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const s = bind(ctx, 'select',
    `<div id="col" class="blockr-ui-select" data-options="${esc(['a'])}"></div>`);
  s.b.initialize(s.el);
  assert.strictEqual(s.el.querySelectorAll('.blockr-select').length, 1);
  ctx.win.close();
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

  assert.ok(g.el.querySelector('svg'), 'the gear icon');
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
