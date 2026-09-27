/* The Shiny input bindings of blockr-inputs.js, driven the way Shiny drives
 * them: find the element, initialize, subscribe, read the value, apply a
 * server update. Shiny is a stub that records what the bindings register
 * and forget; the markup is what the R functions write.
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
  const forgotten = [];
  class InputBinding {
    getId(el) { return el.getAttribute('data-input-id') || el.id; }
  }
  win.Shiny = {
    InputBinding,
    inputBindings: { register: (b, name) => { bindings[name] = b; } },
    forgetLastInputValue: (id) => forgotten.push(id)
  };
  win.jQuery = (scope) => ({ find: (sel) => Array.from(scope.querySelectorAll(sel)) });
  win.eval(inputsJs);
  return { win, bindings, forgotten };
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
const typeIn = (win, input, text) => {
  input.value = text;
  input.dispatchEvent(new win.Event('input', { bubbles: true }));
};

const esc = (x) => JSON.stringify(x).replace(/"/g, '&quot;');

test('inputs: every control registers a binding', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  assert.deepStrictEqual(Object.keys(ctx.bindings).sort(), [
    'blockr.ui.checkbox', 'blockr.ui.gear', 'blockr.ui.number',
    'blockr.ui.segmented', 'blockr.ui.select', 'blockr.ui.text'
  ]);
  ctx.win.close();
});

test('select: mounts a bordered Blockr.Select and reports picks, not pushes', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const s = bind(ctx, 'select',
    `<div id="col" class="blockr-ui-select" data-multiple="false"
      data-options="${esc(['a', { value: 'b', label: 'Bee' }, 'c'])}"
      data-selected="${esc(['b'])}"></div>`);

  assert.ok(s.el.querySelector('.blockr-select--bordered'), 'bordered');
  assert.strictEqual(s.b.getType(s.el), 'blockr.ui.select');
  assert.strictEqual(s.value(), 'b');

  // A pick is reported.
  click(win, s.el.querySelector('.blockr-select__control'));
  const row = [...win.document.querySelectorAll('.blockr-select__option')]
    .find((r) => r.getAttribute('data-value') === 'c');
  click(win, row);
  assert.strictEqual(s.value(), 'c');
  assert.strictEqual(s.calls.n, 1);

  // A push from the server is shown, not reported, and Shiny forgets the
  // last value it sent.
  s.b.receiveMessage(s.el, { choices: ['x', 'y'], selected: ['y'] });
  assert.strictEqual(s.value(), 'y');
  assert.strictEqual(s.calls.n, 1);
  assert.deepStrictEqual(ctx.forgotten, ['col']);

  // New choices alone keep the pick while the list has it; one choice may
  // arrive unboxed.
  s.b.receiveMessage(s.el, { choices: ['y', 'z'] });
  assert.strictEqual(s.value(), 'y');
  s.b.receiveMessage(s.el, { choices: 'z' });
  assert.strictEqual(s.value(), 'z');
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
  assert.strictEqual(multi.calls.n, 0);
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

test('text: the value changes on Enter or blur only, and Escape reverts', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const t = bind(ctx, 'text',
    `<label id="name-label" for="name">Name</label>
     <div class="blockr-commit-field"><input id="name" type="text"
       class="blockr-text-input blockr-ui-text" value="a.csv"></div>`);

  assert.strictEqual(t.value(), 'a.csv');
  typeIn(win, t.el, 'b.csv');
  assert.strictEqual(t.value(), 'a.csv', 'typing reports nothing');
  assert.strictEqual(t.calls.n, 0);
  assert.match(t.el.nextSibling.textContent, /Enter/);

  key(win, t.el, 'Enter');
  assert.strictEqual(t.value(), 'b.csv');
  assert.strictEqual(t.calls.n, 1);

  typeIn(win, t.el, 'c.csv');
  key(win, t.el, 'Escape');
  assert.strictEqual(t.el.value, 'b.csv', 'Escape reverts the field');
  assert.strictEqual(t.calls.n, 1);

  typeIn(win, t.el, 'd.csv');
  t.el.dispatchEvent(new win.Event('blur'));
  assert.strictEqual(t.value(), 'd.csv');
  assert.strictEqual(t.calls.n, 2);

  t.b.receiveMessage(t.el, { value: 'e.csv', label: 'File', placeholder: 'x' });
  assert.strictEqual(t.value(), 'e.csv');
  assert.strictEqual(t.el.value, 'e.csv');
  assert.strictEqual(t.el.placeholder, 'x');
  assert.strictEqual(win.document.getElementById('name-label').textContent, 'File');
  assert.strictEqual(t.calls.n, 2, 'a push is not reported');
  assert.deepStrictEqual(ctx.forgotten, ['name']);
  win.close();
});

test('number: reports a number, null when empty, as shiny.number', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const n = bind(ctx, 'number',
    `<div class="blockr-commit-field"><input id="n" type="number"
       class="blockr-text-input blockr-ui-number" value="10"></div>`);

  assert.strictEqual(n.b.getType(n.el), 'shiny.number');
  assert.strictEqual(n.value(), 10);
  typeIn(win, n.el, '2.5');
  key(win, n.el, 'Enter');
  assert.strictEqual(n.value(), 2.5);
  typeIn(win, n.el, '');
  key(win, n.el, 'Enter');
  assert.strictEqual(n.value(), null);
  n.b.receiveMessage(n.el, { value: '7' });
  assert.strictEqual(n.value(), 7);
  assert.strictEqual(n.calls.n, 2);
  win.close();
});

test('checkbox: reports on change; a push sets it and its label', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const c = bind(ctx, 'checkbox',
    `<label class="blockr-checkbox"><input id="h" type="checkbox"
       class="blockr-ui-checkbox" checked><span class="blockr-checkbox__box"></span>
       <span class="blockr-checkbox__label">Header</span></label>`);

  assert.strictEqual(c.value(), true);
  click(win, c.el);
  assert.strictEqual(c.value(), false);
  assert.strictEqual(c.calls.n, 1);
  c.b.receiveMessage(c.el, { value: true, label: 'First row is a header' });
  assert.strictEqual(c.value(), true);
  assert.strictEqual(c.el.parentElement.querySelector('.blockr-checkbox__label').textContent,
    'First row is a header');
  assert.strictEqual(c.calls.n, 1);
  win.close();
});

test('segmented: mounts Blockr.segmented and reports the segment clicked', (newWindow) => {
  const ctx = shinyWindow(newWindow);
  const { win } = ctx;
  const s = bind(ctx, 'segmented',
    `<label id="from-label">From</label><div id="from" class="blockr-ui-segmented"
      data-choices="${esc([{ value: 'head', label: 'First' }, { value: 'tail', label: 'Last' }])}"
      data-selected="${esc('head')}" data-size="xs"></div>`);

  const wrap = s.el.querySelector('.blockr-segmented');
  assert.ok(wrap.classList.contains('blockr-segmented--xs'));
  assert.strictEqual(wrap.getAttribute('aria-label'), 'From');
  assert.strictEqual(s.value(), 'head');
  click(win, wrap.querySelectorAll('.blockr-segmented__seg')[1]);
  assert.strictEqual(s.value(), 'tail');
  assert.strictEqual(s.calls.n, 1);
  s.b.receiveMessage(s.el, { selected: 'head' });
  assert.strictEqual(s.value(), 'head');
  assert.strictEqual(s.calls.n, 1);
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
