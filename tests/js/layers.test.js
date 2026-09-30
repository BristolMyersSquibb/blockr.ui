/* The dismiss stack, Blockr.layer: what Escape and a click outside close.
 *
 * The rules are tested here once rather than control by control: Escape
 * acts on the top layer only, a pointerdown closes the layers above the one
 * it lands in, a layer in the page takes Escape only from inside it, and a
 * layer takes Escape before the page's own handlers do. Then, for each pair
 * of layers that can nest, one Escape closes exactly one of them.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const nodeTest = require('node:test');
const { newWindow } = require('./select-impls');

const inputJs = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'blockr-input.js'), 'utf8');

/** Register `name`; `fn` receives a window with every control loaded. */
const test = (name, fn) => nodeTest(name, async () => {
  const win = newWindow();
  win.eval(inputJs);
  try { await fn(win); } finally { win.close(); }
});

const wait = (ms) => new Promise((r) => setTimeout(r, ms));

const add = (win, tag, parent) => {
  const el = win.document.createElement(tag || 'div');
  (parent || win.document.body).appendChild(el);
  return el;
};

const press = (win, target, key) => {
  const e = new win.KeyboardEvent('keydown', { key, bubbles: true, cancelable: true });
  target.dispatchEvent(e);
  return e;
};

/* A click the way a browser delivers it: the pointerdown, then the click. */
const click = (win, el) => {
  el.dispatchEvent(new win.PointerEvent('pointerdown', { bubbles: true }));
  el.dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true, detail: 1 }));
};

const type = (win, field, text) => {
  field.value += text;
  field.dispatchEvent(new win.Event('input', { bubbles: true }));
};

/* A dialog that closes on Escape the way Bootstrap's modal does: with a
 * keydown listener on the dialog itself. */
const modal = (win) => {
  const dialog = add(win);
  let open = true;
  dialog.addEventListener('keydown', (e) => { if (e.key === 'Escape') open = false; });
  return { host: dialog, isOpen: () => open };
};

const tray = (win) => {
  const gear = add(win, 'button');
  const band = add(win);
  const t = win.Blockr.gearTray(band, gear);
  t.set(true);
  return { host: band, gear, isOpen: () => t.isOpen() };
};

/* The markup action_menu() renders, without the trigger's tooltip. */
const actionMenuIn = (win, host) => {
  const wrap = add(win, 'span', host);
  wrap.className = 'blockr-action-menu';
  wrap.innerHTML = `
    <button class="blockr-action-menu__trigger" type="button">D</button>
    <div class="blockr-menu" role="menu" tabindex="-1" hidden>
      <button class="blockr-menu__item" role="menuitem" tabindex="-1" type="button">Rename</button>
    </div>`;
  return wrap;
};

/* --- The rules ------------------------------------------------------------ */

test('Escape closes the top layer only, and the next press the one below', (win) => {
  const closed = [];
  for (const name of ['a', 'b', 'c']) {
    win.Blockr.layer(add(win), { escape: () => closed.push(name) });
  }
  let reached = 0;
  win.document.addEventListener('keydown', () => { reached++; });
  const first = press(win, win.document.body, 'Escape');
  assert.deepStrictEqual(closed, ['c']);
  assert.ok(first.defaultPrevented);
  assert.strictEqual(reached, 0, 'the page does not see it');
  press(win, win.document.body, 'Escape');
  press(win, win.document.body, 'Escape');
  assert.deepStrictEqual(closed, ['c', 'b', 'a']);
  const last = press(win, win.document.body, 'Escape');
  assert.ok(!last.defaultPrevented, 'with nothing open, Escape is the page\'s');
  assert.strictEqual(reached, 1);
});

test('a pointerdown closes the layers above the one it lands in', (win) => {
  const closed = [];
  const layer = (name, opts) => {
    const el = add(win);
    win.Blockr.layer(el, Object.assign({ outside: () => closed.push(name) }, opts));
    return el;
  };
  const trigger = add(win, 'button');
  const stays = add(win);
  win.Blockr.layer(stays, { escape: () => {} });
  layer('a');
  const b = layer('b', { from: trigger });
  layer('c');
  click(win, b);
  assert.deepStrictEqual(closed, ['c'], 'b and the layers under it stay');
  click(win, trigger);
  assert.deepStrictEqual(closed, ['c'], 'what opened b counts as inside b');
  click(win, win.document.body);
  assert.deepStrictEqual(closed, ['c', 'b', 'a'], 'outside them all, from the top down');
  assert.strictEqual(win.Blockr.layer.count(), 1, 'a layer without `outside` stays open');
});

test('a layer in the page takes Escape only from inside it', (win) => {
  const trays = [0, 1].map(() => {
    const gear = add(win, 'button');
    const band = add(win);
    const field = add(win, 'input', band);
    const t = win.Blockr.gearTray(band, gear);
    // A tray closed by Escape hands the focus to its gear, whose tooltip
    // would then be the top layer; that pair is tested below.
    win.Blockr.tooltip.clear(gear);
    t.set(true);
    return { gear, field, t };
  });
  const [one, two] = trays;
  press(win, one.field, 'Escape');
  assert.ok(!one.t.isOpen(), 'the tray the key was pressed in closes');
  assert.ok(two.t.isOpen(), 'though the other opened later');
  const e = press(win, add(win, 'input'), 'Escape');
  assert.ok(two.t.isOpen(), 'a key pressed outside every tray leaves them');
  assert.ok(!e.defaultPrevented, 'and goes to the page');
  press(win, two.gear, 'Escape');
  assert.ok(!two.t.isOpen(), 'the gear counts as inside its tray');
});

test('a layer whose elements have left the page is dropped', (win) => {
  const block = add(win);
  const gear = add(win, 'button', block);
  const band = add(win, 'div', block);
  win.Blockr.gearTray(band, gear).set(true);
  assert.strictEqual(win.Blockr.layer.count(), 1);
  block.remove();
  assert.strictEqual(win.Blockr.layer.count(), 0, 'the stack lets go of the removed block');
});

test('no control listens on the page for Escape or a click of its own', (win) => {
  const added = new Set();
  for (const target of [win.document, win]) {
    const on = target.addEventListener.bind(target);
    target.addEventListener = (kind, fn, opts) => { added.add(kind); on(kind, fn, opts); };
  }
  const t = tray(win);
  // Closed by Escape, the tray focuses its gear, which would show this.
  win.Blockr.tooltip.clear(t.gear);
  const sel = win.Blockr.Select.single(t.host, { options: ['AGE', 'SEX'] });
  click(win, sel.el.querySelector('.blockr-select__control'));
  press(win, sel.el.querySelector('.blockr-select__search'), 'Escape');
  const code = win.Blockr.Input.create(t.host, { columns: ['AGE'] });
  type(win, code.el.querySelector('input'), 'A');
  press(win, code.el.querySelector('input'), 'Escape');
  const field = add(win, 'input', t.host);
  win.Blockr.textCommit(field, { onCommit: () => {} });
  type(win, field, 'x');
  press(win, field, 'Escape');
  win.Blockr.menu(add(win, 'button'), { items: [{ label: 'One' }] });
  press(win, win.document.querySelector('.blockr-menu__list'), 'Escape');
  const wrap = actionMenuIn(win, t.host);
  click(win, wrap.querySelector('.blockr-action-menu__trigger'));
  press(win, win.document.activeElement, 'Escape');
  const tip = add(win, 'button');
  win.Blockr.tooltip.set(tip, 'Settings');
  tip.focus();
  press(win, tip, 'Escape');
  press(win, t.gear, 'Escape');
  // Blockr.place follows scroll and resize while a panel is open, and takes
  // those listeners off again when it closes.
  assert.deepStrictEqual([...added].filter((k) => k !== 'scroll' && k !== 'resize'), []);
  assert.strictEqual(win.Blockr.layer.count(), 0, 'and every layer came off');
});

/* --- One Escape, one layer ------------------------------------------------ */

/* Each inner layer is built in its outer one's element and opened; it
 * returns what holds the focus while it is open, and whether it is. */
const INNER = {
  'a Select': (win, host) => {
    const sel = win.Blockr.Select.single(host, { options: ['AGE', 'SEX'] });
    click(win, sel.el.querySelector('.blockr-select__control'));
    return { isOpen: () => sel.el.classList.contains('blockr-select--open') };
  },
  'code completions': (win, host) => {
    const h = win.Blockr.Input.create(host, { columns: ['AGE', 'AGEGR1'] });
    const field = h.el.querySelector('input');
    field.focus();
    type(win, field, 'AG');
    return { isOpen: () => h.el.classList.contains('blockr-input--popup-open') };
  },
  'a dirty field': (win, host) => {
    const field = add(win, 'input', host);
    field.value = 'AGE';
    win.Blockr.textCommit(field, { onCommit: () => {} });
    field.focus();
    type(win, field, 'X');
    return { isOpen: () => field.value !== 'AGE' };
  },
  'a menu': (win, host) => {
    const trigger = add(win, 'button', host);
    win.Blockr.menu(trigger, { items: [{ label: 'Rename' }, { label: 'Remove' }] });
    return { isOpen: () => !!win.document.querySelector('.blockr-menu') };
  },
  'an action menu': (win, host) => {
    const wrap = actionMenuIn(win, host);
    click(win, wrap.querySelector('.blockr-action-menu__trigger'));
    const panel = win.document.body.querySelector(':scope > .blockr-menu');
    return { isOpen: () => !!panel && !panel.hidden };
  }
};

const OUTER = { 'the gear tray': tray, 'a modal': modal };

for (const [outerName, outer] of Object.entries(OUTER)) {
  for (const [innerName, inner] of Object.entries(INNER)) {
    test(`one Escape closes one layer: ${innerName} in ${outerName}`, (win) => {
      const out = outer(win);
      const inn = inner(win, out.host);
      assert.ok(inn.isOpen() && out.isOpen(), 'both open');
      press(win, win.document.activeElement, 'Escape');
      assert.ok(!inn.isOpen(), `${innerName} closes`);
      assert.ok(out.isOpen(), `${outerName} stays`);
      press(win, win.document.activeElement, 'Escape');
      assert.ok(!out.isOpen(), `the next Escape closes ${outerName}`);
    });
  }
}

test('one Escape closes one layer: a tooltip over a menu', async (win) => {
  const trigger = add(win, 'button');
  win.Blockr.menu(trigger, {
    items: [{ label: 'Rename' }, { label: 'Remove', disabled: true, reason: 'The board is locked' }]
  });
  const row = win.document.querySelectorAll('.blockr-menu__item')[1];
  row.dispatchEvent(new win.Event('pointerover', { bubbles: true }));
  await wait(350);
  const card = () => win.document.querySelector('.blockr-tooltip');
  assert.ok(card(), 'the reason shows');
  press(win, win.document.activeElement, 'Escape');
  assert.ok(!card(), 'the tooltip goes');
  assert.ok(win.document.querySelector('.blockr-menu'), 'the menu stays');
  press(win, win.document.activeElement, 'Escape');
  assert.ok(!win.document.querySelector('.blockr-menu'));
});

test('one Escape closes one layer: a tooltip over the gear tray', (win) => {
  const t = tray(win);
  // Keyboard focus on the gear shows its tooltip at once, over the tray.
  t.gear.focus();
  const card = () => win.document.querySelector('.blockr-tooltip');
  assert.strictEqual(card().textContent, 'Settings');
  press(win, t.gear, 'Escape');
  assert.ok(!card(), 'the tooltip goes');
  assert.ok(t.isOpen(), 'the tray stays');
  press(win, t.gear, 'Escape');
  assert.ok(!t.isOpen());
});

/* --- In Chrome ------------------------------------------------------------ */

/* Real keys and a real pointer (browser.js), for what happy-dom only
 * simulates: the order a browser fires its events in. */
const chrome = require('./browser').test;

chrome('a real Escape closes the list, and the next one the tray around it', async (page) => {
  await page.evaluate(() => {
    const gear = document.createElement('button');
    gear.className = 'blockr-gear-btn';
    gear.innerHTML = Blockr.icons.gear;
    const band = document.createElement('div');
    band.className = 'blockr-settings';
    document.body.append(gear, band);
    Blockr.gearTray(band, gear).set(true);
    Blockr.Select.single(band, { options: ['AGE', 'SEX', 'RACE'], bordered: true, label: 'Column' });
  });
  await page.click('.blockr-select__control');
  const state = () => page.evaluate(() => ({
    list: document.querySelector('.blockr-select').classList.contains('blockr-select--open'),
    tray: document.querySelector('.blockr-gear-btn').getAttribute('aria-expanded') === 'true'
  }));
  assert.deepStrictEqual(await state(), { list: true, tray: true });
  await page.keyboard.press('Escape');
  assert.deepStrictEqual(await state(), { list: false, tray: true });
  await page.keyboard.press('Escape');
  assert.deepStrictEqual(await state(), { list: false, tray: false });
});

chrome('a real click outside closes the menu and still presses what it lands on', async (page) => {
  await page.evaluate(() => {
    const trigger = document.createElement('button');
    trigger.textContent = 'Menu';
    const other = document.createElement('button');
    other.id = 'other';
    other.textContent = 'Other';
    other.style.marginTop = '240px';
    other.addEventListener('click', () => { other.dataset.clicks = String(Number(other.dataset.clicks || 0) + 1); });
    document.body.append(trigger, other);
    Blockr.menu.bind(trigger, { items: [{ label: 'Rename' }, { label: 'Remove' }] });
    trigger.click();
  });
  assert.strictEqual(await page.locator('.blockr-menu').count(), 1);
  await page.click('#other');
  assert.strictEqual(await page.locator('.blockr-menu').count(), 0, 'the menu closed');
  assert.strictEqual(await page.getAttribute('#other', 'data-clicks'), '1', 'and the button got its click');
});
