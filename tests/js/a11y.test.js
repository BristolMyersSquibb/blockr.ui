/* Accessibility, checked by axe-core on every control in the states a user
 * meets: closed and open, with picks, disabled, dirty, the gear tray open, a
 * tooltip showing.
 *
 * Only axe's WCAG rules run. Colour contrast needs a layout, which happy-dom
 * does not have, so contrast.test.js checks it in Chrome; landmark rules are
 * about the host page, not a control.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const { test } = require('./select-impls');

const axe = fs.readFileSync(require.resolve('axe-core'), 'utf8');

const OPTS = [{ value: 'AGE', label: 'Age' }, { value: 'SEX', label: 'Sex' }, 'RACE'];

const page = (win) => {
  const doc = win.document;
  const B = win.Blockr;
  const host = () => {
    const el = doc.createElement('div');
    doc.body.appendChild(el);
    return el;
  };
  doc.documentElement.lang = 'en';
  doc.title = 'Controls';

  B.Select.single(host(), { options: OPTS, selected: 'AGE', label: 'Column', bordered: true });
  B.Select.single(host(), { options: OPTS, selected: 'SEX', label: 'Column', disabled: true });
  B.Select.multi(host(), { options: OPTS, selected: ['AGE', 'SEX'], label: 'Columns' });
  const open = B.Select.single(host(), { options: OPTS, selected: 'AGE', label: 'Group by' });
  open.el.querySelector('.blockr-select__control').click();
  B.Select.menu(host(), { options: OPTS, mode: 'multi', selected: ['AGE'], title: 'Columns' });

  const input = doc.createElement('input');
  input.setAttribute('aria-label', 'Title');
  host().appendChild(input);
  B.textCommit(input, { onCommit: () => {} });
  input.value = 'Adverse events';
  input.dispatchEvent(new win.Event('input', { bubbles: true }));

  host().appendChild(B.checkbox('Show totals', true, () => {}).el);
  host().appendChild(B.segmented(
    [{ value: 'asc', label: '', title: 'Ascending' }, { value: 'desc', label: 'Desc', title: 'Descending' }],
    'asc', () => {}, { label: 'Sort direction' }
  ).el);

  const gear = doc.createElement('button');
  const band = doc.createElement('div');
  host().append(gear, band);
  B.gearTray(band, gear).set(true);
  gear.focus();
};

test('every control passes axe in the states a user meets', async (newWindow) => {
  const win = newWindow();
  page(win);
  assert.ok(win.document.querySelector('.blockr-tooltip'), 'the gear\'s card is showing');
  win.eval(axe);
  const { violations } = await win.axe.run(win.document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa'] },
    rules: { 'color-contrast': { enabled: false } }
  });
  assert.deepStrictEqual(
    Array.from(violations, (v) => `${v.id}: ${v.nodes.map((n) => n.target.join(' ')).join(', ')}`),
    []
  );
  win.close();
});

test('the menus pass axe while open, with a head, a filter and every kind of row', async (newWindow) => {
  const win = newWindow();
  const doc = win.document;
  doc.documentElement.lang = 'en';
  doc.title = 'Menus';

  // The markup action_menu() renders in R, opened from its trigger.
  const host = doc.createElement('div');
  host.innerHTML = `
    <span class="blockr-action-menu" data-align="end">
      <button class="blockr-tool blockr-action-menu__trigger" type="button" aria-label="Download"
              aria-haspopup="menu" aria-expanded="false" data-blockr-tooltip="Download">D</button>
      <div class="blockr-menu" role="menu" tabindex="-1" hidden>
        <div class="blockr-menu__title" role="presentation">This patient</div>
        <a class="blockr-menu__item" role="menuitem" tabindex="-1" href="#">PowerPoint</a>
        <a class="blockr-menu__item blockr-menu__item--disabled" role="menuitem" tabindex="-1"
           aria-disabled="true" href="#">Excel</a>
        <div class="blockr-menu__divider" role="separator"></div>
        <button class="blockr-menu__item blockr-menu__item--danger" role="menuitem" tabindex="-1"
                type="button">Remove</button>
      </div>
    </span>`;
  doc.body.appendChild(host);
  host.querySelector('.blockr-action-menu__trigger')
    .dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true, detail: 1 }));

  // Blockr.menu with every part a caller can ask for.
  const views = doc.createElement('button');
  views.textContent = 'Views';
  doc.body.appendChild(views);
  win.Blockr.menu(views, {
    head: { title: 'Filter rows', badge: 'blockr.dplyr', text: 'Keeps the rows that match.' },
    caption: 'Append to Dataset',
    filter: true,
    items: [
      { title: 'Views' },
      { label: 'Page 1', current: true },
      { label: 'Page 2' },
      { divider: true },
      { label: 'Show code', checked: true },
      { label: 'Export', disabled: true, reason: 'Nothing to export' },
      { gap: true },
      { label: 'Remove', icon: 'trash', danger: true }
    ]
  });
  doc.querySelector('.blockr-menu__filter-input')
    .dispatchEvent(new win.KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true }));

  assert.strictEqual(doc.querySelectorAll('body > .blockr-menu').length, 2, 'both open');
  win.eval(axe);
  const { violations } = await win.axe.run(win.document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa'] },
    rules: { 'color-contrast': { enabled: false } }
  });
  assert.deepStrictEqual(
    Array.from(violations, (v) => `${v.id}: ${v.nodes.map((n) => n.target.join(' ')).join(', ')}`),
    []
  );
  win.close();
});

test('the code field passes axe with its list open', async (newWindow) => {
  const win = newWindow();
  win.eval(fs.readFileSync(require.resolve('../../inst/assets/js/blockr-input.js'), 'utf8'));
  const doc = win.document;
  doc.documentElement.lang = 'en';
  doc.title = 'Code field';
  const host = doc.createElement('div');
  doc.body.appendChild(host);
  const h = win.Blockr.Input.create(host, {
    columns: ['AGE', 'AGEGR1'], categories: { math: ['abs'] }, label: 'Expression'
  });
  const field = h.el.querySelector('input');
  field.focus();
  field.value = 'AG';
  field.setSelectionRange(2, 2);
  field.dispatchEvent(new win.Event('input', { bubbles: true }));
  assert.ok(h.el.classList.contains('blockr-input--popup-open'), 'the list is open');
  win.eval(axe);
  const { violations } = await win.axe.run(doc, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa'] },
    rules: { 'color-contrast': { enabled: false } }
  });
  assert.deepStrictEqual(
    Array.from(violations, (v) => `${v.id}: ${v.nodes.map((n) => n.target.join(' ')).join(', ')}`),
    []
  );
  win.close();
});

