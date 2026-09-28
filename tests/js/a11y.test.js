/* Accessibility, checked by axe-core on every control in the states a user
 * meets: closed and open, with picks, disabled, dirty, the gear tray open, a
 * tooltip showing.
 *
 * Only axe's WCAG rules run. Colour contrast needs a layout, which happy-dom
 * does not have, and landmark rules are about the host page, not a control.
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
    [{ value: 'asc', label: '', title: 'Ascending' }, { value: 'desc', label: 'Desc' }],
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
