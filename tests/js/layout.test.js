/* Where the controls land, checked in Chrome (browser.js): the placement,
 * sizes, stacking and scrolling that happy-dom cannot lay out. The first two
 * were bugs found by hand in a browser.
 */
'use strict';

const assert = require('node:assert');
const path = require('node:path');
const { test } = require('./browser');

/** Within half a pixel: a rect is in layout units, a style in CSS pixels. */
const near = (a, b) => Math.abs(a - b) < 0.5;

test('the card naming a cut-off row draws above the open list', async (page) => {
  await page.evaluate(() => {
    const host = document.createElement('div');
    host.style.width = '160px';
    document.body.appendChild(host);
    const options = Array.from({ length: 12 }, (_, i) =>
      ({ value: `COLUMN_${i}_WITH_A_LONG_NAME`, label: `The label of column ${i}` }));
    const select = Blockr.Select.single(host, { options, label: 'Column' });
    select.el.querySelector('.blockr-select__control').click();
  });
  // A row halfway down: the card, drawn above the row, lies over the rows
  // above it.
  await page.locator('.blockr-select__option').nth(6).hover();
  const card = page.locator('.blockr-tooltip');
  await card.waitFor();
  const seen = await card.evaluate((el) => {
    const r = el.getBoundingClientRect();
    const hit = () => document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
    // The card lets the pointer through, so the point first finds what lies
    // under it. Taking the pointer back changes what the point hits, not
    // what is drawn on top.
    const under = hit()?.closest('.blockr-select__dropdown') != null;
    el.style.pointerEvents = 'auto';
    const drawn = el.contains(hit());
    el.style.pointerEvents = '';
    return { under, drawn };
  });
  assert.deepStrictEqual(seen, { under: true, drawn: true });
});

test('a multi menu keeps rows in view under 30 picks', async (page) => {
  const inView = await page.evaluate(() => {
    const word = document.createElement('button');
    word.textContent = 'columns';
    document.body.appendChild(word);
    const options = Array.from({ length: 40 }, (_, i) => `COLUMN_${i}`);
    Blockr.Select.menu(word, { mode: 'multi', options, selected: options.slice(0, 30), title: 'Columns' });
    const panel = document.querySelector('body > .blockr-select__dropdown');
    // Between the sticky head, which holds the tags, and the bottom border.
    const top = panel.querySelector('.blockr-select__head').getBoundingClientRect().bottom;
    const bottom = panel.getBoundingClientRect().top + panel.clientTop + panel.clientHeight;
    return Array.from(panel.querySelectorAll('.blockr-select__option'), (row) => row.getBoundingClientRect())
      .filter((r) => r.top >= top && r.bottom <= bottom).length;
  });
  assert.ok(inView >= 3, `${inView} whole rows in view`);
});

test('a dropdown stays 8px inside the window wherever its control sits, above it where there is no room below', async (page) => {
  const placements = await page.evaluate(() => {
    const host = document.createElement('div');
    host.style.cssText = 'position: fixed; width: 120px';
    document.body.appendChild(host);
    const width = document.documentElement.clientWidth;
    const out = [];
    // Down both edges of the window, a pixel at a time.
    for (const left of [0, width - 120]) {
      for (let top = 0; top <= window.innerHeight - 30; top++) {
        host.style.left = `${left}px`;
        host.style.top = `${top}px`;
        const select = Blockr.Select.single(host, { options: ['AGE', 'SEX', 'RACE', 'ARM', 'SITE', 'COUNTRY'] });
        const control = select.el.querySelector('.blockr-select__control');
        control.click();
        const c = control.getBoundingClientRect();
        const l = document.querySelector('body > .blockr-select__dropdown').getBoundingClientRect();
        out.push({ control: [c.left, c.top, c.bottom], list: [l.left, l.top, l.right, l.bottom] });
        select.destroy();
      }
    }
    host.remove();
    return { placements: out, width, height: window.innerHeight };
  });
  const { width, height } = placements;
  const wrong = placements.placements.filter(({ control: [, cTop, cBottom], list: [left, top, right, bottom] }) => {
    const fitsBelow = cBottom + 4 + (bottom - top) + 8 <= height;
    const inside = left >= 8 && right <= width - 8 && top >= 8 && bottom <= height - 8;
    const hangs = fitsBelow ? near(top, cBottom + 4) : near(bottom, cTop - 4);
    return !(inside && hangs);
  });
  // The window is tall enough for the list to fit above or below the
  // control at every height, so every placement can keep the margin.
  assert.deepStrictEqual(wrong.slice(0, 3), [], `${wrong.length} of ${placements.placements.length} placements`);
}, { viewport: { width: 400, height: 600 } });

test('a checkbox draws its check from Blockr.icons.confirm, at 10px', async (page) => {
  const check = await page.evaluate(() => {
    const { el } = Blockr.checkbox('Show totals', true, () => {});
    document.body.appendChild(el);
    const svg = el.querySelector('.blockr-checkbox__box svg');
    const icon = document.createElement('template');
    icon.innerHTML = Blockr.icons.confirm;
    const r = svg.getBoundingClientRect();
    return { confirm: svg.isEqualNode(icon.content.firstChild), size: [r.width, r.height] };
  });
  // The icon is 14px of its own: the stylesheet sets the 10px.
  assert.deepStrictEqual(check, { confirm: true, size: [10, 10] });
});

test("Shiny's inputs take the design system's sizes in the tray", async (page) => {
  const share = await page.evaluate(() => {
    const field = (inner) => '<div class="blockr-settings__field">' +
      `<div class="form-group shiny-input-container">${inner}</div></div>`;
    const band = document.createElement('div');
    band.className = 'blockr-settings blockr-settings--open';
    band.style.width = '640px';
    band.innerHTML = '<div class="blockr-settings__grid">' +
      field('<input type="number">') +
      field('<div class="checkbox"><label><input type="checkbox"><span>On</span></label></div>') +
      field('<input type="text">') +
      field('<select multiple></select>') +
      '</div>';
    document.body.appendChild(band);
    const grid = band.querySelector('.blockr-settings__grid').getBoundingClientRect().width;
    return [...band.querySelectorAll('.blockr-settings__field')]
      .map((f) => f.getBoundingClientRect().width / grid);
  });
  // Four columns at this width: a number and a checkbox take one, a text
  // field two, a multi select the whole row.
  const [number, checkbox, text, multi] = share;
  assert.ok(number < 0.3 && checkbox < 0.3, `one column: ${number}, ${checkbox}`);
  assert.ok(text > 0.45 && text < 0.55, `two columns: ${text}`);
  assert.ok(Math.abs(multi - 1) < 0.01, `the whole row: ${multi}`);
});

test("a section's checkbox title stays a section title under the theme layer", async (page) => {
  // The theme layer draws a checkbox's words at 14px in text-default. Loaded
  // after the tray's sheet, as a page may load it, it must not reach a title.
  await page.addStyleTag({ path: path.join(__dirname, '..', '..', 'inst', 'assets', 'css', 'blockr-theme.css') });
  const look = await page.evaluate(() => {
    const band = document.createElement('div');
    band.className = 'blockr-settings blockr-settings--open';
    band.innerHTML = '<div class="blockr-settings__title">Format</div>' +
      '<div class="blockr-settings__grid"></div>' +
      '<div class="blockr-settings__title blockr-settings__title--toggle">' +
      '<div class="form-group shiny-input-container"><div class="checkbox"><label>' +
      '<input type="checkbox" checked><span>Skip rows</span></label></div></div></div>';
    document.body.appendChild(band);
    const font = (el) => {
      const s = getComputedStyle(el);
      return [s.fontSize, s.fontWeight, s.color, s.textTransform, s.letterSpacing];
    };
    return {
      toggle: font(band.querySelector('.blockr-settings__title--toggle span')),
      title: font(band.querySelector('.blockr-settings__title')),
    };
  });
  assert.deepStrictEqual(look.toggle, look.title);
});
