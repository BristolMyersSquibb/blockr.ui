/* Colour contrast, checked by axe-core in Chrome (browser.js): the rule
 * a11y.test.js leaves out, since happy-dom has no layout to find the colour
 * behind a word. Every control, light and dark, in the states a user meets:
 * at rest, with picks, dirty, required and empty, in the open gear tray,
 * with its tooltip, and every list open on a keyboard row, which has the
 * look of the row under the pointer.
 *
 * Disabled controls are left out. They are text-disabled, 2.5:1 by design,
 * and WCAG's contrast minimum does not apply to an inactive control
 * (SC 1.4.3), but axe cannot tell that the pick a disabled Select shows
 * belongs to its disabled input.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./browser');

const OPTS = [{ value: 'AGE', label: 'Age' }, { value: 'SEX', label: 'Sex' }, 'RACE'];

/** @param {import('playwright-core').Page} page @param {string} selector */
const shows = async (page, selector) => {
  assert.ok(await page.locator(selector).count(), `${selector} is on the page`);
};

/** A list of options long enough for a filter box, each with a label. */
const columns = Array.from({ length: 12 }, (_, i) => ({ value: `COL${i}`, label: `Column ${i}` }));

/* The markup action_menu() renders in R once Shiny has bound its links,
 * opened from its trigger by the keyboard, which lands on the first row. */
const actionMenu = async (page) => {
  await page.evaluate(() => {
    const host = document.createElement('div');
    host.style.cssText = 'display: flex; justify-content: flex-end';
    host.innerHTML = `
      <span class="blockr-action-menu" data-align="end">
        <button class="blockr-tool blockr-action-menu__trigger" type="button" aria-label="Download"
                aria-haspopup="menu" aria-expanded="false" data-blockr-tooltip="Download">&darr;</button>
        <div class="blockr-menu" role="menu" tabindex="-1" hidden>
          <div class="blockr-menu__title" role="presentation">This patient</div>
          <a class="blockr-menu__item" role="menuitem" tabindex="-1" href="#">
            <span class="blockr-menu__icon">&#9679;</span>
            <span class="blockr-menu__label">PowerPoint</span>
            <span class="blockr-menu__meta">.pptx</span>
          </a>
          <a class="blockr-menu__item blockr-menu__item--disabled" role="menuitem" tabindex="-1" href="#"
             aria-disabled="true" data-blockr-tooltip="Nothing to export">
            <span class="blockr-menu__label">Excel</span>
            <span class="blockr-menu__meta">.xlsx</span>
          </a>
          <div class="blockr-menu__divider" role="separator"></div>
          <a class="blockr-menu__item blockr-menu__item--danger" role="menuitem" tabindex="-1" href="#">
            <span class="blockr-menu__label">Remove</span>
          </a>
        </div>
      </span>`;
    document.body.appendChild(host);
  });
  await page.focus('.blockr-action-menu__trigger');
  await page.keyboard.press('Enter');
};

/* The code field with its list open on what `typed` completes to, and the
 * keyboard on the first row. */
const codeField = async (page, typed) => {
  await page.evaluate(() => {
    const host = document.createElement('div');
    document.body.appendChild(host);
    window.Blockr.Input.create(host, {
      columns: ['AGE', 'AGEGR1'], categories: { math: ['abs'] }, label: 'Expression'
    });
  });
  await page.focus('.blockr-input input');
  await page.keyboard.type(typed);
  await page.keyboard.press('ArrowDown');
};

/* Each state builds its controls on the page and checks that what it
 * opened is there. */
const states = {
  'the fields at rest, with picks, dirty, and required': async (page) => {
    await page.evaluate((options) => {
      const B = window.Blockr;
      const field = (label) => {
        const el = document.createElement('div');
        el.style.marginBottom = '12px';
        const name = document.createElement('div');
        name.className = 'blockr-label';
        name.textContent = label;
        const slot = document.createElement('div');
        el.append(name, slot);
        document.body.appendChild(el);
        return slot;
      };
      B.Select.single(field('Column'), { options, selected: 'AGE', label: 'Column', bordered: true });
      B.Select.single(field('Group by'), { options, selected: 'SEX', label: 'Group by' });
      B.Select.single(field('Colour'),
        { options, allowEmpty: true, placeholder: 'None', label: 'Colour', bordered: true });
      B.Select.multi(field('Columns'), { options, selected: ['AGE', 'SEX'], label: 'Columns', bordered: true });
      const many = Array.from({ length: 12 }, (_, i) => `COLUMN_${i}`);
      const narrow = field('Keep');
      narrow.style.width = '240px';
      B.Select.multi(narrow, { options: many, selected: many, label: 'Keep', singleLine: true, bordered: true });

      const input = document.createElement('input');
      input.className = 'blockr-text-input';
      input.setAttribute('aria-label', 'Title');
      const commit = field('Title');
      commit.className = 'blockr-commit-field';
      commit.appendChild(input);
      B.textCommit(input, { onCommit: () => {} });
      input.value = 'Adverse events';
      input.dispatchEvent(new Event('input', { bubbles: true }));

      const totals = field('Totals');
      totals.append(B.checkbox('Show totals', true, () => {}).el, B.checkbox('Show counts', false, () => {}).el);
      field('Order').appendChild(B.segmented(
        [{ value: 'asc', label: 'Asc', title: 'Ascending' }, { value: 'desc', label: 'Desc', title: 'Descending' }],
        'asc', () => {}, { label: 'Sort direction' }
      ).el);

      const required = field('Weight');
      B.setRequiredEmpty(/** @type {HTMLElement} */ (required.parentElement), true);
      B.Select.single(required, { options, allowEmpty: true, placeholder: 'Pick a column', label: 'Weight', bordered: true });
    }, OPTS);
    await shows(page, '.blockr-select__more');
    await shows(page, '.blockr-expr-confirm:not([style*="none"])');
  },

  'the gear tray open, with its fields and the gear\'s tooltip': async (page) => {
    await page.evaluate((options) => {
      const B = window.Blockr;
      const header = document.createElement('div');
      header.className = 'blockr-gear-header';
      const gear = document.createElement('button');
      gear.type = 'button';
      gear.className = 'blockr-gear-btn';
      gear.innerHTML = B.icons.gear;
      header.appendChild(gear);
      const band = document.createElement('div');
      band.className = 'blockr-settings blockr-settings--beak';
      band.innerHTML = '<div class="blockr-settings__title">Axes</div><div class="blockr-settings__grid"></div>';
      const grid = /** @type {HTMLElement} */ (band.querySelector('.blockr-settings__grid'));
      const field = (label) => {
        const el = document.createElement('div');
        el.className = 'blockr-settings__field';
        const name = document.createElement('div');
        name.className = 'blockr-label';
        name.textContent = label;
        const slot = document.createElement('div');
        el.append(name, slot);
        grid.appendChild(el);
        return slot;
      };
      document.body.append(header, band);
      B.Select.single(field('X axis'), { options, selected: 'AGE', label: 'X axis', bordered: true });
      const title = document.createElement('input');
      title.className = 'blockr-text-input';
      title.setAttribute('aria-label', 'Title');
      title.placeholder = 'No title';
      field('Title').appendChild(title);
      field('Order').appendChild(B.segmented(
        [{ value: 'asc', label: 'Asc' }, { value: 'desc', label: 'Desc' }], 'desc', () => {}, { label: 'Order' }
      ).el);
      field('Totals').appendChild(B.checkbox('Show totals', true, () => {}).el);
      B.gearTray(band, gear).set(true);
      gear.focus();
    }, OPTS);
    await shows(page, '.blockr-settings--open');
    await shows(page, '.blockr-tooltip');
  },

  'a tooltip with a column\'s label and a package badge': async (page) => {
    await page.evaluate(() => {
      const b = document.createElement('button');
      b.textContent = '+2';
      b.style.marginTop = '80px';
      document.body.appendChild(b);
      window.Blockr.tooltip.set(b, [{ name: 'AGE', label: 'Age' }, { name: 'dataset block', badge: 'blockr.core' }]);
      b.focus();
    });
    await shows(page, '.blockr-tooltip__meta');
    await shows(page, '.blockr-tooltip__badge');
  },

  'a list open on a keyboard row with a label': async (page) => {
    await page.evaluate((options) => {
      const host = document.createElement('div');
      document.body.appendChild(host);
      window.Blockr.Select.single(host, { options, selected: 'AGE', label: 'Group by', bordered: true });
    }, OPTS);
    await page.focus('.blockr-select__search');
    await page.keyboard.press('ArrowDown');
    await page.keyboard.press('ArrowDown');
    await shows(page, '.blockr-select__option--highlighted .blockr-select__opt-label');
  },

  'a multi list open, with its ticks, on a keyboard row': async (page) => {
    await page.evaluate((options) => {
      const host = document.createElement('div');
      document.body.appendChild(host);
      window.Blockr.Select.multi(host, { options, selected: ['SEX'], label: 'Columns', bordered: true });
    }, OPTS);
    await page.focus('.blockr-select__search');
    await page.keyboard.press('ArrowDown');
    await shows(page, '.blockr-select__option--highlighted');
  },

  'a multi menu with its picks, its filter box and a keyboard row': async (page) => {
    await page.evaluate((options) => {
      const word = document.createElement('button');
      word.textContent = 'Column 1';
      document.body.appendChild(word);
      window.Blockr.Select.menu(word,
        { options, mode: 'multi', selected: ['COL1', 'COL2'], title: 'Columns', labelFirst: true });
    }, columns);
    await page.keyboard.press('ArrowDown');
    await shows(page, '.blockr-select__tags--menu .blockr-select__tag');
    await shows(page, '.blockr-select__search--menu:not(.blockr-select__search--offscreen)');
    await shows(page, '.blockr-select__option--highlighted');
  },

  'an action menu open on a keyboard row with an icon and meta': async (page) => {
    await actionMenu(page);
    await shows(page, '.blockr-menu__item:focus .blockr-menu__meta');
  },

  'an action menu open on its destructive row': async (page) => {
    await actionMenu(page);
    await page.keyboard.press('ArrowDown');
    await shows(page, '.blockr-menu__item--danger:focus');
  },

  'a menu with every kind of row, on a keyboard row with meta': async (page) => {
    await page.evaluate(() => {
      const views = document.createElement('button');
      views.textContent = 'Views';
      document.body.appendChild(views);
      window.Blockr.menu(views, {
        head: { title: 'Filter rows', badge: 'blockr.dplyr', text: 'Keeps the rows that match.' },
        caption: 'Append to Dataset',
        filter: true,
        items: [
          { label: 'Copy block ID', meta: 'filter_1', mono: true },
          { title: 'Views' },
          { label: 'Page 1', current: true },
          { label: 'Show code', checked: true },
          { label: 'Scatter plot', mark: { icon: 'sliders' }, badge: 'blockr.ggplot' },
          { label: 'Export', disabled: true, reason: 'Nothing to export' },
          { label: 'Manage pages', icon: 'sliders', quiet: true },
          { divider: true },
          { label: 'Remove', icon: 'trash', danger: true }
        ]
      });
    });
    await page.keyboard.press('ArrowDown');
    await shows(page, '.blockr-menu__item--active .blockr-menu__meta');
  },

  'the code field\'s list on a column': async (page) => {
    await codeField(page, 'AG');
    await shows(page, '.blockr-input__item--highlighted .blockr-input__item-meta');
  },

  'the code field\'s list on a function': async (page) => {
    await codeField(page, 'ab');
    await shows(page, '.blockr-input__item--highlighted .blockr-input__item-parens');
  },

  'a segment under the pointer': async (page) => {
    await page.evaluate(() => {
      document.body.appendChild(window.Blockr.segmented(
        [{ value: 'asc', label: 'Asc' }, { value: 'desc', label: 'Desc' }], 'asc', () => {}, { label: 'Order' }
      ).el);
    });
    await page.hover('.blockr-segmented__seg:not(.is-selected)');
  }
};

/** Every element the rule fails in `scheme`, with axe's measurement. */
const failures = (page, scheme) => page.evaluate(async (s) => {
  document.documentElement.setAttribute('data-bs-theme', s);
  // Measure the colours a transition ends on, not a frame of the fade.
  for (const a of document.getAnimations()) a.finish();
  const { violations } = await window.axe.run(document, {
    runOnly: { type: 'rule', values: ['color-contrast'] }
  });
  return violations.flatMap((v) => v.nodes.map(
    (n) => `${n.target.join(' ')}: ${n.any.map((c) => c.message).join(' ')}`
  ));
}, scheme);

for (const [state, build] of Object.entries(states)) {
  test(state, async (page) => {
    await build(page);
    await page.addScriptTag({ path: require.resolve('axe-core/axe.min.js') });
    const seen = { light: await failures(page, 'light'), dark: await failures(page, 'dark') };
    assert.deepStrictEqual(seen, { light: [], dark: [] });
  }, { viewport: { width: 800, height: 1000 } });
}
