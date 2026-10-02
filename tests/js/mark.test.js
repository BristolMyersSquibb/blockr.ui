/* The block's mark, drawn in Chrome (browser.js): its four sizes, the glyph
 * and the corners each derives from its size, and its category's colour,
 * the same in both schemes. The glyph is the markup blockr.core stores, a
 * bsicons SVG sized 1em inline.
 */
'use strict';

const assert = require('node:assert');
const { test } = require('./browser');

const GLYPH = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-funnel " ' +
  'style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img">' +
  '<path d="M0 0h16v16H0z"></path></svg>';

/** Draw a mark and measure it, in the scheme the page is in. */
const measure = (page, cls, category) => page.evaluate(({ cls, category, glyph }) => {
  const mark = document.createElement('span');
  mark.className = cls;
  if (category) mark.dataset.category = category;
  mark.innerHTML = glyph;
  document.body.appendChild(mark);
  const box = mark.getBoundingClientRect();
  const svg = mark.querySelector('svg').getBoundingClientRect();
  const style = getComputedStyle(mark);
  const out = {
    size: [box.width, box.height],
    glyph: [svg.width, svg.height],
    centred: Math.abs((svg.left + svg.right) / 2 - (box.left + box.right) / 2) < 0.5 &&
      Math.abs((svg.top + svg.bottom) / 2 - (box.top + box.bottom) / 2) < 0.5,
    radius: style.borderTopLeftRadius,
    color: style.color,
    fill: getComputedStyle(mark.querySelector('path')).fill,
    tint: style.backgroundColor
  };
  mark.remove();
  return out;
}, { cls, category, glyph: GLYPH });

/**
 * A computed colour as [r, g, b, alpha], 0 to 255 and 0 to 1: Chrome writes
 * a colour mixed in srgb as color(srgb ...), and a plain one as rgb().
 * @param {string} css
 */
const rgba = (css) => {
  const srgb = /^color\(srgb ([\d.]+) ([\d.]+) ([\d.]+)(?: \/ ([\d.]+))?\)$/.exec(css);
  if (srgb) return [...srgb.slice(1, 4).map((c) => Math.round(Number(c) * 255)), Number(srgb[4] ?? 1)];
  const rgb = /^rgba?\((\d+), (\d+), (\d+)(?:, ([\d.]+))?\)$/.exec(css);
  if (rgb) return [...rgb.slice(1, 4).map(Number), Number(rgb[4] ?? 1)];
  throw new Error(`not a colour: ${css}`);
};

test('each size derives its glyph and its corners from its side', async (page) => {
  for (const [side, cls] of [[24, 'blockr-block-mark'], [32, 'blockr-block-mark blockr-block-mark--32'],
    [20, 'blockr-block-mark blockr-block-mark--20'], [16, 'blockr-block-mark blockr-block-mark--16']]) {
    const seen = await measure(page, cls, 'plot');
    assert.deepStrictEqual(
      { size: seen.size, glyph: seen.glyph, centred: seen.centred, radius: seen.radius },
      { size: [side, side], glyph: [side / 2 + 2, side / 2 + 2], centred: true, radius: `${side / 4}px` },
      `the ${side}px mark`
    );
  }
});

test('the glyph and the tint take the category colour, in both schemes', async (page) => {
  for (const scheme of ['light', 'dark']) {
    await page.evaluate((s) => document.documentElement.setAttribute('data-bs-theme', s), scheme);
    for (const [category, colour] of [['plot', [230, 159, 0]], ['input', [0, 114, 178]],
      ['uncategorized', [153, 153, 153]], ['not-a-category', [153, 153, 153]], [null, [153, 153, 153]]]) {
      const seen = await measure(page, 'blockr-block-mark', category);
      assert.deepStrictEqual(
        { color: rgba(seen.color), fill: rgba(seen.fill), tint: rgba(seen.tint) },
        { color: [...colour, 1], fill: [...colour, 1], tint: [...colour, 0.18] },
        `${scheme}: ${category}`
      );
    }
  }
});

test('a colour of its own wins over the category\'s', async (page) => {
  const tint = await page.evaluate(() => {
    const mark = document.createElement('span');
    mark.className = 'blockr-block-mark';
    mark.dataset.category = 'plot';
    mark.style.color = 'rgb(124, 58, 237)';
    document.body.appendChild(mark);
    return getComputedStyle(mark).backgroundColor;
  });
  assert.deepStrictEqual(rgba(tint), [124, 58, 237, 0.18]);
});

test('a menu row draws the 24px mark', async (page) => {
  const side = await page.evaluate((glyph) => {
    const button = document.createElement('button');
    button.textContent = 'Add';
    document.body.appendChild(button);
    Blockr.menu(button, { items: [{ label: 'Chart', mark: { icon: glyph, category: 'plot' } }] });
    const mark = document.querySelector('.blockr-menu__item .blockr-block-mark').getBoundingClientRect();
    return [mark.width, mark.height];
  }, GLYPH);
  assert.deepStrictEqual(side, [24, 24]);
});
