/* Blockr.icons, as controls_dep() writes it into the page's head ahead of
 * blockr-ui.js, built from the same files: one SVG per icon in
 * inst/assets/icons, which small_icon() reads in R. A file holds the icon's
 * note as a comment and its svg a tag a line, so the icon is the file
 * without the comment and the whitespace between tags.
 *
 * Usage:
 *   const icons = require('./icons');
 *   win.eval(icons.script);                  // a happy-dom window
 *   await page.addInitScript(icons.script);  // a Chrome page, before goto()
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');

const DIR = path.join(__dirname, '..', '..', 'inst', 'assets', 'icons');

/** @type {Record<string, string>} */
const icons = Object.fromEntries(
  fs.readdirSync(DIR).filter((f) => f.endsWith('.svg')).map((f) => [
    path.basename(f, '.svg'),
    fs.readFileSync(path.join(DIR, f), 'utf8')
      .replace(/<!--[\s\S]*?-->/g, '')
      .replace(/>\s+</g, '><')
      .trim()
  ])
);

/** The statement that sets Blockr.icons, for a page to run ahead of its own. */
const script = `(window.Blockr = window.Blockr || {}).icons = ${JSON.stringify(icons)};`;

module.exports = { icons, script };
