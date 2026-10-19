// @ts-check
/**
 * blockr-block.js — what a block's UI is written against, in any host:
 * Blockr.defineBlock() and the helpers block UIs share. It knows nothing of
 * Shiny; blockr-block-shiny.js is the Shiny host, and blockx has its own.
 *
 * A block file defines its block and nothing else:
 *
 *   Blockr.defineBlock({
 *     name: 'select',
 *     mount(el, host) {
 *       // host.state, host.context: what they are at mount
 *       // host.submit(state): a user change
 *       // host.request(name, args): a Promise of the answer
 *       return { setState(state) {}, setContext(context) {}, destroy() {} };
 *     }
 *   });
 *
 * The host mounts a block once it has both state and context, and from then
 * on calls setState() and setContext() with what changed. setState() never
 * submits. The contract is in blockr.design, open/blockx-ready-blocks.
 */
window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({});

/** @type {Record<string, BlockrBlockDef>} */
Blockr.blocks = Blockr.blocks || {};

/** @param {BlockrBlockDef} def */
Blockr.defineBlock = (def) => {
  Blockr.blocks[def.name] = def;
};

/**
 * Point a column picker at a new option list without losing a deliberate pick.
 *
 * The block owns its column; the widget does not. A column picker has no
 * `allowEmpty`, so a list that does not carry the block's column slides the
 * widget onto option 0, and during a restore that list arrives routinely: an
 * upstream still settling reports the old frame's columns. So:
 *
 *   pinned   — restored or picked. It survives a list that does not offer it
 *              (`updateOptions` swaps the list without touching the
 *              selection) and lands on its option when the real columns come.
 *   unpinned — only the select's own pick of option 0. It follows the list.
 *
 * @param {BlockrSelectSingleHandle} select
 * @param {BlockrSelectOption[]} options
 * @param {string} column The owner's column, not the widget's.
 * @param {boolean} pinned
 * @returns {string} The column the owner should hold from here on.
 */
Blockr.reconcileColumn = (select, options, column, pinned) => {
  const offered = (options || []).map(
    (o) => (o && typeof o === 'object' ? o.value : o)
  );
  if (pinned && column && offered.indexOf(column) < 0) {
    select.updateOptions(options, column);
    return column;
  }
  select.setOptions(options, column || null);
  return select.getValue() || '';
};

/**
 * The same for a multi column picker: everything in it was restored or
 * picked, so all of it is pinned. `setOptions()` would filter the selection
 * against a list the frame may not carry yet.
 *
 * @param {BlockrSelectMultiHandle} select
 * @param {BlockrSelectOption[]} options
 * @param {string[]} columns The owner's columns, not the widget's.
 * @returns {string[]} The columns the owner should hold from here on.
 */
Blockr.reconcileColumns = (select, options, columns) => {
  const cols = (columns || []).slice();
  select.updateOptions(options, cols);
  return cols;
};

/**
 * A comparable key for a block state, insensitive to key order and to the
 * scalar/length-1-array distinction. For a block that wants to know whether a
 * change it made itself (an auto-pick on new columns) is a change at all.
 *
 * @param {unknown} state
 * @returns {string}
 */
Blockr.stateKey = (state) => JSON.stringify(state, (_key, value) => {
  if (Array.isArray(value)) return value.length === 1 ? value[0] : value;
  return value && typeof value === 'object'
    ? Object.keys(value).sort().reduce((sorted, k) => {
      sorted[k] = value[k];
      return sorted;
    }, /** @type {Record<string, unknown>} */ ({}))
    : value;
});

/**
 * The block IR (see ir_to_expr() in R, blockx shared/ir.ts) as R text, for a
 * block that shows an expression it cannot draw as controls. The same
 * precedence and namespaces as blockx's dplyr emitter.
 */
Blockr.ir = (() => {
  const SYNTACTIC = /^(?:(?:\p{L}|\.(?![0-9]))[\p{L}\p{N}._]*|\.)$/u;
  const RESERVED = new Set(['if', 'else', 'repeat', 'while', 'function', 'for',
    'next', 'break', 'in', 'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA']);
  /** @param {string} n */
  const rname = (n) => (SYNTACTIC.test(n) && !RESERVED.has(n)
    ? n : '`' + n.replace(/\\/g, '\\\\').replace(/`/g, '\\`') + '`');
  /** @param {unknown} v */
  const rlit = (v) => (v === null || v === undefined ? 'NA'
    : typeof v === 'boolean' ? (v ? 'TRUE' : 'FALSE')
      : typeof v === 'number' ? String(v) : JSON.stringify(String(v)));
  /** @type {Record<string, {p: number, right?: boolean, cmp?: boolean, tight?: boolean}>} */
  const BIN = {
    '~': { p: 0 }, '|': { p: 1 }, '&': { p: 2 },
    '==': { p: 4, cmp: true }, '!=': { p: 4, cmp: true }, '>': { p: 4, cmp: true },
    '>=': { p: 4, cmp: true }, '<': { p: 4, cmp: true }, '<=': { p: 4, cmp: true },
    '+': { p: 5 }, '-': { p: 5 }, '*': { p: 6 }, '/': { p: 6 },
    '%%': { p: 7 }, '%in%': { p: 7 }, '^': { p: 9, right: true, tight: true }
  };
  /** @type {Record<string, string>} */
  const NS = {
    if_else: 'dplyr', case_when: 'dplyr', coalesce: 'dplyr', between: 'dplyr',
    str_detect: 'stringr', str_c: 'stringr', str_sub: 'stringr',
    str_to_upper: 'stringr', str_to_lower: 'stringr', n: 'dplyr',
    n_distinct: 'dplyr', first: 'dplyr', last: 'dplyr', median: 'stats',
    sd: 'stats', lag: 'dplyr', lead: 'dplyr', row_number: 'dplyr'
  };
  const ATOM = 10;
  /**
   * @param {BlockrIrExpr} e
   * @returns {{s: string, p: number}}
   */
  const rx = (e) => {
    if ('col' in e) return { s: rname(e.col), p: ATOM };
    if ('lit' in e) return { s: rlit(e.lit), p: ATOM };
    if ('list' in e) return { s: `c(${e.list.map(rlit).join(', ')})`, p: ATOM };
    if ('code' in e) return { s: `(${e.code})`, p: ATOM };
    const b = BIN[e.call];
    const args = e.args || [];
    // & and | take two or more arguments
    if ((e.call === '&' || e.call === '|') && args.length > 2) {
      return rx(args.slice(1).reduce((x, y) => ({ call: e.call, args: [x, y] }), args[0]));
    }
    if (b && args.length === 2) {
      const l = rx(args[0]);
      const r = rx(args[1]);
      const wl = b.right || b.cmp ? l.p <= b.p : l.p < b.p;
      const wr = b.right ? r.p < b.p : r.p <= b.p;
      const sep = b.tight ? '' : ' ';
      return {
        s: `${wl ? `(${l.s})` : l.s}${sep}${e.call}${sep}${wr ? `(${r.s})` : r.s}`,
        p: b.p
      };
    }
    if (e.call === 'neg') {
      const x = rx(args[0]);
      return { s: `-${x.p <= 8 ? `(${x.s})` : x.s}`, p: 8 };
    }
    if (e.call === '!') {
      const x = rx(args[0]);
      return { s: `!${x.p < ATOM ? `(${x.s})` : x.s}`, p: 3 };
    }
    const all = args.map((a) => rx(a).s);
    for (const [k, v] of Object.entries(e.named || {})) all.push(`${k} = ${rx(v).s}`);
    const fn = NS[e.call] ? `${NS[e.call]}::${e.call}` : e.call;
    return { s: `${fn}(${all.join(', ')})`, p: ATOM };
  };
  return {
    /** @param {BlockrIrExpr} e */
    toR: (e) => rx(e).s
  };
})();
