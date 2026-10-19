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
