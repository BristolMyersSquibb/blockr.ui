// @ts-check
/**
 * blockr-block-shiny.js — the Shiny host for blocks defined with
 * Blockr.defineBlock() (blockr-block.js). R's side is new_js_block().
 *
 * One input binding serves every block: its element is
 * `<div class="blockr-block" data-block="<name>" id="<ns>-block">`.
 *
 *   client -> R  input `<id>`: {messages: [...]}, each one of
 *                {type: 'ready'}, {type: 'submit', state},
 *                {type: 'request', seq, name, args}
 *   R -> client  sendInputMessage: {state?, context?} or {seq, result|error}
 *
 * The binding sends `ready` when it binds, and R sends nothing before that,
 * so no message can arrive for a block that is not there yet. The first
 * answer to `ready` carries state and context together, and the block is
 * mounted with both. A re-bound element sends `ready` again and is mounted
 * afresh.
 *
 * Messages of one task go out as one input value: Shiny keeps only the last
 * value an input was set to in a batch.
 *
 * Depends on: blockr-block.js
 */
(() => {
  'use strict';

  if (typeof Shiny === 'undefined') return;

  /**
   * @typedef {Object} ShinyBlockSlot
   * @property {object[]} queue
   * @property {boolean} flushing
   * @property {number} seq
   * @property {Map<number, {resolve: (v: unknown) => void, reject: (e: Error) => void}>} pending
   * @property {BlockrBlockInstance | null} instance
   */

  /**
   * @param {HTMLElement & {_blockr?: ShinyBlockSlot}} el
   * @returns {ShinyBlockSlot}
   */
  const slot = (el) => {
    if (!el._blockr) {
      el._blockr = {
        queue: [], flushing: false, seq: 0, pending: new Map(), instance: null
      };
    }
    return el._blockr;
  };

  /** @param {HTMLElement} el @param {object} msg */
  const post = (el, msg) => {
    const s = slot(el);
    s.queue.push(msg);
    if (s.flushing) return;
    s.flushing = true;
    queueMicrotask(() => {
      s.flushing = false;
      const messages = s.queue.splice(0);
      if (messages.length) {
        Shiny.setInputValue(el.id, { messages }, { priority: 'event' });
      }
    });
  };

  /** @param {HTMLElement} el @param {unknown} state @param {BlockrContext} context */
  const mount = (el, state, context) => {
    const def = Blockr.blocks[el.dataset.block || ''];
    if (!def) {
      throw new Error(`blockr: no block "${el.dataset.block}" defined`);
    }
    const s = slot(el);
    /** @type {BlockrHost<any>} */
    const host = {
      state,
      context,
      submit: (next) => post(el, { type: 'submit', state: next }),
      request: (name, args) => new Promise((resolve, reject) => {
        const seq = ++s.seq;
        s.pending.set(seq, { resolve, reject });
        post(el, { type: 'request', seq, name, args: args || {} });
      })
    };
    s.instance = def.mount(el, host);
  };

  /** @param {HTMLElement} el */
  const unmount = (el) => {
    const s = slot(el);
    s.instance?.destroy?.();
    s.instance = null;
    for (const p of s.pending.values()) p.reject(new Error('block unmounted'));
    s.pending.clear();
    el.replaceChildren();
  };

  /**
   * @param {HTMLElement} el
   * @param {{state?: unknown, context?: BlockrContext, seq?: number,
   *   result?: unknown, error?: string}} msg
   */
  const receive = (el, msg) => {
    const s = slot(el);
    if (msg.seq !== undefined && msg.seq !== null) {
      const p = s.pending.get(msg.seq);
      s.pending.delete(msg.seq);
      if (!p) return;
      if (msg.error !== undefined) p.reject(new Error(msg.error));
      else p.resolve(msg.result);
      return;
    }
    if (!s.instance) {
      if ('state' in msg && 'context' in msg) {
        mount(el, msg.state, /** @type {BlockrContext} */ (msg.context));
      }
      return;
    }
    if ('state' in msg) s.instance.setState(msg.state);
    if ('context' in msg) s.instance.setContext(/** @type {BlockrContext} */ (msg.context));
  };

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    /** @param {HTMLElement} scope */
    find: (scope) => $(scope).find('.blockr-block'),
    /** @param {HTMLElement} el */
    getId: (el) => el.id || null,
    // the block talks through setInputValue(); the binding's own value is
    // never read
    getValue: () => null,
    subscribe: () => {},
    /** @param {HTMLElement} el */
    unsubscribe: (el) => unmount(el),
    /** @param {HTMLElement} el */
    initialize: (el) => {
      if (slot(el).instance) unmount(el);
      post(el, { type: 'ready' });
    },
    receiveMessage: receive
  });
  Shiny.inputBindings.register(binding, 'blockr.block');
})();
