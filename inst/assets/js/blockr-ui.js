// @ts-check
/**
 * blockr-ui.js — the design system's shared JS layer: the Blockr namespace,
 * DOM helpers, the dismiss stack (Blockr.layer), placement (Blockr.place),
 * keyboard hints, the tooltip, text edited in place, the menus of actions
 * (Blockr.menu and the R-built Blockr.actionMenu), and the small controls
 * every block builds on (the Enter button, the required-empty cue, the
 * checkbox, the segmented control, the gear tray). Blockr.Select
 * (blockr-select.js) and Blockr.Input (blockr-input.js) build on it.
 *
 * Load it first. It holds nothing block-specific: the block protocol
 * (Blockr.registerBlock and the restore queue) is blockr.dplyr's
 * blockr-core.js. Nor does it hold the icons it draws: Blockr.icons comes
 * from the files small_icon() reads in R, one SVG per icon in
 * inst/assets/icons, and controls_dep() writes it into the page ahead of
 * this file.
 */
window.Blockr = window.Blockr || /** @type {BlockrNamespace} */ ({});

Blockr.uid = (() => {
  let counter = 0;
  return (prefix = 'b') => `${prefix}-${++counter}`;
})();

Blockr.escapeHtml = (s) => {
  const div = document.createElement('div');
  div.appendChild(document.createTextNode(s));
  return div.innerHTML;
};

Blockr.removeNode = (node) => {
  node?.parentNode?.removeChild(node);
};

/**
 * Natural (nowrap, unconstrained) width of an element's rendered content.
 *
 * Clones the element's innerHTML into a hidden shrink-wrapped measurer in
 * the document, so class-based styles (e.g. .blockr-select__opt-label)
 * still apply. Row factories use this to size a shared left column to the
 * widest committed value instead of a fixed pixel width — the source
 * element itself is usually ellipsized (flex: 1 + overflow: hidden), so
 * its own scrollWidth can't be trusted for shrinking.
 *
 * @param {Element} el
 * @returns {number}
 */
Blockr.contentWidth = (el) => {
  let m = Blockr._measureEl;
  if (!m || !m.isConnected) {
    m = document.createElement('div');
    m.style.cssText =
      'position:absolute;left:-9999px;top:0;visibility:hidden;' +
      'white-space:nowrap;width:auto;pointer-events:none;';
    document.body.appendChild(m);
    Blockr._measureEl = m;
  }
  const cs = window.getComputedStyle(el);
  m.style.fontFamily = cs.fontFamily;
  m.style.fontSize = cs.fontSize;
  m.style.fontWeight = cs.fontWeight;
  m.innerHTML = el.innerHTML;
  const w = Math.ceil(m.getBoundingClientRect().width);
  m.innerHTML = '';
  return w;
};

/**
 * Whether `el`, or anything in it, is cut off: its content is wider than its
 * box. A tooltip marked `overflow` shows only then.
 *
 * @param {Element} el
 * @returns {boolean}
 */
Blockr.cutOff = (el) => [el, ...Array.from(el.querySelectorAll('*'))].some(
  (n) => n.clientWidth > 0 && n.scrollWidth > n.clientWidth + 1
);

/**
 * The dismiss stack (design system, "Dismissing"). Anything Escape or a
 * click outside can close is a layer: a menu, a list, a tooltip, the gear
 * tray, a field holding an edit not yet committed. A control registers a
 * layer when it opens and removes it when it closes, and one pair of
 * listeners on the window decides for every layer, so no control listens
 * for these itself or stops the event:
 *
 * - Escape acts on the top layer only; the next press acts on the one below.
 * - A pointerdown closes the layers above the topmost one it lands in.
 *
 * Blockr.layer(el, opts) puts `el` (an element, or a list of them) on top
 * and returns `{ remove }`. The option `from` names what opened the layer: a
 * pointerdown there is inside, as the trigger's own click toggles the layer.
 * The options `escape` and `outside` say what Escape and a click outside do;
 * a layer without `outside` stays open, as the gear tray does. The layer is
 * off the stack before either runs.
 *
 * The option `inPage` marks a layer that sits in the page rather than over
 * it: the gear tray, a dirty field. Escape reaches it only from inside it,
 * so it closes the tray that holds the focus, and several can be open at
 * once.
 *
 * Both listeners run in the capture phase, ahead of every other: a menu over
 * a modal, or a field in a side panel, takes the Escape before the modal or
 * the panel sees it. A layer whose elements have all left the page (a block
 * removed while its menu or tray was open) is dropped, so the stack does not
 * hold on to the block.
 */
Blockr.layer = (() => {
  /** @type {BlockrLayerEntry[]} */
  const stack = [];

  /** @param {BlockrLayerEntry} entry */
  const drop = (entry) => {
    const i = stack.indexOf(entry);
    if (i >= 0) stack.splice(i, 1);
  };

  const prune = () => {
    for (let i = stack.length - 1; i >= 0; i--) {
      if (!stack[i].els.some((el) => el.isConnected)) stack.splice(i, 1);
    }
  };

  /** @param {BlockrLayerEntry} entry @param {EventTarget | null} node */
  const holds = (entry, node) =>
    node instanceof Node && entry.els.some((el) => el.contains(node));

  window.addEventListener('keydown', (e) => {
    if (e.key !== 'Escape' || e.isComposing) return;
    prune();
    // The top layer, passing over a layer in the page that the key was not
    // pressed in.
    let i = stack.length - 1;
    while (i >= 0 && (!stack[i].escape || (stack[i].inPage && !holds(stack[i], e.target)))) i--;
    if (i < 0) return;
    const entry = stack[i];
    e.preventDefault();
    e.stopPropagation();
    drop(entry);
    /** @type {(e: KeyboardEvent) => void} */ (entry.escape)(e);
  }, true);

  window.addEventListener('pointerdown', (e) => {
    prune();
    let at = stack.length - 1;
    while (at >= 0 && !holds(stack[at], e.target)) at--;
    for (const entry of stack.slice(at + 1).reverse()) {
      // One that closed with the layer above it is gone already.
      if (!entry.outside || stack.indexOf(entry) < 0) continue;
      drop(entry);
      entry.outside(e);
    }
  }, true);

  /**
   * @param {Element | Element[]} el
   * @param {BlockrLayerOptions} [opts]
   * @returns {BlockrLayerHandle}
   */
  const layer = (el, opts) => {
    const o = opts || {};
    const entry = {
      els: (Array.isArray(el) ? el : [el]).concat(o.from ? [o.from] : []),
      inPage: !!o.inPage,
      escape: o.escape || null,
      outside: o.outside || null
    };
    stack.push(entry);
    return { remove: () => drop(entry) };
  };
  /** How many layers are open. Tests read it. */
  layer.count = () => { prune(); return stack.length; };
  return layer;
})();

/**
 * Hang a fixed-position panel under an anchor and keep it there.
 *
 * For a floating panel that has been portalled to <body> to escape its
 * ancestors' clipping and stacking contexts (dock panels, offcanvas, modals;
 * see blockr.design/open/blockr-select-portal). Being out of the ancestor's
 * flow, the panel has to be told where the anchor is: `gap` px under it,
 * flipped above when there is no room below and there is room above.
 *
 * `width: 'anchor'` (the default) spans the anchor's width, at least
 * `minWidth`, which is what a dropdown under its control does. `width:
 * { min, max }` lets the panel size to its own content within those bounds,
 * which is what a menu under a word needs: a word is not a control. Either
 * way the panel stays `margin` px inside the viewport horizontally.
 *
 * `align: 'start'` (the default) lines the panel up with the anchor's left
 * edge; `'end'` with its right edge, for a trigger in the header row, where
 * a menu opening to the right would leave the block.
 *
 * The panel follows scroll (capture phase, so an ancestor scrolling counts),
 * window resize, and size changes of the anchor and of the panel itself
 * (ResizeObserver, coalesced to the next frame): a pick that adds a tag row
 * moves the anchor's bottom edge, and fewer options left moves a panel that
 * opened above. `onFlip(above)` reports each placement so the caller can mark
 * its own element. `stop()` removes every listener.
 *
 * @param {HTMLElement} panel
 * @param {HTMLElement} anchor
 * @param {BlockrPlaceOptions} [opts]
 * @returns {BlockrPlaceHandle}
 */
Blockr.place = (panel, anchor, opts) => {
  const o = opts || {};
  const gap = o.gap == null ? 4 : o.gap;
  const margin = o.margin == null ? 8 : o.margin;
  const width = o.width || 'anchor';
  const minWidth = o.minWidth || 0;
  const align = o.align || 'start';

  // The anchor can leave the page while the panel is open: a block that
  // redraws its band on every pick replaces the word a multi menu hangs off.
  // A detached element measures 0x0 at 0,0, which threw the panel to the top
  // left corner. `reanchor` returns the element that now stands for the
  // anchor; without one, or while it finds none, the panel stays put.
  /** @type {ResizeObserver | null} */
  let obs = null;
  const current = () => {
    if (anchor.isConnected) return true;
    const next = o.reanchor ? o.reanchor() : null;
    if (!next || !next.isConnected) return false;
    if (obs) { obs.unobserve(anchor); obs.observe(next); }
    anchor = next;
    return true;
  };

  const update = () => {
    if (!current()) return;
    const r = anchor.getBoundingClientRect();
    // Not laid out yet on the first call after display: block; a guess is
    // better than 0, which would never flip.
    const h = panel.offsetHeight || 240;
    // The gap counts too: without it, a panel that just fit below ended 4px
    // from the window's edge.
    const spaceBelow = window.innerHeight - r.bottom - gap - margin;
    const above = spaceBelow < h && r.top > h;

    panel.style.position = 'fixed';
    panel.style.bottom = 'auto';
    // A stylesheet may pin `right` for the in-flow case; on a fixed box that
    // would stretch it to the window edge.
    panel.style.right = 'auto';
    let w;
    if (width === 'anchor') {
      w = Math.max(r.width, minWidth);
      panel.style.width = w + 'px';
    } else {
      panel.style.width = '';
      panel.style.minWidth = width.min + 'px';
      panel.style.maxWidth = width.max + 'px';
      w = panel.offsetWidth || width.min;
    }
    const left = align === 'end' ? r.right - w : r.left;
    panel.style.left = Math.max(margin, Math.min(left,
      document.documentElement.clientWidth - w - margin)) + 'px';
    panel.style.top = (above ? r.top - h - gap : r.bottom + gap) + 'px';
    if (o.onFlip) o.onFlip(above);
  };

  update();
  window.addEventListener('scroll', update, { capture: true, passive: true });
  window.addEventListener('resize', update, { passive: true });

  let frame = 0;
  if (typeof ResizeObserver !== 'undefined') {
    obs = new ResizeObserver(() => {
      if (frame) return;
      frame = requestAnimationFrame(() => { frame = 0; update(); });
    });
    obs.observe(anchor);
    obs.observe(panel);
  }

  // Removing the anchor fires nothing the panel listens to (no scroll, no
  // resize, and a ResizeObserver does not reliably report a node leaving
  // the page), so with `reanchor` the page is watched while the panel is
  // open, and the panel moves on the next frame after its anchor is gone.
  /** @type {MutationObserver | null} */
  let mut = null;
  if (o.reanchor && typeof MutationObserver !== 'undefined') {
    mut = new MutationObserver(() => {
      if (frame || anchor.isConnected) return;
      frame = requestAnimationFrame(() => { frame = 0; update(); });
    });
    mut.observe(document.body, { childList: true, subtree: true });
  }

  return {
    update,
    stop: () => {
      window.removeEventListener('scroll', update, { capture: true });
      window.removeEventListener('resize', update);
      if (obs) { obs.disconnect(); obs = null; }
      if (mut) { mut.disconnect(); mut = null; }
      if (frame) { cancelAnimationFrame(frame); frame = 0; }
    }
  };
};

/**
 * Carry a panel rendered in R to <body> while it floats, and back.
 *
 * The action menu's list and the dropdown's panel hold Shiny markup and
 * float over every ancestor's overflow and stacking (dock panels, offcanvas,
 * modals), so while open they sit on <body>, and closed they go back where
 * they were, to leave the page with their owner. Shiny's bindings go with
 * the node. Blockr.portal(panel, anchor, gone) moves `panel` to <body>; the
 * handle's restore() puts it back. If `anchor` leaves the page meanwhile (a
 * block removed, a dropdown rendered again), `gone` runs to close the panel,
 * and restore() unbinds the panel and removes it: there is nothing to go
 * back to.
 *
 * @param {HTMLElement} panel
 * @param {HTMLElement} anchor
 * @param {() => void} gone
 * @returns {BlockrPortalHandle}
 */
Blockr.portal = (panel, anchor, gone) => {
  const home = panel.parentNode;
  const next = panel.nextSibling;
  document.body.appendChild(panel);
  /** @type {MutationObserver | null} */
  let watch = null;
  if (typeof MutationObserver !== 'undefined') {
    watch = new MutationObserver(() => { if (!anchor.isConnected) gone(); });
    watch.observe(document.body, { childList: true, subtree: true });
  }
  return {
    restore: () => {
      if (watch) { watch.disconnect(); watch = null; }
      if (home && home.isConnected) {
        home.insertBefore(panel, next && next.parentNode === home ? next : null);
      } else {
        const shiny = /** @type {any} */ (window).Shiny;
        if (shiny && shiny.unbindAll) shiny.unbindAll(panel);
        panel.remove();
      }
    }
  };
};

/**
 * Toggle the canonical required-empty amber cue (blockr-blocks.css
 * .blockr-field--required-empty) on a field wrapper or standalone input.
 * One name keeps call sites greppable for the blockr.ui move.
 * @param {Element} el
 * @param {boolean} empty
 */
Blockr.setRequiredEmpty = (el, empty) => {
  el.classList.toggle('blockr-field--required-empty', !!empty);
};

/**
 * Keyboard shortcuts (design system, "Keyboard shortcuts"). The platform is
 * decided here, once per page: on a Mac `.blockr-mac` goes on the root,
 * which shows the Mac form of every hint R drew with shortcut(), and
 * `Blockr.keys()` writes a hint for JavaScript-built UI.
 */
Blockr.isMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent);

if (Blockr.isMac) document.documentElement.classList.add('blockr-mac');

/**
 * A shortcut written for this platform: keys joined by "+", `Mod` for
 * Command on a Mac and Ctrl elsewhere. "Mod+Shift+S" is "⌘⇧S" on a Mac and
 * "Ctrl+Shift+S" elsewhere; Enter is ↵ on both.
 *
 * @param {string} keys
 * @returns {string}
 */
Blockr.keys = (keys) => {
  const mac = { Mod: '⌘', Shift: '⇧', Alt: '⌥', Ctrl: '⌃', Enter: '↵', Esc: 'Esc' };
  const other = { Mod: 'Ctrl', Shift: 'Shift', Alt: 'Alt', Ctrl: 'Ctrl', Enter: '↵', Esc: 'Esc' };
  /** @type {Record<string, string>} */
  const names = Blockr.isMac ? mac : other;
  const parts = keys.split('+').map((k) =>
    names[k] || (k.length === 1 ? k.toUpperCase() : k));
  return parts.join(Blockr.isMac ? '' : '+');
};

/**
 * Commit-on-Enter text input (design-system §5.5): typing never submits —
 * a ↵ button arms while the value is dirty, the value commits on Enter,
 * blur, a click outside or the button (which then fades to the ✓ icon), and
 * Escape reverts to the last committed value.
 *
 * The input must already sit in its parent: the chip is inserted directly
 * after it. Programmatic value changes (setState restores, mode switches)
 * go through the returned `sync(value)`, which resets the committed
 * baseline so a restored value never shows an armed chip.
 *
 * The button shows ↵ alone: it appears as you type, at the end of the
 * field, so the key's own symbol is enough, and the field keeps the room
 * (design system, "Keyboard shortcuts").
 *
 * @param {HTMLInputElement} input
 * @param {{ onCommit: (value: string) => void }} opts
 * @returns {{ chip: HTMLButtonElement, commit: () => void,
 *             sync: (value: string) => void }}
 */
Blockr.textCommit = (input, opts) => {
  const chip = document.createElement('button');
  chip.type = 'button';
  chip.className = 'blockr-expr-confirm blockr-expr-confirm--key';
  // The key is the whole action, so the button shows it and has no tooltip;
  // screen readers get its name.
  chip.setAttribute('aria-label', 'Apply (Enter)');
  chip.style.display = 'none';
  let committed = input.value;
  let everCommitted = false;
  /** @type {BlockrLayerHandle | null} */
  let layer = null;
  const syncChip = () => {
    const dirty = input.value !== committed;
    if (dirty) {
      chip.style.display = '';
      chip.classList.remove('confirmed');
      chip.textContent = '↵';
    } else if (everCommitted) {
      chip.style.display = '';
      chip.classList.add('confirmed');
      chip.innerHTML = Blockr.icons.confirm;
    } else {
      chip.style.display = 'none';
    }
    // A dirty field is a layer (design system, "Dismissing"): Escape reverts
    // it, so the gear tray it sits in stays open, and a click outside
    // commits it, as blur does. It is in the page, so only an Escape pressed
    // in the field reverts it; a clean field lets the key through to the
    // tray.
    if (dirty && !layer) {
      layer = Blockr.layer([input, chip], {
        inPage: true,
        escape: () => { layer = null; revert(); },
        outside: () => { layer = null; commit(); }
      });
    } else if (!dirty && layer) {
      layer.remove();
      layer = null;
    }
  };
  const commit = () => {
    if (input.value === committed) return;
    committed = input.value;
    everCommitted = true;
    opts.onCommit(input.value);
    syncChip();
  };
  const revert = () => {
    input.value = committed;
    syncChip();
  };
  input.addEventListener('input', syncChip);
  input.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') { e.preventDefault(); commit(); }
  });
  input.addEventListener('blur', commit);
  // Keep focus on the input so the chip click doesn't race blur-commit.
  chip.addEventListener('mousedown', (e) => e.preventDefault());
  chip.addEventListener('click', commit);
  /** @type {Element} */ (input.parentElement).insertBefore(chip, input.nextSibling);
  return {
    chip,
    commit,
    sync: (value) => {
      input.value = value;
      committed = value;
      everCommitted = false;
      syncChip();
    }
  };
};

/**
 * The light-card tooltip (design system, "Tooltips"): one style for every
 * name shown on hover, in place of the browser's native `title` box, which
 * ignores the tokens and dark mode, never shows on keyboard focus, and waits
 * as long as the browser likes.
 *
 * Blockr.tooltip.set(el, content, { overflow }) gives `el` a tooltip.
 * `content` is a string, a column `{ name, label }` (the label shows muted
 * after the name), a line with a `badge` (a neutral badge after the name, as
 * a block type with its package), a list of either (one per line, as the
 * "+N" chip's), or a function returning one of those at show time. With `overflow: true` it
 * shows only while the element or a child is cut off, so a value that fits
 * has none. Blockr.tooltip.clear(el) takes it away.
 *
 * Markup built in R cannot call `set()`, so an element can also carry its
 * tooltip as an attribute: `data-blockr-tooltip="Download"`, plus
 * `data-blockr-tooltip-badge` for a badge after the name (the dock header's
 * mark: "filter block" with "blockr.dplyr") and `data-blockr-tooltip-overflow`
 * for the cut-off-only case. A `set()` on the same element wins over the
 * attributes.
 *
 * One set of document listeners serves every tooltip, added when this file
 * loads, so no instance adds or leaks its own. The card shows after the
 * pointer rests 300ms, at once on keyboard focus, and at once while "warm":
 * within 400ms of another card leaving, so moving along a row of icons does
 * not wait at each one. A card on screen is a layer (Blockr.layer): Escape
 * hides it, and so does a click, which can only land outside it.
 */
Blockr.tooltip = (() => {
  const DELAY = 300;
  const WARM = 400;
  const GAP = 6;
  const MARGIN = 8;
  /** @type {WeakMap<Element, { content: BlockrTooltipContent | (() => BlockrTooltipContent), overflow: boolean }>} */
  const tips = new WeakMap();
  /** @type {HTMLDivElement | null} */
  let card = null;
  /** @type {Element | null} */
  let current = null;
  /** @type {ReturnType<typeof setTimeout> | null} */
  let timer = null;
  let warmUntil = 0;
  /** @type {BlockrLayerHandle | null} */
  let layer = null;

  /** @param {BlockrTooltipLine} line */
  const lineText = (line) => {
    if (typeof line === 'string') return line;
    let text = line.label && line.label !== line.name ? `${line.name} · ${line.label}` : line.name;
    if (line.badge) text += ` · ${line.badge}`;
    return text;
  };

  /** @param {BlockrTooltipContent} content */
  const asLines = (content) => (Array.isArray(content) ? content : [content]);

  const ATTR = 'data-blockr-tooltip';

  /**
   * The line `el`'s attributes give: the name, then its badge if it has one.
   * An empty name shows nothing, badge or not.
   * @param {Element} el
   * @returns {BlockrTooltipLine}
   */
  const attrLine = (el) => {
    const name = el.getAttribute(ATTR) || '';
    const badge = el.getAttribute(ATTR + '-badge');
    return name && badge ? { name, badge } : name;
  };

  /**
   * The tooltip `el` carries: one given by set(), else its attributes.
   * @param {Element} el
   */
  const tipOf = (el) => tips.get(el) || (el.hasAttribute(ATTR)
    ? { content: attrLine(el), overflow: el.hasAttribute(ATTR + '-overflow') }
    : null);

  /**
   * The nearest element, from `el` up, that has a tooltip.
   * @param {Element | null} el
   */
  const owner = (el) => {
    while (el && !tipOf(el)) el = el.parentElement;
    return el;
  };

  /** @param {Element} el */
  const contentOf = (el) => {
    const tip = tipOf(el);
    if (!tip) return null;
    return typeof tip.content === 'function' ? tip.content() : tip.content;
  };

  /** @param {HTMLElement} parent @param {BlockrTooltipLine} line */
  const drawLine = (parent, line) => {
    if (typeof line === 'string') { parent.textContent = line; return; }
    parent.textContent = line.name;
    if (line.label && line.label !== line.name) {
      const meta = document.createElement('span');
      meta.className = 'blockr-tooltip__meta';
      meta.textContent = line.label;
      parent.append(' ', meta);
    }
    if (line.badge) {
      const badge = document.createElement('span');
      badge.className = 'blockr-tooltip__badge';
      badge.textContent = line.badge;
      parent.append(' ', badge);
    }
  };

  const hide = () => {
    if (timer) { clearTimeout(timer); timer = null; }
    if (layer) { layer.remove(); layer = null; }
    if (card && card.isConnected) {
      card.remove();
      warmUntil = Date.now() + WARM;
    }
    if (current) current.removeAttribute('aria-describedby');
    current = null;
  };

  /** @param {Element} el */
  const show = (el) => {
    timer = null;
    const content = el.isConnected ? contentOf(el) : null;
    if (!content) return;
    if (!card) {
      card = document.createElement('div');
      card.className = 'blockr-tooltip';
      card.id = Blockr.uid('blockr-tooltip');
      card.setAttribute('role', 'tooltip');
    }
    card.textContent = '';
    for (const line of asLines(content)) {
      const row = document.createElement('div');
      row.className = 'blockr-tooltip__line';
      drawLine(row, line);
      card.appendChild(row);
    }
    document.body.appendChild(card);
    // Above the element, centred on it; below only where there is no room.
    const r = el.getBoundingClientRect();
    const c = card.getBoundingClientRect();
    const left = Math.max(MARGIN,
      Math.min(r.left + r.width / 2 - c.width / 2, window.innerWidth - c.width - MARGIN));
    let top = r.top - c.height - GAP;
    if (top < MARGIN) top = r.bottom + GAP;
    card.style.left = `${left}px`;
    card.style.top = `${top}px`;
    el.setAttribute('aria-describedby', card.id);
    current = el;
    if (layer) layer.remove();
    layer = Blockr.layer(card, { escape: hide, outside: hide });
  };

  /** @param {Event} e */
  const enter = (e) => {
    const target = e.target instanceof Element ? e.target : null;
    const el = owner(target);
    if (!el || el === current) return;
    hide();
    const tip = /** @type {{ overflow: boolean }} */ (tipOf(el));
    if (tip.overflow && !Blockr.cutOff(el)) return;
    // A click focuses a button too, right after its pointerdown hid the
    // card; only keyboard focus brings the card at once.
    if (e.type === 'focusin' && target && !target.matches(':focus-visible')) return;
    const found = el;
    const now = e.type === 'focusin' || Date.now() < warmUntil;
    if (now) show(found);
    else timer = setTimeout(() => show(found), DELAY);
  };

  /** @param {PointerEvent} e */
  const leave = (e) => {
    if (!current && !timer) return;
    const to = e.relatedTarget;
    const el = owner(e.target instanceof Element ? e.target : null);
    if (!el) return;
    if (to instanceof Node && el.contains(to)) return;
    hide();
  };

  document.addEventListener('pointerover', enter, true);
  document.addEventListener('focusin', enter, true);
  document.addEventListener('pointerout', /** @type {EventListener} */ (leave), true);
  document.addEventListener('focusout', hide, true);
  document.addEventListener('scroll', hide, true);

  return {
    /**
     * @param {Element} el
     * @param {BlockrTooltipContent | (() => BlockrTooltipContent)} content
     * @param {{ overflow?: boolean }} [opts]
     */
    set(el, content, opts) {
      tips.set(el, { content, overflow: !!(opts && opts.overflow) });
    },
    /** @param {Element} el */
    clear(el) {
      if (current === el) hide();
      tips.delete(el);
    },
    /**
     * The tooltip as plain text, lines joined by newlines ("AGE · Age"), or
     * '' when `el` has none. Tests read it; it ignores `overflow`.
     * @param {Element} el
     */
    text(el) {
      const content = contentOf(el);
      return content ? asLines(content).map(lineText).join('\n') : '';
    }
  };
})();

/* --- Text edited in place ---------------------------------------------- */

/**
 * Text the user edits in place (design system, "Text edited in place"): an
 * element marked `data-blockr-editable` shows the text cursor (CSS) and a
 * tooltip naming the gesture, "Double-click to edit" unless the attribute's
 * value names another ("Click to rename"). While the text is cut off, the
 * tooltip leads with the whole text and the gesture follows, muted. An
 * element is taken up on its first hover or focus, in the capture phase on
 * `window`, ahead of Blockr.tooltip's listeners on `document`, so markup
 * drawn at any time needs nothing but the attribute; removing it takes both
 * away again.
 */
(() => {
  const HINT = 'Double-click to edit';
  const seen = new WeakSet();

  /** @param {Event} e */
  const take = (e) => {
    const el = e.target instanceof Element ? e.target.closest('[data-blockr-editable]') : null;
    if (!el || seen.has(el)) return;
    seen.add(el);
    Blockr.tooltip.set(el, () => {
      // The attribute can go again (a name editable only in a mode).
      if (!el.hasAttribute('data-blockr-editable')) return null;
      const hint = el.getAttribute('data-blockr-editable') || HINT;
      return Blockr.cutOff(el) ? { name: (el.textContent || '').trim(), label: hint } : hint;
    });
  };

  window.addEventListener('pointerover', take, true);
  window.addEventListener('focusin', take, true);
})();

/* --- Menus of actions --------------------------------------------------- */

/**
 * The menus of actions (design system, "Menus"): Blockr.menu, built in
 * JavaScript, and Blockr.actionMenu, which drives the menus action_menu()
 * builds in R. Blockr.Select.menu() is the other kind, a list of values to
 * pick from.
 *
 * Both run on one controller, drive(), so both have one keyboard model: the
 * focus stays in the filter box or on the list, and the keyboard row is its
 * aria-activedescendant. Arrows move the row, Home and End jump, and the
 * pointer moves it too; Enter or Space clicks it, so a row does the same
 * whether it is picked by key or by pointer. A row disabled by its author is
 * passed over and does nothing. Opened from the keyboard, a menu starts on
 * its first row. Tab stops at the menu's tool where it has one, then closes
 * it and leaves the focus on the trigger, for the browser to move on from;
 * Escape closes it and hands the focus back, and a click outside closes it.
 * One menu, of either kind, is open at a time.
 */
(() => {
  const ROW = '.blockr-menu__item';
  const ACTIVE = 'blockr-menu__item--active';
  const DISABLED = 'blockr-menu__item--disabled';

  /** @type {{ anchor: HTMLElement, close: (refocus?: boolean) => void } | null} */
  let open = null;

  /**
   * Drive an open menu: the keyboard row, the pointer, the placement and
   * the dismissing, all taken off the panel again when it closes.
   * @param {BlockrMenuDrive} m
   */
  const drive = (m) => {
    if (open) open.close();
    const { panel, list, anchor } = m;
    const focus = m.focus || list;
    const ac = new AbortController();
    const on = { signal: ac.signal };
    /** @type {HTMLElement | null} */
    let active = null;
    /** @type {BlockrPlaceHandle | null} */
    let placed = null;
    /** @type {BlockrLayerHandle | null} */
    let layer = null;

    // What the keyboard moves over: every row but one filtered out or
    // disabled by its author. A download Shiny has not bound yet keeps its
    // place, as it works a moment later: on the first open every download is
    // still unbound, and passing over them put the keyboard row on the one
    // after, Remove in a block's menu.
    /** @param {Element} row */
    const usable = (row) => !(/** @type {HTMLElement} */ (row).hidden) &&
      !row.classList.contains(DISABLED);
    const pickable = () => Array.from(
      /** @type {NodeListOf<HTMLElement>} */ (list.querySelectorAll(ROW))).filter(usable);
    // Inert: disabled by its author, or a download whose handler Shiny has
    // not bound yet (aria-disabled, which Shiny clears once it has).
    /** @param {Element} row */
    const inert = (row) => row.classList.contains(DISABLED) ||
      row.getAttribute('aria-disabled') === 'true';
    /** @param {EventTarget | null} t */
    const rowOf = (t) => {
      const row = t instanceof Element ? t.closest(ROW) : null;
      return row && list.contains(row) ? /** @type {HTMLElement} */ (row) : null;
    };

    /** @param {HTMLElement | null} row */
    const setActive = (row) => {
      if (active) active.classList.remove(ACTIVE);
      active = row;
      if (row) {
        if (!row.id) row.id = Blockr.uid('blockr-menu-item');
        row.classList.add(ACTIVE);
        row.scrollIntoView({ block: 'nearest' });
        focus.setAttribute('aria-activedescendant', row.id);
      } else {
        focus.removeAttribute('aria-activedescendant');
      }
    };
    /** @param {number} dir */
    const step = (dir) => {
      const rows = pickable();
      if (!rows.length) return;
      const at = active ? rows.indexOf(active) : -1;
      setActive(rows[at < 0 ? (dir > 0 ? 0 : rows.length - 1)
        : (at + dir + rows.length) % rows.length]);
    };

    let closed = false;
    /** @param {boolean} [refocus] */
    const close = (refocus) => {
      if (closed) return;
      closed = true;
      if (layer) layer.remove();
      if (placed) placed.stop();
      ac.abort();
      setActive(null);
      anchor.setAttribute('aria-expanded', 'false');
      anchor.removeAttribute('aria-controls');
      if (open && open.close === close) open = null;
      m.detach();
      if (refocus && anchor.isConnected) anchor.focus();
      if (m.onClose) m.onClose();
    };

    // In the capture phase, so an inert row is stopped before a handler of
    // its own (Shiny's, on a link) sees the click.
    panel.addEventListener('click', (e) => {
      const row = rowOf(e.target);
      if (!row) return;
      if (inert(row)) { e.preventDefault(); e.stopImmediatePropagation(); return; }
      m.onPick(row, e);
    }, { capture: true, signal: ac.signal });
    list.addEventListener('mousemove', (e) => {
      const row = rowOf(e.target);
      if (row && row !== active && usable(row)) setActive(row);
    }, on);
    panel.addEventListener('mouseleave', () => setActive(null), on);
    const tool = m.tool || null;
    panel.addEventListener('keydown', (e) => {
      // The tool is a button of its own, so Enter and Space click it. From it,
      // Shift+Tab goes back to what holds the focus, and Tab leaves the menu.
      if (tool && e.target === tool) {
        if (e.key === 'Tab' && e.shiftKey) { e.preventDefault(); focus.focus(); }
        else if (e.key === 'Tab') close(true);
        return;
      }
      // Home, End and Space belong to the filter box while it has the focus.
      const box = e.target instanceof HTMLInputElement ? e.target : null;
      if (e.key === 'ArrowDown') { e.preventDefault(); step(1); }
      else if (e.key === 'ArrowUp') { e.preventDefault(); step(-1); }
      else if ((e.key === 'Home' || e.key === 'End') && !box) {
        e.preventDefault();
        const rows = pickable();
        if (rows.length) setActive(e.key === 'Home' ? rows[0] : rows[rows.length - 1]);
      }
      else if (e.key === 'Enter' || (e.key === ' ' && !box)) {
        e.preventDefault();
        // With no keyboard row, Enter in the filter box takes the first match.
        const row = active || (box && box.value ? pickable()[0] : null);
        if (row) row.click();
      }
      // Tab stops at the menu's tool, where it has one. Otherwise the focus
      // goes back on the trigger first, so the browser's Tab moves on from
      // there; from the removed panel it would start over at the top of the
      // page.
      else if (e.key === 'Tab') {
        if (tool && !e.shiftKey) { e.preventDefault(); tool.focus(); }
        else close(true);
      }
    }, on);
    panel.addEventListener('focusout', (e) => {
      const to = e.relatedTarget;
      if (to instanceof Node && (panel.contains(to) || anchor.contains(to))) return;
      if (to) close(false);
    }, on);

    placed = Blockr.place(panel, anchor, { width: m.width, align: m.align });
    if (!list.id) list.id = Blockr.uid('blockr-menu');
    anchor.setAttribute('aria-expanded', 'true');
    anchor.setAttribute('aria-haspopup', 'menu');
    anchor.setAttribute('aria-controls', list.id);
    // A layer (Blockr.layer): Escape closes it and hands focus back to the
    // trigger, and a click outside closes it. The trigger is not outside:
    // its own click is its binding's.
    layer = Blockr.layer(panel, {
      from: anchor,
      escape: () => close(true),
      outside: () => close(false)
    });
    focus.focus({ preventScroll: true });
    if (m.byKeyboard) step(1);
    open = { anchor, close };
    return { close, setActive, pickable };
  };

  // A row's icon: the name of one of Blockr.icons, or an SVG/HTML string.
  /** @param {string} name */
  const iconFor = (name) =>
    (Object.prototype.hasOwnProperty.call(Blockr.icons, name) ? Blockr.icons[name] : name);

  /**
   * A menu of actions built in JavaScript: a block's "…" menu, the views
   * menu, a user menu.
   *
   * Blockr.menu(anchor, config) opens a menu under `anchor` and returns
   * `{ el, close }`. `config.items` is a list of entries:
   *
   *   { label, icon?, meta?, mono?, current?, checked?, danger?, quiet?,
   *     disabled?, reason?, onSelect? }
   *                            a row; `meta` is grey text after the label
   *                            (`mono` sets it in the code face), `current` the
   *                            item in use (weight 600 and a check), `checked`
   *                            a toggle that is on (a check), `danger` a
   *                            destructive action (red only under the pointer
   *                            or as the keyboard row),
   *                            `quiet` a muted row such as "Manage pages",
   *                            `reason` the tooltip on a disabled row
   *   { gap: true }            a small space between groups
   *   { divider: true }        a rule between groups
   *   { title }                a group title
   *
   * Rows have no icon unless `icon` is given, either the name of one of
   * Blockr.icons ('trash', 'sliders') or an SVG/HTML string, and only a
   * row that is more than a plain action gets one: it opens a mode or another
   * surface, or it destroys something. Such a row sits in a group of its own,
   * after a gap or a divider, so a label with an icon never sits right under
   * one without.
   *
   * A row may also carry `mark` ({ icon, category, color? }: a block's mark,
   * its glyph in its category's colour on a tint of it, as block_mark()
   * draws it in R; `color` draws it in a colour of its own, such as a
   * stack's), `badge` (a neutral badge at the end, as a package) and
   * `keywords` (more text the filter matches). A `config.caption` is one
   * muted line on top ("Append to Dataset"); `config.filter` (true, or the
   * placeholder) adds a filter box that narrows the rows as you type and
   * holds the focus; `config.minWidth` widens the panel.
   *
   * A `config.tool` ({ icon, label, onSelect }) puts a tool at the end of the
   * caption line, in the menu's top right corner: an icon button, named by
   * `label` in its tooltip, that opens the menu's list somewhere fuller, as
   * the block actions' menu opens the dock's block browser in the sidebar.
   * Tab moves to it from the filter box. A click closes the menu and hands
   * `onSelect` the filter box's text.
   *
   * `config.head` ({ title, badge?, text? }) puts a block of text above the
   * rows, as a link's menu in the outline names the link. `align` is 'start'
   * (default) or 'end', for a trigger in a header row. `onClose` runs once
   * whichever way the menu closes.
   *
   * The panel is portalled to <body> and placed with Blockr.place: 4px under
   * the trigger, above when there is no room below, 180 to 320px wide. A pick
   * closes it and runs the row's `onSelect`.
   *
   * Blockr.menu.bind(trigger, config) wires a button to open and close its
   * menu; `config` may be a function, read on each open.
   * Blockr.menu.delegate(selector, config) does the same for every trigger
   * that matches `selector`, including those added to the page later.
   *
   * @param {HTMLElement} anchor
   * @param {BlockrMenuConfig} config
   * @param {boolean} [byKeyboard]
   * @returns {{ el: HTMLDivElement, close: () => void }}
   */
  const build = (anchor, config, byKeyboard) => {
    const panel = document.createElement('div');
    panel.className = 'blockr-menu';
    panel.id = Blockr.uid('blockr-menu');

    if (config.head) {
      const head = document.createElement('div');
      head.className = 'blockr-menu__head';
      const line = document.createElement('div');
      line.className = 'blockr-menu__head-title';
      line.textContent = config.head.title;
      if (config.head.badge) {
        const badge = document.createElement('span');
        badge.className = 'blockr-menu__badge';
        badge.textContent = config.head.badge;
        line.append(' ', badge);
      }
      head.appendChild(line);
      if (config.head.text) {
        const text = document.createElement('div');
        text.className = 'blockr-menu__head-text';
        text.textContent = config.head.text;
        head.appendChild(text);
      }
      panel.appendChild(head);
    }

    /** @type {HTMLButtonElement | null} */
    let tool = null;
    if (config.tool) {
      const cap = document.createElement('div');
      cap.className = 'blockr-menu__caption blockr-menu__caption--tool';
      const text = document.createElement('span');
      text.className = 'blockr-menu__caption-text';
      text.textContent = config.caption || '';
      tool = document.createElement('button');
      tool.type = 'button';
      tool.className = 'blockr-tool blockr-menu__tool';
      tool.setAttribute('aria-label', config.tool.label);
      tool.innerHTML = iconFor(config.tool.icon);
      Blockr.tooltip.set(tool, config.tool.label);
      cap.append(text, tool);
      panel.appendChild(cap);
    } else if (config.caption) {
      const cap = document.createElement('div');
      cap.className = 'blockr-menu__caption';
      cap.textContent = config.caption;
      panel.appendChild(cap);
    }

    /** @type {HTMLInputElement | null} */
    let filterInput = null;
    if (config.filter) {
      const wrap = document.createElement('div');
      wrap.className = 'blockr-menu__filter';
      filterInput = document.createElement('input');
      filterInput.type = 'text';
      filterInput.className = 'blockr-menu__filter-input';
      filterInput.placeholder = typeof config.filter === 'string' ? config.filter : 'Search';
      filterInput.setAttribute('aria-label', filterInput.placeholder);
      filterInput.autocomplete = 'off';
      wrap.appendChild(filterInput);
      panel.appendChild(wrap);
    }

    // The rows sit in a list of their own that carries the menu role: a menu
    // may hold only its items, groups and separators, so the head, the
    // caption and the filter box stay outside it, in the panel. The keyboard
    // row is the active descendant of whichever holds focus, the filter box
    // or the list.
    const list = document.createElement('div');
    list.className = 'blockr-menu__list';
    list.id = Blockr.uid('blockr-menu-list');
    list.setAttribute('role', 'menu');
    list.tabIndex = -1;
    panel.appendChild(list);
    if (filterInput) filterInput.setAttribute('aria-controls', list.id);

    /** @type {Map<HTMLElement, { item: BlockrMenuItem, search: string }>} */
    const rows = new Map();
    /** @type {{ kind: 'row' | 'title' | 'sep', el: HTMLElement }[]} */
    const nodes = [];
    for (const entry of config.items || []) {
      if ('gap' in entry) {
        const gap = document.createElement('div');
        gap.className = 'blockr-menu__gap';
        gap.setAttribute('role', 'separator');
        list.appendChild(gap);
        nodes.push({ kind: 'sep', el: gap });
        continue;
      }
      if ('divider' in entry) {
        const hr = document.createElement('div');
        hr.className = 'blockr-menu__divider';
        hr.setAttribute('role', 'separator');
        list.appendChild(hr);
        nodes.push({ kind: 'sep', el: hr });
        continue;
      }
      if (!('label' in entry)) {
        const t = document.createElement('div');
        t.className = 'blockr-menu__title';
        t.textContent = entry.title;
        list.appendChild(t);
        nodes.push({ kind: 'title', el: t });
        continue;
      }
      const item = entry;
      const row = document.createElement('button');
      row.type = 'button';
      row.id = Blockr.uid('blockr-menu-item');
      row.tabIndex = -1;
      row.className = 'blockr-menu__item' +
        (item.current ? ' blockr-menu__item--current' : '') +
        (item.danger ? ' blockr-menu__item--danger' : '') +
        (item.quiet ? ' blockr-menu__item--quiet' : '');
      row.setAttribute('role', 'menuitem');
      if (item.disabled) {
        row.classList.add(DISABLED);
        row.setAttribute('aria-disabled', 'true');
        if (item.reason) Blockr.tooltip.set(row, item.reason);
      }
      if (item.mark) {
        const mk = document.createElement('span');
        mk.className = 'blockr-block-mark';
        if (item.mark.category) mk.dataset.category = item.mark.category;
        if (item.mark.color) mk.style.color = item.mark.color;
        mk.innerHTML = item.mark.icon || '';
        row.appendChild(mk);
      }
      if (item.icon) {
        const ic = document.createElement('span');
        ic.className = 'blockr-menu__icon';
        ic.innerHTML = iconFor(item.icon);
        row.appendChild(ic);
      }
      const label = document.createElement('span');
      label.className = 'blockr-menu__label';
      label.textContent = item.label;
      row.appendChild(label);
      // `checked`: a toggle that is on (a check, no weight); `current`: the
      // item in use (weight 600 and a check).
      if ('checked' in item) {
        row.setAttribute('role', 'menuitemcheckbox');
        row.setAttribute('aria-checked', item.checked ? 'true' : 'false');
      }
      if (item.meta) {
        const meta = document.createElement('span');
        meta.className = 'blockr-menu__meta' + (item.mono ? ' blockr-menu__meta--mono' : '');
        meta.textContent = item.meta;
        row.appendChild(meta);
      }
      if (item.badge) {
        const badge = document.createElement('span');
        badge.className = 'blockr-menu__badge';
        badge.textContent = item.badge;
        row.appendChild(badge);
      }
      // The check of the current item or a toggle that is on: at the end of
      // the row (design system, "Menus"), after its meta text and badge.
      if (item.current || item.checked) {
        const check = document.createElement('span');
        check.className = 'blockr-menu__check';
        check.innerHTML = iconFor('check');
        row.appendChild(check);
      }
      const search = [item.label, item.keywords || '', item.badge || '', item.meta || '']
        .join(' ').toLowerCase();
      nodes.push({ kind: 'row', el: row });
      rows.set(row, { item, search });
      list.appendChild(row);
    }

    const empty = document.createElement('div');
    empty.className = 'blockr-menu__empty';
    empty.textContent = 'No matches';
    empty.hidden = true;
    if (filterInput) panel.appendChild(empty);

    document.body.appendChild(panel);
    const d = drive({
      panel,
      list,
      focus: filterInput || list,
      tool,
      anchor,
      width: { min: config.minWidth || 180, max: Math.max(config.minWidth || 180, 320) },
      align: config.align || 'start',
      byKeyboard,
      onPick: (row, e) => {
        e.stopPropagation();
        d.close(true);
        const r = rows.get(row);
        if (r && r.item.onSelect) r.item.onSelect();
      },
      detach: () => panel.remove(),
      onClose: config.onClose
    });

    // Typing filters the rows by label, keywords, badge and meta text (every
    // word has to match); a group title shows while one of its rows does, the
    // gaps and rules only while nothing is typed. The first match is the
    // keyboard row, so Enter takes it.
    const applyFilter = () => {
      if (!filterInput) return;
      const terms = filterInput.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
      rows.forEach((r, row) => {
        row.hidden = terms.length > 0 && !terms.every((t) => r.search.indexOf(t) >= 0);
      });
      nodes.forEach((n, k) => {
        if (n.kind === 'sep') { n.el.hidden = terms.length > 0; return; }
        if (n.kind !== 'title') return;
        let any = false;
        for (let j = k + 1; j < nodes.length && nodes[j].kind !== 'title'; j++) {
          if (nodes[j].kind === 'row' && !nodes[j].el.hidden) { any = true; break; }
        }
        n.el.hidden = !any;
      });
      const left = d.pickable();
      empty.hidden = left.length > 0 || !terms.length;
      // The keyboard row is the first whose label matches, before one that
      // matched on its keywords or badge only.
      const byLabel = left.find((row) => {
        const r = rows.get(row);
        const label = r ? r.item.label.toLowerCase() : '';
        return terms.every((t) => label.indexOf(t) >= 0);
      });
      d.setActive(terms.length && left.length ? (byLabel || left[0]) : null);
    };
    if (filterInput) filterInput.addEventListener('input', applyFilter);

    // The tool closes the menu as a pick does, then hands on what was typed,
    // so the fuller view can open on the same rows.
    if (tool && config.tool) {
      const { onSelect } = config.tool;
      tool.addEventListener('click', (e) => {
        e.stopPropagation();
        const query = filterInput ? filterInput.value : '';
        d.close(true);
        onSelect(query);
      });
    }

    return { el: panel, close: () => d.close(false) };
  };

  /** @param {HTMLElement} anchor @param {BlockrMenuConfig} config */
  const menu = (anchor, config) => build(anchor, config);

  /**
   * A click or the down arrow on a trigger, wired by bind() or matched by
   * delegate(): it closes the trigger's menu if that is the one open, and
   * otherwise opens it, on its first row if the keyboard opened it.
   * @param {HTMLElement} trigger
   * @param {BlockrMenuSource} config
   * @param {boolean} keyboard
   */
  const toggle = (trigger, config, keyboard) => {
    if (open && open.anchor === trigger) { open.close(); return; }
    build(trigger, typeof config === 'function' ? config(trigger) : config, keyboard);
  };

  /**
   * @param {HTMLElement} trigger
   * @param {BlockrMenuConfig | (() => BlockrMenuConfig)} config
   */
  menu.bind = (trigger, config) => {
    trigger.setAttribute('aria-haspopup', 'menu');
    trigger.setAttribute('aria-expanded', 'false');
    // A click the keyboard made (Enter or Space on the button) has no
    // pointer position: detail is 0.
    trigger.addEventListener('click', (e) => {
      e.preventDefault();
      toggle(trigger, config, e.detail === 0);
    });
    trigger.addEventListener('keydown', (e) => {
      if (e.key === 'ArrowDown') { e.preventDefault(); toggle(trigger, config, true); }
    });
  };

  /*
   * The menus of triggers that come and go, as the dock's block cards do:
   * bind() wires one trigger, and one added to the page after it ran has no
   * menu. Blockr.menu.delegate(selector, config) serves every trigger that
   * matches `selector` from one pair of document listeners, as the action
   * menu's triggers are served, so a trigger added later needs nothing but
   * to match. A click and the down arrow act on it as on a trigger wired
   * with bind(). A function `config` is given the trigger, so one selector
   * serves triggers whose menus differ.
   *
   * Nothing marks a trigger before its first open, so its markup carries
   * aria-haspopup="menu" and aria-expanded="false", as action_menu()'s
   * does. Where triggers nest, the innermost one opens. Delegating a
   * selector again replaces its config. The listeners run in the capture
   * phase, so a handler between the document and the trigger that stops
   * the event cannot keep the menu shut.
   */

  /** @type {Map<string, BlockrMenuSource>} */
  const delegated = new Map();

  /**
   * The nearest element, from `target` up, that matches a delegated
   * selector, with that selector's config.
   * @param {EventTarget | null} target
   */
  const delegateOf = (target) => {
    if (!delegated.size || !(target instanceof Element)) return null;
    for (let el = /** @type {Element | null} */ (target); el; el = el.parentElement) {
      for (const [selector, config] of delegated) {
        if (el.matches(selector)) return { trigger: /** @type {HTMLElement} */ (el), config };
      }
    }
    return null;
  };

  /** @param {string} selector @param {BlockrMenuSource} config */
  menu.delegate = (selector, config) => {
    delegated.set(selector, config);
  };

  document.addEventListener('click', (e) => {
    const hit = delegateOf(e.target);
    if (!hit) return;
    e.preventDefault();
    toggle(hit.trigger, hit.config, e.detail === 0);
  }, true);

  document.addEventListener('keydown', (e) => {
    const hit = e.key === 'ArrowDown' ? delegateOf(e.target) : null;
    if (!hit) return;
    e.preventDefault();
    toggle(hit.trigger, hit.config, true);
  }, true);

  /*
   * The action menu: a list of actions opened by a button, built in R by
   * action_menu(). A row does one thing, a download, a rename, a removal,
   * and the menu closes; nothing is remembered. That is the whole difference
   * from Blockr.Select.menu, which sets a value. The two share the menu
   * surface and Blockr.place.
   *
   * The markup comes from R, so one pair of document listeners serves every
   * trigger on the page, including those a uiOutput renders later: a click
   * opens and closes the menu, and so does the down arrow, as on a trigger
   * wired with Blockr.menu.bind(). While closed, the list waits, hidden,
   * beside its trigger. Opening moves it to <body>, where no dock panel's
   * overflow clips it; closing moves it back, so it leaves the page with its
   * block and Shiny's unbinding still reaches the download and action links
   * inside it.
   *
   * A row is a link or a button with a handler of its own, so a pick lets
   * the click through to it and closes the menu a moment later, once the
   * row has done its job.
   */

  /** @type {{ trigger: HTMLElement, close: (refocus?: boolean) => void } | null} */
  let shown = null;

  /** @param {EventTarget | null} el */
  const triggerOf = (el) => /** @type {HTMLElement | null} */ (
    el instanceof Element ? el.closest('.blockr-action-menu__trigger') : null);

  /**
   * @param {HTMLElement} trigger
   * @param {boolean} byKeyboard
   */
  const show = (trigger, byKeyboard) => {
    const wrap = trigger.parentElement;
    const panel = /** @type {HTMLElement | null} */ (
      wrap && wrap.querySelector(':scope > .blockr-menu'));
    if (!wrap || !panel) return;
    const lifted = Blockr.portal(panel, trigger, () => d.close());
    panel.hidden = false;
    const d = drive({
      panel,
      list: panel,
      anchor: trigger,
      width: { min: 180, max: 320 },
      align: wrap.getAttribute('data-align') === 'start' ? 'start' : 'end',
      byKeyboard,
      onPick: () => { setTimeout(() => d.close(true), 0); },
      detach: () => {
        if (shown && shown.trigger === trigger) shown = null;
        panel.hidden = true;
        lifted.restore();
      }
    });
    shown = { trigger, close: d.close };
  };

  document.addEventListener('click', (e) => {
    const trigger = triggerOf(e.target);
    if (!trigger) return;
    if (shown && shown.trigger === trigger) shown.close();
    // A click the keyboard made (Enter or Space on the button) has no
    // pointer position: detail is 0.
    else show(trigger, e.detail === 0);
  }, true);

  document.addEventListener('keydown', (e) => {
    const trigger = e.key === 'ArrowDown' ? triggerOf(e.target) : null;
    if (!trigger || (shown && shown.trigger === trigger)) return;
    e.preventDefault();
    show(trigger, true);
  }, true);

  Blockr.menu = menu;
  Blockr.actionMenu = {
    /** The trigger of the action menu that is open, or null. */
    current: () => (shown ? shown.trigger : null),
    close: () => { if (shown) shown.close(); }
  };
})();

/* --- Controls ----------------------------------------------------------- */

(function () {
  'use strict';

  /**
   * Build a design-system checkbox.
   * @param {string} label
   * @param {boolean} checked
   * @param {(checked: boolean) => void} onChange
   * @returns {{ el: HTMLLabelElement, input: HTMLInputElement,
   *             set: (v: boolean) => void, get: () => boolean }}
   */
  function checkbox(label, checked, onChange) {
    var wrap = document.createElement('label');
    wrap.className = 'blockr-checkbox';
    var input = document.createElement('input');
    input.type = 'checkbox';
    input.checked = !!checked;
    var box = document.createElement('span');
    box.className = 'blockr-checkbox__box';
    box.innerHTML = Blockr.icons.confirm;
    var txt = document.createElement('span');
    txt.className = 'blockr-checkbox__label';
    txt.textContent = label;
    input.addEventListener('change', function () { onChange(input.checked); });
    wrap.appendChild(input);
    wrap.appendChild(box);
    wrap.appendChild(txt);
    return {
      el: wrap,
      input: input,
      set: function (v) { input.checked = !!v; },
      get: function () { return input.checked; }
    };
  }

  /**
   * The gear tray (design system, "The gear tray"): the gear toggles the band
   * in flow under the header row. It slides open and closed over 0.22s, so
   * the content below is seen moving; Escape inside it, or on the gear,
   * closes it and returns focus to the gear. The gear carries the tooltip "Settings", reports its
   * state in aria-expanded and takes the accent tint while open
   * (.blockr-gear-active). `open: true` starts it open, without the slide:
   * a tray drawn again keeps the state it had.
   * @param {HTMLElement} band
   * @param {HTMLButtonElement} gear
   * @param {{ label?: string, open?: boolean }} [opts]
   * @returns {BlockrGearTrayHandle}
   */
  function gearTray(band, gear, opts) {
    var open = false;
    /** @type {Animation | null} */
    var anim = null;
    /** @type {BlockrLayerHandle | null} */
    var layer = null;
    var still = typeof window.matchMedia === 'function' &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    band.setAttribute('role', 'region');
    band.setAttribute('aria-label', (opts && opts.label) || 'Settings');
    Blockr.tooltip.set(gear, 'Settings');
    gear.setAttribute('aria-label', 'Settings');
    gear.setAttribute('aria-expanded', 'false');
    // Open, the tray is a layer (Blockr.layer) in the page: an Escape
    // pressed in the band or on the gear closes it, and a click outside
    // leaves it open.
    function addLayer() {
      layer = Blockr.layer(band, {
        from: gear,
        inPage: true,
        escape: function () { set(false); gear.focus(); }
      });
    }

    if (opts && opts.open) {
      open = true;
      gear.classList.add('blockr-gear-active');
      gear.setAttribute('aria-expanded', 'true');
      band.classList.add('blockr-settings--open');
      addLayer();
    }

    /** @param {boolean} next */
    function set(next) {
      if (next === open) return;
      open = next;
      gear.classList.toggle('blockr-gear-active', open);
      gear.setAttribute('aria-expanded', open ? 'true' : 'false');
      if (open) {
        addLayer();
      } else if (layer) {
        layer.remove();
        layer = null;
      }
      if (anim) { anim.cancel(); anim = null; }
      if (open) band.classList.add('blockr-settings--open');
      if (still || typeof band.animate !== 'function') {
        if (!open) band.classList.remove('blockr-settings--open');
        return;
      }
      // Animate from nothing to the band's natural size (or back). The
      // clip keeps the beak, which sits above the band, visible throughout.
      var cs = getComputedStyle(band);
      var full = {
        height: band.offsetHeight + 'px',
        paddingTop: cs.paddingTop, paddingBottom: cs.paddingBottom,
        marginBottom: cs.marginBottom
      };
      var none = { height: '0px', paddingTop: '0px', paddingBottom: '0px',
                   marginBottom: '0px' };
      band.style.clipPath = 'inset(-12px 0 0 0)';
      anim = band.animate(open ? [none, full] : [full, none],
                          { duration: 220, easing: 'ease' });
      anim.onfinish = function () {
        anim = null;
        band.style.clipPath = '';
        if (!open) band.classList.remove('blockr-settings--open');
      };
    }

    gear.addEventListener('click', function () { set(!open); });

    return {
      set: set,
      toggle: function () { set(!open); },
      isOpen: function () { return open; }
    };
  }

  /**
   * A segmented control (design system): a fixed choice of two or three short
   * values, all in view, the pick in the accent tint. 42px in the field grid;
   * `size: 'xs'` is the 26px form for a row or a bar.
   * @param {{ value: string, label: string, title?: string }[]} options
   * @param {string} selected
   * @param {(value: string) => void} onChange
   * @param {{ size?: 'xs', label?: string }} [opts]
   * @returns {BlockrSegmentedHandle}
   */
  function segmented(options, selected, onChange, opts) {
    var wrap = document.createElement('div');
    wrap.className = 'blockr-segmented' +
      (opts && opts.size === 'xs' ? ' blockr-segmented--xs' : '');
    wrap.setAttribute('role', 'radiogroup');
    if (opts && opts.label) wrap.setAttribute('aria-label', opts.label);
    var current = selected;
    /** @type {Record<string, HTMLButtonElement>} */
    var segs = {};
    /** @param {string} v */
    function set(v) {
      current = v;
      Object.keys(segs).forEach(function (k) {
        var on = k === v;
        segs[k].classList.toggle('is-selected', on);
        segs[k].setAttribute('aria-checked', on ? 'true' : 'false');
      });
    }
    options.forEach(function (o) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'blockr-segmented__seg';
      b.textContent = o.label;
      // A caller passes `title` where the label says too little: it explains
      // a terse label (`%`, `All`) or names an icon-only segment. It is the
      // tooltip either way. A screen reader gets it as the description, or,
      // without a label, as the name: the tooltip only describes, and only
      // while it shows.
      if (o.title) {
        Blockr.tooltip.set(b, o.title);
        b.setAttribute(o.label ? 'aria-description' : 'aria-label', o.title);
      }
      b.setAttribute('role', 'radio');
      b.addEventListener('click', function () {
        if (current === o.value) return;
        set(o.value);
        onChange(o.value);
      });
      segs[o.value] = b;
      wrap.appendChild(b);
    });
    set(current);
    return { el: wrap, set: set, get: function () { return current; } };
  }

  Blockr.checkbox = checkbox;
  Blockr.gearTray = gearTray;
  Blockr.segmented = segmented;
})();

/**
 * Blockr.dropdown: a panel that opens under its toggle and stays open while
 * it is worked in, for a menu that holds more than rows to pick (a search
 * field, a list that is edited in place, a checkbox). A click outside,
 * Escape or opening another dropdown closes it; a click inside does not.
 * Blockr.actionMenu is the other kind, a list of rows that closes on a pick.
 *
 * The markup comes from R, from dropdown():
 *
 *   <div class="blockr-dropdown" data-align="end">
 *     <button class="blockr-dropdown__toggle" aria-expanded="false">...</button>
 *     <div class="blockr-dropdown__panel blockr-menu">...</div>
 *   </div>
 *
 * Closed, the panel waits hidden beside its toggle. Open, it is on <body>
 * (Blockr.portal), placed under the toggle with Blockr.place, so no
 * ancestor's overflow or stacking clips it, as an action menu's list is;
 * closing moves it back. Shiny's bindings go with the node, so the inputs
 * and outputs in it work while it is open, and a dropdown that leaves the
 * page while open (its markup rendered again) closes, its panel unbound and
 * removed. The open
 * panel is a layer on the dismiss stack (Blockr.layer), which closes it on
 * Escape and on a click outside. The wrapper carries `.is-open` while it
 * is open and fires `blockr:dropdown-shown` and `blockr:dropdown-hidden`,
 * which bubble.
 */
Blockr.dropdown = (() => {
  /** @type {{ wrap: HTMLElement, toggle: HTMLElement, panel: HTMLElement,
   *           layer: BlockrLayerHandle, placed: BlockrPlaceHandle,
   *           lifted: BlockrPortalHandle } | null} */
  let open = null;

  /** @param {Element | null} el */
  const wrapOf = (el) => /** @type {HTMLElement | null} */ (
    el && el.closest('.blockr-dropdown'));

  /** @param {HTMLElement} wrap @param {string} part */
  const partOf = (wrap, part) => /** @type {HTMLElement | null} */ (
    wrap.querySelector(`:scope > .blockr-dropdown__${part}`));

  /** @param {HTMLElement} wrap @param {string} name */
  const fire = (wrap, name) => {
    wrap.dispatchEvent(new CustomEvent(name, { bubbles: true }));
  };

  /** @param {boolean} [refocus] */
  const hide = (refocus) => {
    if (!open) return;
    const { wrap, toggle, layer, placed, lifted } = open;
    open = null;
    layer.remove();
    placed.stop();
    wrap.classList.remove('is-open');
    lifted.restore();
    toggle.setAttribute('aria-expanded', 'false');
    if (refocus && toggle.isConnected) toggle.focus();
    fire(wrap, 'blockr:dropdown-hidden');
  };

  /** @param {HTMLElement} wrap */
  const show = (wrap) => {
    if (open && open.wrap === wrap) return;
    hide();
    const toggle = partOf(wrap, 'toggle');
    const panel = partOf(wrap, 'panel');
    if (!toggle || !panel) return;
    wrap.classList.add('is-open');
    const lifted = Blockr.portal(panel, toggle, () => hide());
    const placed = Blockr.place(panel, toggle, {
      width: { min: 180, max: 320 },
      align: wrap.getAttribute('data-align') === 'end' ? 'end' : 'start'
    });
    // The stack has taken the layer off before these run; removing it again
    // in hide() does nothing.
    const layer = Blockr.layer(panel, {
      from: toggle,
      escape: () => hide(true),
      outside: () => hide()
    });
    open = { wrap, toggle, panel, layer, placed, lifted };
    toggle.setAttribute('aria-expanded', 'true');
    fire(wrap, 'blockr:dropdown-shown');
  };

  document.addEventListener('click', (e) => {
    const target = e.target instanceof Element ? e.target : null;
    const toggle = target && target.closest('.blockr-dropdown__toggle');
    const wrap = toggle ? wrapOf(toggle) : null;
    if (!wrap || !toggle || toggle.parentElement !== wrap) return;
    if (open && open.wrap === wrap) hide();
    else show(wrap);
  });

  return {
    /**
     * Close the dropdown that holds `el` (or is `el`), or whichever is open.
     * @param {Element} [el]
     */
    hide: (el) => {
      if (!el || (open && (open.wrap === el || open.wrap.contains(el) ||
                           open.panel.contains(el)))) hide();
    },
    /** @param {Element} el the dropdown, or anything inside it */
    show: (el) => {
      const wrap = wrapOf(el);
      if (wrap) show(wrap);
    },
    /** The dropdown that is open, or null. */
    current: () => (open ? open.wrap : null)
  };
})();
