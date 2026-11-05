// @ts-check
/**
 * Blockr.Select: the dropdown every blockr block uses.
 *
 * Single and multi are two widgets on one list, filter, keyboard, placement
 * and server search.
 *
 * `single` shows its pick in a control and closes on a pick.
 *
 * `multi` shows each pick once. The field has the picks as text
 * ("Placebo, Xanomeline High Dose", or "5 of 48" and the names muted), not
 * as tags, and the list is the only place to change them. The open list has
 * the picks on top, under "Selected", then the other options in their own
 * order; a tick moves a row up, an untick moves it back, both animated. The
 * list head has the count, "All" and "None" (`all: false` drops All); with a
 * filter typed they act on the matching rows, and in server-search mode All
 * waits for a search. The picks under "Selected" drag into order unless the
 * caller passes `reorderable: false`. `min` (default 0) is the fewest picks
 * the caller accepts: None is disabled where it would leave fewer, the last
 * picks do not untick, and `minHint` says why. Options may carry a `group`,
 * which heads the rest of the list, and `keywords`, which the filter matches
 * with the value, the shown text and the label.
 *
 * A multi comes in three placements of the one list. A field (`multi`) and a
 * word (`menu` with `mode: 'multi'`) open the list under them and report the
 * picks once, when it closes. In place (`list`) the list is the control,
 * always open; there is no close, so the picks are reported after a pause in
 * the picking (`delay`, 1 s). A long list in place folds what is not picked
 * under "+ Add …" until the filter is used or the fold is opened.
 *
 * `menu` is the list alone, hung under an element the caller owns (a word in
 * a block's sentence), open from the start and gone when it closes.
 *
 * The public surface is described in types.d.ts (BlockrSelect*). Other
 * packages read the markup (blockr.dm reads .blockr-select__dropdown,
 * blockr.extra reads .blockr-select__value) and several style it, so the
 * class names are part of that surface.
 *
 * Depends on: blockr-ui.js (Blockr.uid, removeNode, place, layer, tooltip)
 * and Blockr.icons, which controls_dep() writes into the page.
 */
(() => {
  'use strict';

  /** @param {BlockrSelectOption} o @returns {string} */
  const optValue = (o) => (typeof o === 'object' && o !== null ? o.value : o);
  /** @param {BlockrSelectOption} o @returns {string} */
  const optLabel = (o) => (typeof o === 'object' && o !== null ? (o.label || '') : '');
  /** @param {BlockrSelectOption} o @returns {string} */
  const optGroup = (o) => (typeof o === 'object' && o !== null ? (o.group || '') : '');
  /** @param {BlockrSelectOption[]} opts @param {string} val */
  const findOpt = (opts, val) => opts.find((o) => optValue(o) === val);
  /** @param {string[]} a @param {string[]} b */
  const sameList = (a, b) => a.length === b.length && a.every((v, i) => v === b[i]);

  /** Both facts, and `labelFirst` decides which one leads.
   *
   * A menu opened from a word has to lead with the word that was clicked. A
   * sentence printing `{label(@color)}` says "Actual Treatment", and a list
   * whose rows read "TRTA *Actual Treatment*" puts what the reader just
   * clicked in the muted half of the row and bolds a string that is nowhere
   * on screen. blockr.docs design-system/pinned-controls.md.
   *
   * A multi row puts the lead in its own span (`blockr-select__name`), which
   * ellipsizes on its own so the label and the grip keep their room, and
   * shows an option's `display` in place of its value.
   *
   * @param {HTMLElement} el @param {BlockrSelectOption} o
   * @param {boolean} [labelFirst] @param {boolean} [multi]
   */
  const fillOptContent = (el, o, labelFirst, multi) => {
    el.textContent = '';
    const val = optValue(o);
    const lbl = optLabel(o);
    const shown = multi && typeof o === 'object' && o !== null && o.display ? o.display : val;
    const lead = labelFirst && lbl ? lbl : shown;
    const trail = labelFirst && lbl ? val : lbl;
    if (multi) {
      const name = document.createElement('span');
      name.className = 'blockr-select__name';
      name.textContent = String(lead);
      el.appendChild(name);
    } else {
      el.appendChild(document.createTextNode(lead));
    }
    // The element (or an ancestor) ellipsizes on overflow; the full text
    // shows on hover while it is cut off.
    Blockr.tooltip.set(el, { name: String(val), label: lbl }, { overflow: true });
    if (trail) {
      const span = document.createElement('span');
      span.className = 'blockr-select__opt-label';
      span.textContent = trail;
      el.appendChild(span);
    }
  };

  /** @param {any} x @returns {BlockrSelectOption[]} */
  const toOptions = (x) => (Array.isArray(x) ? x : (x != null ? [x] : []));
  /**
   * A multi-select's selection is an array, but a caller may hand over a
   * scalar (a block constructed with `names_from = "y"`). Coerce rather than
   * throw: an unhandled throw here aborts the rest of the Shiny message
   * batch, which can drop later custom messages (a chart's drilldown-data,
   * say) and leave those blocks blank.
   * @param {any} x @returns {string[]}
   */
  const toArray = (x) => (Array.isArray(x) ? x.slice() : (x != null && x !== '' ? [x] : []));

  // Cap how many options get DOM nodes per render. The full list stays
  // searchable; rendering 50K divs froze the tab on every open and keystroke.
  const MAX_ROWS = 200;
  // Past this many options a list shows its filter box.
  const SEARCH_AFTER = 8;
  const SEARCH_DEBOUNCE = 250;
  // A multi row's slide to and from "Selected".
  const MOVE_MS = 240;
  // A multi in place reports this long after the last pick, so quick picks
  // are one report, and folds the unpicked rows of a list longer than this.
  const REPORT_DELAY = 1000;
  const FOLD_AFTER = 12;

  /** @param {string} cls @param {string} [text] */
  const div = (cls, text) => {
    const el = document.createElement('div');
    el.className = cls;
    if (text != null) el.textContent = text;
    return el;
  };

  const reducedMotion = () => typeof window.matchMedia === 'function' &&
    window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  /** @param {HTMLInputElement} input */
  const comboboxAttrs = (input) => {
    input.type = 'text';
    input.setAttribute('role', 'combobox');
    input.setAttribute('aria-expanded', 'false');
    input.setAttribute('aria-haspopup', 'listbox');
    input.setAttribute('aria-autocomplete', 'list');
    input.setAttribute('autocomplete', 'off');
    input.setAttribute('autocorrect', 'off');
    input.setAttribute('autocapitalize', 'off');
    input.setAttribute('spellcheck', 'false');
  };

  /**
   * The field's text for a set of picks: one or two by name, more as a count
   * with the names after it. Never "All": the picks are a list of values,
   * and a value that turns up in the data later is not among them. `total`
   * is null while the options are not loaded (a list that fetches its values
   * on the first open), and then the names alone show, since a count against
   * the picks themselves would read as every value.
   * @param {string[]} sel @param {number | null} total @param {string} placeholder
   * @returns {{ lead: string, rest: string, empty: boolean }}
   */
  const summary = (sel, total, placeholder) => {
    if (!sel.length) return { lead: placeholder, rest: '', empty: true };
    if (sel.length <= 2 || total == null) return { lead: sel.join(', '), rest: '', empty: false };
    return { lead: `${sel.length} of ${total.toLocaleString()}`, rest: sel.join(', '), empty: false };
  };

  // =========================================================================
  // Single
  // =========================================================================

  /**
   * @param {HTMLElement} container
   * @param {BlockrSelectSingleConfig & BlockrSelectMenuConfig} config
   * @param {HTMLElement | null} anchor Set by Blockr.Select.menu(): the list
   *   alone, hung under `anchor`, open from the start. The control is built
   *   but never mounted; the search input moves into the list's head.
   * @returns {BlockrSelectSingleHandle}
   */
  const createSingle = (container, config, anchor) => {
    const headless = !!anchor;
    const id = Blockr.uid('bsel');
    const listId = `${id}-lb`;

    // --- Settings: fixed for the life of the widget ------------------------

    // Opt out of the first-option fallback so '' means "nothing selected"
    // and survives setOptions(). Without it any option refresh silently
    // picks option 0, which is fine for a column picker and wrong wherever
    // an unrequested pick would change what the user is looking at.
    const allowEmpty = config.allowEmpty === true;
    const placeholder = config.placeholder || '';
    const labelFirst = config.labelFirst === true;
    // What a screen reader calls the combobox and its list: the field's
    // label, which the caller knows and the select does not. A menu falls
    // back to its title.
    const label = config.label || (headless && config.title) || '';
    // `search: false` never shows the filter box (a short fixed list such as
    // operators or join types).
    const searchAfter = config.search === false ? Infinity : SEARCH_AFTER;
    const onChange = config.onChange || null;
    const onOpen = config.onOpen || null;
    const onClose = config.onClose || null;
    const onSearch = config.onSearch || null;

    // --- State: everything render() reads ----------------------------------
    //
    // Events change these fields and call render(); nothing else touches the
    // DOM. The keyboard row is `highlight`, an index into the rows the last
    // render showed, clamped there, so it can never point at a row that is
    // not on screen.
    const st = {
      options: toOptions(config.options),
      /** @type {string} */
      selected: '',
      query: '',
      open: false,
      highlight: 0,
      // The keyboard row is shown only once the keyboard is in use: opened
      // with a key, an arrow pressed, a query typed. Opened with the mouse,
      // a grey first row reads as a default pick, which it is not.
      keyboard: false,
      loading: !!config.loading,
      // Server-search mode (high-cardinality columns): the option list is a
      // server-truncated page; typing re-queries through onSearch instead of
      // relying on the client-side filter alone. Set by setSearchInfo().
      serverSearch: false,
      serverTotal: 0,
      // Shown in text-disabled, never opens, out of the tab order (the
      // combobox input carries the disabled attribute). setDisabled() flips
      // it after construction.
      disabled: config.disabled === true,
      destroyed: false
    };
    st.selected = config.selected != null
      ? /** @type {string} */ (config.selected)
      : (allowEmpty || st.options.length === 0 ? '' : optValue(st.options[0]));
    /** The rows the last render showed; what Enter and the arrows walk. */
    /** @type {BlockrSelectOption[]} */
    let rows = [];

    // --- DOM ---------------------------------------------------------------

    const root = div('blockr-select blockr-select--single');
    // A standalone field (in the grid, in the gear tray) is 42px and
    // bordered; inside a row the select stays bare.
    if (config.bordered === true) root.classList.add('blockr-select--bordered');
    if (headless) root.classList.add('blockr-select--headless');

    const control = div('blockr-select__control');
    const face = document.createElement('span');
    face.className = 'blockr-select__value';

    // The combobox (WAI-ARIA 1.2) is the input: it is what holds focus, what
    // the arrows and Enter arrive at, and what a screen reader announces. The
    // root is a plain wrapper.
    const input = document.createElement('input');
    input.className = 'blockr-select__search';
    comboboxAttrs(input);
    input.setAttribute('aria-controls', listId);
    if (label) input.setAttribute('aria-label', label);

    control.appendChild(face);
    control.appendChild(input);
    // The control ends on the chevron, turned up while the list is open
    // (design system, Chevrons).
    const arrow = document.createElement('span');
    arrow.className = 'blockr-select__arrow';
    arrow.innerHTML = Blockr.icons.chevron;
    control.appendChild(arrow);

    // The panel is the floating surface; the listbox inside it holds the
    // rows and nothing else, since a listbox may contain only options. Only
    // the listbox is rebuilt on a render. A menu keeps its title and its
    // filter box in the panel above it, so a keystroke cannot re-create (or
    // move, which blurs) the input the user is typing into.
    const list = div('blockr-select__dropdown blockr-select__dropdown--single');
    const listbox = div('blockr-select__listbox');
    listbox.id = listId;
    listbox.setAttribute('role', 'listbox');
    if (label) listbox.setAttribute('aria-label', label);
    if (headless) {
      // One sticky element holds the title and the filter box, so a long
      // list scrolls under both (design system, Menus).
      const head = div('blockr-select__head');
      list.classList.add('blockr-select__dropdown--menu');
      if (config.title) head.appendChild(div('blockr-select__menu-title', config.title));
      // Always mounted, because focus is what makes the arrows, Enter and
      // type-ahead work; shown only once the list is long enough to need
      // filtering (render() decides).
      input.classList.add('blockr-select__search--menu');
      input.setAttribute('placeholder', config.searchPlaceholder || 'Filter');
      head.appendChild(input);
      list.appendChild(head);
    }
    list.appendChild(listbox);

    if (!headless) root.appendChild(control);
    container.appendChild(root);

    // --- Render ------------------------------------------------------------

    /** The options the list offers for the current query, capped. */
    const filtered = () => {
      const q = st.query.toLowerCase();
      /** @type {BlockrSelectOption[]} */
      const out = [];
      let extra = 0;
      for (const opt of st.options) {
        const val = optValue(opt);
        // `val` may be a number (numeric value pickers send JSON numbers).
        if (q && String(val).toLowerCase().indexOf(q) < 0 &&
            optLabel(opt).toLowerCase().indexOf(q) < 0) continue;
        if (out.length < MAX_ROWS) out.push(opt);
        else extra++;
      }
      return { rows: out, extra };
    };

    const renderFace = () => {
      // A menu has no face on screen, and its filter box keeps the caller's
      // prompt.
      if (headless) return;
      const sel = st.selected;
      if (sel !== '') {
        const opt = findOpt(st.options, sel);
        if (opt) fillOptContent(face, opt);
        else face.textContent = String(sel);
        face.classList.remove('blockr-select__value--placeholder');
        // While open the input replaces the face, so the pick shows there.
        input.setAttribute('placeholder', st.open ? String(sel) : '');
      } else {
        face.textContent = placeholder;
        face.classList.add('blockr-select__value--placeholder');
        input.setAttribute('placeholder', placeholder);
      }
      // The face has pointer-events: none, so hover happens on the control;
      // the tooltip shows while the face is cut off.
      if (sel === '') Blockr.tooltip.clear(control);
      else {
        const opt = findOpt(st.options, sel);
        Blockr.tooltip.set(control,
          { name: String(sel), label: opt ? optLabel(opt) : '' }, { overflow: true });
      }
    };

    const renderList = () => {
      listbox.textContent = '';
      input.removeAttribute('aria-activedescendant');
      list.style.display = st.open ? 'block' : '';
      if (!st.open) { rows = []; return; }

      const f = filtered();
      rows = f.rows;
      if (st.loading) {
        listbox.appendChild(div('blockr-select__empty', 'Loading…'));
        return;
      }
      if (!rows.length) {
        listbox.appendChild(div('blockr-select__empty',
          st.query ? 'No matches' : 'No options'));
        return;
      }
      st.highlight = Math.max(0, Math.min(st.highlight, rows.length - 1));
      rows.forEach((opt, i) => {
        const val = optValue(opt);
        const row = div('blockr-select__option');
        const picked = val === st.selected;
        if (st.keyboard && i === st.highlight) row.classList.add('blockr-select__option--highlighted');
        if (picked) row.classList.add('blockr-select__option--selected');
        row.setAttribute('role', 'option');
        row.id = `${id}-opt-${i}`;
        row.setAttribute('aria-selected', picked ? 'true' : 'false');
        row.setAttribute('data-value', val);
        fillOptContent(row, opt, labelFirst);
        listbox.appendChild(row);
      });
      if (st.serverSearch) {
        listbox.appendChild(div('blockr-select__empty',
          `${st.serverTotal.toLocaleString()} values — type to search`));
      } else if (f.extra > 0) {
        listbox.appendChild(div('blockr-select__empty',
          `+${f.extra.toLocaleString()} more — type to narrow`));
      }
      if (st.keyboard) input.setAttribute('aria-activedescendant', `${id}-opt-${st.highlight}`);
      else input.removeAttribute('aria-activedescendant');
    };

    const render = () => {
      if (st.destroyed) return;
      renderFace();
      root.classList.toggle('blockr-select--open', st.open);
      root.classList.toggle('blockr-select--disabled', st.disabled);
      input.disabled = st.disabled;
      input.setAttribute('aria-expanded', st.open ? 'true' : 'false');
      // A short menu still filters as you type, as a native menu does; only
      // the box is kept off screen (display: none could not hold focus).
      if (headless) {
        input.classList.toggle('blockr-select__search--offscreen',
          st.options.length <= searchAfter);
      }
      renderList();
      if (placed) placed.update();
    };

    // Scrolls the keyboard row into view, or without one the pick.
    const showHighlight = () => {
      (list.querySelector('.blockr-select__option--highlighted') ||
       list.querySelector('.blockr-select__option--selected'))
        ?.scrollIntoView({ block: 'nearest' });
    };

    // --- Placement and dismissing ------------------------------------------

    // The list lives on <body> while open so it escapes any clipping or
    // stacking-context ancestor (dock panels, offcanvas, modals; see
    // blockr.design/open/blockr-select-portal). Blockr.place keeps it under
    // the control, or under the caller's anchor when there is no control.
    /** @type {BlockrPlaceHandle | null} */
    let placed = null;
    // The open list is a layer (Blockr.layer), on the stack while it lasts.
    /** @type {BlockrLayerHandle | null} */
    let listLayer = null;

    // --- Open, close, pick -------------------------------------------------

    /** @param {boolean} [byKeyboard] */
    const open = (byKeyboard) => {
      if (st.open || st.destroyed || st.disabled) return;
      st.open = true;
      st.keyboard = !!byKeyboard || !!st.query;
      // A single select opens on its pick, so the keyboard row and the pick
      // start as the same row. Typing into a closed select opens it on the
      // typed query instead, from the top.
      st.highlight = 0;
      if (!st.query && st.selected !== '') {
        st.highlight = Math.max(0, st.options.findIndex((o) => optValue(o) === st.selected));
      }
      if (list.parentElement !== document.body) document.body.appendChild(list);
      render();
      placed = Blockr.place(list, anchor || root, {
        // A field dropdown is the control's width, at least 190px; a word
        // is not a control, so its menu sizes to its own content within
        // 180 to 320px (design system, Menus).
        width: headless ? { min: 180, max: 320 } : 'anchor',
        minWidth: 190,
        // The word can be redrawn under an open menu; the caller says how to
        // find it again.
        reanchor: typeof config.reanchor === 'function' ? config.reanchor : undefined,
        onFlip: (above) => root.classList.toggle('blockr-select--above', above)
      });
      // Escape closes the list and leaves the focus on the input, the
      // combobox (a menu hands it back to its anchor when it tears down). A
      // click outside the list and the control closes it too; a menu's
      // anchor is not outside, as its click is the caller's toggle.
      listLayer = Blockr.layer(list, { from: anchor || root, escape: close, outside: close });
      // The pick may be far down a long column list.
      showHighlight();
      input.focus();
      if (onOpen) onOpen();
    };

    const close = () => {
      if (!st.open) return;
      st.open = false;
      st.query = '';
      input.value = '';
      st.keyboard = false;
      if (placed) { placed.stop(); placed = null; }
      if (listLayer) { listLayer.remove(); listLayer = null; }
      root.classList.remove('blockr-select--above');
      render();
      // Last, with the DOM settled: a menu tears the whole widget down from
      // here.
      if (onClose) onClose();
    };

    /** @param {string} value */
    const pick = (value) => {
      const changed = st.selected !== value;
      st.selected = value;
      close();
      render();
      if (changed && onChange) onChange(st.selected);
    };

    // --- Events ------------------------------------------------------------

    control.addEventListener('click', () => {
      if (st.disabled) return;
      if (st.open) close();
      else open();
    });

    list.addEventListener('click', (e) => {
      const row = /** @type {Element} */ (e.target).closest('.blockr-select__option');
      if (!row) return;
      const val = row.getAttribute('data-value');
      if (val != null) pick(val);
    });

    /** @type {ReturnType<typeof setTimeout> | null} */
    let searchTimer = null;
    input.addEventListener('input', () => {
      st.query = input.value;
      st.highlight = 0;
      st.keyboard = true;
      if (st.open) render();
      else open(true);
      // Server search: re-query after the user pauses. The client-side filter
      // gives instant feedback on the loaded page; the server response then
      // replaces the option list through updateOptions.
      if (st.serverSearch && onSearch) {
        if (searchTimer) clearTimeout(searchTimer);
        searchTimer = setTimeout(() => onSearch(st.query), SEARCH_DEBOUNCE);
      }
    });

    /** @param {number} step */
    const move = (step) => {
      const n = rows.length || 1;
      // The first arrow press shows the row the list opened on; the next
      // ones move it.
      if (!st.keyboard) st.keyboard = true;
      else st.highlight = (st.highlight + step + n) % n;
      renderList();
      showHighlight();
    };

    input.addEventListener('keydown', (e) => {
      switch (e.key) {
        case 'ArrowDown':
        case 'ArrowUp':
          e.preventDefault();
          if (st.open) move(e.key === 'ArrowDown' ? 1 : -1);
          else open(true);
          break;
        case 'Enter':
          e.preventDefault();
          if (!st.open) open(true);
          // With no keyboard row on screen, Enter shows it rather than
          // picking a row the user cannot see.
          else if (!st.keyboard) { st.keyboard = true; renderList(); showHighlight(); }
          else if (rows[st.highlight]) pick(optValue(rows[st.highlight]));
          break;
        case ' ':
          // Opens a closed select as a native one does; once open, a space
          // is typing.
          if (!st.open) { e.preventDefault(); open(true); }
          break;
        case 'Tab':
          close();
          break;
      }
    });

    // --- Reconciling -------------------------------------------------------

    /**
     * Point the widget at a new list and settle the pick against it. Falls
     * back to the first option when `sel` is absent or unknown, unless
     * allowEmpty, where '' survives, an unknown pick clears, and omitting
     * `sel` keeps a still-valid pick.
     * @param {any} opts @param {any} sel
     */
    const reconcile = (opts, sel) => {
      st.options = toOptions(opts);
      const vals = st.options.map(optValue);
      if (sel != null && vals.indexOf(sel) >= 0) {
        st.selected = sel;
      } else if (allowEmpty) {
        st.selected = sel == null && vals.indexOf(st.selected) >= 0 ? st.selected : '';
      } else {
        st.selected = st.options.length ? optValue(st.options[0]) : '';
      }
      render();
    };

    render();
    // A menu is open from the start: there is no control to click.
    if (headless) open();

    return {
      el: root,
      setOptions: reconcile,
      /**
       * As setOptions with the current list; never reports.
       * @param {any} value
       */
      setValue(value) { reconcile(st.options, value); },
      getValue() { return st.selected || ''; },
      /**
       * Swap the list without touching the pick. With `sel`, force the pick
       * even if the list lacks it: for a caller that OWNS the value and only
       * borrows the widget to show it, such as a column picker showing the
       * column a board restored while the frame that has it is still loading
       * (Blockr.reconcileColumn). Nothing else may set a pick the list does
       * not contain.
       * @param {any} opts @param {any} [sel]
       */
      updateOptions(opts, sel) {
        st.options = toOptions(opts);
        if (sel != null) st.selected = sel;
        render();
      },
      /** @param {boolean} flag */
      setLoading(flag) { st.loading = !!flag; render(); },
      /**
       * Grey the control out and take it out of the tab order, or put it
       * back. Disabling an open select closes it first.
       * @param {boolean} flag
       */
      setDisabled(flag) {
        st.disabled = !!flag;
        if (st.disabled) close();
        render();
      },
      // Enter or leave server-search mode from a column-values response.
      // `truncated` means the full value list exceeds the server's limit
      // (sticky across queries); `total` is the full distinct count.
      /** @param {{ total?: number, truncated?: boolean } | null | undefined} info */
      setSearchInfo(info) {
        st.serverSearch = !!(info && info.truncated);
        st.serverTotal = (info && info.total) || 0;
        render();
      },
      destroy() {
        if (st.destroyed) return;
        if (searchTimer) clearTimeout(searchTimer);
        close();
        st.destroyed = true;
        Blockr.removeNode(list);
        Blockr.removeNode(root);
      }
    };
  };

  // =========================================================================
  // Multi
  // =========================================================================

  /**
   * @param {HTMLElement} container
   * @param {BlockrSelectMultiConfig & BlockrSelectMenuConfig} config
   * @param {HTMLElement | null} anchor Set by the menu form: the list alone,
   *   hung under `anchor`, open from the start.
   * @param {boolean} [inline] The list form: the list in place, always open.
   * @returns {BlockrSelectMultiHandle}
   */
  const createMulti = (container, config, anchor, inline = false) => {
    const headless = !!anchor || inline;
    const id = Blockr.uid('bsel');
    const listId = `${id}-lb`;

    const placeholder = config.placeholder || (headless ? '' : 'None');
    const labelFirst = config.labelFirst === true;
    const label = config.label || (headless && config.title) || '';
    const searchAfter = config.search === false ? Infinity : SEARCH_AFTER;
    // The picks drag into order unless the caller opts out.
    const reorderable = config.reorderable !== false;
    const onChange = config.onChange || null;
    const onOpen = config.onOpen || null;
    const onClose = config.onClose || null;
    const onSearch = config.onSearch || null;
    const countLabel = config.countLabel || 'selected';
    // `all: false` drops the All button, where picking everything at once is
    // never meant (every panel on one profile).
    const showAll = config.all !== false;
    const min = typeof config.min === 'number' && config.min > 0 ? config.min : 0;
    const minHint = config.minHint || (min === 1 ? 'At least one is needed' : `At least ${min} are needed`);
    const delay = typeof config.delay === 'number' ? config.delay : REPORT_DELAY;
    // Fold only in place; a dropdown is already the open state.
    const foldAfter = !inline || config.fold === false ? Infinity
      : (typeof config.fold === 'number' ? config.fold : FOLD_AFTER);

    const st = {
      options: toOptions(config.options),
      selected: toArray(config.selected),
      query: '',
      open: false,
      highlight: 0,
      keyboard: false,
      loading: !!config.loading,
      serverSearch: false,
      serverTotal: 0,
      disabled: config.disabled === true,
      // The picks the dropdown opened with, to tell on close whether to
      // report.
      openedWith: /** @type {string[] | null} */ (null),
      // In place, a long list: the unpicked rows show.
      unfolded: false,
      destroyed: false
    };
    /** The option rows the last render showed, in screen order. */
    /** @type {BlockrSelectOption[]} */
    let rows = [];

    // --- DOM ---------------------------------------------------------------

    const root = div('blockr-select blockr-select--multi');
    if (config.bordered === true) root.classList.add('blockr-select--bordered');
    if (headless && !inline) root.classList.add('blockr-select--headless');
    if (inline) root.classList.add('blockr-select--list');

    // The field: the picks as text and the chevron. It takes focus itself,
    // since the filter box is in the list.
    const control = div('blockr-select__control');
    control.tabIndex = st.disabled ? -1 : 0;
    control.setAttribute('role', 'button');
    control.setAttribute('aria-haspopup', 'listbox');
    control.setAttribute('aria-controls', listId);
    control.setAttribute('aria-expanded', 'false');
    if (label) control.setAttribute('aria-label', label);
    const face = document.createElement('span');
    face.className = 'blockr-select__value blockr-select__face';
    const faceLead = document.createElement('span');
    faceLead.className = 'blockr-select__lead';
    const faceRest = document.createElement('span');
    faceRest.className = 'blockr-select__rest';
    face.append(faceLead, faceRest);
    control.appendChild(face);
    const arrow = document.createElement('span');
    arrow.className = 'blockr-select__arrow';
    arrow.innerHTML = Blockr.icons.chevron;
    control.appendChild(arrow);

    // The combobox is the filter box in the list head: what holds focus
    // while open, what the arrows and Enter reach.
    const input = document.createElement('input');
    input.className = 'blockr-select__search blockr-select__search--menu';
    comboboxAttrs(input);
    input.setAttribute('aria-controls', listId);
    input.setAttribute('placeholder', config.searchPlaceholder || 'Filter');
    if (label) input.setAttribute('aria-label', label);

    const list = div(inline ? 'blockr-select__panel blockr-select__panel--list'
      : 'blockr-select__dropdown blockr-select__dropdown--multi blockr-select__dropdown--menu blockr-select__panel');
    const head = div('blockr-select__head');
    if (headless && !inline && config.title) head.appendChild(div('blockr-select__menu-title', config.title));
    head.appendChild(input);
    // "3 of 12 selected", then All and None.
    const bulk = div('blockr-select__bulk');
    const count = document.createElement('span');
    count.className = 'blockr-select__count';
    const allBtn = document.createElement('button');
    allBtn.type = 'button';
    allBtn.className = 'blockr-select__bulk-btn';
    const noneBtn = document.createElement('button');
    noneBtn.type = 'button';
    noneBtn.className = 'blockr-select__bulk-btn';
    const btns = document.createElement('span');
    btns.className = 'blockr-select__bulk-btns';
    if (showAll) btns.appendChild(allBtn);
    btns.appendChild(noneBtn);
    bulk.append(count, btns);
    head.appendChild(bulk);
    list.appendChild(head);
    const listbox = div('blockr-select__listbox');
    listbox.id = listId;
    listbox.setAttribute('role', 'listbox');
    listbox.setAttribute('aria-multiselectable', 'true');
    if (label) listbox.setAttribute('aria-label', label);
    list.appendChild(listbox);

    if (inline) root.appendChild(list);
    else if (!headless) root.appendChild(control);
    container.appendChild(root);

    // --- Render ------------------------------------------------------------

    /** @param {string} val */
    const find = (val) => findOpt(st.options, val);
    /** @param {BlockrSelectOption} o */
    const matches = (o) => {
      const q = st.query.toLowerCase();
      if (!q) return true;
      // The value, what is shown, its label, its group, and any keywords the
      // caller adds (so "liver" can find Chemistry).
      const obj = typeof o === 'object' && o !== null ? o : null;
      const hay = [optValue(o), optLabel(o), obj && obj.display, obj && obj.group,
        obj && obj.keywords];
      return hay.some((t) => t != null && String(t).toLowerCase().indexOf(q) >= 0);
    };
    // Picks the options do not hold (not loaded yet, or gone from the data)
    // still count, so the head never reads "2 of 1".
    const total = () => (st.serverSearch && st.serverTotal) ||
      st.options.length + st.selected.filter((v) => !find(v)).length;
    // The count the field may show: none while a pick is missing from the
    // options (not loaded yet, or gone from the data), where the total would
    // be a guess.
    const knownTotal = () => (st.serverSearch && st.serverTotal) ||
      (st.selected.every((v) => !!find(v)) ? st.options.length : null);

    /**
     * The two groups the list shows: the picks (in pick order, each from the
     * option list or, when the list has not loaded it, as a bare value) and
     * the rest of the options, in their own order.
     */
    const groups = () => {
      const sel = st.selected;
      const picked = sel.map((v) => find(v) || v).filter(matches);
      /** @type {BlockrSelectOption[]} */
      const rest = [];
      let extra = 0;
      for (const o of st.options) {
        if (sel.indexOf(optValue(o)) >= 0 || !matches(o)) continue;
        if (rest.length < MAX_ROWS) rest.push(o);
        else extra++;
      }
      // Folded: the picks alone, and a row that opens the rest.
      const folded = st.options.length > foldAfter && !st.unfolded && !st.query;
      return { picked, rest: folded ? [] : rest, extra: folded ? 0 : extra,
        folded: folded ? rest.length + extra : 0 };
    };

    /** Whether a pick may not be unticked: one fewer would be below `min`. */
    const locked = () => min > 0 && st.selected.length <= min;

    const renderFace = () => {
      if (headless) return;
      const s = summary(st.selected, knownTotal(), placeholder);
      faceLead.textContent = s.lead;
      faceRest.textContent = s.rest;
      face.classList.toggle('blockr-select__value--placeholder', s.empty);
      // The tooltip names every pick with its label, one per line, as the
      // tags did. A lone pick without a label would only repeat the field,
      // so its tooltip waits until the field is cut off.
      const named = st.selected.map((v) => {
        const o = find(v);
        return { name: v, label: o ? optLabel(o) : '' };
      });
      if (named.length > 1) Blockr.tooltip.set(control, named);
      else if (named.length === 1) Blockr.tooltip.set(control, named[0], { overflow: !named[0].label });
      else Blockr.tooltip.clear(control);
    };

    const renderBulk = () => {
      const g = st.open ? groups() : null;
      const shown = g ? g.picked.length + g.rest.length : 0;
      const filtered = !!st.query;
      const n = st.selected.length;
      count.textContent = `${n} of ${total().toLocaleString()} ${countLabel}`;
      // All waits for a search while only part of the values are loaded.
      const allOff = !g || (st.serverSearch && !filtered) || !g.rest.length || g.folded > 0;
      allBtn.textContent = filtered ? `All ${shown.toLocaleString()} shown` : 'All';
      allBtn.disabled = allOff;
      if (st.serverSearch && !filtered) {
        Blockr.tooltip.set(allBtn, 'Type to search first: only part of the values are loaded');
      } else Blockr.tooltip.clear(allBtn);
      noneBtn.textContent = filtered ? 'None shown' : 'None';
      noneBtn.disabled = !g || !g.picked.length;
      // Below `min` None stays focusable and hoverable, so it can say why.
      const short = !!g && g.picked.length > 0 && n - g.picked.length < min;
      noneBtn.setAttribute('aria-disabled', short ? 'true' : 'false');
      if (short) Blockr.tooltip.set(noneBtn, minHint);
      else Blockr.tooltip.clear(noneBtn);
      bulk.classList.toggle('blockr-select__bulk--search', st.serverSearch);
    };

    /** @param {BlockrSelectOption} opt @param {number} i @param {boolean} picked */
    const optionRow = (opt, i, picked) => {
      const val = optValue(opt);
      const row = div('blockr-select__option blockr-select__option--multi');
      if (st.keyboard && i === st.highlight) row.classList.add('blockr-select__option--highlighted');
      if (picked) row.classList.add('blockr-select__option--selected');
      if (picked && locked()) {
        row.classList.add('blockr-select__option--locked');
        row.setAttribute('aria-disabled', 'true');
      }
      row.setAttribute('role', 'option');
      row.id = `${id}-opt-${i}`;
      row.setAttribute('aria-selected', picked ? 'true' : 'false');
      row.setAttribute('data-value', val);
      fillOptContent(row, opt, labelFirst, true);
      // Every row keeps the tick's space, so the names line up whether or
      // not they are picked.
      const tick = document.createElement('span');
      tick.className = 'blockr-select__tick';
      tick.setAttribute('aria-hidden', 'true');
      if (picked) tick.innerHTML = Blockr.icons.confirm;
      row.insertBefore(tick, row.firstChild);
      if (picked && reorderable && !st.query && !st.disabled) {
        row.setAttribute('draggable', 'true');
        const grip = document.createElement('span');
        grip.className = 'blockr-select__grip';
        grip.setAttribute('aria-hidden', 'true');
        grip.innerHTML = Blockr.icons.grip || '';
        row.appendChild(grip);
      }
      return row;
    };

    /** A row by its value, a group heading by its name. @param {Element} r */
    const slideKey = (r) => r.hasAttribute('data-group')
      ? `group:${r.getAttribute('data-group')}` : /** @type {string} */ (r.getAttribute('data-value'));

    /** Where each row was, so the next render can slide it from there. */
    const positions = () => {
      /** @type {Map<string, number>} */
      const at = new Map();
      listbox.querySelectorAll('[data-value], [data-group]').forEach((r) => {
        at.set(slideKey(r), r.getBoundingClientRect().top);
      });
      return at;
    };

    /** @param {Map<string, number>} before @param {string | null} moved */
    const slide = (before, moved) => {
      if (!before.size || reducedMotion() || typeof Element.prototype.animate !== 'function') return;
      listbox.querySelectorAll('[data-value], [data-group]').forEach((r) => {
        const v = slideKey(r);
        const was = before.get(v);
        if (was == null) return;
        const dy = was - r.getBoundingClientRect().top;
        if (Math.abs(dy) < 1) return;
        /** @type {Keyframe[]} */
        const frames = v === moved
          ? [{ transform: `translateY(${dy}px)`, backgroundColor: 'var(--blockr-color-bg-accent-subtle)' },
             { transform: 'translateY(0)', backgroundColor: 'transparent' }]
          : [{ transform: `translateY(${dy}px)` }, { transform: 'translateY(0)' }];
        r.animate(frames, { duration: MOVE_MS, easing: 'cubic-bezier(.2,.7,.2,1)' });
      });
    };

    /** @param {string} cls @param {string} text @param {string} key */
    const heading = (cls, text, key) => {
      const h = div(cls, text);
      h.setAttribute('role', 'presentation');
      h.setAttribute('data-group', key);
      return h;
    };

    const renderList = () => {
      listbox.textContent = '';
      input.removeAttribute('aria-activedescendant');
      // A dropdown shows while open; the list in place keeps its own layout.
      if (!inline) list.style.display = st.open ? 'block' : '';
      if (!st.open) { rows = []; return; }
      renderBulk();
      if (st.loading && !st.selected.length) {
        rows = [];
        listbox.appendChild(div('blockr-select__empty', 'Loading…'));
        return;
      }
      const g = groups();
      rows = g.picked.concat(g.rest);
      if (!rows.length && !g.folded) {
        listbox.appendChild(div('blockr-select__empty', st.query ? 'No matches' : 'No options'));
        return;
      }
      st.highlight = Math.max(0, Math.min(st.highlight, rows.length - 1));
      let i = 0;
      if (g.picked.length) {
        listbox.appendChild(heading('blockr-select__group', 'Selected', 'selected'));
        for (const o of g.picked) listbox.appendChild(optionRow(o, i++, true));
        // Named groups head the rest themselves.
        if ((g.rest.length && !optGroup(g.rest[0])) || st.loading) {
          listbox.appendChild(heading('blockr-select__group', 'Other', 'other'));
        }
      }
      if (st.loading) listbox.appendChild(div('blockr-select__empty', 'Loading…'));
      else {
        let grp = '';
        for (const o of g.rest) {
          const og = optGroup(o);
          if (og && og !== grp) {
            grp = og;
            listbox.appendChild(heading('blockr-select__subgroup', og, `sub:${og}`));
          }
          listbox.appendChild(optionRow(o, i++, false));
        }
      }
      if (g.folded) {
        const more = document.createElement('button');
        more.type = 'button';
        more.className = 'blockr-select__unfold';
        more.textContent = `+ ${config.addLabel || 'Add'} (${g.folded.toLocaleString()})`;
        more.addEventListener('click', () => {
          const before = positions();
          st.unfolded = true;
          renderList();
          slide(before, null);
        });
        listbox.appendChild(more);
      } else if (inline && st.unfolded && !st.query && st.options.length > foldAfter) {
        const less = document.createElement('button');
        less.type = 'button';
        less.className = 'blockr-select__unfold';
        less.textContent = 'Show only the picks';
        less.addEventListener('click', () => { st.unfolded = false; renderList(); });
        listbox.appendChild(less);
      }
      if (st.serverSearch) {
        listbox.appendChild(div('blockr-select__empty',
          `${st.serverTotal.toLocaleString()} values — type to search`));
      } else if (g.extra > 0) {
        listbox.appendChild(div('blockr-select__empty',
          `+${g.extra.toLocaleString()} more — type to narrow`));
      }
      if (st.keyboard) input.setAttribute('aria-activedescendant', `${id}-opt-${st.highlight}`);
    };

    const render = () => {
      if (st.destroyed) return;
      if (inline) st.open = true;
      renderFace();
      root.classList.toggle('blockr-select--open', st.open);
      root.classList.toggle('blockr-select--disabled', st.disabled);
      control.tabIndex = st.disabled ? -1 : 0;
      control.setAttribute('aria-expanded', st.open ? 'true' : 'false');
      input.disabled = st.disabled;
      input.setAttribute('aria-expanded', st.open ? 'true' : 'false');
      // A short list still filters as you type; only the box is kept off
      // screen (display: none could not hold focus).
      input.classList.toggle('blockr-select__search--offscreen',
        st.options.length <= searchAfter && !st.serverSearch);
      renderList();
      if (placed) placed.update();
    };

    const showHighlight = () => {
      const r = list.querySelector('.blockr-select__option--highlighted');
      if (r) r.scrollIntoView({ block: 'nearest' });
    };

    // --- Placement and dismissing ------------------------------------------

    /** @type {BlockrPlaceHandle | null} */
    let placed = null;
    /** @type {BlockrLayerHandle | null} */
    let listLayer = null;

    /** @param {boolean} [byKeyboard] */
    const open = (byKeyboard) => {
      if (st.open || st.destroyed || st.disabled) return;
      st.open = true;
      st.keyboard = !!byKeyboard;
      st.highlight = 0;
      st.openedWith = st.selected.slice();
      if (list.parentElement !== document.body) document.body.appendChild(list);
      render();
      placed = Blockr.place(list, anchor || root, {
        width: headless ? { min: 180, max: 320 } : 'anchor',
        minWidth: headless ? 180 : 240,
        reanchor: typeof config.reanchor === 'function' ? config.reanchor : undefined,
        onFlip: (/** @type {boolean} */ above) => root.classList.toggle('blockr-select--above', above)
      });
      listLayer = Blockr.layer(list, {
        from: anchor || root,
        escape: () => close('escape'),
        outside: () => close()
      });
      list.scrollTop = 0;
      input.focus();
      if (onOpen) onOpen();
    };

    /** @param {'escape' | 'destroy'} [how] */
    const close = (how) => {
      if (!st.open) return;
      st.open = false;
      st.query = '';
      input.value = '';
      st.keyboard = false;
      if (placed) { placed.stop(); placed = null; }
      if (listLayer) { listLayer.remove(); listLayer = null; }
      root.classList.remove('blockr-select--above');
      // The picks are reported once, here: a click outside, Tab, the
      // chevron, Escape, or the list destroyed open (a redraw, a caller
      // closing it).
      let changed = false;
      if (st.openedWith) {
        const was = st.openedWith;
        st.openedWith = null;
        changed = !sameList(st.selected, was);
      }
      render();
      if (!headless && how === 'escape') control.focus();
      if (changed) emit();
      if (onClose) onClose();
    };

    const emit = () => {
      if (onChange) onChange(st.selected.slice());
    };

    // In place there is no close to report at: report after a pause.
    /** @type {ReturnType<typeof setTimeout> | null} */
    let reportTimer = null;
    /** @type {string[]} */
    let reported = st.selected.slice();
    const flush = () => {
      if (reportTimer) { clearTimeout(reportTimer); reportTimer = null; }
      if (sameList(st.selected, reported)) return;
      reported = st.selected.slice();
      emit();
    };
    const scheduleReport = () => {
      if (reportTimer) clearTimeout(reportTimer);
      reportTimer = setTimeout(flush, delay);
    };

    /** Change the picks with the list open, sliding the rows that move.
     *  @param {string[]} next @param {string | null} moved */
    const repick = (next, moved) => {
      const before = positions();
      st.selected = next;
      render();
      slide(before, moved);
      if (inline) scheduleReport();
    };

    /** @param {string} value */
    const pick = (value) => {
      const sel = st.selected.slice();
      const i = sel.indexOf(value);
      if (i >= 0 && locked()) return;
      if (i >= 0) sel.splice(i, 1);
      else sel.push(value);
      // The keyboard row follows the row it was on.
      repick(sel, value);
      if (st.keyboard) {
        const at = rows.findIndex((o) => optValue(o) === value);
        if (at >= 0) { st.highlight = at; renderList(); showHighlight(); }
      }
    };

    // Every matching option, not only the rows shown (the list stops at
    // MAX_ROWS).
    const pickAll = () => {
      const add = st.options
        .filter((o) => st.selected.indexOf(optValue(o)) < 0 && matches(o))
        .map(optValue);
      repick(st.selected.concat(add), null);
    };
    const pickNone = () => {
      const drop = groups().picked.map(optValue);
      const next = st.selected.filter((v) => drop.indexOf(v) < 0);
      if (next.length < min) return;
      repick(next, null);
    };

    // --- Events ------------------------------------------------------------

    control.addEventListener('click', () => {
      if (st.disabled) return;
      if (st.open) close();
      else open();
    });
    control.addEventListener('keydown', (e) => {
      if (st.disabled || st.open) return;
      if (e.key === 'Enter' || e.key === ' ' || e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        open(true);
      } else if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        // Typing on the closed field starts the filter.
        open(true);
        input.value = e.key;
        st.query = e.key;
        e.preventDefault();
        render();
      }
    });

    list.addEventListener('mousedown', (e) => {
      // Keep the focus in the filter box while clicking rows. Not on a
      // draggable row: a cancelled mousedown never starts a drag.
      const t = /** @type {Element} */ (e.target);
      if (t === input || t.closest('[draggable="true"]')) return;
      e.preventDefault();
    });
    list.addEventListener('click', (e) => {
      const t = /** @type {Element} */ (e.target);
      if (t.closest('.blockr-select__grip')) return;
      const row = t.closest('.blockr-select__option');
      if (row) {
        const val = row.getAttribute('data-value');
        if (val != null) pick(val);
      }
    });
    allBtn.addEventListener('click', (e) => { e.stopPropagation(); pickAll(); input.focus(); });
    noneBtn.addEventListener('click', (e) => { e.stopPropagation(); pickNone(); input.focus(); });

    /** @type {ReturnType<typeof setTimeout> | null} */
    let searchTimer = null;
    input.addEventListener('input', () => {
      st.query = input.value;
      st.highlight = 0;
      st.keyboard = true;
      if (st.open) render();
      else open(true);
      if (st.serverSearch && onSearch) {
        if (searchTimer) clearTimeout(searchTimer);
        searchTimer = setTimeout(() => onSearch(st.query), SEARCH_DEBOUNCE);
      }
    });

    /** @param {number} step */
    const move = (step) => {
      const n = rows.length || 1;
      if (!st.keyboard) st.keyboard = true;
      else st.highlight = (st.highlight + step + n) % n;
      renderList();
      showHighlight();
    };

    input.addEventListener('keydown', (e) => {
      switch (e.key) {
        case 'ArrowDown':
        case 'ArrowUp':
          e.preventDefault();
          if (st.open) move(e.key === 'ArrowDown' ? 1 : -1);
          else open(true);
          break;
        case 'Enter':
          e.preventDefault();
          if (!st.open) open(true);
          else if (!st.keyboard) { st.keyboard = true; renderList(); showHighlight(); }
          else if (rows[st.highlight]) pick(optValue(rows[st.highlight]));
          break;
        case ' ':
          // Space ticks the highlighted row while the filter is empty; in a
          // filter already typed it is a space.
          if (st.open && st.keyboard && !input.value && rows[st.highlight]) {
            e.preventDefault();
            pick(optValue(rows[st.highlight]));
          }
          break;
        case 'Tab':
          // The list hangs at the end of the page, so the browser's Tab
          // would go on from there: back to the field or the word instead.
          if (!inline) {
            e.preventDefault();
            close();
            const back = headless ? anchor : control;
            if (back && typeof back.focus === 'function') back.focus();
          }
          break;
        case 'Escape':
          // In place there is nothing to close: Escape empties the filter.
          if (inline && st.query) {
            e.preventDefault();
            input.value = '';
            st.query = '';
            render();
          }
          break;
      }
    });

    // --- Drag the picks into order -----------------------------------------
    //
    // Pointer feedback goes straight on the rows; the order changes once, on
    // the drop, and is reported with the rest when the list closes.
    if (reorderable) {
      /** @type {string | null} */
      let dragValue = null;
      /** @type {Element | null} */
      let overRow = null;
      /** @type {'before' | 'after'} */
      let side = 'after';
      const clear = () => {
        listbox.querySelectorAll('.blockr-select__option--drop-before, .blockr-select__option--drop-after')
          .forEach((r) => r.classList.remove('blockr-select__option--drop-before', 'blockr-select__option--drop-after'));
      };
      const dragEnd = () => {
        clear();
        listbox.querySelectorAll('.blockr-select__option--dragging')
          .forEach((r) => r.classList.remove('blockr-select__option--dragging'));
        dragValue = null;
        overRow = null;
      };
      /** @param {Event} e */
      const dragRow = (e) => /** @type {Element} */ (e.target).closest('.blockr-select__option[draggable="true"]');
      listbox.addEventListener('dragstart', (e) => {
        const row = dragRow(e);
        if (!row) return;
        dragValue = row.getAttribute('data-value');
        const dt = /** @type {DataTransfer} */ (e.dataTransfer);
        dt.effectAllowed = 'move';
        dt.setData('text/plain', /** @type {string} */ (dragValue));
        row.classList.add('blockr-select__option--dragging');
      });
      // A row (or the name inside it) accepts the drop from the moment the
      // pointer enters it, not only from its next dragover.
      listbox.addEventListener('dragenter', (e) => {
        if (dragValue != null && dragRow(e)) e.preventDefault();
      });
      listbox.addEventListener('dragover', (e) => {
        if (dragValue == null) return;
        const row = dragRow(e);
        if (!row) return;
        e.preventDefault();
        /** @type {DataTransfer} */ (e.dataTransfer).dropEffect = 'move';
        if (row.getAttribute('data-value') === dragValue) { clear(); overRow = null; return; }
        const r = row.getBoundingClientRect();
        const s = e.clientY < r.top + r.height / 2 ? 'before' : 'after';
        if (row !== overRow || s !== side) {
          clear();
          overRow = row;
          side = s;
          row.classList.add(`blockr-select__option--drop-${s}`);
        }
      });
      listbox.addEventListener('dragend', dragEnd);
      listbox.addEventListener('drop', (e) => {
        e.preventDefault();
        // The row under the pointer at the drop, which a quick drag may not
        // have reported in a dragover yet.
        const row = dragRow(e) || overRow;
        const target = row && row.getAttribute('data-value');
        const moved = dragValue;
        let drop = side;
        if (row) {
          const r = row.getBoundingClientRect();
          drop = e.clientY < r.top + r.height / 2 ? 'before' : 'after';
        }
        dragEnd();
        if (moved == null || target == null || moved === target) return;
        const sel = st.selected.filter((v) => v !== moved);
        sel.splice(sel.indexOf(target) + (drop === 'after' ? 1 : 0), 0, moved);
        repick(sel, null);
      });
    }

    // --- Reconciling -------------------------------------------------------

    /** Picks made here that the caller has not heard yet: in place, those
     *  waiting for the pause; in a dropdown, a change since it opened. */
    const pending = () => (inline ? !!reportTimer
      : st.open && !!st.openedWith && !sameList(st.selected, st.openedWith));

    /** The caller's picks and choices, as R sends them. Picks still pending
     *  here are newer than anything the caller can send, so they stay (less
     *  what is no longer offered) and go out as usual; the caller's answer
     *  to them comes after. Otherwise the caller's picks are taken.
     *  @param {any} opts @param {any} sel */
    const reconcile = (opts, sel) => {
      st.options = toOptions(opts);
      const vals = st.options.map(optValue);
      const hold = pending();
      const take = sel != null && !hold;
      st.selected = toArray(take ? sel : st.selected).filter((v) => vals.indexOf(v) >= 0);
      if (!hold) {
        // What the caller sent is what it knows: no report owed for it.
        if (inline) reported = st.selected.slice();
        else if (st.openedWith) st.openedWith = st.selected.slice();
      }
      render();
    };

    render();
    if (headless && !inline) open();

    return {
      el: root,
      setOptions: reconcile,
      /** As setOptions with the current list; never reports. @param {any} value */
      setValue(value) { reconcile(st.options, value); },
      getValue() { return st.selected.slice(); },
      /**
       * Swap the list without touching the picks (setOptions would drop picks
       * whose value list has not arrived yet). With `sel`, force the picks.
       * @param {any} opts @param {any} [sel]
       */
      updateOptions(opts, sel) {
        st.options = toOptions(opts);
        if (sel != null) st.selected = toArray(sel);
        render();
      },
      /** @param {boolean} flag */
      setLoading(flag) { st.loading = !!flag; render(); },
      /** @param {boolean} flag */
      setDisabled(flag) {
        st.disabled = !!flag;
        if (st.disabled) close();
        render();
      },
      /** @param {{ total?: number, truncated?: boolean } | null | undefined} info */
      setSearchInfo(info) {
        st.serverSearch = !!(info && info.truncated);
        st.serverTotal = (info && info.total) || 0;
        render();
      },
      /** In place: report a pending change now rather than after the pause. */
      flush() { if (inline) flush(); },
      destroy() {
        if (st.destroyed) return;
        if (searchTimer) clearTimeout(searchTimer);
        // A change still waiting for its pause, or made in a list destroyed
        // open, is reported, not lost.
        if (inline) flush();
        else close('destroy');
        st.destroyed = true;
        Blockr.removeNode(list);
        Blockr.removeNode(root);
      }
    };
  };

  // =========================================================================
  // Menu
  // =========================================================================

  /** The list on its own, hung off something the caller owns.
   *
   * For a word in a block's sentence that IS one of its settings: clicking it
   * has to give the list, not a popover holding a control that gives the
   * list. Everything below the surface is the select, because a menu that
   * drifts from the select is a second idiom to maintain. See blockr.docs
   * design-system/pinned-controls.md.
   *
   * Opens immediately and destroys itself when it closes, so the caller keeps
   * a handle only to close it early (a re-render under it, say) or, for a
   * multi menu, to push new choices or picks into it. `mode: 'multi'` keeps
   * the panel open across picks and reports them when it closes, so adding
   * three columns is one gesture.
   *
   * @param {HTMLElement} anchor The element to hang under, usually a word.
   * @param {BlockrSelectMenuConfig} config `title`, `labelFirst`, `mode`,
   *   `searchPlaceholder`, plus the usual options.
   */
  const createMenu = (anchor, config) => {
    const multi = config.mode === 'multi';
    const host = div('blockr-select-menu-host');
    document.body.appendChild(host);
    /** @type {BlockrSelectSingleHandle | BlockrSelectMultiHandle | null} */
    let handle = null;
    let done = false;
    const teardown = () => {
      if (done) return;
      done = true;
      // After the click that closed it has finished: close() runs before the
      // row's own onChange, and destroying here synchronously would pull the
      // DOM out from under it.
      setTimeout(() => {
        if (handle) handle.destroy();
        Blockr.removeNode(host);
        // The filter box had focus and is gone. Unless the closing click put
        // focus somewhere else, hand it back to the word (a no-op on an
        // element that cannot take it).
        const active = document.activeElement;
        if (!active || active === document.body) anchor.focus();
        if (config.onClose) config.onClose();
      }, 0);
    };
    // The menu's config holds either shape; `mode` says which.
    const cfg = /** @type {any} */ (Object.assign({}, config, { onClose: teardown }));
    handle = multi
      ? createMulti(host, cfg, anchor)
      : createSingle(host, Object.assign(cfg, { allowEmpty: true }), anchor);
    return {
      close: teardown,
      // New choices or picks from R while the menu is open (its word redrawn
      // under it). Picks made in a multi menu and not reported yet stay.
      /** @param {any} opts @param {any} [sel] */
      setOptions: (opts, sel) => { if (handle && !done) handle.setOptions(opts, sel); }
    };
  };

  /** @param {HTMLElement} container @param {BlockrSelectMultiConfig} config */
  const multi = (container, config) => createMulti(container, config, null);
  /** @param {HTMLElement} container @param {BlockrSelectMultiConfig} config */
  const inPlace = (container, config) => createMulti(container, config, null, true);

  Blockr.Select = {
    single: (container, config) => createSingle(container, config, null),
    multi,
    // The multi list in place: always open, reports after a pause in the
    // picking.
    list: inPlace,
    menu: createMenu,
    // Capability flag for the packages that PAINT the words (blockr.viz,
    // blockr.sandbox) against whatever build of Select a deployment carries.
    // Without it, a caller asking for `mode: 'multi'` on an older build gets
    // a SINGLE menu that looks right and sends one string where the setting
    // holds a list. A missing flag is meant to be thrown on, not felt out.
    menuMulti: true,
    // The field's text for a set of picks; exposed for tests.
    summary
  };
})();
