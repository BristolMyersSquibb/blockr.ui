// @ts-check
/**
 * Blockr.Input — lightweight code input with autocomplete
 *
 * Replaces ACE editor in blockr blocks.
 * Depends on blockr-ui.js (Blockr.uid, Blockr.removeNode, Blockr.place).
 *
 * API:
 *   Blockr.Input.create(container, config) -> { el, getValue, setValue, setColumns, focus, destroy }
 */
(() => {
  'use strict';

  const BOUNDARY = /[(\s,+\-*/><!=&|~^%:[\]{}]/;

  /** @param {string} n */
  const backtickIfNeeded = (n) =>
    /^[a-zA-Z.][a-zA-Z0-9._]*$/.test(n) ? n : '`' + n + '`';

  /**
   * @typedef {Object} BlockrInputCompletion
   * @property {string} text Display text (columns arrive pre-backticked).
   * @property {string} insertText Inserted text (functions get a trailing "()").
   * @property {string} meta Right-aligned tag: 'column' or the category name.
   * @property {boolean} isFunc Cursor lands between the parens when true.
   * @property {number} score Sort rank (columns above functions).
   */

  /**
   * @param {HTMLElement} container
   * @param {BlockrInputConfig} config
   */
  const createInput = (container, config) => {
    const id = Blockr.uid('binp');
    const popupId = `${id}-popup`;

    // Config
    let columns = (config.columns || []).slice();
    const categories = config.categories || {};
    const placeholder = config.placeholder || '';
    const multiline = config.multiline === true;
    const onChange = config.onChange || null;
    const onConfirm = config.onConfirm || null;
    let destroyed = false;

    // Autocomplete state
    /** @type {BlockrInputCompletion[]} */
    let completions = [];
    /** @type {BlockrInputCompletion[]} */
    let filtered = [];
    let highlightIdx = -1;
    let popupOpen = false;
    let tokenStart = 0;
    let tokenEnd = 0;

    // DOM
    const root = document.createElement('div');
    root.className = 'blockr-input';

    let field;
    if (multiline) {
      field = document.createElement('textarea');
      field.rows = 1;
    } else {
      field = document.createElement('input');
      field.type = 'text';
    }
    field.className = 'blockr-input__field';
    field.placeholder = placeholder;
    field.setAttribute('autocomplete', 'off');
    field.setAttribute('autocorrect', 'off');
    field.setAttribute('autocapitalize', 'off');
    field.setAttribute('spellcheck', 'false');
    if (config.value) field.value = config.value;
    if (config.label) field.setAttribute('aria-label', config.label);

    const popup = document.createElement('div');
    popup.className = 'blockr-input__popup';
    popup.id = popupId;
    popup.setAttribute('role', 'listbox');
    popup.setAttribute('aria-label', 'Completions');

    // The field and its list, as screen readers meet them: a single-line
    // field is a combobox, like Blockr.Select's search box, with the
    // highlighted completion as its active descendant. A textarea cannot be
    // a combobox, so it only names the list it controls.
    field.setAttribute('aria-controls', popupId);
    field.setAttribute('aria-autocomplete', 'list');
    if (!multiline) {
      field.setAttribute('role', 'combobox');
      field.setAttribute('aria-expanded', 'false');
    }

    root.appendChild(field);
    container.appendChild(root);

    // Portal: the popup lives on document.body while open, so no clipping,
    // paint-containment or stacking-context ancestor (dock panels,
    // offcanvas, modals) cuts it off. Blockr.place holds it against the
    // field, as it does Blockr.Select's list: above where there is no room
    // below, and following scroll, resize and the list's own height, which
    // shrinks as typing narrows it.
    /** @type {BlockrPlaceHandle | null} */
    let placement = null;

    // --- Completion list building ---

    const rebuildCompletions = () => {
      completions = [];
      const cols = columns.map(backtickIfNeeded);
      for (const col of cols) {
        completions.push({
          text: col, insertText: col,
          meta: 'column', isFunc: false, score: 1001
        });
      }
      for (const cat of Object.keys(categories)) {
        for (const fn of categories[cat]) {
          completions.push({
            text: fn, insertText: fn + '()',
            meta: cat, isFunc: true, score: 1000
          });
        }
      }
      completions.sort((a, b) =>
        a.score !== b.score ? b.score - a.score : a.text.localeCompare(b.text)
      );
    };

    rebuildCompletions();

    // --- Token extraction ---

    const getCurrentToken = () => {
      const val = field.value;
      const cursor = field.selectionStart || 0;
      let start = cursor;
      while (start > 0) {
        const ch = val.charAt(start - 1);
        if (BOUNDARY.test(ch)) break;
        if (ch === '`') { start--; break; }
        start--;
      }
      tokenStart = start;
      tokenEnd = cursor;
      return val.substring(start, cursor);
    };

    // --- Popup rendering ---

    /** @param {string} token */
    const filterCompletions = (token) => {
      if (!token) { filtered = []; return; }
      let q = token.toLowerCase();
      if (q.charAt(0) === '`') q = q.substring(1);
      filtered = [];
      for (const c of completions) {
        let matchText = c.text.toLowerCase();
        if (matchText.charAt(0) === '`') matchText = matchText.substring(1);
        if (matchText.indexOf(q) === 0) filtered.push(c);
      }
    };

    const scrollHighlightIntoView = () => {
      popup.querySelector('.blockr-input__item--highlighted')
        ?.scrollIntoView({ block: 'nearest' });
    };

    const renderPopup = () => {
      popup.innerHTML = '';
      if (filtered.length === 0) { closePopup(); return; }

      if (highlightIdx >= filtered.length) highlightIdx = filtered.length - 1;
      if (highlightIdx < 0) highlightIdx = 0;

      for (let i = 0; i < filtered.length; i++) {
        const c = filtered[i];
        const item = document.createElement('div');
        item.id = `${popupId}-opt-${i}`;
        item.className = 'blockr-input__item';
        if (i === highlightIdx) item.className += ' blockr-input__item--highlighted';
        item.setAttribute('role', 'option');
        item.setAttribute('aria-selected', i === highlightIdx ? 'true' : 'false');
        item.setAttribute('data-idx', /** @type {any} */ (i));

        const textSpan = document.createElement('span');
        textSpan.className = 'blockr-input__item-text';
        textSpan.textContent = c.text;
        item.appendChild(textSpan);

        if (c.isFunc) {
          const parens = document.createElement('span');
          parens.className = 'blockr-input__item-parens';
          parens.textContent = '()';
          item.appendChild(parens);
        }

        const metaSpan = document.createElement('span');
        metaSpan.className = 'blockr-input__item-meta';
        metaSpan.textContent = c.meta;
        item.appendChild(metaSpan);

        popup.appendChild(item);
      }

      if (!popupOpen) openPopup();
      field.setAttribute('aria-activedescendant', `${popupId}-opt-${highlightIdx}`);
      scrollHighlightIntoView();
    };

    // --- Popup open / close ---

    const openPopup = () => {
      if (popupOpen || destroyed) return;
      popupOpen = true;

      if (popup.parentElement !== document.body) {
        document.body.appendChild(popup);
      }
      popup.style.display = 'block';

      root.classList.add('blockr-input--popup-open');
      if (!multiline) field.setAttribute('aria-expanded', 'true');
      placement = Blockr.place(popup, root, { gap: 2 });
      // Only while open: a listener per field for the whole life of the page
      // would hold the field and its block after they leave it.
      document.addEventListener('click', onDocumentClick, true);
    };

    const closePopup = () => {
      if (!popupOpen) return;
      popupOpen = false;

      if (placement) { placement.stop(); placement = null; }
      document.removeEventListener('click', onDocumentClick, true);

      popup.style.display = '';
      root.classList.remove('blockr-input--popup-open');
      if (!multiline) field.setAttribute('aria-expanded', 'false');
      field.removeAttribute('aria-activedescendant');
      popup.innerHTML = '';
      highlightIdx = 0;
    };

    // --- Completion insertion ---

    /** @param {number} idx */
    const acceptCompletion = (idx) => {
      if (idx < 0 || idx >= filtered.length) return;
      const c = filtered[idx];
      const val = field.value;
      const before = val.substring(0, tokenStart);
      const after = val.substring(tokenEnd);
      field.value = before + c.insertText + after;

      const cursorPos = c.isFunc
        ? tokenStart + c.insertText.length - 1
        : tokenStart + c.insertText.length;
      field.setSelectionRange(cursorPos, cursorPos);

      closePopup();
      if (onChange) onChange();
    };

    // --- Event handlers ---

    const onFieldInput = () => {
      const token = getCurrentToken();
      filterCompletions(token);
      highlightIdx = 0;

      if (filtered.length > 0 && token.length > 0) {
        renderPopup();
      } else {
        closePopup();
      }

      if (onChange) onChange();
    };

    /** @param {KeyboardEvent} e */
    const onFieldKeydown = (e) => {
      if (popupOpen) {
        switch (e.key) {
          case 'ArrowDown':
            e.preventDefault();
            highlightIdx = (highlightIdx + 1) % filtered.length;
            renderPopup();
            break;
          case 'ArrowUp':
            e.preventDefault();
            highlightIdx = (highlightIdx - 1 + filtered.length) % filtered.length;
            renderPopup();
            break;
          case 'Enter':
            e.preventDefault();
            acceptCompletion(highlightIdx);
            break;
          case 'Tab':
            e.preventDefault();
            acceptCompletion(highlightIdx);
            break;
          case 'Escape':
            // An open list owns this Escape, as an open Blockr.Select's does:
            // it closes and the key goes no further, so the gear tray the
            // field sits in stays open.
            e.preventDefault();
            e.stopPropagation();
            closePopup();
            break;
        }
      } else {
        if (e.key === 'Enter' && !multiline) {
          e.preventDefault();
          if (onConfirm) onConfirm(field.value.trim());
        }
      }
    };

    /** @param {MouseEvent} e */
    const onPopupClick = (e) => {
      const item = /** @type {Element} */ (e.target).closest('.blockr-input__item');
      if (item) {
        const idx = parseInt(/** @type {string} */ (item.getAttribute('data-idx')), 10);
        if (!isNaN(idx)) {
          getCurrentToken();
          acceptCompletion(idx);
          field.focus();
        }
      }
    };

    /** @param {MouseEvent} e */
    const onDocumentClick = (e) => {
      if (root.contains(/** @type {Node | null} */ (e.target)) || popup.contains(/** @type {Node | null} */ (e.target))) return;
      closePopup();
    };

    const onFieldBlur = () => {
      setTimeout(() => {
        if (!root.contains(document.activeElement) &&
            !popup.contains(document.activeElement)) {
          closePopup();
        }
      }, 150);
    };

    // --- Bind events ---

    field.addEventListener('input', onFieldInput);
    // Cast: the textarea|input union resolves addEventListener to the bare
    // EventListener overload; HTMLElement restores the KeyboardEvent typing.
    /** @type {HTMLElement} */ (field).addEventListener('keydown', onFieldKeydown);
    field.addEventListener('blur', onFieldBlur);
    popup.addEventListener('mousedown', (e) => e.preventDefault());
    popup.addEventListener('click', onPopupClick);

    // --- Public API ---

    return {
      el: root,

      getValue() { return field.value.trim(); },

      /** @param {string | null | undefined} v */
      setValue(v) { field.value = v || ''; },

      /** @param {string[] | null | undefined} cols */
      setColumns(cols) {
        columns = (cols || []).slice();
        rebuildCompletions();
      },

      focus() { field.focus(); },

      destroy() {
        if (destroyed) return;
        destroyed = true;
        closePopup();

        if (popup.parentElement === document.body) {
          popup.remove();
        }

        field.removeEventListener('input', onFieldInput);
        /** @type {HTMLElement} */ (field).removeEventListener('keydown', onFieldKeydown);
        field.removeEventListener('blur', onFieldBlur);
        popup.removeEventListener('click', onPopupClick);
        Blockr.removeNode(root);
      }
    };
  };

  Blockr.Input = {
    create: (container, config) => createInput(container, config)
  };
})();
