/**
 * blockr-inputs.js: Shiny input bindings for the shared controls, for blocks
 * whose UI is rendered in R (select_input(), text_input(), number_input(),
 * checkbox_input(), segmented_input(), gear_tray()). R writes the markup and
 * the settings as data-* attributes; the binding mounts the control from
 * blockr-ui.js or blockr-select.js on it and reports its value.
 *
 * Every control is written once, as a spec in Blockr.inputs: `mount` builds
 * it (and does nothing the second time, so a panel that Shiny unbinds and
 * binds again keeps its control), `value` is what Shiny sends, and `receive`
 * applies an update from the server. The tests drive the specs directly.
 *
 * An update from the server is not reported back. The server knows what it
 * sent, and reporting it echoes every push as a user gesture: with two
 * pushes in flight the echoes alternate, Shiny's dedup no longer drops them,
 * and a server that reacts to its input pushes again, forever. The binding
 * tells Shiny to forget the last value it sent instead, so a later pick of
 * that same value still goes through.
 *
 * Depends on: blockr-ui.js, blockr-select.js, and Shiny.
 */
(() => {
  'use strict';

  /** @param {Element} el @param {string} name @param {any} fallback */
  const json = (el, name, fallback) => {
    const raw = el.getAttribute(name);
    if (raw == null || raw === '') return fallback;
    try { return JSON.parse(raw); } catch (e) { return fallback; }
  };

  /** Shiny unboxes a length-1 vector; the controls want arrays. */
  const toArray = (x) => (Array.isArray(x) ? x : (x == null ? [] : [x]));

  /** @param {HTMLElement} el */
  const notify = (el) => {
    const cb = /** @type {any} */ (el)._blockrNotify;
    if (typeof cb === 'function') cb();
  };

  /** The label R drew for the control, found by id. @param {HTMLElement} el */
  const setLabel = (el, text) => {
    const lab = document.getElementById(el.id + '-label');
    if (lab) lab.textContent = text;
  };

  /* --- Select ------------------------------------------------------------ */

  const select = {
    selector: '.blockr-ui-select',
    type: 'blockr.ui.select',
    mount(el) {
      if (el._blockrSelect) return;
      const multiple = el.getAttribute('data-multiple') === 'true';
      const placeholder = el.getAttribute('data-placeholder') || '';
      const selected = toArray(json(el, 'data-selected', null));
      const slot = document.createElement('div');
      el.appendChild(slot);
      const factory = multiple ? Blockr.Select.multi : Blockr.Select.single;
      el._blockrSelect = factory(slot, {
        options: toArray(json(el, 'data-options', [])),
        selected: multiple ? selected : (selected.length ? selected[0] : null),
        placeholder: placeholder,
        bordered: true,
        // With a placeholder, a single select shows it until something is
        // picked; without one it takes the first option, as selectInput().
        allowEmpty: placeholder !== '',
        onChange: () => notify(el)
      });
      el._blockrMultiple = multiple;
    },
    value(el) {
      return el._blockrSelect.getValue();
    },
    receive(el, data) {
      const sel = el._blockrSelect;
      const pick = (x) => (el._blockrMultiple ? toArray(x) : toArray(x)[0]);
      if ('choices' in data) {
        // Without `selected`, keep what is picked while the list still has it.
        sel.setOptions(toArray(data.choices),
          'selected' in data ? pick(data.selected) : sel.getValue());
      } else if ('selected' in data) {
        sel.setValue(pick(data.selected));
      }
      if ('label' in data) setLabel(el, data.label);
    },
    unmount(el) {
      // Shiny unbinds before it removes; a panel that is only rebound stays.
      setTimeout(() => {
        if (!el.isConnected && el._blockrSelect) {
          el._blockrSelect.destroy();
          el._blockrSelect = null;
        }
      }, 0);
    }
  };

  /* --- Text and number fields: commit on Enter or blur ------------------- */

  const field = (numeric) => ({
    selector: numeric ? 'input.blockr-ui-number' : 'input.blockr-ui-text',
    type: numeric ? 'shiny.number' : undefined,
    mount(el) {
      if (el._blockrText) return;
      el._blockrCommitted = el.value;
      el._blockrText = Blockr.textCommit(el, {
        onCommit: (value) => {
          el._blockrCommitted = value;
          notify(el);
        }
      });
    },
    value(el) {
      const v = el._blockrCommitted;
      if (!numeric) return v;
      const n = v === '' ? NaN : Number(v);
      return Number.isFinite(n) ? n : null;
    },
    receive(el, data) {
      if ('value' in data) {
        const v = data.value == null ? '' : String(data.value);
        el._blockrCommitted = v;
        el._blockrText.sync(v);
      }
      if ('placeholder' in data) el.placeholder = data.placeholder || '';
      if ('label' in data) setLabel(el, data.label);
    }
  });

  /* --- Checkbox ---------------------------------------------------------- */

  const checkbox = {
    selector: 'input.blockr-ui-checkbox',
    mount(el) {
      if (el._blockrCheckbox) return;
      el._blockrCheckbox = true;
      el.addEventListener('change', () => notify(el));
    },
    value(el) {
      return el.checked;
    },
    receive(el, data) {
      if ('value' in data) el.checked = !!data.value;
      if ('label' in data) {
        const wrap = el.closest('.blockr-checkbox');
        const lab = wrap && wrap.querySelector('.blockr-checkbox__label');
        if (lab) lab.textContent = data.label;
      }
    }
  };

  /* --- Segmented control ------------------------------------------------- */

  const segmented = {
    selector: '.blockr-ui-segmented',
    mount(el) {
      if (el._blockrSegmented) return;
      const choices = toArray(json(el, 'data-choices', []));
      const selected = json(el, 'data-selected', null);
      const lab = document.getElementById(el.id + '-label');
      el._blockrSegmented = Blockr.segmented(
        choices,
        selected != null ? selected : (choices[0] && choices[0].value),
        () => notify(el),
        {
          size: el.getAttribute('data-size') === 'xs' ? 'xs' : undefined,
          label: lab ? lab.textContent : undefined
        }
      );
      el.appendChild(el._blockrSegmented.el);
    },
    value(el) {
      return el._blockrSegmented.get();
    },
    receive(el, data) {
      if ('selected' in data) el._blockrSegmented.set(toArray(data.selected)[0]);
      if ('label' in data) setLabel(el, data.label);
    }
  };

  /* --- Gear and tray ----------------------------------------------------- */

  // Open trays by gear id: the tray stays open when its block draws it
  // again, for the session, and is not saved with the board.
  const openTrays = {};

  const gear = {
    selector: 'button.blockr-ui-gear',
    mount(el) {
      if (el._blockrTray) return;
      const band = document.getElementById(el.getAttribute('aria-controls'));
      if (!band) return;
      if (!el.firstElementChild) el.innerHTML = Blockr.icons.gear;
      el._blockrTray = Blockr.gearTray(band, el, {
        label: band.getAttribute('aria-label') || 'Settings',
        open: !!openTrays[el.id]
      });
      // The tray opens on the gear and closes on the gear or Escape; its
      // aria-expanded is the one place both show.
      new MutationObserver(() => {
        openTrays[el.id] = el._blockrTray.isOpen();
        notify(el);
      }).observe(el, { attributes: true, attributeFilter: ['aria-expanded'] });
    },
    value(el) {
      return el._blockrTray ? el._blockrTray.isOpen() : false;
    },
    receive(el, data) {
      if ('open' in data && el._blockrTray) el._blockrTray.set(!!data.open);
    }
  };

  Blockr.inputs = {
    select: select,
    text: field(false),
    number: field(true),
    checkbox: checkbox,
    segmented: segmented,
    gear: gear
  };

  /* --- Shiny ------------------------------------------------------------- */

  const register = () => {
    const Shiny = /** @type {any} */ (window).Shiny;
    const $ = /** @type {any} */ (window).jQuery;
    if (!Shiny || !Shiny.inputBindings || !$ || !Blockr.Select) {
      setTimeout(register, 50);
      return;
    }
    Object.keys(Blockr.inputs).forEach((name) => {
      const spec = Blockr.inputs[name];
      const binding = new Shiny.InputBinding();
      Object.assign(binding, {
        find: (scope) => $(scope).find(spec.selector),
        initialize: (el) => spec.mount(el),
        getValue: (el) => { spec.mount(el); return spec.value(el); },
        getType: () => spec.type || null,
        subscribe: (el, callback) => { el._blockrNotify = () => callback(false); },
        unsubscribe: (el) => {
          el._blockrNotify = null;
          if (spec.unmount) spec.unmount(el);
        },
        receiveMessage: (el, data) => {
          spec.mount(el);
          spec.receive(el, data || {});
          if (Shiny.forgetLastInputValue) Shiny.forgetLastInputValue(binding.getId(el));
        }
      });
      // Registered after Shiny's own, so these bind first: Shiny's text,
      // number and checkbox bindings would take the same <input>s.
      Shiny.inputBindings.register(binding, 'blockr.ui.' + name);
    });
  };
  register();
})();
