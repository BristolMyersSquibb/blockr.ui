/**
 * blockr-inputs.js: Shiny input bindings for the controls R renders for a
 * block whose UI is written in R (select_input(), gear_tray()). R writes the
 * markup and the settings as data-* attributes; the binding mounts the
 * control from blockr-select.js or blockr-ui.js on it and reports its value.
 * Text, numbers and checkboxes are Shiny's own inputs.
 *
 * Every control is written once, as a spec in Blockr.inputs: `mount` builds
 * it (and does nothing the second time, so a panel that Shiny unbinds and
 * binds again keeps its control), `value` is what Shiny sends, and `receive`
 * applies an update from the server. The tests drive the specs directly.
 *
 * An update from the server is sent back, as Shiny's own inputs send theirs,
 * so the input follows what the control shows, a pick the select settles on
 * by itself included. Shiny drops a value that did not change.
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
          notify(el);
        }
      });
      Shiny.inputBindings.register(binding, 'blockr.ui.' + name);
    });
  };
  register();
})();
