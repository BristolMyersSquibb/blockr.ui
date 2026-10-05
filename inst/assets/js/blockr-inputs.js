/**
 * blockr-inputs.js: the Shiny side of the gear tray R renders for a block
 * whose UI is written in R (gear_tray()), and Shiny's selects on the dismiss
 * stack. The fields in the tray are Shiny's own inputs.
 *
 * The gear's binding is written as a spec in Blockr.inputs: `mount` wires
 * the tray up (and does nothing the second time, so a panel that Shiny
 * unbinds and binds again keeps it) and `value` is what Shiny sends, whether
 * the tray is open. The tests drive the spec directly.
 *
 * Depends on: blockr-ui.js and Shiny.
 */
(() => {
  'use strict';

  /** @param {HTMLElement} el */
  const notify = (el) => {
    const cb = /** @type {any} */ (el)._blockrNotify;
    if (typeof cb === 'function') cb();
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
    }
  };

  Blockr.inputs = {
    gear: gear
  };

  /* --- Shiny's selects on the dismiss stack ------------------------------ */

  // The dropdown of a selectize input, as Shiny's selectInput() draws it, is
  // a layer while it is open, as the design system's lists are, so Escape
  // closes it before the gear tray, modal or panel around it. An instance is
  // hooked when its control first takes the focus, which comes before the
  // dropdown can open. Shiny replaces the instance when its options change,
  // and the new one is hooked the same way.

  /** @param {any} s A selectize instance. */
  const layerSelectize = (s) => {
    if (s._blockrLayered) return;
    s._blockrLayered = true;
    /** @type {BlockrLayerHandle | null} */
    let layer = null;
    s.on('dropdown_open', () => {
      layer = Blockr.layer([s.$wrapper[0], s.$dropdown[0]], {
        escape: () => s.close()
      });
    });
    s.on('dropdown_close', () => {
      if (layer) { layer.remove(); layer = null; }
    });
  };

  document.addEventListener('focusin', (e) => {
    const ctl = e.target instanceof Element && e.target.closest('.selectize-control');
    const sel = ctl && ctl.parentElement && ctl.parentElement.querySelector('.selectized');
    const s = sel && /** @type {any} */ (sel).selectize;
    if (s) layerSelectize(s);
  }, true);

  /* --- Shiny ------------------------------------------------------------- */

  const register = () => {
    const Shiny = /** @type {any} */ (window).Shiny;
    const $ = /** @type {any} */ (window).jQuery;
    if (!Shiny || !Shiny.inputBindings || !$) {
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
        subscribe: (el, callback) => { el._blockrNotify = () => callback(false); },
        unsubscribe: (el) => { el._blockrNotify = null; }
      });
      Shiny.inputBindings.register(binding, 'blockr.ui.' + name);
    });
  };
  register();
})();
