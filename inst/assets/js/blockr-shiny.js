/**
 * blockr-shiny.js: the Shiny side of the controls. The gear tray R renders
 * (gear_tray()) gets an input binding, and the dropdown of Shiny's selectize
 * input joins the dismiss stack while it is open. The fields in the tray are
 * Shiny's own inputs.
 *
 * Depends on: blockr-ui.js, and Shiny for the binding.
 */
(() => {
  'use strict';

  /* --- The gear tray's binding ------------------------------------------- */

  // Open trays by gear id: the tray stays open when its block draws it
  // again, for the session, and is not saved with the board.
  /** @type {Record<string, boolean>} */
  const openTrays = {};

  /** Wire the tray up once, so a gear Shiny binds again keeps it. @param {any} el */
  const mount = (el) => {
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
      if (el._blockrNotify) el._blockrNotify();
    }).observe(el, { attributes: true, attributeFilter: ['aria-expanded'] });
  };

  const Shiny = /** @type {any} */ (window).Shiny;
  const $ = /** @type {any} */ (window).jQuery;
  // On a Shiny page Shiny's own script comes ahead of the controls; on any
  // other page there is nothing to bind.
  if (Shiny && Shiny.inputBindings && $) {
    const binding = new Shiny.InputBinding();
    Object.assign(binding, {
      find: (scope) => $(scope).find('button.blockr-ui-gear'),
      initialize: mount,
      getValue: (el) => {
        mount(el);
        return el._blockrTray ? el._blockrTray.isOpen() : false;
      },
      subscribe: (el, callback) => { el._blockrNotify = () => callback(false); },
      unsubscribe: (el) => { el._blockrNotify = null; }
    });
    Shiny.inputBindings.register(binding, 'blockr.ui.gear');
  }

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
})();
