(function () {
  "use strict";

  // The block browser is a Shiny input: its value is the block the user
  // chose to add. Adding is an event (click a card, or the in-card add
  // button), so the value carries a monotonically increasing `nonce`
  // that guarantees Shiny sees a change and re-fires even when the same
  // block is added twice. R reads it as `input[[<root id>]]` and ignores
  // the nonce. The binding also owns the (future) R->JS direction via
  // `receiveMessage` - reserved here for the AI-search card filter.
  var COMMIT_EVENT = "blockr-block-browser:commit";
  var commitSeq = 0;

  // Card-list helpers shared with the stack-menu binding. Both modules
  // render the same `.blockr-block-browser-card` markup and the same
  // `data-name` / `data-description` / `data-package` / `data-category`
  // search contract, so the filter and the card-iteration helper live
  // on a tiny `window.BlockrUI.cardSearch` namespace. The stack-menu
  // module depends on `block_browser_dep()` being attached first
  // (which it is wherever `stack_menu_ui()` is rendered) and just
  // calls into this API. Keep the surface deliberately small.
  var BlockrUI = window.BlockrUI = window.BlockrUI || {};
  BlockrUI.cardSearch = BlockrUI.cardSearch || {
    getCards: function (root) {
      return Array.prototype.slice.call(
        root.querySelectorAll(".blockr-block-browser-card")
      );
    },
    applySearch: function (root, query) {
      var q = (query || "").trim().toLowerCase();
      var anyVisible = false;
      BlockrUI.cardSearch.getCards(root).forEach(function (card) {
        if (q.length === 0) {
          card.classList.remove("hidden");
          anyVisible = true;
          return;
        }
        var haystack = [
          card.getAttribute("data-name") || "",
          card.getAttribute("data-description") || "",
          card.getAttribute("data-package") || "",
          card.getAttribute("data-category") || ""
        ]
          .join(" ")
          .toLowerCase();
        var hit = haystack.indexOf(q) !== -1;
        card.classList.toggle("hidden", !hit);
        if (hit) anyVisible = true;
      });
      root.classList.toggle("is-empty", !anyVisible);
    }
  };

  // Shared structural reconciler for the instance-backed menus (stack,
  // link). Given the full desired card set (`[{ id, html }, ...]`, each
  // `html` the server-rendered markup for that card), it removes cards
  // no longer desired, inserts desired cards not yet in the DOM into the
  // matching category section (creating the section when absent), and
  // drops emptied category sections. It does NOT touch eligibility /
  // selection / search / empty-state - callers retune those after, so a
  // board change never disturbs scroll, expansion, or in-progress input.
  function cssEscapeAttr(s) {
    return String(s).replace(/["\\]/g, "\\$&");
  }
  function cardById(root, id) {
    return root.querySelector(
      '.blockr-block-browser-card[data-block-type="' + cssEscapeAttr(id) + '"]'
    );
  }
  function parseCardHtml(html) {
    var tmp = document.createElement("div");
    tmp.innerHTML = String(html).trim();
    return tmp.firstElementChild;
  }
  function insertCardNode(cats, category, node) {
    var sec = cats.querySelector(
      '.blockr-block-browser-category[data-category="' +
        cssEscapeAttr(category) + '"]'
    );
    if (sec) {
      (sec.querySelector(".blockr-block-browser-cards") || sec)
        .appendChild(node);
      return;
    }
    sec = document.createElement("div");
    sec.className = "blockr-block-browser-category";
    sec.setAttribute("data-category", category);
    var h = document.createElement("h3");
    h.textContent = category;
    var list = document.createElement("div");
    list.className = "blockr-block-browser-cards";
    list.appendChild(node);
    sec.appendChild(h);
    sec.appendChild(list);
    cats.appendChild(sec);
  }
  BlockrUI.cardSync = BlockrUI.cardSync || function (root, cards) {
    var cats = root.querySelector(".blockr-block-browser-categories");
    if (!cats) return;
    // `sendInputMessage` auto-unboxes a length-1 list to a scalar.
    if (!cards) cards = [];
    if (!Array.isArray(cards)) cards = [cards];

    var desired = {};
    cards.forEach(function (c) {
      if (c && c.id != null) desired[c.id] = c;
    });

    BlockrUI.cardSearch.getCards(root).forEach(function (card) {
      var id = card.getAttribute("data-block-type");
      if (!Object.prototype.hasOwnProperty.call(desired, id)) {
        if (card.parentNode) card.parentNode.removeChild(card);
      }
    });

    cards.forEach(function (c) {
      if (!c || c.id == null || !c.html) return;
      if (cardById(root, c.id)) return;
      var node = parseCardHtml(c.html);
      if (node) insertCardNode(cats, node.getAttribute("data-category") || "", node);
    });

    Array.prototype.slice
      .call(cats.querySelectorAll(".blockr-block-browser-category"))
      .forEach(function (sec) {
        if (!sec.querySelector(".blockr-block-browser-card")) {
          if (sec.parentNode) sec.parentNode.removeChild(sec);
        }
      });
  };

  function getField(card, fieldClass) {
    return card.querySelector(
      "." + fieldClass + " input, ." + fieldClass + " select"
    );
  }

  // Fields are rendered only for the flows that use them, so an absent
  // field simply means "not applicable" -> null. An empty value is also
  // reported as null.
  function getFieldValue(card, fieldClass) {
    var el = getField(card, fieldClass);
    if (!el) return null;
    var v = el.value;
    return v === "" ? null : v;
  }

  function gatherSpec(card) {
    return {
      type: card.getAttribute("data-block-type"),
      id: getFieldValue(card, "blockr-block-browser-field-id"),
      title: getFieldValue(card, "blockr-block-browser-field-title"),
      link_id: getFieldValue(card, "blockr-block-browser-field-link-id"),
      block_input: getFieldValue(
        card, "blockr-block-browser-field-block-input"
      ),
      target_input: getFieldValue(
        card, "blockr-block-browser-field-target-input"
      ),
      nonce: ++commitSeq
    };
  }

  // Record the chosen block on the root and fire the commit event; the
  // binding's `subscribe` callback then has Shiny read `getValue`.
  function commitCard(root, card) {
    root._blockrBrowserValue = gatherSpec(card);
    root.dispatchEvent(new CustomEvent(COMMIT_EVENT));
  }

  // Wire the search box and the delegated card-area click handler.
  // Idempotent: safe to call from both initialize() and subscribe()
  // (Shiny may invoke either first).
  function initBrowser(root) {
    if (root.dataset.blockrBlockBrowserInit === "1") return;
    root.dataset.blockrBlockBrowserInit = "1";

    var search = root.querySelector(".blockr-block-browser-search");
    if (search) {
      search.addEventListener("input", function () {
        BlockrUI.cardSearch.applySearch(root, search.value);
      });
    }

    var cardsArea = root.querySelector(".blockr-block-browser-categories");
    if (cardsArea) {
      cardsArea.addEventListener("click", function (event) {
        var card = event.target.closest(".blockr-block-browser-card");
        if (!card || !root.contains(card)) return;

        if (event.target.closest(".blockr-block-browser-card-chevron")) {
          card.classList.toggle("card-expanded");
          event.preventDefault();
          return;
        }
        if (event.target.closest(".blockr-block-browser-card-add")) {
          commitCard(root, card);
          event.preventDefault();
          return;
        }
        if (event.target.closest(".blockr-block-browser-card-advanced")) {
          return;
        }
        commitCard(root, card);
      });
    }
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-block-browser");
    },
    initialize: function (el) {
      initBrowser(el);
    },
    getValue: function (el) {
      return el._blockrBrowserValue || null;
    },
    subscribe: function (el, callback) {
      initBrowser(el);
      // Shiny's callback takes no args; addEventListener passes the
      // Event, which breaks the value-update path - wrap to drop it.
      var handler = function () { callback(); };
      el._blockrBrowserHandler = handler;
      el.addEventListener(COMMIT_EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrBrowserHandler) {
        el.removeEventListener(COMMIT_EVENT, el._blockrBrowserHandler);
        el._blockrBrowserHandler = null;
      }
    }
    // receiveMessage: reserved for the AI-search card filter (R -> JS),
    // added with block_browser_server() in a follow-up change.
  });

  Shiny.inputBindings.register(binding, "blockr.ui.blockBrowser");
})();
