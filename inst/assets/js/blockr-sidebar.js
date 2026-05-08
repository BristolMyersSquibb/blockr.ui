(function () {
  "use strict";

  var FOCUSABLE_SELECTOR = [
    "a[href]",
    "button:not([disabled])",
    "input:not([disabled]):not([type=\"hidden\"])",
    "select:not([disabled])",
    "textarea:not([disabled])",
    "[tabindex]:not([tabindex=\"-1\"])"
  ].join(", ");

  var STATE_EVENT = "blockr-sidebar:state";
  var INIT_FLAG = "blockrSidebarInit";

  function getBody(panel) {
    return panel.querySelector(".blockr-sidebar-body");
  }

  function getTitle(panel) {
    return panel.querySelector(".blockr-sidebar-title");
  }

  function getFocusables(scope) {
    return Array.prototype.slice
      .call(scope.querySelectorAll(FOCUSABLE_SELECTOR))
      .filter(function (el) {
        return !el.hasAttribute("disabled") && el.offsetParent !== null;
      });
  }

  function dispatchState(panel) {
    panel.dispatchEvent(new CustomEvent(STATE_EVENT));
  }

  function isOpen(panel) {
    return panel.classList.contains("blockr-sidebar-open");
  }

  function isPinned(panel) {
    return panel.classList.contains("blockr-sidebar-pinned");
  }

  function refreshBodyReflow() {
    // Push-mode panels shift the page content aside via a class + CSS
    // variable on `<html>` whenever they are open. We apply to `<html>`
    // (rather than `<body>`) because bslib's page-fill layouts pin body
    // to 100% of html and zero its padding inline — body-level padding /
    // margin no longer constrains the visible viewport. Padding on html
    // shrinks html's content area, and body (sized 100% of it) follows.
    // Overlay-mode panels never reflow.
    var root = document.documentElement;
    root.classList.remove(
      "blockr-html-pushed-left",
      "blockr-html-pushed-right"
    );
    var pushedOpen = document.querySelector(
      ".blockr-sidebar.blockr-sidebar-open[data-mode=\"push\"]"
    );
    if (pushedOpen) {
      var side = pushedOpen.dataset.side || "right";
      root.classList.add("blockr-html-pushed-" + side);
      var width = pushedOpen.getBoundingClientRect().width;
      root.style.setProperty("--blockr-sidebar-width", width + "px");
    } else {
      root.style.setProperty("--blockr-sidebar-width", "0px");
    }
  }

  function trapFocus(panel, event) {
    if (event.key !== "Tab") return;
    var focusables = getFocusables(panel);
    if (focusables.length === 0) {
      event.preventDefault();
      panel.focus();
      return;
    }
    var first = focusables[0];
    var last = focusables[focusables.length - 1];
    var active = document.activeElement;
    if (event.shiftKey && active === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && active === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function attachOutsideClick(panel) {
    if (panel._blockrSidebarOutsideHandler) return;
    var handler = function (event) {
      if (!isOpen(panel) || isPinned(panel)) return;
      if (panel.contains(event.target)) return;
      hidePanel(panel);
    };
    panel._blockrSidebarOutsideHandler = handler;
    // Defer one tick so the click that opened the panel does not
    // immediately close it.
    setTimeout(function () {
      document.addEventListener("mousedown", handler, true);
    }, 0);
  }

  function detachOutsideClick(panel) {
    var handler = panel._blockrSidebarOutsideHandler;
    if (!handler) return;
    document.removeEventListener("mousedown", handler, true);
    panel._blockrSidebarOutsideHandler = null;
  }

  function showPanel(panel, data) {
    var body = getBody(panel);
    if (!body) return;

    // Remember previously-focused element so close can restore it.
    panel._blockrSidebarPreviousActive = document.activeElement;

    Shiny.unbindAll(body);
    if (data.dependencies && data.dependencies.length) {
      Shiny.renderDependencies(data.dependencies);
    }
    body.replaceChildren.apply(
      body,
      window.$ ? $.parseHTML(data.html || "", document, true) : []
    );
    Shiny.initializeInputs(body);
    Shiny.bindAll(body);

    var titleEl = getTitle(panel);
    if (titleEl) {
      titleEl.textContent = data.title == null ? "" : String(data.title);
    }

    panel.classList.add("blockr-sidebar-open");
    panel.setAttribute("aria-hidden", "false");

    refreshBodyReflow();
    attachOutsideClick(panel);
    dispatchState(panel);

    // Move focus to first focusable element inside the body.
    var focusables = getFocusables(body);
    if (focusables.length) {
      focusables[0].focus();
    } else {
      panel.focus();
    }
  }

  function hidePanel(panel) {
    var body = getBody(panel);
    if (body) Shiny.unbindAll(body);

    panel.classList.remove("blockr-sidebar-open");
    // Closing is an explicit user/server action; pin opts in to "stay open
    // while I work elsewhere", so clearing pin on close prevents a stale pin
    // bleeding into the next show.
    panel.classList.remove("blockr-sidebar-pinned");
    panel.setAttribute("aria-hidden", "true");

    detachOutsideClick(panel);
    refreshBodyReflow();
    dispatchState(panel);

    // Restore focus to previously-focused element.
    var prev = panel._blockrSidebarPreviousActive;
    if (prev && typeof prev.focus === "function") {
      prev.focus();
    }
    panel._blockrSidebarPreviousActive = null;
  }

  function togglePinned(panel) {
    // Pin only affects dismissal (Esc / outside click). Body reflow is
    // governed by the panel's `data-mode`, not by pin state, so no
    // refreshBodyReflow() call here.
    panel.classList.toggle("blockr-sidebar-pinned");
    dispatchState(panel);
  }

  function initPanel(panel) {
    if (panel.dataset[INIT_FLAG] === "true") return;
    panel.dataset[INIT_FLAG] = "true";

    var pinBtn = panel.querySelector(".blockr-sidebar-pin");
    if (pinBtn) {
      pinBtn.addEventListener("click", function (event) {
        event.preventDefault();
        togglePinned(panel);
      });
    }

    var closeBtn = panel.querySelector(".blockr-sidebar-close");
    if (closeBtn) {
      closeBtn.addEventListener("click", function (event) {
        event.preventDefault();
        hidePanel(panel);
      });
    }

    panel.addEventListener("keydown", function (event) {
      if (!isOpen(panel)) return;
      if (event.key === "Escape" && !isPinned(panel)) {
        event.preventDefault();
        hidePanel(panel);
        return;
      }
      trapFocus(panel, event);
    });
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-sidebar");
    },
    initialize: function (el) {
      initPanel(el);
    },
    getValue: function (el) {
      return {
        open: el.classList.contains("blockr-sidebar-open"),
        pinned: el.classList.contains("blockr-sidebar-pinned")
      };
    },
    receiveMessage: function (el, data) {
      if (!data) return;
      initPanel(el);
      if (data.action === "show") {
        showPanel(el, data);
      } else if (data.action === "hide") {
        hidePanel(el);
      }
    },
    subscribe: function (el, callback) {
      initPanel(el);
      // Shiny's input-binding callback expects to be invoked with no args.
      // addEventListener passes the Event object, which silently breaks the
      // value-update path inside Shiny — so wrap to drop it.
      var handler = function () { callback(); };
      el._blockrSidebarHandler = handler;
      el.addEventListener(STATE_EVENT, handler);
    },
    unsubscribe: function (el) {
      if (el._blockrSidebarHandler) {
        el.removeEventListener(STATE_EVENT, el._blockrSidebarHandler);
        el._blockrSidebarHandler = null;
      }
    }
  });

  Shiny.inputBindings.register(binding, "blockr.ui.sidebar");
})();
