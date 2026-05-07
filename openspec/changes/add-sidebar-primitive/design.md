## Context

`blockr.dock` today carries a lot of code that has nothing to do with docking — design tokens, base CSS, the block card, page chrome (navbar, options accordion), modal builders, selectize widgets, Bootstrap offcanvases used as DOM staging areas. Every extension that wants the same look and feel ends up depending on `blockr.dock` (and therefore on `dockViewR`) just to pick up the design system, and there's no clean place for non-dock renderers or embedded apps to land.

The plan is to lower `blockr.dock`'s codebase over time by lifting cross-cutting UI into `blockr.ui`, one piece per `openspec` change / pull request. A previous attempt tried to extract everything in one go and got stuck on namespacing / over-abstraction problems; this restart commits to a sequence of small, independently shippable increments instead.

This change is the first such increment. It ships a sidebar primitive in `blockr.ui` and migrates `blockr.dock`'s modal flows (`block_modal()`, `link_modal()`, `stack_modal()`, settings offcanvas) onto it. Modals are the most painful UX pattern in the current build cadence — dismiss-click-fill-confirm-dismiss interrupts every iteration — and a persistent right-side panel that stays around between iterations was prototyped earlier on `blockr.dock@feat/sidebar-s3-dispatch` and tested well with users. Replacing modals with the sidebar removes a Bootstrap dependency from the mutation flows, gives `blockr.ui` its first concrete consumer, and proves out the "extract one piece at a time" pattern that subsequent changes will follow.

Future extractions (token / CSS / block-card / page-chrome / selectize widgets / dock-pool / block-browser) are sequenced as separate `openspec` changes — explicitly out of scope here.

## Goals

- Tiny API surface, modeled on `shiny::showModal()` / `shiny::removeModal()`. Two server helpers — `sidebar_show(id, ui)` and `sidebar_hide(id)` — plus a UI builder and a dependency. That's it.
- Caller composes the panel body with arbitrary shiny tags. No content registry, no S3 generic, no trigger constructors. The diff at every `blockr.dock` call site is approximately `showModal(modalDialog(body))` → `sidebar_show(id, body)` and `removeModal()` → `sidebar_hide(id)`.
- No nested `moduleServer`s. No session walking. The caller's session is the calling session — exactly how `showModal()` works. Inputs inside the panel land in `input$X` for the calling module, just like in a modal.
- Ship as a self-contained primitive. `blockr.dock` adoption is a separate PR.

## Non-Goals

- No S3 generic for the body content. No "trigger" types. The previous attempt's `sidebar_content` / `sidebar_content_server` / 7 trigger constructors / paired `sidebar_content_server` methods were too much abstraction for the use case — callers can pass whatever tags they want. If a future extension wants pluggable content types it can add its own dispatch on top.
- No `input[[id]]$content` reactive value. R-side knows what content was last set via its own `sidebar_show()` call; tracking it again client-side is duplicate state. (`$open` and `$pinned` *are* exposed — see API below — because those can be flipped client-side via Esc / X / pin button.)
- No `bslib::offcanvas` / `data-bs-*`. The panel is plain `<div>` + CSS + a single Shiny custom-message handler. Bootstrap-free.
- No theming API at v0. Tokens stay hard-coded inside the panel CSS bundle.
- No drag-and-drop, no sidebar↔dock-panel promotion, no JS framework, no keyboard chords beyond Esc.

## API

```r
sidebar_ui(
  id,
  side = c("right", "left"),
  width = "360px"
)

sidebar_dep()  # htmlDependency, attached automatically by sidebar_ui()

sidebar_show(
  id,
  ui,
  title = NULL,
  session = shiny::getDefaultReactiveDomain()
)

sidebar_hide(
  id,
  session = shiny::getDefaultReactiveDomain()
)
```

Four exported names. Compare to the previous attempt's ~25.

### Server-side flow

```r
# In any module (or top-level server):
observeEvent(input$open_panel, {
  blockr.ui::sidebar_show(
    "main_sidebar",
    title = "Add new block",
    ui = tagList(
      blockr.ui::block_registry_selectize(ns("registry")),  # or any inputs
      shiny::textInput(ns("name"), "Block name"),
      shiny::actionButton(ns("confirm"), "Add", class = "btn-primary")
    )
  )
})

observeEvent(input$confirm, {
  # validate inputs, mutate the board
  blockr.ui::sidebar_hide("main_sidebar")
})
```

`session` defaults to `getDefaultReactiveDomain()`. Custom messages aren't namespaced by Shiny, so the JS handler reads the absolute element id from the message payload. Callers don't need to walk session proxies or compute parent namespaces — the panel id is a fixed DOM id chosen at `sidebar_ui()` time.

### Client-side flow (`sidebar-binding.js`)

A single custom-message handler:

```text
"blockr.ui:sidebar" → switch on data.action:
  "show":
    body = document.getElementById(data.id).querySelector('.blockr-sidebar-body')
    Shiny.unbindAll(body)
    if (data.dependencies) Shiny.renderDependencies(data.dependencies)
    body.replaceChildren(...$.parseHTML(data.html))
    Shiny.initializeInputs(body)
    Shiny.bindAll(body)
    update title if data.title
    add `.blockr-sidebar-open` class
    set aria-hidden="false"
    move focus to first focusable inside body, remember previous activeElement

  "hide":
    body = ...
    Shiny.unbindAll(body)
    remove `.blockr-sidebar-open` class
    set aria-hidden="true"
    restore focus to remembered activeElement
```

Two handlers maximum (or one with a `data.action` switch). The pin button and Esc-to-close are wired up at `sidebar_ui()` render time as plain `addEventListener` calls — they only toggle a class and update `--blockr-sidebar-width` on `<body>`; no message round-trip.

> **Lesson from the previous attempt.** The `unbindAll` → `renderDependencies` → `replaceChildren` → `initializeInputs` → `bindAll` order is what we landed on; sticking with it.

### Dependency-aware content

`sidebar_show()` calls `htmltools::renderTags(ui)` and ships both the produced HTML and `htmltools::resolveDependencies(rendered$dependencies)` in the message payload. The JS calls `Shiny.renderDependencies()` before `bindAll()` so content that pulls in new CSS / JS (selectize, htmlwidgets, etc.) works on the first show.

### Pinned mode

The panel header has a pin toggle. When pinned + open, JS sets `--blockr-sidebar-width` on `<body>` to the panel's pixel width. The CSS-equipped page can use `margin-right: var(--blockr-sidebar-width, 0px)` on its main container to reflow beside the panel rather than overlay it. When closed (or open-but-unpinned), the variable is `0px`. Esc does *not* close a pinned panel.

### Shiny input binding

`blockr.ui` ships a `Shiny.InputBinding` registered against `.blockr-sidebar` that exposes `list(open, pinned)` to R via `input[[id]]`. `getValue` reads the panel's `.blockr-sidebar-open` and `.blockr-sidebar-pinned` classes. `subscribe` listens for a custom `blockr-sidebar:state` event that the JS dispatches whenever it toggles either class — show / hide messages from R, the close button, the pin toggle, the Esc-to-close handler. Everything that flips state on the panel ends with `el.dispatchEvent(new CustomEvent("blockr-sidebar:state"))` so the binding fires `callback()`.

There's no `setValue` / `receiveMessage` on the binding — R-side state changes go through the custom-message handler (`sidebar_show()` / `sidebar_hide()`), and the binding observes the resulting class flip just like any other client-driven flip. One code path, no duplication.

R-side, this is straightforwardly:

```r
observe({
  state <- input$main_sidebar
  if (is.null(state)) return()
  cat("open:", state$open, " pinned:", state$pinned, "\n")
})
```

Useful for two patterns:

- **Auto-open on empty board.** When a renderer's session starts and the board has no blocks, the renderer can call `sidebar_show(id, hint_ui)` from a `session$onSessionInitialized` (or just the top of the server function). If the user dismisses the hint via Esc / X, `input[[id]]$open` flips to `FALSE`; an observer can read that to suppress re-opening on subsequent reactive flushes (or to cleanly stop hint-related observers).
- **Coordinating multiple flows.** An action handler that wants to short-circuit when the sidebar is already open with a different content can read `input[[id]]$open` and decide whether to overwrite or skip. (R-side still owns "what content is showing" — we don't track that client-side.)

### What about the input id?

`sidebar_ui("main_sidebar")` renders a `<div id="main_sidebar" class="blockr-sidebar">`. That id is the one passed to `sidebar_show()` / `sidebar_hide()`. Custom messages aren't namespaced, so the JS reads it directly. The caller of `sidebar_ui()` is responsible for picking a unique id — typically a fixed string like `"main_sidebar"` for an app with one panel, or a session-namespaced one (`NS(id, "main_sidebar")`) for a renderer module. The same id is what R reads via `input[[id]]` to get the binding's `{open, pinned}` value.

## Use in `blockr.dock` (Phase 2 of this change)

For each existing modal-using action handler, the change is mechanical:

```r
# Before:
add_block_action <- function(trigger, board, update, ...) {
  new_action(function(input, output, session) {
    blk <- reactiveVal()
    observeEvent(trigger(), {
      blk(NULL)
      showModal(block_modal(session$ns, board$board, mode = "add"))
    })
    observeEvent(input$add_block_selection, { ... })
    observeEvent(input$add_block_confirm, {
      ...
      update(list(blocks = list(add = bk)))
      removeModal()
    })
    NULL
  }, id = "add_block_action")
}

# After:
add_block_action <- function(trigger, board, update, ...,
                             sidebar_id = "main_sidebar") {
  new_action(function(input, output, session) {
    blk <- reactiveVal()
    observeEvent(trigger(), {
      blk(NULL)
      blockr.ui::sidebar_show(
        sidebar_id,
        title = "Add new block",
        ui = block_modal_body(session$ns, board$board, mode = "add")
      )
    })
    observeEvent(input$add_block_selection, { ... })   # unchanged
    observeEvent(input$add_block_confirm, {
      ...
      update(list(blocks = list(add = bk)))
      blockr.ui::sidebar_hide(sidebar_id)
    })
    NULL
  }, id = "add_block_action")
}
```

`block_modal_body()` is `block_modal()` with the outer `modalDialog()` wrapper stripped — same `tagList` of inputs, same input ids. All existing `observeEvent(input$X, ...)` handlers keep working: the form is now inside the sidebar body but its inputs are namespaced under the same `session$ns()`.

Same shape for `append_block_action`, `prepend_block_action`, `add_link_action`, `add_stack_action`, `edit_stack_action`. The settings gear in `board_ui.dock_board()`'s navbar gets a small `observeEvent(input$settings_btn, ...)` that calls `sidebar_show()` with the existing options-accordion content; the Bootstrap settings offcanvas markup is removed.

`board_ui.dock_board()` mounts the sidebar with `blockr.ui::sidebar_ui(NS(id, "main_sidebar"))`.

## Risks / Trade-offs

- **No S3 dispatch on body content** means extensions can't add new content "types" via S3 method registration. Trade-off: extensions just call `sidebar_show()` themselves with whatever tags they want — same model as `showModal()`. If a future extension wants a typed dispatch system on top, it can build one.
- **No `input[[id]]$state` value at v0** means R-side code can't react to user-driven close (Esc / X button) without observing… nothing. Trade-off: the modal-replacement use case doesn't need it. Adding a `<id>__closed` custom input later is straightforward and additive.
- **One sidebar per app at v0.** The id is a free-form DOM id chosen by the caller, so multi-sidebar apps work today as long as ids are unique. We just don't ship explicit guidance for it. No API change needed if multi-sidebar becomes a thing.
- **Settings gear loses Bootstrap offcanvas styling.** The settings content (board options accordion) renders inside the sidebar body. Visually different from before (right slide-in vs Bootstrap offcanvas) but uses the same accordion content; all existing option observers (owned by `blockr.core`) keep firing on input changes inside the sidebar body.

## Migration (`blockr.dock`)

Phase 2 of this change. Six action handlers switched + one settings-gear observer added + one `board_ui.dock_board()` slot added. Modal-body builders stay (private), lose their `modalDialog()` wrapper. Settings Bootstrap offcanvas markup deleted. No public API change in `blockr.dock`; no lifecycle shims needed.
