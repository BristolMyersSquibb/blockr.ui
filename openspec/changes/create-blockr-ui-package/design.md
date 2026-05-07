## Context

`blockr.dock` today is the only place that owns "what a blockr-flavoured page looks like": tokens, typography, block cards, dropdowns, the sparkle UI, modal builders, the navbar, the board-options accordion, and the page shell. That has accreted into ~8.5k lines of `R/` + `inst/assets/`, of which a substantial fraction has nothing to do with DockView. Two unrelated symptoms point at the same cause:

1. Every extension (`blockr.ai`, `blockr.io`, `blockr.sdtm`, `blockr.dplyr`, `blockr.quarto`, `blockr.dag`, `blockr-demo-*`) and every embedded / non-dock app must depend on `blockr.dock` purely for the design system. There is no clean dependency for a non-dock renderer or a single-block preview.
2. The modal-driven add flow (`block_modal()` → `removeModal()` → repeat) interrupts the build cadence; a persistent sidebar (prototyped on `blockr.dock@feat/sidebar-s3-dispatch`) tested well with users but landed in the wrong package.

The fix is one architectural move: extract a `blockr.ui` package between `blockr.core` and `blockr.dock` that owns the cross-cutting UI surface (tokens, base CSS, block card, page chrome, sidebar). `blockr.dock` keeps only what is genuinely about docking: DockView integration, layouts, view nav, action class, dock extensions, and a hidden `<div>` pool replacing the offcanvas misuse.

## Goals / Non-Goals

**Goals:**

- A new `blockr.ui` package with its own DESCRIPTION / NAMESPACE / pkgdown / NEWS / semver. Imports: `blockr.core`, `shiny`, `htmltools`, `rlang`. **No `dockViewR`. No `bslib::offcanvas`.**
- `blockr.dock` adds `blockr.ui` to its `Imports`; no code in `blockr.ui` imports from `blockr.dock`.
- A clean dependency graph: `blockr.core` ← `blockr.ui` ← `blockr.dock` ← extensions. Sibling renderers (`blockr.canvas`, …) can sit at the same layer as `blockr.dock` and reuse the entire shell / navbar / sidebar / block card surface unchanged.
- Replace `block_modal()` / `link_modal()` / `stack_modal()` and the Bootstrap settings offcanvas with a single sidebar primitive that supports pinned mode, swappable content via S3 dispatch, a Shiny `InputBinding`, and an extension hook for downstream content types.
- Replace the misused `blocks_offcanvas` / `exts_offcanvas` (invisible DOM staging) with a plain hidden `<div>` injected by `blockr.dock::dock_pool_ui()`.
- Backwards compatibility for one deprecation cycle: every previously-exported symbol that moves gets a `lifecycle::deprecate_warn()` shim in `blockr.dock`. Existing extensions keep working without code changes.

**Non-Goals:**

- No drag-and-drop between sidebar and canvas.
- No sidebar ↔ dock-panel promotion. Pinned sidebars stay sidebars.
- No theming API at v0. Tokens stay hard-coded in `blockr.ui`'s CSS files; `bslib`-style theming is a follow-up.
- No retention of modal builders. `block_modal()` / `link_modal()` / `stack_modal()` are migration scaffolding inside `blockr.dock` only, deleted at the end of the migration.
- No changes to `blockr.core`.
- No JS framework (React / Vue / Svelte). The sidebar is plain JS plus a Shiny `InputBinding`.
- No keyboard chord system. Only the per-component shortcuts listed in the spec (Esc, arrows, Enter).
- No multiple-sidebar-per-app design at v0. The API takes `id` so it can grow there later, but action handlers default to `sidebar_id = "main_sidebar"`.

## Decisions

### Package layering

```text
blockr.core                 board / block / link / stack data model, reactivity,
  ^                         serialization, registry
  |
blockr.ui                   tokens, base CSS, block card, sparkle UI, selectize
  ^                         widgets, button helpers, app shell, navbar, options
  |                         renderer, sidebar primitive, sidebar content registry,
  |                         block browser
blockr.dock                 DockView wrapper, dock outputs container, view nav,
  ^                         layouts and dock_layouts, action class and triggers,
  |                         board mutation actions, hidden block / extension pool,
  |                         dock-only CSS
blockr.{ai,io,sdtm,...}     extensions
```

`blockr.ui` is the only place that knows what a blockr-flavoured page looks like. `blockr.dock` is the only place that knows what a dock layout looks like. A sibling `blockr.<other>` renderer fills `app_shell()`'s `content` slot with its own widget and reuses the entire shell / navbar / sidebar / block card surface unchanged.

### Extensibility model: S3 generics for variation points

`blockr.core` already uses S3 (`block_ui()`, `board_ui()`, `remove_block_ui()`, `insert_block_ui()` dispatch on board class). `blockr.ui` introduces five new generics where downstream variation is realistic; everything else stays concrete.

| Generic | Dispatches on | Default behaviour | Concrete variation example |
| --- | --- | --- | --- |
| `app_shell()` | board (`x`) | standard 3-slot shell (navbar, sidebar, content) | a non-dock renderer adding a footer or a left-rail slot |
| `blockr_navbar()` | board (`x`) | `.blockr-navbar` with `left` / `right` slots and an optional settings gear | a renderer adding undo / redo, or hiding the settings gear |
| `options_ui()` | board (`x`) | accordion of board option categories | a renderer grouping options differently |
| `block_card()` | block (`blk`) | the sparkle card we have today | a `data_block` showing a "data source" badge other blocks should not show |
| `sidebar_content()` | trigger | dispatch table for built-in content types | an extension registering an AI-assistant content type |

Helpers that intentionally stay concrete: `sidebar_ui()`, all selectize widgets, `toggle_button()`, `confirm_button()`, `auto_focus_script()`, all trigger constructors, all sidebar R helpers (`sidebar_open` / `close` / `toggle` / `set_content`). `off_canvas()` and `move_dom_element()` are not part of `blockr.ui` at v0; they stay in `blockr.dock` (see the package-boundary note below).

**Generic signatures.** All UI generics dispatching on board take `(id, x, ...)` so dispatch is on the second argument, matching `blockr.core::board_ui(id, x, ...)`:

```r
app_shell      <- function(id, x, ...) UseMethod("app_shell", x)
blockr_navbar  <- function(id, x, ...) UseMethod("blockr_navbar", x)
options_ui     <- function(id, x, ...) UseMethod("options_ui", x)
block_card     <- function(blk, blk_id, ...) UseMethod("block_card", blk)
sidebar_content <- function(x, ...) UseMethod("sidebar_content")
```

> **Correction vs. the original design doc.** An earlier sketch in the design (the `board_ui.dock_board()` example) called `blockr_navbar(left = …, right = …, settings_id = …)` without `id` / `x`. The signatures above are the canonical ones; **all call sites pass `(id, x, …)`**. The two sketches in §3-design had drifted.

A `dock_board` does not need its own `app_shell.dock_board()` / `blockr_navbar.dock_board()` / `options_ui.dock_board()` method out of the gate — the default does what we want today. The methods exist as future extension points; if dock-only customisation lands later, it is added at that time as a single `S3method()` registration and a small method body, with no API change.

### Page chrome (canonical sketch)

```r
# blockr.dock/R/board-ui.R, post-refactor
board_ui.dock_board <- function(id, x,
                                plugins = board_plugins(x),
                                options = board_options(x), ...) {
  views      <- board_views(x)
  sidebar_id <- NS(id, "main_sidebar")

  blockr.ui::app_shell(
    id, x,
    deps    = list(blockr.ui::blockr_ui_dep(), blockr_dock_dep()),
    navbar  = blockr.ui::blockr_navbar(
      id, x,
      left        = opt_ui_or_null("preserve_board", plugins, x),
      right       = view_nav_ui(id, views),
      settings_id = sidebar_id
    ),
    sidebar = blockr.ui::sidebar_ui(sidebar_id),
    content = tagList(
      dock_outputs_ui(id, views),     # dock-specific
      dock_pool_ui(id)                # dock-specific (replaces offcanvases)
    )
  )
}
```

`blockr_navbar()` itself renders the settings gear when `settings_id` is non-NULL; the gear's click handler (wired in JS by `blockr.ui`) calls `sidebar_set_content(settings_id, settings_trigger())`.

### Hidden block / extension pool

Today's `hide_block_ui()` / `hide_ext_ui()` move DOM nodes into `.offcanvas-body` of an offcanvas that exists only to be a hidden container. Replacement:

```r
# blockr.dock/R/dock-pool.R (new)
dock_pool_ui <- function(id) {
  div(
    id           = NS(id, "block_pool"),
    class        = "blockr-dock-pool",
    `aria-hidden` = "true",
    hidden       = NA            # display: none, removed from a11y tree
  )
}
```

`hide_block_ui()` / `hide_ext_ui()` (kept in `blockr.dock`) change one line each: their destination selector becomes `paste0("#", board_ns("block_pool"))` instead of the offcanvas body. A single shared pool is enough; if a downstream concern emerges, the helper can take a `kind` argument and produce two pools later.

The R wrapper `move_dom_element()` and its `move-element` Shiny custom message handler stay in `blockr.dock` together (the dock pool is their only v0 consumer). No cross-package split, no separate `dock-move-guard.js` — the guard ("silently drop moves into inactive view docks") is folded into the single handler in `blockr.dock`.

### CSS layering

`blockr.ui` ships five CSS files, bundled into a single `htmlDependency` (`blockr_ui_dep()`):

| File | Contents |
| --- | --- |
| `blockr-tokens.css` | `:root { --blockr-* }` design tokens (palettes, shadows, font sizes / weights, semantic colours) |
| `blockr-base.css` | Typography (`.blockr-title`, `.blockr-label`, …), button / form overrides, error styling, issues toggle, inline-edit, popover / tooltip overrides, badges |
| `blockr-shell.css` | App shell layout, `.blockr-navbar*` |
| `blockr-block.css` | Block card chrome, dropdown, sparkle UI, edit-block layout, ctrl panel |
| `blockr-sidebar.css` | Sidebar shell, slide transition, header, pinned-mode reflow, focus-trap visuals |

`blockr.dock`'s remaining CSS shrinks to: DockView (`dv-*`) overrides, view nav (`.blockr-view-*`), empty-dock prompt, `.blockr-dock-pool`, `g6-toolbar`.

Load order is enforced via `htmltools::resolveDependencies()`: `blockr.dock`'s dependency lists `blockr.ui`'s as a transitive dependency, so `blockr.ui` always loads first.

### Sidebar — DOM, state, and binding

```html
<div id="my_sidebar" class="blockr-sidebar"
     data-side="right" data-content-type="" aria-hidden="true">
  <div class="blockr-sidebar-header">
    <span class="blockr-sidebar-title"></span>
    <button class="blockr-sidebar-pin"   aria-pressed="false" title="Pin"></button>
    <button class="blockr-sidebar-close"                      title="Close"></button>
  </div>
  <div class="blockr-sidebar-body"><!-- content swapped in --></div>
</div>
```

State lives entirely in DOM classes / attributes on `.blockr-sidebar` so a DOM snapshot is enough to restore the panel:

- `.blockr-sidebar-open` — visible, pointer-events on, `aria-hidden="false"`.
- `.blockr-sidebar-pinned` — pin pressed; layout reflows the page beside the panel via `--blockr-sidebar-width` on the body.

The Shiny `InputBinding` exposes `list(open, pinned, content)`:

```js
class SidebarBinding extends Shiny.InputBinding {
  find(scope) { return $(scope).find('.blockr-sidebar'); }

  getValue(el) {
    return {
      open:    el.classList.contains('blockr-sidebar-open'),
      pinned:  el.classList.contains('blockr-sidebar-pinned'),
      content: el.dataset.contentType || null,
    };
  }

  subscribe(el, callback) {
    el.addEventListener('blockr-sidebar:state', () => callback());
    el.querySelector('.blockr-sidebar-close').addEventListener('click', () => {
      this._setOpen(el, false); this._emit(el);
    });
    el.querySelector('.blockr-sidebar-pin').addEventListener('click', () => {
      el.classList.toggle('blockr-sidebar-pinned'); this._emit(el);
    });
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && this.getValue(el).open && !this.getValue(el).pinned) {
        this._setOpen(el, false); this._emit(el);
      }
    });
  }

  receiveMessage(el, data) {
    switch (data.action) {
      case 'open':        this._setOpen(el, true); break;
      case 'close':       this._setOpen(el, false); break;
      case 'toggle':      this._setOpen(el, !this.getValue(el).open); break;
      case 'set-content':
        const body = el.querySelector('.blockr-sidebar-body');
        Shiny.unbindAll(body);                       // clean up old bindings
        body.replaceChildren(...$.parseHTML(data.html));
        Shiny.initializeInputs(body);                // initialize fresh inputs
        Shiny.bindAll(body);                         // wire new bindings
        el.dataset.contentType = data.content;
        if (!this.getValue(el).open) this._setOpen(el, true);
        break;
    }
    this._emit(el);
  }

  _setOpen(el, open) {
    el.classList.toggle('blockr-sidebar-open', open);
    el.setAttribute('aria-hidden', open ? 'false' : 'true');
    document.body.style.setProperty('--blockr-sidebar-width',
      (open && el.classList.contains('blockr-sidebar-pinned')) ? el.offsetWidth + 'px' : '0px');
    if (open) this._trapFocus(el); else this._releaseFocus(el);
  }

  _emit(el) { el.dispatchEvent(new CustomEvent('blockr-sidebar:state')); }
}

Shiny.inputBindings.register(new SidebarBinding(), 'blockr.sidebar');
```

> **Correction vs. the original sketch.** The original `set-content` handler called only `Shiny.bindAll(body)` after `replaceChildren`. To prevent stale Shiny bindings from leaking on every content swap, we must call `Shiny.unbindAll(body)` *before* replacing the children. The sketch above shows the full unbind → replace → initialize → bind sequence.

**Focus trap.** `_trapFocus(el)` records `document.activeElement` (so close can restore it), moves focus to the first focusable element inside the panel, and installs a `keydown` listener on the panel that intercepts `Tab` / `Shift+Tab` to cycle focus among focusable descendants. `_releaseFocus(el)` removes the listener and returns focus to the recorded element. The implementation is plain JS, no library; it must re-find focusables after each `set-content` because content swaps change the focusable set.

### Sidebar — dependency-aware content swap

The simple "render server-side, ship HTML over the wire" approach hits two snags:

1. Content like the block browser may need CSS / JS dependencies (selectize, custom widget JS) that aren't loaded yet.
2. Server-side observers for the new content (e.g. `block_browser_server()`) need to be started when content swaps in, and torn down when it swaps out.

We resolve this with one decision: **`sidebar_set_content()` pre-renders content via `htmltools::renderTags()` (not just `doRenderTags()`), forwards the produced HTML *and* any new dependencies to the client, and lets the client install missing dependencies via `Shiny.renderDependencies()` before binding.**

```r
sidebar_set_content <- function(id, trigger, ..., session = shiny::getDefaultReactiveDomain()) {
  stopifnot(inherits(trigger, "sidebar_trigger"))
  rendered <- htmltools::renderTags(sidebar_content(trigger, ...))
  session$sendInputMessage(id, list(
    action       = "set-content",
    content      = setdiff(class(trigger), "sidebar_trigger")[1L],
    html         = rendered$html,
    dependencies = htmltools::resolveDependencies(rendered$dependencies)
  ))
}
```

```js
// in receiveMessage 'set-content':
if (data.dependencies) Shiny.renderDependencies(data.dependencies);
```

This is the pattern `shiny::renderUI()` uses internally and is robust against new content types adding new deps.

**Server-side wiring.** `sidebar_content.<x>()` methods own UI; their server-side observers live in a single `sidebar_server(id, board, …)` module that runs *once* alongside `sidebar_ui()` and uses an `observe()` over the binding's `content` field to dispatch to the right per-content server (`block_browser_server()`, `add_link_server()`, …) using `removeUI()` / `insertUI()`-style or namespaced module activation. Methods that introduce a new server-side flow ship as **paired** generics: `sidebar_content()` (UI) and an internal `sidebar_content_server()` (server). For simple content with no server logic (e.g. `settings_trigger()` rendering `options_ui()`, where the underlying option observers are owned by `blockr.core`), `sidebar_content_server()` is a no-op.

> **Correction vs. the original spec.** The `2-requirements.md` and `3-design.md` documents framed `sidebar_content()` as returning a `shiny.tag` and stopped there. They did not describe how content-side observers (search inputs, quick-add clicks, link source/target selectors) are mounted and unmounted on content swap. The companion `sidebar_content_server()` generic above closes that gap. This is the most material addition vs. the original design.

### R helpers and `get_session()`

```r
sidebar_open  <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, list(action = "open"))
}
sidebar_close  <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, list(action = "close"))
}
sidebar_toggle <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(id, list(action = "toggle"))
}
```

> **Correction vs. the original spec.** The original design used `session = get_session()` without defining `get_session()` in the public API. We standardise on `shiny::getDefaultReactiveDomain()` (the canonical Shiny idiom) so the helpers work inside any reactive context without an internal alias.

### Trigger objects (S3)

```r
new_sidebar_trigger <- function(class, ...) {
  structure(list(...), class = c(class, "sidebar_trigger"))
}

add_block_trigger     <- function(...)         new_sidebar_trigger("add_block",     ...)
append_block_trigger  <- function(source, ...) new_sidebar_trigger("append_block",  source = source, ...)
prepend_block_trigger <- function(target, ...) new_sidebar_trigger("prepend_block", target = target, ...)
add_link_trigger      <- function(source = NULL, ...) new_sidebar_trigger("add_link", source = source, ...)
create_stack_trigger  <- function(...)         new_sidebar_trigger("create_stack",  ...)
edit_stack_trigger    <- function(stack, ...)  new_sidebar_trigger("edit_stack",    stack = stack, ...)
settings_trigger      <- function(...)         new_sidebar_trigger("settings",      ...)
```

`sidebar_content.<class>()` methods receive any extra args passed to `sidebar_set_content(id, trigger, ...)` — this is how `blockr.dock` forwards its `board` reactive into a `sidebar_content.add_block` invocation without `blockr.ui` ever knowing about boards at the trigger-construction site.

### Block card extraction

```r
# In blockr.ui
block_ui.board <- function(id, x, edit_ui, blocks = NULL, ...,
                           plugins = board_plugins(x), session = shiny::getDefaultReactiveDomain()) {
  imap(blocks, function(blk, blk_id) {
    block_card(blk, blk_id, plugins$edit_block, x, session$ns)
  })
}

# In blockr.dock
block_ui.dock_board <- function(id, x, edit_ui, blocks = NULL, ...,
                                plugins = board_plugins(x), session = shiny::getDefaultReactiveDomain()) {
  cards <- NextMethod()                        # blockr.ui::block_ui.board
  imap(cards, wrap_dock_panel)
}
```

`wrap_dock_panel(card, blk_id, ...)` (in `blockr.dock`) wraps a card with the panel handle and panel-id attributes DockView needs. It is reused by `insert_block_ui.dock_board()` so the wrapping markup is defined in one place. The card content itself is unchanged; non-dock apps now get the same look by calling `block_ui()` on a plain `board` without DockView.

### Action flow migration

Before:

```r
add_block_action <- function(trigger, board, update, ...) {
  observeEvent(trigger(), {
    showModal(block_modal(session$ns, board$board, mode = "add"))
  })
  observeEvent(input$confirm_add_block, {
    update(board, ...)
    removeModal()
  })
}
```

After:

```r
add_block_action <- function(trigger, board, update, ...,
                             sidebar_id = "main_sidebar") {
  observeEvent(trigger(), {
    blockr.ui::sidebar_set_content(
      sidebar_id,
      blockr.ui::add_block_trigger(),
      board = board$board                       # forwarded to sidebar_content method
    )
  })
  # Confirmation event is now `input[[NS(sidebar_id, "confirm_add")]]`
  # from the sidebar's content module; no removeModal() needed.
}
```

Same pattern for `append_block_action()` / `prepend_block_action()` / `add_link_action()` / `add_stack_action()` / `edit_stack_action()`. The Bootstrap settings offcanvas becomes another `sidebar_set_content(id, settings_trigger())` call.

### Backwards compatibility

Symbols that simply move and remain exported (`block_card`, `block_input_select`, `block_registry_selectize`, `board_select`, plus other `@export`s that move) get a `lifecycle::deprecate_warn()` shim in `blockr.dock` for one cycle:

```r
#' @keywords internal
#' @export
block_card <- function(...) {
  lifecycle::deprecate_warn(
    "0.next.0",
    "blockr.dock::block_card()",
    "blockr.ui::block_card()"
  )
  blockr.ui::block_card(...)
}
```

Symbols that disappear entirely (`block_modal`, `link_modal`, `stack_modal`, `css_modal`, `css_modal_advanced`) are listed under "Breaking changes" in `blockr.dock`'s `NEWS.md` with the sidebar-trigger replacement noted. Internal-only helpers (no `@export`) move silently.

> **Note for `blockr.dock`'s DESCRIPTION.** The shims call `lifecycle::deprecate_warn()`, so `lifecycle` must be added to `blockr.dock`'s `Imports` if it isn't already. (`blockr.ui` itself does not depend on `lifecycle`.)

## Risks / Trade-offs

- **CSS load order across two packages.** Splitting one CSS file between `blockr.ui` and `blockr.dock` risks selector overrides loading in the wrong order. → `blockr.dock`'s `htmlDependency` lists `blockr.ui`'s as a transitive dependency via `htmltools::resolveDependencies()`; `blockr.ui` always loads first. Visual regression tests on the dock app at every phase boundary.
- **Selectize-render JS drift.** The R-side selectize helpers and the JS render function evolve together. → They move together to `blockr.ui` so the cross-package coupling disappears.
- **Lifecycle warnings noise in running apps.** Every Phase 1 re-export emits a deprecation warning the first time it is called. → Document the migration in `blockr.dock`'s `NEWS.md`; suggest `lifecycle.verbosity = "default"` for noisy upstreams.
- **Modal-to-sidebar feature parity.** The detailed-add modal accordion (custom name / id / inputs) must land 1:1 in the sidebar's detailed-add view, otherwise users lose functionality. → Phase 3 ships an end-to-end shinytest2 covering quick-add, detailed-add, and three-blocks-in-pinned-mode.
- **Stale Shiny bindings on content swap.** Replacing `.blockr-sidebar-body` children must unbind first or input/output bindings leak. → Decision section above pins the unbind → replace → initialize → bind sequence in the JS handler.
- **Dependency injection on content swap.** Content types may introduce CSS / JS deps not loaded at page render. → `sidebar_set_content()` ships `htmltools::renderTags()` deps; client calls `Shiny.renderDependencies()` before binding.
- **Server-side teardown on content swap.** Content-side observers (search box, quick-add) need to be started/stopped when content swaps. → A companion `sidebar_content_server()` generic is invoked alongside `sidebar_content()`; a single `sidebar_server(id, board, …)` module dispatches based on the binding's `content` field.
- **One-sidebar-per-app assumption.** v0 assumes a single sidebar instance per app at a known `id`. → API takes `id`; action handlers accept `sidebar_id` (default `"main_sidebar"`). Multi-sidebar support is API-compatible and can land later.

## Migration Plan

The split happens in five phases, each shippable independently with green CI at every step.

- **Phase 0 — package skeleton.** `usethis::create_package("blockr.ui")`, set `Imports`, scaffold testthat / pkgdown / NEWS / GitHub Actions. CI green; nothing in `blockr.dock` changes yet.
- **Phase 1 — tokens, base CSS, block card, helpers, page chrome.** Move the cross-cutting UI surface that requires only relocation. CSS split into five files; R files relocated as detailed in tasks.md; lifecycle shims added in `blockr.dock`. The two staging Bootstrap offcanvases are deleted at this phase and replaced by `dock_pool_ui()`. The settings offcanvas remains until Phase 4.
- **Phase 2 — sidebar primitive.** New code, no migration of existing flows yet: sidebar UI / binding / R helpers / CSS / JS, content-registry stubs, vignette. `blockr.dock` does not yet use the sidebar.
- **Phase 3 — block-add flow on sidebar.** Implement `block_browser_ui()` / `block_browser_server()`; flesh out `sidebar_content.add_block` / `.append_block` / `.prepend_block`; switch `add_block_action()` / `append_block_action()` / `prepend_block_action()` from `showModal()` to `sidebar_set_content()`. `block_modal()` is unused but still present.
- **Phase 4 — remaining flows + settings offcanvas removal.** Migrate `add_link_action()` / `add_stack_action()` / `edit_stack_action()`; implement their content methods; move board settings to `sidebar_content.settings`. Delete `action-modal.R`, `block_modal()`, `link_modal()`, `stack_modal()`, `css_modal()`, `css_modal_advanced()`; delete the settings offcanvas markup; list deletions under "Breaking changes" in `blockr.dock`'s `NEWS.md`.
- **Phase 5 — cleanup.** Drop the lifecycle shims if a release window has elapsed; re-evaluate whether `blockr.dock` still needs `bslib`; update README and dependency-graph diagrams.

> **Correction vs. the original implementation doc.** §4-implementation framed Phase 1 as "the staging offcanvases are still present; they are removed at Phase 4" but later said the file-by-file table deletes them at Phase 1, and Phase 4's description confirmed they were "already gone after Phase 1". Resolved here: **the two staging offcanvases (`blocks_offcanvas`, `exts_offcanvas`) are deleted at Phase 1**, replaced by `dock_pool_ui()`. Only the settings offcanvas survives until Phase 4.

## Open Questions

1. **Action package?** The action class and registration system (currently in `blockr.dock`'s `R/action-class.R` plus `R/action-*.R`) are not really dock-specific. A future `blockr.actions` package could host them, with `blockr.dock` providing a `register_actions()` wiring layer. **Out of scope for v0**; flag for re-evaluation after Phase 5.
2. **Pin-state persistence.** Should the pinned state survive page reload (cookie / `localStorage`)? **Out of scope for v0**; the binding can grow this without API change.
3. **Content-side validation.** Where does validation (non-empty name, unique id, …) live? **Resolved for v0:** validation stays in the action handler in `blockr.dock`, mirroring the modal case. Content methods build inputs and surface raw values; the action handler validates before mutating the board. (The original spec left this open; we close it here.)
4. **Multiple sidebars per app.** Not supported at v0. The API takes `id`, so multi-sidebar can land later with no API change; action handlers will need a way to discover the right `sidebar_id` (resolver function, board option, …).
