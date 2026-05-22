## 1. Phase 0 — package skeleton (DONE)

- [x] 1.1 `usethis::create_package("blockr.ui")` with `Authors@R`, `License: GPL (>= 3)`, `Encoding: UTF-8`, `Roxygen: list(markdown = TRUE)`
- [x] 1.2 Set `Imports: blockr.core, htmltools, rlang, shiny` in `DESCRIPTION` (no `dockViewR`, no `bslib`)
- [x] 1.3 Set `Suggests: testthat (>= 3.0.0), knitr, rmarkdown, shinytest2, withr`
- [x] 1.4 `usethis::use_testthat(3)`, `use_pkgdown()`, `use_news_md()`, `use_package_doc()`
- [x] 1.5 `usethis::use_github_action("check-standard")` (CI uses `cynkra/blockr.ci` reusable workflow)
- [x] 1.6 `R/blockr.ui-package.R` with `"_PACKAGE"` and `@importFrom rlang %||%` / `@importFrom htmltools tagList` / `@importFrom shiny tags`
- [x] 1.7 `R/utils-pkg.R` with `pkg_file()` lifted from `blockr.dock`, `@importFrom blockr.core pkg_name`
- [x] 1.8 `devtools::check()` clean (0 errors / 0 warnings / 1 NOTE — timestamp only)

## 2. Phase 1 — sidebar primitive in `blockr.ui` (PR 1)

Self-contained: no `blockr.dock` change. After this phase, a non-dock Shiny app can `library(blockr.ui)` and use `sidebar_ui()` + `show_sidebar()` + `hide_sidebar()` to drive a slide-in panel with arbitrary content.

- [x] 2.1 Create `blockr.ui/inst/assets/css/blockr-sidebar.css` with the panel shell (fixed right or left, slide transition, header with title + pin + close, body region), `.blockr-sidebar-open` open state, `.blockr-sidebar-pinned` pinned state, focus-trap visuals.
- [x] 2.2 Create `blockr.ui/inst/assets/js/blockr-sidebar.js`: register a single `Shiny.InputBinding` against `.blockr-sidebar` whose `receiveMessage(el, data)` switches on `data.action` (`"show"` / `"hide"`), implementing the `unbindAll → renderDependencies → replaceChildren → initializeInputs → bindAll` sequence on show, and `unbindAll` + class-removal on hide. NO standalone `Shiny.addCustomMessageHandler(...)` registration — the binding owns both directions of the R↔JS conversation. Also wire pin button (toggle `.blockr-sidebar-pinned` + update `--blockr-sidebar-width` on `<body>`), close button, Esc-to-close (only when not pinned), outside-click-to-close (only when not pinned), and a Tab/Shift+Tab focus trap that re-finds focusables after each content swap. Every code path that flips `.blockr-sidebar-open` or `.blockr-sidebar-pinned` MUST end with `el.dispatchEvent(new CustomEvent("blockr-sidebar:state"))` so the binding's `subscribe` callback fires.
- [x] 2.3 In the same `Shiny.InputBinding` from 2.2: `find()` returns matching elements; `initialize(el)` runs the per-panel listener wiring; `getValue(el)` returns `list(open = el.classList.contains("blockr-sidebar-open"), pinned = el.classList.contains("blockr-sidebar-pinned"))`; `subscribe(el, callback)` listens for the `blockr-sidebar:state` custom event on `el` and calls `callback()`; `unsubscribe(el)` removes the listener. No `setValue()` override. Register with `Shiny.inputBindings.register(binding, "blockr.ui.sidebar")`.
- [x] 2.4 Implement `R/sidebar.R`:
  - `sidebar_ui(id, side = c("right", "left"), width = "360px")` — returns a `<div id="<id>" class="blockr-sidebar" data-side aria-hidden="true">` with header + body slots; attaches `sidebar_dep()` via `htmltools::attachDependencies()` so callers don't need to attach the dep separately.
  - `sidebar_dep()` — `htmltools::htmlDependency()` referencing the CSS + JS under `inst/assets/`.
  - `show_sidebar(id, ui, title = NULL, session = shiny::getDefaultReactiveDomain())` — pre-renders `ui` via `htmltools::renderTags()` and dispatches via `session$rootScope()$sendInputMessage(id, list(action = "show", html = rendered$html, dependencies = lapply(htmltools::resolveDependencies(rendered$dependencies), shiny::createWebDependency), title = title))`. Walking to `rootScope` ensures the panel id is treated as an absolute DOM id (no namespace prefix from the calling module).
  - `hide_sidebar(id, session = shiny::getDefaultReactiveDomain())` — `session$rootScope()$sendInputMessage(id, list(action = "hide"))`.
  - `sidebar_state(id, session = shiny::getDefaultReactiveDomain())` — snapshot helper returning `list(open, pinned)`. Walks to `session$rootScope()$input[[id]]` and `shiny::isolate()`s the read so action-handler observers can branch on pin state without picking up a reactive dependency. Returns `list(open = FALSE, pinned = FALSE)` if the binding has not yet reported.
  - `keep_or_hide_sidebar(id, ui, title = NULL, session = shiny::getDefaultReactiveDomain())` — convenience helper that branches on `sidebar_state(id, session)$pinned`: pinned -> `show_sidebar(id, ui, title, session)` (re-render fresh body for chaining), unpinned -> `hide_sidebar(id, session)`. Lives in `blockr.ui` so consumers (e.g. `blockr.dock`) don't duplicate the chain/hide logic.
- [x] 2.5 Tests: `tests/testthat/test-sidebar.R` covering (a) `sidebar_ui()` markup is Bootstrap-free (no `.offcanvas` / `.modal` / `data-bs-*` anywhere), (b) `sidebar_dep()` references the right files, (c) `show_sidebar()` sends the expected payload via a `shiny::MockShinySession` whose `sendInputMessage` is overridden to capture, (d) `hide_sidebar()` likewise, (e) calls dispatched from a nested `makeScope("mod_a")` proxy still target the absolute id (`rootScope` walking).
- [x] 2.6 Vignette `vignettes/sidebar.Rmd`: minimal example showing how to mount a sidebar, open it from R with arbitrary content, observe the inputs inside the body, and dismiss it on confirm — modelled on Shiny's modal vignette. Include a short "auto-open on empty board" recipe that uses `input[[id]]$open` to avoid re-opening after a user dismissal.
- [x] 2.7 `devtools::check()` on `blockr.ui` clean (0/0/0 modulo timestamp NOTE); all tests pass.

### Static-content support (amendment to PR 1)

Folded into the same PR. Closes a gap exposed during Phase 2 adoption: today the only way to populate a panel's body is `show_sidebar(id, ui, ...)` from the server, which forces every static-content panel through a server round-trip and a closure capture for app-start state. Pre-PR `blockr.dock` mounted such panels (the board-options offcanvas) with their bodies baked at UI-build time and toggled them with Bootstrap's `data-bs-toggle` — no R involvement at all. Restore that capability.

- [x] 2.8 Extend `sidebar_ui()` signature with `ui = NULL` and `title = NULL`. When `ui` is non-NULL, pre-render it via `htmltools::renderTags()` and inject the resulting `$html` into the `.blockr-sidebar-body` slot at UI-build time; pass the resolved deps to `htmltools::attachDependencies()` alongside `sidebar_dep()`. When `title` is non-NULL, set the `.blockr-sidebar-title` slot's text. Defaults preserve today's empty-shell behaviour.
- [x] 2.9 In `inst/assets/js/blockr-sidebar.js`, split `showPanel(panel, data)` so the body-swap sequence (`unbindAll` → `renderDependencies` → `replaceChildren` → `initializeInputs` → `bindAll`) only runs when `data.html` is defined. Title is updated when `data.title !== undefined`. The open suffix (open class, `aria-hidden`, reflow, outside-click, focus, state dispatch) runs in both branches. Selectize-aware focus still applies on the open suffix.
- [x] 2.10 Update `show_sidebar(id, ui = NULL, title = NULL, session = ...)` to accept `NULL` for `ui`: when `NULL`, omit `html` and `dependencies` from the payload so the JS takes the open-only branch. Mirror the optional `title` (only include in the payload when non-NULL).
- [x] 2.11 Add a document-level `click` listener in the bundled JS that resolves `event.target.closest("[data-blockr-sidebar-target]")`, looks up the matching panel by id, and toggles its open state. Open path reuses the open suffix from 2.9; close path reuses `hidePanel`. Any element type is supported; nested children inside a trigger element work via `.closest()`. Skip silently if the matching panel id is missing.
- [x] 2.12 Tests: `sidebar_ui(id, ui = body, title = "x")` produces a tag tree whose body slot contains `body` and whose title slot contains `"x"`; `show_sidebar(id)` (no `ui`) sends the open-only payload (no `html`, no `dependencies`); body-level htmltools deps travel with `sidebar_ui()` output. (Chromote-driven trigger-click scenarios deferred to a follow-up; manual smoke covers the path.)
- [x] 2.13 `openspec validate add-sidebar-primitive` clean. (`devtools::check()` left for the PR-review pass.)

## 3. Phase 2 — `blockr.dock` adopts the sidebar (PR 2)

- [x] 3.1 Add `blockr.ui` to `blockr.dock`'s `Imports` in `DESCRIPTION`.
- [x] 3.2 In each existing modal builder (`block_modal`, `link_modal`, `stack_modal`), keep the body `tagList()` but drop the `modalDialog(...)` wrapper. Rename to `*_body()` (or similar) so the call site reads `block_modal_body(...)`. Input ids and form structure unchanged.
- [x] 3.3 Switch each action handler from `showModal(*_modal(...))` / `removeModal()` to `blockr.ui::show_sidebar(sidebar_id, ui = *_body(...), title = ...)` / `blockr.ui::hide_sidebar(sidebar_id)`. Add a `sidebar_id = "main_sidebar"` parameter (default value matches what `board_ui.dock_board()` mounts). On confirm, branch on `blockr.ui::sidebar_state(sidebar_id)$pinned` — when pinned, re-show with a fresh body so the user can chain another add; when not pinned, `hide_sidebar()`. Applies to:
  - `add_block_action()`
  - `append_block_action()`
  - `prepend_block_action()`
  - `add_link_action()`
  - `add_stack_action()`
  - `edit_stack_action()`
- [x] 3.4 Update `board_ui.dock_board()` to mount `blockr.ui::sidebar_ui(NS(id, "main_sidebar"))` somewhere in the page.
- [x] 3.5 Replace the navbar settings gear's `data-bs-toggle="offcanvas"` markup with a regular `actionButton()` whose `observeEvent` (in `board_server_callback()` or a small new internal helper) calls `blockr.ui::show_sidebar(sidebar_id, ui = settings_body(id, board, options))` — where `settings_body()` is the existing options-accordion content extracted from `board_ui.dock_board()`. The Bootstrap settings offcanvas markup is removed.
- [x] 3.6 Tests: existing action-handler tests adapted — replace `expect`s on modal-specific input ids with `with_mocked_bindings()` around `blockr.ui::show_sidebar` / `hide_sidebar` to verify the action handler triggers the right call. Existing form-validation logic is unchanged so the bulk of the tests stays.
- [x] 3.7 `devtools::check()` on `blockr.dock` clean (0/0/0 modulo timestamp NOTE); all tests pass.
- [x] 3.8 Manual smoke test: run the dock + dag example (right-click append node, "+" toolbar button, settings gear) and confirm the sidebar opens / closes / accepts input correctly.
- [x] 3.9 Selectize-aware focus: extend `showPanel` in `inst/assets/js/blockr-sidebar.js` so that after `focusables[0].focus()` it walks up to `.selectize-control`, finds the inner `select.selectized`, and calls `select.selectize.focus()`. Restores the keyboard UX previously wired per-form via `shown.bs.modal` in `blockr.dock`'s `auto_focus_script()`, without requiring per-consumer opt-in. `blockr.dock`'s `*_sidebar_body` builders therefore stay free of focus-wiring code.

### Settings sidebar adopts the static-content path (amendment to PR 2)

Folded into the same PR. Resolves the chain we discovered while threading `serve(..., options = ...)` to the settings sidebar: with static content supported by the primitive, the settings panel can live entirely UI-side and the closure / `app_options` plumbing in `board_server_callback()` disappears.

- [x] 3.10 In `R/board-ui.R`, change the settings sidebar mount from `sidebar_ui("settings_sidebar", mode = "overlay", side = "right")` to `sidebar_ui("settings_sidebar", ui = settings_body(id, x, options = options), title = "Board options", mode = "overlay", side = "right")`. The `options` argument that was previously dead on `board_ui.dock_board()` (and the source of the original threading bug) becomes load-bearing again.
- [x] 3.11 Change the gear `actionButton()` in `board_ui.dock_board()` to carry `data-blockr-sidebar-target="settings_sidebar"`; drop the `inputId = "settings_btn"` since no server observer is needed.
- [x] 3.12 Delete `settings_observer()` from `R/board-server.R`, the call to it from `board_server_callback()`, and the `app_options` parameter on `board_server_callback()`. Drop the closure in `blockr_app_server.dock_board()` that captured `options` to inject `app_options`; `board_server_callback` becomes the direct callback again. The `options` parameter on `blockr_app_server.dock_board()` is still consumed by `blockr.core::board_server()` for its userData wiring.
- [ ] 3.13 Tests: rewrite `settings_observer` tests as `board_ui.dock_board()` markup assertions — the mounted `<div id="settings_sidebar">` contains the rendered options accordion; the gear button carries `data-blockr-sidebar-target="settings_sidebar"`. Add a `serve(board, options = custom_options(...))` test that asserts the rendered settings sidebar body reflects the override.
- [ ] 3.14 Manual smoke: load the dock + dag example, click gear, panel opens without a visible Shiny-busy round-trip; change an option, save, restore, verify the option round-trips correctly (also confirms the save-restore regression flagged during PR 2 review).

## 4. Validation

- [ ] 4.1 `openspec validate add-sidebar-primitive` — no spec / template / scenario errors.
- [ ] 4.2 `devtools::check(remote = TRUE, manual = TRUE)` on `blockr.ui` — 0 errors / 0 warnings / 0 notes.
- [ ] 4.3 `devtools::check()` on `blockr.dock` after Phase 2 — 0 errors / 0 warnings / 0 notes.
- [ ] 4.4 Confirm dependency graph: `blockr.core` ← `blockr.ui` ← `blockr.dock` ← extensions, no cycles.
