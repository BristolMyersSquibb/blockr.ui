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
- [x] 2.5 Tests: `tests/testthat/test-sidebar.R` covering (a) `sidebar_ui()` markup is Bootstrap-free (no `.offcanvas` / `.modal` / `data-bs-*` anywhere), (b) `sidebar_dep()` references the right files, (c) `show_sidebar()` sends the expected payload via a `shiny::MockShinySession` whose `sendInputMessage` is overridden to capture, (d) `hide_sidebar()` likewise, (e) calls dispatched from a nested `makeScope("mod_a")` proxy still target the absolute id (`rootScope` walking).
- [x] 2.6 Vignette `vignettes/sidebar.Rmd`: minimal example showing how to mount a sidebar, open it from R with arbitrary content, observe the inputs inside the body, and dismiss it on confirm — modelled on Shiny's modal vignette. Include a short "auto-open on empty board" recipe that uses `input[[id]]$open` to avoid re-opening after a user dismissal.
- [x] 2.7 `devtools::check()` on `blockr.ui` clean (0/0/0 modulo timestamp NOTE); all tests pass.

## 3. Phase 2 — `blockr.dock` adopts the sidebar (PR 2)

- [ ] 3.1 Add `blockr.ui` to `blockr.dock`'s `Imports` in `DESCRIPTION`.
- [ ] 3.2 In each existing modal builder (`block_modal`, `link_modal`, `stack_modal`), keep the body `tagList()` but drop the `modalDialog(...)` wrapper. Rename to `*_body()` (or similar) so the call site reads `block_modal_body(...)`. Input ids and form structure unchanged.
- [ ] 3.3 Switch each action handler from `showModal(*_modal(...))` / `removeModal()` to `blockr.ui::show_sidebar(sidebar_id, ui = *_body(...), title = ...)` / `blockr.ui::hide_sidebar(sidebar_id)`. Add a `sidebar_id = "main_sidebar"` parameter (default value matches what `board_ui.dock_board()` mounts). On confirm, branch on `blockr.ui::sidebar_state(sidebar_id)$pinned` — when pinned, re-show with a fresh body so the user can chain another add; when not pinned, `hide_sidebar()`. Applies to:
  - `add_block_action()`
  - `append_block_action()`
  - `prepend_block_action()`
  - `add_link_action()`
  - `add_stack_action()`
  - `edit_stack_action()`
- [ ] 3.4 Update `board_ui.dock_board()` to mount `blockr.ui::sidebar_ui(NS(id, "main_sidebar"))` somewhere in the page.
- [ ] 3.5 Replace the navbar settings gear's `data-bs-toggle="offcanvas"` markup with a regular `actionButton()` whose `observeEvent` (in `board_server_callback()` or a small new internal helper) calls `blockr.ui::show_sidebar(sidebar_id, ui = settings_body(id, board, options))` — where `settings_body()` is the existing options-accordion content extracted from `board_ui.dock_board()`. The Bootstrap settings offcanvas markup is removed.
- [ ] 3.6 Tests: existing action-handler tests adapted — replace `expect`s on modal-specific input ids with `with_mocked_bindings()` around `blockr.ui::show_sidebar` / `hide_sidebar` to verify the action handler triggers the right call. Existing form-validation logic is unchanged so the bulk of the tests stays.
- [ ] 3.7 `devtools::check()` on `blockr.dock` clean (0/0/0 modulo timestamp NOTE); all tests pass.
- [ ] 3.8 Manual smoke test: run the dock + dag example (right-click append node, "+" toolbar button, settings gear) and confirm the sidebar opens / closes / accepts input correctly.

## 4. Validation

- [ ] 4.1 `openspec validate add-sidebar-primitive` — no spec / template / scenario errors.
- [ ] 4.2 `devtools::check(remote = TRUE, manual = TRUE)` on `blockr.ui` — 0 errors / 0 warnings / 0 notes.
- [ ] 4.3 `devtools::check()` on `blockr.dock` after Phase 2 — 0 errors / 0 warnings / 0 notes.
- [ ] 4.4 Confirm dependency graph: `blockr.core` ← `blockr.ui` ← `blockr.dock` ← extensions, no cycles.
