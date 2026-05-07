## 1. Phase 0 — package skeleton

- [x] 1.1 Run `usethis::create_package("blockr.ui")` with `Authors@R`, `License: MIT + file LICENSE`, `Encoding: UTF-8`, `Roxygen: list(markdown = TRUE)`
- [x] 1.2 Set `Imports: blockr.core, shiny, htmltools, rlang` in `DESCRIPTION` and verify no `dockViewR` / `bslib` entries
- [x] 1.3 Set `Suggests: testthat (>= 3.0.0), knitr, rmarkdown, shinytest2, withr` in `DESCRIPTION`
- [x] 1.4 Run `usethis::use_testthat(3)`, `use_pkgdown()`, `use_news_md()`, `use_mit_license()`, `use_package_doc()`
- [x] 1.5 Run `usethis::use_github_action("check-standard")` and `use_github_action("test-coverage")`
- [x] 1.6 Create empty `inst/assets/css/` and `inst/assets/js/` directories
- [x] 1.7 Create `R/blockr.ui-package.R` with `"_PACKAGE"` and `@importFrom rlang %||%`
- [x] 1.8 Create `R/utils-pkg.R` with `pkg_name()` (lifted from `blockr.dock`) and small internal helpers
- [x] 1.9 Run `devtools::check()` — expect 0 ERRORs / 0 WARNINGs / 0 NOTEs
- [x] 1.10 Confirm `blockr.dock` builds unchanged (no edits to `blockr.dock` in Phase 0)

## 2. Phase 1 — CSS split

- [ ] 2.1 Cut `blockr.dock/inst/assets/css/blockr-dock.css` into five files: `blockr-tokens.css`, `blockr-base.css`, `blockr-shell.css`, `blockr-block.css` in `blockr.ui/inst/assets/css/`; keep only `dv-*` overrides + `.blockr-view-*` + `.blockr-empty-dock-prompt` + `.blockr-dock-pool` + `g6-toolbar` in the rewritten `blockr.dock/inst/assets/css/blockr-dock.css`
- [ ] 2.2 Implement `blockr.ui/R/utils-dep.R` with `blockr_ui_dep()`; verify it returns one `htmlDependency` referencing all four CSS files
- [ ] 2.3 Update `blockr.dock`'s dependency builder to list `blockr.ui`'s dep as a transitive dependency so `htmltools::resolveDependencies()` orders it first

## 3. Phase 1 — relocate block card and edit-block

- [ ] 3.1 Move `blockr.dock/R/block-ui.R` content as follows: extract `block_card` and `has_external_ctrl` to `blockr.ui/R/block-card.R`; keep `block_ui.dock_board`, `insert_block_ui.dock_board`, `remove_block_ui.dock_board`, `show_block_panel`, `hide_block_panel`, `show_block_ui`, `hide_block_ui`, and `show_hide_block_dep` in `blockr.dock/R/block-ui.R`
- [ ] 3.2 Promote `block_card()` to an S3 generic in `blockr.ui/R/block-card.R`: define `block_card <- function(blk, blk_id, ...) UseMethod("block_card", blk)` and rename the existing body to `block_card.default()`
- [ ] 3.3 Add `block_ui.board()` default method in `blockr.ui/R/block-card.R` that iterates blocks and calls `block_card()` per block
- [ ] 3.4 Update `blockr.dock::block_ui.dock_board()` to call `NextMethod()` and wrap each card with `wrap_dock_panel(card, blk_id, ...)` (defined in `blockr.dock`); reuse the same helper from `insert_block_ui.dock_board()`
- [ ] 3.5 Move `blockr.dock/R/plugin-block.R` to `blockr.ui/R/edit-block.R` (rename for clarity); migrate `block_card_title`, `block_card_toggles`, `ctrl_button_label`, `block_card_dropdown`, `block_card_content`, `build_ctrl_panel`, `ctrl_btn_*`, `edit_block_server`, `update_blk_cond_observer`, `create_issues_ui`, `edit_block_validator`
- [ ] 3.6 Move `blockr.dock/R/block-meta.R` to `blockr.ui/R/block-meta.R`

## 4. Phase 1 — relocate UI helpers

- [ ] 4.1 Move `blockr.dock/R/action-utils.R` to `blockr.ui/R/select-widgets.R` (`block_input_select`, `block_registry_selectize`, `board_select`)
- [ ] 4.2 Split `blockr.dock/R/action-ui.R`: move `css_block_selectize()` (selectize styling only — drop modal-only bits), `js_blk_selectize_render()`, `auto_focus_script()`, `toggle_button()`, `confirm_button()` to `blockr.ui/R/ui-helpers.R`; keep `css_modal()`, `css_modal_advanced()` in `blockr.dock` for now (deleted at Phase 4)
- [ ] 4.3 `blockr.dock/R/utils-ui.R` stays put: `off_canvas()`, `collapse_container()`, `move_dom_element()`, `determine_active_views`, `visible_exts`, `determine_panel_pos`, `show_panel`, `empty_dock_prompt`, `drop_nulls` all stay in `blockr.dock`. No file move at v0 — these have no `blockr.ui` consumer.
- [ ] 4.4 `blockr.dock/inst/assets/js/show-hide-block.js` stays in `blockr.dock` unchanged; no separate `dock-move-guard.js` needed since the dock-only guard is already part of the single handler.

## 5. Phase 1 — page chrome generics

- [ ] 5.1 Create `blockr.ui/R/app-shell.R`: define `app_shell <- function(id, x, ...) UseMethod("app_shell", x)` and `app_shell.default(id, x, ..., navbar = NULL, sidebar = NULL, content = NULL, deps = NULL)` rendering the four-slot layout with `--blockr-sidebar-width` setup
- [ ] 5.2 Create `blockr.ui/R/navbar.R`: define `blockr_navbar <- function(id, x, ...) UseMethod("blockr_navbar", x)` and `blockr_navbar.default(id, x, ..., left = NULL, right = NULL, settings_id = NULL)` rendering `.blockr-navbar` with left/right slots and optional settings gear (with `data-blockr-action="settings"`, no `data-bs-*`)
- [ ] 5.3 Create `blockr.ui/R/options-ui.R`: define `options_ui <- function(id, x, ...) UseMethod("options_ui", x)` and `options_ui.default(id, x, options, ...)` returning a plain accordion (no offcanvas wrapper) using `blockr.core::board_option_ui()`
- [ ] 5.4 Rewrite `blockr.dock::board_ui.dock_board()` as a thin assembly calling `blockr.ui::app_shell()` / `blockr_navbar()` / `sidebar_ui()` (sidebar UI builder lands in Phase 2 — for Phase 1 the sidebar slot stays empty / placeholder), injecting only `view_nav_ui()`, `dock_outputs_ui()`, and `dock_pool_ui()`
- [ ] 5.5 Confirm no `app_shell.dock_board()` / `blockr_navbar.dock_board()` / `options_ui.dock_board()` methods are registered in `blockr.dock` at this phase

## 6. Phase 1 — replace staging offcanvases with the dock pool

- [ ] 6.1 Create `blockr.dock/R/dock-pool.R` with `dock_pool_ui(id)` returning a single hidden `<div id="<id>-block_pool" class="blockr-dock-pool" hidden aria-hidden="true">`
- [ ] 6.2 Update `blockr.dock::hide_block_ui()` and `hide_ext_ui()` to target the pool element (`paste0("#", board_ns("block_pool"))`) instead of `.offcanvas-body`
- [ ] 6.3 Delete the `blocks_offcanvas` and `exts_offcanvas` markup from `board_ui.dock_board()`
- [ ] 6.4 Add the single `.blockr-dock-pool` rule to `blockr.dock/inst/assets/css/blockr-dock.css`
- [ ] 6.5 Verify with shinytest2 that hidden block staging still works (block reactivity preserved across view switches)

## 7. Phase 1 — backwards-compat shims and tests

- [ ] 7.1 Add `lifecycle` to `blockr.dock`'s `Imports` (it is needed for the deprecate shims but was not in `blockr.dock`'s deps before)
- [ ] 7.2 Add `blockr.ui` to `blockr.dock`'s `Imports`
- [ ] 7.3 Add `lifecycle::deprecate_warn()` re-export shims in `blockr.dock` for every previously-`@export`ed symbol that moved: `block_card`, `block_input_select`, `block_registry_selectize`, `board_select`, plus any other `@export` discovered during Phases 1.3–1.5. (`off_canvas()` and `move_dom_element()` do not move and need no shim.)
- [ ] 7.4 Verify internal-only helpers (no `@export`) move silently with no shim
- [ ] 7.5 Relocate tests: `tests/testthat/test-block-card.R`, `test-edit-block.R`, `test-select-widgets.R`, `test-app-shell.R`, `test-navbar.R`, `test-options-ui.R` end up in `blockr.ui`; tests for `off_canvas()`, `move_dom_element()`, and `show-hide-block.js` stay in `blockr.dock` (the code stays). `blockr.dock` counterparts that exercised the shims for moved symbols become tiny tests that verify the shim still calls the new home and emits the deprecation warning
- [ ] 7.6 Run `devtools::check()` on both `blockr.ui` and `blockr.dock`; expect 0 ERRORs / 0 WARNINGs and only the documented deprecation NOTE (if any)
- [ ] 7.7 Manual / shinytest2 visual-regression check on the dock app: rendered output is unchanged from pre-Phase 1

## 8. Phase 2 — sidebar primitive

- [ ] 8.1 Create `blockr.ui/inst/assets/css/blockr-sidebar.css` with the sidebar shell, slide transition, header, pinned-mode reflow, and focus-trap visuals
- [ ] 8.2 Create `blockr.ui/inst/assets/js/sidebar-binding.js` with `SidebarBinding extends Shiny.InputBinding` (`find` / `getValue` / `subscribe` / `receiveMessage` / `_setOpen` / `_emit` / `_trapFocus` / `_releaseFocus`) plus the four custom message handlers (`sidebar-open` / `-close` / `-toggle` / `-set-content`)
- [ ] 8.3 Implement the `set-content` handler with the unbind → renderDependencies → replaceChildren → initializeInputs → bindAll sequence
- [ ] 8.4 Create `blockr.ui/R/sidebar.R` with `sidebar_ui()`, `sidebar_dep()`, and the R helpers `sidebar_open()` / `sidebar_close()` / `sidebar_toggle()` / `sidebar_set_content()` (each defaulting `session = shiny::getDefaultReactiveDomain()`)
- [ ] 8.5 Implement `sidebar_set_content()` to call `htmltools::renderTags()` and ship both `html` and `dependencies` in the message payload
- [ ] 8.6 Create `blockr.ui/R/sidebar-content.R` with the `sidebar_content()` S3 generic, `new_sidebar_trigger()`, all built-in trigger constructors, and stub method bodies (`tags$div("add_block content goes here")`) for the seven built-in content types
- [ ] 8.7 Create `blockr.ui/tests/testthat/test-sidebar.R` covering trigger constructors, message-envelope shape (via `testServer()` with stub session), `getValue` reading DOM state, and the `sidebar_set_content()` validation path
- [ ] 8.8 Write `blockr.ui/vignettes/sidebar.Rmd` showing how to mount a sidebar in a non-dock Shiny app, register a custom content type, and toggle it from R
- [ ] 8.9 Run `devtools::check()` on `blockr.ui`; CI green

## 9. Phase 3 — block-add flow on sidebar

- [ ] 9.1 Implement `blockr.ui/R/block-browser.R` with `block_browser_ui(id, board)`: cards by category, search box, quick-add button per card, detailed-add accordion (custom name, ID, input selection via `block_input_select()`)
- [ ] 9.2 Implement `block_browser_server(id, board, on_select)` with reactive search filter, quick-add `on_select` callback, detailed-add submit, and keyboard navigation (arrow keys, Enter, Escape) — Esc routes to `sidebar_close()` if not pinned
- [ ] 9.3 Replace the stub `sidebar_content.add_block` / `.append_block` / `.prepend_block` methods with full implementations that render `block_browser_ui()` and forward `on_select` via the per-content server module
- [ ] 9.4 Introduce a companion internal `sidebar_content_server(trigger, ...)` S3 generic; implement methods for `add_block` / `append_block` / `prepend_block` that mount `block_browser_server()`. The sidebar's parent module dispatches on `input[[id]]$content` and starts/stops the per-content server on swap
- [ ] 9.5 Switch `blockr.dock::add_block_action()` / `append_block_action()` / `prepend_block_action()` from `showModal(block_modal(...))` to `blockr.ui::sidebar_set_content(sidebar_id, ...trigger, board = board$board)`; remove `removeModal()` calls
- [ ] 9.6 Introduce a `sidebar_id` parameter on those action handlers (default `"main_sidebar"`)
- [ ] 9.7 Write `blockr.dock/tests/testthat/test-sidebar-add-block.R` (shinytest2): open via "+" button, add three blocks in pinned mode, close on Esc when not pinned, verify search filter, verify detailed-add custom name/ID round-trip
- [ ] 9.8 Verify `block_modal()` is unused but still present (deleted at Phase 4); CI green

## 10. Phase 4 — remaining flows + settings offcanvas removal

- [ ] 10.1 Implement `sidebar_content.add_link` (and its `sidebar_content_server.add_link`): pick source / target / input via `block_input_select()`
- [ ] 10.2 Implement `sidebar_content.create_stack` and `sidebar_content.edit_stack` (plus their server companions): block selection plus colour / name fields
- [ ] 10.3 Implement `sidebar_content.settings`: render `blockr.ui::options_ui(id, board, options)` (pure accordion, no offcanvas wrapper)
- [ ] 10.4 Wire the navbar settings gear (rendered by `blockr_navbar(settings_id = ...)`) to `sidebar_set_content(settings_id, settings_trigger())` — this is part of `blockr.ui`'s sidebar JS, triggered by `data-blockr-action="settings"`
- [ ] 10.5 Switch `blockr.dock::add_link_action()` / `add_stack_action()` / `edit_stack_action()` from `showModal(...)` to `sidebar_set_content(...)`
- [ ] 10.6 Delete `blockr.dock/R/action-modal.R` (the file containing `block_modal`, `link_modal`, `stack_modal`)
- [ ] 10.7 Delete `css_modal()` and `css_modal_advanced()` from `blockr.dock/R/action-ui.R`; if the file ends up empty, delete the file
- [ ] 10.8 Delete the Bootstrap settings-offcanvas markup from `blockr.dock/R/board-ui.R`
- [ ] 10.9 Update `blockr.dock/NEWS.md` under "Breaking changes": list deletions of `block_modal`, `link_modal`, `stack_modal`, `css_modal`, `css_modal_advanced`, plus the offcanvas removal, with the sidebar-trigger replacement noted per item
- [ ] 10.10 End-to-end shinytest2 covering all migrated flows: add block, append block, prepend block, add link, create stack, edit stack, settings open and edit
- [ ] 10.11 Verify the rendered DOM contains no `.offcanvas`-classed element other than (transiently) any caller-driven `blockr.dock::off_canvas()` invocation outside the migration scope

## 11. Phase 5 — cleanup

- [ ] 11.1 If a sufficient release window has elapsed (one minor cycle), drop the Phase 1 `lifecycle::deprecate_warn()` shims from `blockr.dock`; otherwise file a tracking issue with the target version
- [ ] 11.2 Audit `blockr.dock`'s `DESCRIPTION` for `bslib`: if no remaining usage justifies it, remove from `Imports`
- [ ] 11.3 Update `blockr.dock`'s README and the dependency-graph diagram to show `blockr.ui` underneath it
- [ ] 11.4 Update `blockr.ui`'s pkgdown navbar to surface the three vignettes (`block-card.Rmd`, `sidebar.Rmd`, `app-shell.Rmd`)
- [ ] 11.5 Run `revdepcheck::revdep_check()` on `blockr.dock` to confirm no extension breaks; address any fallout in the affected extension before tagging the release

## 12. Documentation

- [ ] 12.1 Write `blockr.ui/vignettes/block-card.Rmd` showing how to render a block card outside a dock
- [ ] 12.2 Write `blockr.ui/vignettes/app-shell.Rmd` showing how to build a non-dock blockr-flavoured page (alternative renderer story)
- [ ] 12.3 Update `blockr.ui` README.Rmd with the pitch, install instructions, minimal example, and links to vignettes
- [ ] 12.4 Run `usethis::use_pkgdown_github_pages()` and verify the site builds

## 13. Demo app and chromote integration tests

- [ ] 13.1 Create `blockr.ui/inst/examples/blockr-ui-demo/app.R`: a small Shiny app built on `blockr.core::new_board()` (NOT `dock_board`) with 2–3 blocks (e.g., a data block on `mtcars`, a transform block, a plot block). Render via `blockr.ui::app_shell()` + `blockr_navbar()` + `sidebar_ui()` + the default `block_ui.board`. Wire the navbar "+" button to `sidebar_set_content("main_sidebar", add_block_trigger())` and the settings gear to `settings_trigger()`. No `library(blockr.dock)`.
- [ ] 13.2 Implement `blockr.ui/R/run-demo.R` exporting `run_demo(name = "blockr-ui-demo", ...)` that resolves `system.file("examples", name, "app.R", package = "blockr.ui")` and launches it via `shiny::runApp()`. Error with a clear message if the example directory is missing.
- [ ] 13.3 Write `blockr.ui/tests/testthat/test-demo-smoke.R` using the `shiny-chromote-inspect` workflow: launch the demo in a background R process, drive Chrome via `chromote`, assert (a) the page loads without console errors, (b) at least one `block_card` is rendered, (c) no `dv-*` (DockView) class appears.
- [ ] 13.4 Write `blockr.ui/tests/testthat/test-demo-sidebar.R` (chromote): click the "+" button → assert `.blockr-sidebar-open` is added and `input[["main_sidebar"]]$open` is `TRUE`; click a quick-add card → assert a new `block_card` is mounted and the board's block count increased by one; click pin → assert `--blockr-sidebar-width` on `<body>` becomes the panel width; press Escape with not-pinned → assert sidebar closes.
- [ ] 13.5 Write `blockr.ui/tests/testthat/test-demo-keyboard.R` (chromote): focus any card in the block browser → press ArrowRight → assert focus moves to the next card; press Enter → assert quick-add fires.
- [ ] 13.6 Add the demo to the pkgdown site as an article (linked from the navbar) so the published site has a runnable reference; mention `run_demo()` in `README.Rmd`.
- [ ] 13.7 Skip chromote tests on CRAN (`skip_on_cran()`) and gate them on `chromote::find_chrome()` being non-NULL (`skip_if_not_installed("chromote")`, `skip_if(is.null(chromote::find_chrome()))`).

## 14. Validation

- [ ] 14.1 Run `openspec validate create-blockr-ui-package` and confirm no spec / template / scenario errors
- [ ] 14.2 Run `devtools::check(remote = TRUE, manual = TRUE)` on `blockr.ui`: 0 ERRORs / 0 WARNINGs / 0 NOTEs
- [ ] 14.3 Run `devtools::check_win_devel()` on `blockr.ui`
- [ ] 14.4 Run `revdepcheck::revdep_check(num_workers = 4)` against any reverse dependencies that exist for `blockr.dock`
- [ ] 14.5 Confirm the dependency graph: `blockr.core` ← `blockr.ui` ← `blockr.dock` ← extensions, with no cycles or back-imports
