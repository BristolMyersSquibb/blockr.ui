## 1. Phase 1 - block browser in `blockr.ui` (PR 1)

Self-contained: no `blockr.dock` change. After this phase a non-dock Shiny app can `library(blockr.ui)`, mount `block_browser_ui(id, ...)` / `block_browser_server(id)`, and `observeEvent()` the returned committed-block reactive.

> Design note: an earlier iteration of this phase implemented a multi-select model (parallel/sequential connection toggle, order badges, arity cap, `target_input` slot arbitration, reorderable chip tray). It was superseded by the simpler single-shot model below - see "Design evolution" and "Deferred work" in `design.md`. The tasks here describe the shipped (simplified) design.

- [x] 1.1 `inst/assets/css/blockr-block-browser.css`:
  - Card-list shell (rounded card, hover affordance signalling the card is clickable, `.card-expanded` reveals the advanced form)
  - Category headers (uppercase eyebrow) + per-category colour (icon tile + heading), Okabe-Ito palette, CSS-only via `[data-category]`
  - Search bar styling
  - Per-card advanced form fields with per-mode visibility (`link-id`, `block-input`, `target-input` opt in by `data-mode`; `target-input` hidden for `data-target-arity` `1` / `inf` / absent)
  - Per-card add button (`.blockr-block-browser-card-add`)
  - Description clamped (2 lines) when collapsed, full when `.card-expanded`
  - Empty-state; kebab-case classes under `.blockr-block-browser-*`; no Bootstrap classes
- [x] 1.2 `inst/assets/js/blockr-block-browser.js`:
  - `input` listener on `.blockr-block-browser-search` filters cards by case-insensitive substring on `data-name + data-description + data-package + data-category`; empty categories collapse via `:has()` CSS; empty-state toggled
  - Delegated `click` on the cards area: chevron → toggle `.card-expanded`; in-card add button → commit with edited values; other clicks inside the advanced form → ignore; plain card-body click → commit with default values
  - `commitCard` gathers the card's applicable fields (hidden fields → `null` via computed-style check), records the spec (with a `nonce`) on the root, and dispatches a `blockr-block-browser:commit` event
  - A `Shiny.InputBinding` (`blockr.ui.blockBrowser`, bound to `.blockr-block-browser`): `getValue` returns the recorded spec, `subscribe` listens for the commit event, `unsubscribe` removes it; `receiveMessage` reserved for the AI-search filter. Registered via `Shiny.inputBindings.register`. No ad-hoc `setInputValue`; mounted through Shiny's `bindAll` (run by `show_sidebar`), so no MutationObserver
- [x] 1.3 `R/block-browser.R` (Shiny module):
  - `block_browser_ui(id, board, mode, trigger_id)` does `ns <- NS(id)`, returns a `tag` with `block_browser_dep()` attached; iterates `available_blocks()`, groups by category (`"Uncategorized"` fallback), renders cards with `data-*` metadata
  - Sets the root `id` to `NS(id)("commit")` (the input-binding target, read by the server as `input$commit`) and stamps `data-mode`; for prepend stamps `data-target-arity` (integer / `"inf"`)
  - Per card: advanced form (`id`, `title`, `link_id`, `block_input`, `target_input`) namespaced by module id + block type, plus the add button
  - Seeds default `id` / `link_id` via `blockr.core::rand_names(old_names = board ids, n)` so suggestions are unique among cards and against the board
  - `block_browser_server(id)` - `moduleServer` returning `eventReactive(input$commit, ...)` with the `nonce` stripped
  - `block_browser_dep()` - `htmlDependency()` for the CSS + JS
- [x] 1.4 Unit tests `tests/testthat/test-block-browser.R`: card-per-block + `data-*`; category grouping + Uncategorized fallback; per-card add button with mode label; no footer/connection/chip/order UI; commit-input id + `data-mode`; arity stamping (`2`/`1`/`inf`, absent for add/append); context subtitle; namespaced field ids; default ids unique + avoid board ids; no Shiny-bound per-field inputs; dependency; malformed-input errors
- [x] 1.5 shinytest2 `tests/testthat/test-block-browser-shinytest2.R`: search filters live; card-body click adds with defaults (commit type + non-empty id, no expand); chevron opens form then in-card button adds with edited id; clicking a form input does not add
- [x] 1.6 Vignette `vignettes/block-browser.Rmd`: mount sidebar, open the browser, click-to-add vs open-edit-add, commit-spec shape, unique-id behaviour, per-mode fields
- [x] 1.7 Runnable example `inst/examples/block-browser/app.R` (used by 1.5)
- [x] 1.8 `devtools::check()` clean (0/0/1 timestamp NOTE; run with `--no-vignettes` locally as pandoc is absent - vignette build verified on CI); `lintr::lint_package()` clean (0 lints); `openspec validate add-block-browser --strict` deferred to CI (CLI not installed locally)

## 2. Phase 2 - `blockr.dock` adopts the browser (PR 2)

- [ ] 2.1 In `add_block_action()`, `append_block_action()`, `prepend_block_action()`: mount `added <- blockr.ui::block_browser_server(<sub_id>)` once, and replace the `block_sidebar_body(...)` call (inside both `observeEvent(trigger(), ...)` and the `keep_or_hide_sidebar()` chain) with `blockr.ui::block_browser_ui(session$ns(<sub_id>), board$board, mode = <mode>, trigger_id = trigger())` (omit `trigger_id` for `mode = "add"`).
- [ ] 2.2 Replace the per-field observers (`input$<mode>_block_selection`, `input$<mode>_block_id`, `input$<mode>_block_name`, `input$<mode>_link_id`, `input$<mode>_block_input`, `input$<mode>_block_confirm`) with a single `observeEvent(added(), ...)` on the browser module's returned reactive that consumes the single-block spec:
  - Build the block via `do.call(spec$type, list())`; set `block_name(blk) <- spec$title` only when `spec$title` is non-NULL; name the block by `spec$id`.
  - Build the link(s) per mode: `add` → none; `append` → one link from `trigger_id` to the new block using `spec$link_id` and `spec$block_input`; `prepend` → one link from the new block to `trigger_id` using `spec$link_id` and `spec$target_input` (omit `target_input` for arity-1 / variadic targets, where `blockr.core` assigns the slot).
  - Single `update(list(blocks = ..., links = ...))` call, then `keep_or_hide_sidebar()` (re-render with the updated board on pin so the next suggested id is fresh).
- [ ] 2.3 Drop `block_sidebar_body()`'s `mode = "add" | "append" | "prepend"` branches from `R/block-sidebar.R` (nothing in-tree calls them). Keep `mode = "edit"`. Update roxygen + add a NEWS entry for out-of-tree consumers.
- [ ] 2.4 Update action-handler tests in `blockr.dock` to drive the browser module's `input$commit` (the module's `commit` input under the sub-namespace) with single-block payloads. Cover add (no link), append (link from source with port), prepend into arity-1 target (implicit slot), prepend into arity-2 target (explicit `target_input`).
- [ ] 2.5 Manual smoke test of the dock + dag example: right-click a node → append → browser opens with context, search filters, click a card to add immediately, open a card to edit id/title/link then add; repeated clicks on the same card produce distinct blocks (pinned); gear/settings unaffected.
- [ ] 2.6 `devtools::check()` clean on `blockr.dock`; `lintr::lint_package()` clean.
- [ ] 2.7 NEWS in both packages: `blockr.ui` ("New `block_browser_ui()` / `block_browser_server()` module ..."); `blockr.dock` ("Action handlers now mount the `block_browser` module and observe its committed-block reactive; per-field inputs and `block_sidebar_body(mode = 'add'|'append'|'prepend')` removed - use `blockr.ui::block_browser_ui()` / `block_browser_server()`.").

## 3. Validation

- [ ] 3.1 `openspec validate add-block-browser --strict` - no spec / template / scenario errors
- [ ] 3.2 `devtools::check(remote = TRUE, manual = TRUE)` on `blockr.ui` - 0 errors / 0 warnings / 0 notes
- [ ] 3.3 `devtools::check()` on `blockr.dock` after Phase 2 - 0 errors / 0 warnings / 0 notes
- [ ] 3.4 `lintr::lint_package()` clean on both packages
