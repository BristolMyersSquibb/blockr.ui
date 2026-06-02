## 1. Phase 1 - link menu in `blockr.ui` (PR 1)

Self-contained: no `blockr.dock` change. After this phase a non-dock Shiny app can `library(blockr.ui)`, mount `link_menu_ui(id, board, anchor)` / `link_menu_server(id)`, and `observeEvent()` the returned committed-link reactive.

- [x] 1.1 `inst/assets/css/blockr-link-menu.css`:
  - Cards drop description + package badge; show `id: <block_id>` subtitle (matches the stack-menu trimmed card layout). Reuse `.blockr-stack-menu-card-id` styling if generalised, or define a parallel `.blockr-link-menu-card-id`.
  - Section headers above OUTGOING and INCOMING sections - matches the visual treatment of `.blockr-stack-menu-section-header`. Generalise the class name if practical (e.g. `.blockr-card-section-header`), otherwise define a parallel rule.
  - Per-card advanced area styling reuses the existing `.blockr-block-browser-card-advanced` / `-add` rules from `blockr-block-browser.css`. The "Add link" button uses the same `.blockr-block-browser-card-add` chrome.
  - Empty-state styling reuses `.blockr-block-browser-empty`.
- [x] 1.2 `inst/assets/js/blockr-link-menu.js`:
  - Register a `Shiny.InputBinding` (`blockr.ui.linkMenu`) bound to `.blockr-link-menu`. `find` / `initialize` / `getValue` / `subscribe` / `unsubscribe` + `receiveMessage`.
  - Reuse `window.BlockrUI.cardSearch` (set up by `blockr-block-browser.js`) for the search filter, applied across both sections at once.
  - `click` on a card body (not in the chevron, not in the advanced area) commits with the card's defaults: read `data-direction` + `data-block-type` from the card + `data-anchor` from the root, compose `source` / `target` accordingly, set `link_id = default link-id input value`, `block_input = first option of the block-input select if rendered else null`.
  - `click` on the chevron toggles `.card-expanded` on the card and does NOT commit.
  - `click` on the in-card "Add link" button commits using the current per-card form values.
  - On commit, the binding records `{ source, target, link_id, block_input, nonce }` on the root and dispatches `blockr-link-menu:commit`.
  - `receiveMessage` handles `{ type: "pool-update", eligible: { outgoing, incoming }, free_inputs, link_id_seed }`: toggle `.hidden` on every card against `eligible[data-direction]`, rebuild each visible card's block-input `<select>` options from `free_inputs[target_id]` (preserving the previously selected port when still valid; no-op for cards whose advanced area doesn't render a picker), recompute the panel's `is-empty` class, optionally re-seed per-card Link ID inputs from `link_id_seed` (skip fields the user has edited; append a per-card monotonic suffix so the seed produces distinct ids across cards). MUST NOT touch card-expansion state, scroll position, or the search input value. Defensive against `sendInputMessage` auto-unboxing - both `eligible$*` and `free_inputs[[...]]` values may arrive as JSON scalars instead of arrays; the JS normalises via a small `asArray()` helper.
- [x] 1.3 `R/link-menu.R` (Shiny module):
  - `link_menu_ui(id, board, anchor)`: validate `id` (non-empty character scalar) and `anchor` (non-empty character scalar, must be in `board_block_ids(board)`; otherwise raise `blockr_ui_link_menu_unknown_anchor`). Compute eligible pools via `link_eligible_pools(board, anchor)` (see below). Render search + up-to-two category-grouped sections with section headers + the empty-state slot. Attach `block_browser_dep()` + `link_menu_dep()`. Set `data-anchor` on the root and `data-direction` on each card.
  - `link_eligible_pools(board, anchor)` (exported): returns `list(outgoing = <chr>, incoming = <chr>, free_inputs = <named list>)`. `free_inputs` is keyed by target block id (OUTGOING targets = the cards themselves; INCOMING targets share the anchor) and carries the per-target list of free named input ports - the JS uses it to rebuild each card's block-input `<select>` options on every `pool-update`. Internally the function composes two reachability filters (`ancestors_of(anchor)` / `descendants_of(anchor)`) so candidates that would close a cycle (direct or transitive) are dropped from the OUTGOING / INCOMING pools respectively. Same `anchor` validation as `link_menu_ui()`.
  - `link_menu_server(id)`: `moduleServer` wrapping `eventReactive(input$commit, ...)` with the `nonce` stripped. The server does NOT reshape `source` / `target` - the JS already did that based on `data-direction`.
  - `link_menu_dep()`: `htmlDependency` for the bundled CSS + JS.
- [x] 1.4 Unit tests `tests/testthat/test-link-menu.R`:
  - Anchor with free outputs only (data block) renders OUTGOING section only.
  - Anchor with free inputs only (e.g. plot block) renders INCOMING section only.
  - Variadic anchor renders both sections.
  - Targets with no free input ports (finite arity, all wired) are filtered out of OUTGOING.
  - Arity-1 target renders only the Link ID field; arity > 1 renders the Block input picker; variadic renders only the Link ID field.
  - INCOMING port picker reflects the **anchor's** free slots (not the candidate source's).
  - Cards in each section carry `data-direction` set to `"outgoing"` / `"incoming"` correctly.
  - Unknown `anchor` id raises a classed error.
  - Empty pool in both directions: the panel renders with an empty-state, not NULL.
  - `link_menu_dep()` references the bundled CSS / JS; the rendered panel resolves both `blockr-block-browser` and `blockr-link-menu` deps.
  - `link_menu_server()` strips the `nonce` and returns the spec (via `testServer`) for both an outgoing-direction commit (composed `source = anchor`) and an incoming-direction commit (composed `target = anchor`).
  - `link_eligible_pools(board, anchor)`: returns both pools for a variadic anchor; returns `incoming = character()` when the anchor has no free input and isn't variadic; raises `blockr_ui_link_menu_unknown_anchor` for an unknown anchor id. The `free_inputs` field is keyed by target id, shrinks after a port is wired, and is `character()` for variadic targets.
  - Cycle filter: with an existing `h -> r` link, anchor `r` excludes `h` from OUTGOING; with `a -> h -> r`, anchor `r` excludes both `h` and `a` (transitive); anchor `h` excludes `r` from INCOMING.
  - Malformed inputs rejected.
- [x] 1.5 shinytest2 `tests/testthat/test-link-menu-shinytest2.R` against `inst/examples/link-menu/app.R`:
  - Search filters cards live across both sections.
  - OUTGOING card body click commits with the correct `source = anchor`, `target = card`.
  - INCOMING card body click commits with the correct `source = card`, `target = anchor`.
  - Chevron toggle does not commit.
  - In-card "Add link" commits with the edited link id and block input.
  - Repeat commits re-fire (the nonce advances).
  - Live `pool-update` via `session$sendInputMessage` hides a just-wired card without re-rendering the panel (scroll position + expanded card state preserved), and shows the empty-state when the pool drains.
  - `pool-update` rebuilds the per-card block-input `<select>` options - after committing a link to `m` on port `x`, `m`'s card offers only `y`.
- [x] 1.6 Runnable example `inst/examples/link-menu/app.R` exercising the flow against a small board with a mix of arity-1, arity > 1, and variadic blocks. Provide buttons that open the menu anchored on a source-only block, a target-only block, and a variadic block so the suite hits all three layout cases.
- [x] 1.7 Vignette `vignettes/link-menu.Rmd`: minimal example + commit-spec shape + the bidirectional behaviour.
- [x] 1.8 README screenshot: a third figure under "Example", captured via shinytest2 in `README.Rmd`, named `man/figures/link-menu.png` (anchored on a block whose OUTGOING section is non-trivial so the picker is interesting).
- [x] 1.9 `devtools::check()` clean (0/0/1 timestamp NOTE); `lintr::lint_package()` clean under the CI config (`linters_with_defaults(object_name_linter = NULL)`); `openspec validate add-link-menu --strict` clean.

## 2. Phase 2 - `blockr.dock` adopts the link menu (PR 2)

- [ ] 2.1 In `add_link_action()`: mount `committed <- blockr.ui::link_menu_server("menu")` once; replace the `link_sidebar_body(...)` call (in `observeEvent(trigger(), ...)` and the `onFlushed` chain) with `blockr.ui::link_menu_ui(session$ns("menu"), board$board, anchor = trigger())`. Extract a small `menu_ui()` helper since the call appears twice in the handler. The dock's pre-flight `NULL`-check + notification disappears (the menu now owns the empty-state).
- [ ] 2.2 Replace the per-field observers (`create_link`, `add_link_input`, `add_link_id`, `add_link_confirm`) with a single `observeEvent(committed(), ...)`:
  - Validate `spec$link_id` against `board_link_ids(board$board)` (the menu seeds via `rand_names`, so the typical case is a no-op pass; still guard against the rare collision).
  - Resolve `chosen_input`: `spec$block_input` when supplied; otherwise the target's first free slot via `block_input_select(blk, spec$target, links, mode = "inputs")[1L]`. Variadic targets generate a fresh slot via the same helper.
  - Build the link: `new_link(from = spec$source, to = spec$target, input = chosen_input)`, then `update(list(links = list(add = as_links(set_names(list(new_lnk), spec$link_id)))))`.
  - Bump the sidebar title to `paste0("Connect ", trigger())` so the anchor is always visible and the wording reads correctly whichever direction the user picks.
  - **Multi-link session**: after `update(...)`, in `session$onFlushed(once = TRUE, ...)`, recompute pools via `blockr.ui::link_eligible_pools(board$board, trigger())`. If both pools are empty, `hide_sidebar()`. Otherwise `session$sendInputMessage(session$ns("menu-commit"), list(type = "pool-update", eligible = pools, link_id_seed = rand_names(board_link_ids(board$board))))` so the open menu drops the just-wired card client-side without a full re-render. Do NOT call `keep_or_hide_sidebar()`.
- [ ] 2.3 Delete `link_sidebar_body()` from `R/action-sidebar.R` (no in-tree callers remain). The whole file goes away if no other `*_sidebar_body()` helpers remain at that point.
- [ ] 2.4 Update / replace `tests/testthat/test-action-link.R` to drive `session$setInputs(\`menu-commit\` = list(source, target, link_id, block_input, nonce))` with the new spec. Cover:
  - Valid OUTGOING commit (default port).
  - Valid OUTGOING commit (explicit port for arity > 1 target).
  - Valid INCOMING commit (anchor is target, source from the card, default port).
  - Invalid link id (duplicate).
  - Empty-pool render (anchor with no free inputs and no other blocks with free inputs).
- [ ] 2.5 Manual smoke test of the dock + dag example:
  - Right-click a data block (e.g. `dataset_block`) -> Add link -> OUTGOING section lists the downstream blocks with free inputs; click a card -> link appears with default port; **the menu stays open and the just-wired card disappears from the list live** (no flicker / re-render).
  - Right-click a downstream block (e.g. `head_block`) -> Add link -> INCOMING section lists the upstream candidate(s); click a card -> link appears.
  - Right-click a variadic block (e.g. `rbind_block`) -> Add link -> BOTH sections render; verify each direction works; verify a second click on a remaining card still works without re-opening.
  - Multi-link session: from a single anchor, add several links in sequence without closing the sidebar; verify the cards drop one-by-one until the pool drains, at which point the sidebar closes by itself.
  - Right-click a block whose targets are all fully-wired AND has no free inputs -> sidebar opens with the empty-state; close manually.
- [ ] 2.6 `devtools::check()` clean on `blockr.dock`; `lintr::lint_package()` clean under the CI config.
- [ ] 2.7 NEWS in both packages: `blockr.ui` ("New `link_menu_ui()` / `link_menu_server()` module ... bidirectional: right-click any block to add a link in either direction."); `blockr.dock` ("`add_link_action` now mounts the link-menu module; the action is bidirectional - right-clicking a downstream block now lets you pick an upstream source. Per-field link inputs and `link_sidebar_body()` removed - use `blockr.ui::link_menu_ui()` / `link_menu_server()`.").

## 3. Validation

- [ ] 3.1 `openspec validate add-link-menu --strict` - no spec / template / scenario errors.
- [ ] 3.2 `devtools::check(remote = TRUE, manual = TRUE)` on `blockr.ui` - 0 errors / 0 warnings / 0 notes (modulo timestamp).
- [ ] 3.3 `devtools::check()` on `blockr.dock` after Phase 2 - same.
- [ ] 3.4 `lintr::lint_package()` clean on both packages under the CI config.
