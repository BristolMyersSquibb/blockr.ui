## 1. Phase 1 - stack menu in `blockr.ui` (PR 1)

Self-contained: no `blockr.dock` change. After this phase a non-dock Shiny app can `library(blockr.ui)`, mount `stack_menu_ui(id, board, target)` / `stack_menu_server(id)`, and `observeEvent()` the returned committed-stack reactive.

- [x] 1.1 `inst/assets/css/blockr-stack-menu.css`:
  - Multi-select state styling (`.card-selected` ring already exists in `blockr-block-browser.css` - confirm it is sized / coloured consistently; add a stack-menu-specific tweak if needed).
  - Panel-level form layout: `stack_name` and `stack_color` rows visible top-level; `stack_id` is a hidden Shiny text input (`display:none`) in create, absent in edit.
  - Colour picker widget: hue + lightness `<input type="range">` sliders styled as CSS gradients (rainbow for hue, dark->light for lightness) + a preview swatch + hex `<input type="text">`. No popover, no floating context.
  - Confirm-button styling reuses `.blockr-block-browser-confirm` (or a thin alias).
- [x] 1.2 `inst/assets/js/blockr-stack-menu.js`:
  - Register a `Shiny.InputBinding` (`blockr.ui.stackMenu`) bound to `.blockr-stack-menu`. `find` / `initialize` / `getValue` / `subscribe` / `unsubscribe`, with `receiveMessage` reserved.
  - Maintain a client-side selection set (ordered list of `data-block-type` strings - here, board block ids).
  - `click` on a card body toggles `.card-selected` and adds / removes from the set.
  - `input` on the search box runs the same case-insensitive substring filter as the block browser (`data-name + data-description + data-package + data-category`); cards keep `.card-selected` across visibility changes.
  - `click` on the confirm button gathers only the selection set, increments `nonce`, records `{ blocks, nonce }` on the root, and dispatches `blockr-stack-menu:commit`. The binding does NOT read `stack_name` / `stack_color` / `stack_id` - those are regular Shiny inputs the server reads directly.
  - Colour-picker wiring: HSL <-> hex round-trip in JS. Moving a hue / lightness slider recomputes `HSL(hue, 60%, lightness) -> hex`, writes the hex `<input>`, and dispatches a native `"input"` event so Shiny's built-in text-input binding picks the change up without a custom path. Typing in the hex field parses back the other way and repositions the sliders. Preview swatch reflects the current hex.
- [x] 1.3 `R/stack-menu.R` (Shiny module):
  - `stack_menu_ui(id, board, target = NULL)`: `ns <- NS(id)`; resolve `target` via `board_stacks(board)[[target]]` (clean error if absent); compute the pool (`available_stack_blocks(board)` augmented with the target stack's blocks); render search + category-grouped cards (mark `.card-selected` on cards already in the target stack) + the panel-level form fields + the confirm button. The parameter name mirrors `block_browser_ui(id, board, target = NULL)`.
  - Panel-level form fields are real Shiny inputs: visible `textInput(ns("stack_name"), ...)`; visible `textInput(ns("stack_color"), ...)` wrapped in the hue / lightness slider + preview markup; in create mode also a hidden `textInput(ns("stack_id"), ...)` (rendered as `<input type="text">` with `display:none` so the default text-input binding still hooks it).
  - `stack_menu_server(id)`: `moduleServer` wrapping `eventReactive(input$commit, list(blocks = input$commit$blocks, name = input$stack_name, color = input$stack_color, id = input$stack_id))`. `input$commit$nonce` is intentionally dropped on the way out; in edit mode `input$stack_id` is `NULL` (no input rendered).
  - `stack_menu_dep()`: `htmlDependency` for the bundled CSS + JS; the rendered panel ALSO attaches `block_browser_dep()` so the shared card classes resolve.
  - Validates `id` (non-empty character scalar) and `target` (NULL or non-empty character scalar).
  - The slider-based picker has no planned fallback. Switching to `shinyWidgets::colorPickr(inline = TRUE)` remains a localised UI substitution (same `NS(id)("stack_color")` input, same hex contract) if a future need arises - not in scope here.
- [x] 1.4 Unit tests `tests/testthat/test-stack-menu.R`:
  - Card-per-eligible-block + `data-*` attributes mirror the block browser.
  - Edit mode pre-selects the stack's blocks (`.card-selected` present); augments the pool with the stack's blocks; pre-fills `stack_name` and `stack_color`.
  - Create mode: no preselection; `stack_id` field visible top-level; name empty.
  - Unknown `target` id errors with a clear message.
  - `stack_menu_dep()` references the bundled CSS / JS; the rendered panel resolves both `blockr-block-browser` and `blockr-stack-menu` deps.
  - `stack_menu_server()` returns the spec composed from `input$commit$blocks` + panel inputs (via `testServer`: set `input$commit`, `input$stack_name`, `input$stack_color`, `input$stack_id`; assert returned reactive value).
  - Malformed inputs rejected.
- [x] 1.5 shinytest2 `tests/testthat/test-stack-menu-shinytest2.R` against `inst/examples/stack-menu/app.R`:
  - Search filters cards live.
  - Card-body click toggles `.card-selected` and selection set.
  - Selection survives a search query that hides a selected card.
  - Moving the hue slider updates the hex input (`input$stack_color`) - verifies the slider -> HSL -> hex -> `"input"` event dispatch reaches Shiny.
  - Confirm publishes a spec with the selected ids, name, and color; repeated confirms re-fire (the nonce advances).
- [x] 1.6 Runnable example `inst/examples/stack-menu/app.R` exercising both create and edit flows.
- [x] 1.7 Vignette section in `vignettes/block-browser.Rmd` (or a new `vignettes/stack-menu.Rmd`): minimal example + commit-spec shape + the create/edit toggle.
- [x] 1.8 `devtools::check()` clean (0/0/1 timestamp NOTE); `lintr::lint_package()` clean under the CI config (`linters_with_defaults(object_name_linter = NULL)`); `openspec validate add-stack-menu --strict` clean.

## 2. Phase 2 - `blockr.dock` adopts the stack menu (PR 2)

> Refined by the `reactive-board-menus` change (blockr.dock #172): the
> handlers pass the board (+ `target` on edit) as **reactives**, so the
> spec validation in 2.2 now lives in `blockr.ui` (the dock-side
> `valid_stack_*` validators are gone) and a pinned menu stays in sync
> with the board via a `menu:sync` diff. `dock_stack` colour checks reuse
> the exported `blockr.ui::is_hex_color()`. The committed-spec shape is
> unchanged; the dock still wraps it into a `dock_stack`.

- [x] 2.1 In `add_stack_action()` and `edit_stack_action()`: mount `committed <- blockr.ui::stack_menu_server("menu")` once; replace the `stack_sidebar_body(...)` call (in `observeEvent(trigger(), ...)` and the `keep_or_hide_sidebar()` chain) with `blockr.ui::stack_menu_ui(session$ns("menu"), board$board, target)` where `target` is `NULL` (create) or `trigger()` (edit). Extract a small `menu_ui()` helper since the call appears twice in each handler.
- [x] 2.2 Replace the per-field observers (`stack_block_selection`, `stack_id`, `stack_name`, `stack_color`, `stack_confirm`, `edit_stack_blocks`, `edit_stack_color`, `edit_stack_name`, `edit_stack_confirm`) with a single `observeEvent(committed(), ...)` per handler:
  - Validate `spec$id` (create) / look up the existing stack (edit), `spec$name`, `spec$color` (hex). Block ids are already pool members, but check membership defensively.
  - Build / update the stack: `new_dock_stack(blocks, name, color)` (create) or `stack_blocks<-` / `stack_name<-` / `stack_color<-` (edit), then `update(list(stacks = list(add = ...)))` or `... = list(mod = ...)`.
- [x] 2.3 Delete `stack_sidebar_body()` from `R/action-sidebar.R` (no in-tree callers remain; `link_sidebar_body()` stays).
- [x] 2.4 Update / replace `tests/testthat/test-action-stack.R` to drive `session$setInputs(\`menu-commit\` = list(blocks, name, color, id, nonce))` with the new spec. Cover create (valid), create with invalid id / invalid color / unknown block id, edit (valid), edit with invalid color / unknown block id.
- [ ] 2.5 Manual smoke test of the dock + dag example:
  - Right-click empty area / open stacks panel -> Create new stack -> pick 2-3 blocks via cards, type name, confirm -> stack appears on the board.
  - Right-click an existing stack -> Edit -> deselect a block / select another / change name + color -> Update -> board reflects the change.
- [x] 2.6 `devtools::check()` clean on `blockr.dock`; `lintr::lint_package()` clean under the CI config.
- [x] 2.7 NEWS in both packages: `blockr.ui` ("New `stack_menu_ui()` / `stack_menu_server()` module ..."); `blockr.dock` ("`add_stack_action` / `edit_stack_action` now mount the stack-menu module; per-field stack inputs and `stack_sidebar_body()` removed - use `blockr.ui::stack_menu_ui()` / `stack_menu_server()`.").

## 3. Validation

- [x] 3.1 `openspec validate add-stack-menu --strict` - no spec / template / scenario errors.
- [ ] 3.2 `devtools::check(remote = TRUE, manual = TRUE)` on `blockr.ui` - 0 errors / 0 warnings / 0 notes (modulo timestamp).
- [x] 3.3 `devtools::check()` on `blockr.dock` after Phase 2 - same.
- [x] 3.4 `lintr::lint_package()` clean on both packages under the CI config.
