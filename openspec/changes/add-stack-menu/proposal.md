## Why

The add-stack / edit-stack flows still use the legacy selectize-style dropdown for picking which blocks belong to a stack. After `add-block-browser` shipped the card-list pattern for the add / append / prepend flows, the dropdown is the odd one out: same panel, same sidebar primitive, completely different chooser. Users have to relearn how to pick blocks the moment they switch from "add a block" to "create a stack".

Bringing the stacks panel in line is also the natural moment to factor what is genuinely a single visual primitive - a category-grouped, search-filterable, click-to-select card list - so future consumers (link picker, ...) build on one set of card classes instead of two.

The stack flow differs from the block-browser flow in three ways that matter:

- Cards represent **board instances** (existing blocks on the board), not registry **types**. The eligibility rule is the existing `available_stack_blocks(board)` pool, augmented in edit mode with the stack's currently-stacked blocks so the user can deselect them.
- Selection is **multi-select** with explicit toggle semantics; clicking a card adds / removes it from the set, and the set is published on the confirm button.
- The stack-level form (name / id / color) lives at the **panel level**, not per-card - blocks added to a stack carry no extra per-block parameter.

This change adds a `stack_menu_ui()` / `stack_menu_server()` module in `blockr.ui` that lifts the card markup classes from `block_browser` (no rename) and layers the multi-select state + panel-level form on top. `blockr.dock` adopts it in `add_stack_action` and `edit_stack_action`, and `stack_sidebar_body()` is removed.

## What Changes

- **NEW** `blockr.ui::stack_menu_ui(id, board, target = NULL)` - the card-list-with-search body, with `target` mirroring `block_browser_ui()`'s parameter. `target = NULL` is the *create* flow; `target = "<stack_id>"` is the *edit* flow (the function looks the stack up in the board, pre-selects its current blocks, and pre-fills its name / color).
- **NEW** `blockr.ui::stack_menu_server(id)` - module server returning a `reactive` that fires once per confirm with the stack spec: `list(blocks, name, color, id)` (`id` is the user-edited stack id for create; for edit, the handler ignores it and keeps the original).
- **NEW** A `Shiny.InputBinding` (`blockr.ui.stackMenu`) registered on the new module's root element. `getValue` returns the spec (with a `nonce` so repeat confirms re-fire), `subscribe` listens for the confirm event, `receiveMessage` is reserved.
- **NEW** Bundled JS (`inst/assets/js/blockr-stack-menu.js`) for the multi-select state machine: card-body click toggles `.card-selected` and updates the selection set. On confirm the binding publishes only `list(blocks, nonce)`; the panel-level name / color / id fields are normal Shiny inputs the server reads directly.
- **REUSE** The `blockr-block-browser-card*` CSS classes are reused as-is (no rename). The category-color rules, search-bar styling, card-hover, empty-state, and per-card icon / name / package badge / description markup are shared. A small `inst/assets/css/blockr-stack-menu.css` adds only the bits the stack menu needs (panel-level form layout, `.card-selected` visual confirmation, the multi-select `aria-multiselectable` polish).
- **MODIFIED (in `blockr.dock`)** `add_stack_action` and `edit_stack_action` mount the new module and observe its returned reactive; the per-field input observers (`stack_block_selection`, `stack_id`, `stack_name`, `stack_color`, `stack_confirm` / `edit_stack_*`) and `stack_sidebar_body()` are removed.

## What's *not* in this change

- A bigger CSS rename from `blockr-block-browser-*` to `blockr-card-*`. The card classes are reused by reference; a rename is reasonable once a third consumer appears - flagged as a follow-up in `design.md`.
- Card-level multi-select features beyond toggle (no drag-reorder; the order doesn't matter for `stack_blocks(stack) <- ids` - blockr.core treats the stack's blocks as a set).
- Stack ID editing in edit mode. The id remains fixed once a stack is created (same as today).

## Capabilities

### New Capabilities

- `stack-menu`: the multi-select card-list block picker for stacks (`stack_menu_ui()`, `stack_menu_server()`, `stack_menu_dep()`), plus the JS binding that owns the multi-select state and confirm publish.

### Modified Capabilities

None in `blockr.ui`. In `blockr.dock`, `add_stack_action` and `edit_stack_action` are modified to mount the new module and observe its committed-stack reactive; `stack_sidebar_body()` is removed.

## Phases as separate PRs

Two PRs - one per repo, sequenced like `add-block-browser`.

1. **PR 1 (`blockr.ui`):** `stack_menu_ui()` + `stack_menu_server()` + `stack_menu_dep()`, bundled CSS / JS, unit tests + shinytest2 against a runnable example, vignette section, and the openspec change. Reuses the existing `block_browser_dep()` for the shared card visuals.
2. **PR 2 (`blockr.dock`):** action-handler adoption. `add_stack_action` / `edit_stack_action` mount the module, observe the returned reactive, build / update the stack. `stack_sidebar_body()` is removed. Tests updated. NEWS.

Builds on top of `add-block-browser`; both PRs there must be merged before this lands cleanly. The `blockr.ui` Remotes pin in `blockr.dock` is dropped once `add-block-browser` reaches main.

## Impact

- **Affected packages.** `blockr.ui` gains the stack-menu capability; `blockr.dock` rewrites two action handlers and drops `stack_sidebar_body()`.
- **Affected dependencies.** None new. The stack-menu's color picker is a self-contained inline palette + hex input shipped in `blockr.ui` (no `shinyWidgets` dependency). Should the fallback path be needed, it adds `shinyWidgets` (already an `Imports` of `blockr.dock`); flagged as a contingency, not a planned change.
- **Affected APIs.** `stack_sidebar_body()` is removed (no out-of-tree consumers known; called out in NEWS). The per-field stack inputs (`stack_id`, `stack_name`, `stack_color`, `stack_block_selection`, `stack_confirm`, `edit_stack_*`) are replaced by `stack_menu_server()`'s returned reactive.
- **Affected downstream.** Extensions inherit the new picker for free.
