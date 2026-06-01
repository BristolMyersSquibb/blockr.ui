## Context

`add-block-browser` shipped a card-list pattern for picking a registered block to add / append / prepend. The stack flow (add / edit) still uses a selectize dropdown plus per-field Shiny inputs (`stack_id`, `stack_name`, `stack_color`, `stack_block_selection`, `stack_confirm`). That predates the card-list pattern and is now the only chooser in the panel that looks different.

This change brings the stacks panel in line by adding a sibling module `stack_menu_ui()` / `stack_menu_server()` in `blockr.ui`. It is **not** a generalisation of `block_browser` - the two share visual primitives but differ in three meaningful ways:

| Concern         | `block_browser`                           | `stack_menu`                                                |
| --------------- | ----------------------------------------- | ----------------------------------------------------------- |
| Card source     | Registry types (`available_blocks()`)     | Board instances (`available_stack_blocks()` + target stack) |
| Selection model | Single-shot click-to-add                  | Multi-select toggle, published on confirm                   |
| Per-card form   | id / title / link / port (mode-dependent) | None (stack-level fields live at panel level)               |
| Confirm action  | Add one block (+ optional link)           | Create or update a stack                                    |

## Goals

- One new public module - `blockr.ui::stack_menu_ui()` / `stack_menu_server()` - that the stack action handlers mount in place of `stack_sidebar_body()`.
- The chooser is a card list grouped by category, with the same look as the block browser (icon, name, package badge, description, per-category colour, search bar, empty-state).
- Multi-select with explicit toggle; selection state is purely client-side until the confirm button fires.
- Edit flow pre-selects the stack's current blocks and pre-fills name / color (all fields visible top-level - the id is fixed and not rendered).
- Create flow leaves the selection empty and surfaces the stack name, colour and (auto-seeded) id as visible top-level form fields - no Advanced toggle. Edit mode omits the id input entirely (the id is immutable).
- Reuse `block_browser`'s shipped CSS classes by reference. No rename in this change.

## Non-Goals

- No rename of `blockr-block-browser-*` CSS classes. The third-consumer rename (to `blockr-card-*` or similar) is a follow-up.
- No drag-reorder of selected blocks. Stack membership is a set; order is irrelevant for `stack_blocks(stack) <- ids`.
- No per-card form on the stack cards. Stack-level params (name / id / color) stay at the panel level.
- No change to the stack data model in `blockr.core` (`stack_blocks` / `stack_name` / `stack_color` / `new_dock_stack`).

## API

```r
stack_menu_ui(id, board, target = NULL)
stack_menu_server(id)
stack_menu_dep()
```

- `id` is the module id (the `*_ui()` call site passes `NS(id)("...")` from the parent).
- `board` is the current board (read-only for rendering).
- `target = NULL` selects the *create* flow. `target = "<stack_id>"` selects the *edit* flow: the function looks up the stack via `board_stacks(board)[[target]]`, augments the available pool with the stack's current blocks (so they can be deselected), pre-selects them, and pre-fills name / color. An unknown `target` id raises a clean error. The parameter name mirrors `block_browser_ui(id, board, target = NULL)` for cross-module consistency.

The root element's `id` is `NS(id)("commit")`, the Shiny input the `blockr.ui.stackMenu` binding reports against. `stack_menu_server(id)` strips the internal `nonce` and returns a `reactive` whose value is the spec.

### Commit spec

```json
{
  "blocks": ["a", "b"],
  "name": "Imports",
  "color": "#A8DCEF",
  "id": "imports_42",
  "nonce": 1
}
```

`blocks` is a character vector of selected board-block ids (in click order, which the consumer ignores). `id` is the user-editable stack id (create flow). `nonce` is an internal counter the server strips.

### Server-side flow

```r
# In add_stack_action()'s moduleServer body:
committed <- blockr.ui::stack_menu_server("menu")

observeEvent(trigger(), {
  blockr.ui::show_sidebar(
    sidebar_id, title = "Create new stack",
    ui = blockr.ui::stack_menu_ui(session$ns("menu"), board$board)
  )
})

observeEvent(committed(), {
  spec <- committed()
  # validate spec$id / spec$color / etc., then:
  new_stk <- as_stacks(
    set_names(
      list(new_dock_stack(blocks = spec$blocks, name = spec$name,
                          color = spec$color)),
      spec$id
    )
  )
  update(list(stacks = list(add = new_stk)))
  blockr.ui::keep_or_hide_sidebar(
    sidebar_id, title = "Create new stack",
    ui = blockr.ui::stack_menu_ui(session$ns("menu"), board$board)
  )
})
```

`edit_stack_action` is the same shape with `target = trigger()` (the stack id) and `update(list(stacks = list(mod = ...)))` on confirm.

## Architecture

### Reuse

- **CSS classes** (`blockr-block-browser-card*`, `blockr-block-browser-category*`, `blockr-block-browser-search`, per-category colour rules) are reused by reference. `stack_menu_ui()`'s tag tree builds the same card markup so the visual surface is identical. `stack_menu_dep()` references the existing `inst/assets/css/blockr-block-browser.css` (attached automatically) plus a small `blockr-stack-menu.css` for stack-only additions (`.card-selected` ring is already in the block-browser CSS, but unused there; the stack menu actually uses it, so the existing rule earns its keep).
- **Search filter** logic is duplicated across the two JS files. ~20 lines; small enough not to factor for v1. A shared `card_list` primitive is the deferred rename target if a third consumer appears.

### What's new

- `inst/assets/js/blockr-stack-menu.js` - a `Shiny.InputBinding` for `.blockr-stack-menu`:
  - Tracks the **selection set** (ordered list of `data-block-type` strings, where `data-block-type` is repurposed to carry the board block id).
  - Click on a card body toggles `.card-selected` and adds / removes from the set. No chevron, no per-card form.
  - Listens for `input` on the search box (same substring filter over `data-name + data-description + data-package + data-category`).
  - Listens for `click` on the confirm button: assembles `list(blocks = <selection set>, nonce = ...)`, records on the root, and dispatches `blockr-stack-menu:commit`. The binding's `subscribe` callback fires; Shiny re-reads `getValue`; the server's `eventReactive` fires. The panel-level name / color / id fields are NOT gathered by the JS - they are normal Shiny inputs (see below) and the server reads them directly.

- `R/stack-menu.R`:
  - `stack_menu_ui(id, board, target = NULL)` - renders the panel:
    - Optional context subtitle ("Editing **Imports**" in edit mode).
    - Search input.
    - Card list grouped by category (one card per board-block id in the available pool).
    - Panel-level form below the cards: `stack_name` and `stack_color` always visible top-level; `stack_id` under an Advanced toggle in create mode only - fixed and absent in edit.
    - Confirm button (`Create Stack` / `Update Stack`).
  - `stack_menu_server(id)` - `moduleServer` wrapping `eventReactive(input$commit, list(blocks = input$commit$blocks, name = input$stack_name, color = input$stack_color, id = input$stack_id))`. The internal `nonce` is dropped on the way out; `stack_id` is `NULL` in edit mode (no such input rendered).
  - Helpers reuse the block-browser pattern: `entry_attr()` (lifted as it's the same logic) - or kept private duplicates if we want zero coupling.

### Panel-level form fields and DOM ids

The three panel-level fields are **real Shiny inputs** (not plain DOM read by JS). Because there are only three of them - no per-card multiplication, no noise in the reactive graph - this is the right model: the server composes the spec directly from `input$stack_name` / `input$stack_color` / `input$stack_id`, and any rich widget (sliders, colour picker, ...) drops in without bespoke JS gathering.

| Field       | DOM id                  | Notes                                                                                                                     |
| ----------- | ----------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| Stack name  | `NS(id)("stack_name")`  | `textInput`. Always visible.                                                                                              |
| Stack color | `NS(id)("stack_color")` | Hex `<input type="text">` driven by hue / lightness sliders + the hex field itself (see below). Always visible top-level. |
| Stack id    | `NS(id)("stack_id")`    | `textInput`. Visible top-level in create; absent in edit (the id is immutable).                                           |

### Stack color: hue + lightness sliders + hex

Rendered inline in the panel - no popover, no floating context menu, no escape from the sidebar's bounding box. The widget is four pieces wired to a single Shiny input value (a hex string):

```text
[#] [#66c2a5_______________]   <- preview swatch + hex field (value carrier)
[============o=============]   <- hue slider (range 0..360, CSS-gradient track)
[==========o===============]   <- lightness slider (range 20..85, dark->light)
```

- The hex `<input type="text">` is the **value carrier**: a regular Shiny text input under `NS(id)("stack_color")`. The server reads it via Shiny's built-in text-input binding; we never touch `Shiny.setInputValue`.
- The sliders are `<input type="range">` styled with CSS-gradient tracks (rainbow for hue, black->white for lightness). On `input` the JS computes `HSL(hue, 60%, lightness) -> hex`, writes the hex field, and dispatches a native `"input"` event so Shiny picks the change up. Saturation is fixed at 60% to keep the picker simple while staying inside a pleasant chroma range.
- Typing in the hex field is the reverse path: the JS parses `#RRGGBB -> HSL`, repositions the sliders, and updates the preview swatch.
- The hex field's value is validated client-side (regex `^#[0-9A-Fa-f]{6}$`) only for visual feedback; downstream consumers (the dock action handlers) do the authoritative `is_hex_color()` check before applying.
- Defaults: in create mode, a neutral seed (`#66c2a5`); the dock can call `updateTextInput(session, "menu-stack_color", value = suggest_new_colors(...))` after `show_sidebar()` to seed a smarter starting colour. In edit mode, the stack's current `attr(stack, "color")`.

No new `Imports` for `blockr.ui`. Implementation budget: ~30 lines R (markup), ~60 lines JS (HSL <-> hex round-trip + slider/hex sync), ~70 lines CSS (slider track + thumb styling for WebKit + Gecko, preview swatch).

The earlier fallback to `shinyWidgets::colorPickr(inline = TRUE)` is no longer planned - the sliders give a continuous-feeling picker without the dependency. Switching to `colorPickr` remains a local substitution if a future need arises (same input id, same hex string contract).

### Edit-mode lookup

`stack_menu_ui(id, board, target = "abc")`:

1. Resolve `stack <- board_stacks(board)[[target]]`; raise an error if absent.
2. `selected <- stack_blocks(stack)`.
3. `pool <- union(available_stack_blocks(board), selected)` - so the user can deselect a current member.
4. Each rendered card carries `data-selected="true"` when its id is in `selected`; the JS reads that on mount and seeds the selection set + applies `.card-selected`.
5. Pre-fill `stack_name` / `stack_color` inputs with `stack_name(stack)` / `stack_color(stack)`.

## Risks / Trade-offs

- **Two cars in one garage**: the stack-menu registers its own `Shiny.InputBinding` against `.blockr-stack-menu`; the block-browser binding stays scoped to `.blockr-block-browser`. They never share an element, so there's no binding-collision risk - but it does mean two small JS files instead of one. Acceptable for v1; a `card_list` primitive is the cleanup.
- **CSS class reuse by reference**: the stack menu depends on the block-browser CSS file existing. If `block_browser_dep()` were dropped or renamed, the stack menu visuals break. Mitigated by `stack_menu_dep()` attaching `block_browser_dep()` explicitly, so the dep graph is honest.
- **Edit-flow data integrity**: the user can deselect every block before confirming. `new_dock_stack(blocks = character())` is valid (an empty stack); we don't block it. The existing `add_stack_action` allows zero-block stacks too.

## Migration / Compatibility

- `stack_sidebar_body()` is removed in `blockr.dock` PR 2. No in-tree callers remain after the action-handler swap; out-of-tree consumers (if any) migrate to `blockr.ui::stack_menu_ui()` - called out in NEWS.
- Per-field stack inputs (`stack_id`, `stack_name`, `stack_color`, `stack_block_selection`, `stack_confirm`, and the `edit_stack_*` equivalents) are removed in favour of `stack_menu_server()`'s returned reactive. Out-of-tree code observing those inputs migrates to the reactive. Called out in NEWS.
- No change to the stack data model in `blockr.core`.

## Deferred / follow-up

- **Card-list primitive**: factor the shared CSS + JS search filter into a `blockr-card-list.*` namespace once a third consumer arrives. Touching the block-browser code is needed but contained.
