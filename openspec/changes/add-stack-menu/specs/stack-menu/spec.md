## ADDED Requirements

### Requirement: `stack_menu_ui()` builder

`blockr.ui` SHALL export `stack_menu_ui(id, board, target = NULL)` returning an `htmltools::tag` with `stack_menu_dep()` attached, where `id` is the module id. The parameter name `target` mirrors `block_browser_ui(id, board, target = NULL)` for cross-module consistency. The function MUST render a card-list-with-search panel, plus the stack-level form fields (name, color, id) below the cards, plus a confirm button.

Cards SHALL be rendered one per board block in the eligible pool. The pool is `blockr.dock::available_stack_blocks(board)` augmented with the target stack's current blocks (so the user can deselect them). When `target = NULL` (create) the pool is just the available set and no card is pre-selected. When `target = "<stack_id>"` (edit) the function MUST resolve `board_stacks(board)[[target]]`, raise a clean error if no such stack exists, and pre-select the stack's current blocks.

The root element's `id` SHALL be `NS(id)("commit")` (the input-binding target read by `stack_menu_server(id)` as `input$commit`). The root SHALL carry `data-mode` set to `"create"` or `"edit"`. Cards SHALL use the existing `blockr-block-browser-card*` CSS classes - the card visuals are reused by reference, no rename.

The panel-level form SHALL include:

- `stack_name` (always visible top-level). Pre-filled with `stack_name(stack)` in edit mode, empty otherwise.
- `stack_color` (always visible top-level). A hue + lightness slider + hex text input widget shipped by `blockr.ui` (the hex `<input type="text">` is the value carrier under `NS(id)("stack_color")`; sliders write the hex on input, the JS keeps slider positions in sync when the hex is edited by typing). Defaults to a neutral seed in create (callers can override via `updateTextInput("stack_color", ...)`); pre-filled with `attr(stack, "color")` in edit.
- `stack_id` is a visible top-level `textInput` in create mode, pre-filled with `blockr.core::rand_names(board_stack_ids)`. Absent entirely in edit mode (the id is immutable once a stack is created).

The function SHALL NOT create per-field Shiny `inputId`s in the multi-select card list (cards are plain DOM, read by the bundled JS on confirm). The panel-level form fields ARE Shiny inputs since they carry persistent state across renders; their ids are `NS(id)("stack_name")`, `NS(id)("stack_color")`, `NS(id)("stack_id")`.

#### Scenario: Create mode renders the empty multi-select card list

- **WHEN** `stack_menu_ui("mod", board)` is rendered on a board with at least one block in the available pool
- **THEN** the rendered tag tree contains one `.blockr-block-browser-card` per eligible block
- **AND** no card carries `.card-selected`
- **AND** the root element has `id="mod-commit"` and `data-mode="create"`

#### Scenario: Edit mode pre-selects the stack's blocks

- **WHEN** the board contains stack `"s1"` with blocks `c("a", "b")` and `stack_menu_ui("mod", board, target = "s1")` is rendered
- **THEN** the cards for `"a"` and `"b"` carry `.card-selected`
- **AND** the stack name input is pre-filled with the stack's current name
- **AND** the color input is pre-filled with the stack's current color
- **AND** no `stack_id` input is rendered (id is fixed in edit mode)
- **AND** the root has `data-mode="edit"`

#### Scenario: Edit mode augments the pool with the stack's own blocks

- **WHEN** stack `"s1"` contains block `"x"` AND `"x"` is NOT in `available_stack_blocks(board)` (it's owned by `"s1"`) AND `stack_menu_ui("mod", board, target = "s1")` is rendered
- **THEN** a card for `"x"` is present (so the user can deselect it)
- **AND** the card for `"x"` is `.card-selected`

#### Scenario: Unknown target id raises a clean error

- **WHEN** `stack_menu_ui("mod", board, target = "no_such_stack")` is called
- **THEN** an informative error is raised

#### Scenario: All three fields visible top-level in create, stack_id absent in edit

- **WHEN** create mode renders
- **THEN** `stack_name`, `stack_color`, and `stack_id` are all visible top-level form fields
- **AND** there is no Advanced toggle (no `<details>` / `<summary>`)

- **WHEN** edit mode renders
- **THEN** `stack_name` and `stack_color` remain visible and the `stack_id` input is not rendered at all

### Requirement: `stack_menu_server()` returns the committed stack spec

`blockr.ui` SHALL export `stack_menu_server(id)`, a Shiny module server returning a `reactive` that fires once per confirm with the committed stack spec:

```json
{ "blocks": ["a", "b"], "name": "Imports", "color": "#A8DCEF", "id": "imports_42" }
```

The JS binding SHALL publish only `list(blocks = <selection>, nonce = ...)` on `input$commit`. The server SHALL compose the full spec by reading the panel-level Shiny inputs directly (`input$stack_name`, `input$stack_color`, and - in create mode - `input$stack_id`); `id` is `NULL` for edit mode (no such input exists). The internal `nonce` SHALL be stripped before the spec is returned. The reactive SHALL re-fire on repeat confirms (the binding includes the nonce so identical specs still register as changes).

#### Scenario: Server composes the spec from binding + panel inputs

- **WHEN** the binding sets `input$commit` to `list(blocks = c("a"), nonce = 1)` AND the panel inputs are `input$stack_name = "S"`, `input$stack_color = "#fff"`, `input$stack_id = "s1"`
- **THEN** `stack_menu_server("mod")`'s returned reactive yields a list with `blocks = c("a")`, `name = "S"`, `color = "#fff"`, `id = "s1"`, and no `nonce`

#### Scenario: Repeat confirms re-fire the reactive

- **WHEN** the same selection is confirmed twice (two `input$commit` updates with different `nonce`) and the panel inputs are unchanged
- **THEN** the returned reactive fires twice

### Requirement: Multi-select via input binding (no `Shiny.setInputValue`)

`blockr.ui` SHALL ship a `Shiny.InputBinding` registered as `blockr.ui.stackMenu` and bound to `.blockr-stack-menu`. Clicking a card body SHALL toggle `.card-selected` and add/remove the card's `data-block-type` from the selection set. The binding SHALL NOT issue ad-hoc `Shiny.setInputValue` calls; `getValue` returns the recorded `list(blocks, nonce)`; `subscribe` listens for `blockr-stack-menu:commit`. `receiveMessage` is reserved. The panel-level name / color / id fields are normal Shiny inputs - the binding does NOT read them.

#### Scenario: Card click toggles selection

- **WHEN** an unselected card is clicked
- **THEN** the card gains `.card-selected`
- **AND** the card's `data-block-type` is appended to the selection set

- **WHEN** a selected card is clicked again
- **THEN** the card loses `.card-selected`
- **AND** its id is removed from the selection set

#### Scenario: Confirm publishes the selection set

- **WHEN** the user has two cards selected and clicks the confirm button
- **THEN** the binding sets `input[[<root id>]]` to an object with `blocks` (the two ids) and a `nonce`
- **AND** the publish fires whether the selection is new or identical to the previous one (the `nonce` advances)

### Requirement: Client-side search filters cards in place

The bundled JS SHALL listen for `input` events on the search box and hide cards whose `data-name + data-description + data-package + data-category` does NOT contain the (case-insensitive) search string. Empty category sections SHALL collapse via CSS. There SHALL NOT be a server round-trip for search. Selection state SHALL be preserved across visibility changes (a hidden-but-selected card stays in the set; clearing the search re-shows it with its `.card-selected` intact).

#### Scenario: Substring search hides non-matching cards

- **WHEN** the user types `"data"` into the search input
- **THEN** cards whose metadata contains `"data"` (case-insensitive) remain visible
- **AND** non-matching cards get `.hidden`
- **AND** category headers whose cards are all hidden are themselves hidden

#### Scenario: Search preserves selection

- **WHEN** the user has selected a card AND types a search term that hides that card
- **THEN** the card retains `.card-selected`
- **AND** clearing the search re-shows the card with its `.card-selected` state intact

### Requirement: `stack_menu_dep()` dependency and CSS reuse

`blockr.ui` SHALL export `stack_menu_dep()` returning an `htmltools::htmlDependency` referencing the bundled `inst/assets/css/blockr-stack-menu.css` and `inst/assets/js/blockr-stack-menu.js`. `stack_menu_ui()` MUST attach this dependency automatically. The block-browser CSS (`block_browser_dep()`) SHALL also be attached (or its file linked from the stack-menu CSS) so the shared card visual classes resolve.

#### Scenario: Both the shared block-browser CSS and the stack-menu CSS/JS resolve

- **WHEN** the result of `stack_menu_ui("mod", board)` is passed through `htmltools::findDependencies()`
- **THEN** the resolved dependencies include both `blockr-block-browser` (for the card visuals) and `blockr-stack-menu` (for the multi-select JS and panel-level form CSS)

### Requirement: Drop-in replacement for `stack_sidebar_body()`

`blockr.dock`'s `add_stack_action` and `edit_stack_action` SHALL mount the module - `stack_menu_server(<sub_id>)` once per handler returning the committed-stack reactive - and on the trigger `show_sidebar(ui = blockr.ui::stack_menu_ui(session$ns(<sub_id>), board$board, target))` where `target` is `NULL` (create) or `trigger()` (edit). The action handlers SHALL `observeEvent()` the returned reactive to build / update the stack, replacing the per-field observers (`stack_block_selection`, `stack_id`, `stack_name`, `stack_color`, `stack_confirm`, and the `edit_stack_*` equivalents). `stack_sidebar_body()` SHALL be removed from `blockr.dock`.

#### Scenario: Action handlers observe the returned reactive

- **WHEN** a user opens the sidebar via `add_stack_action`'s trigger, picks two blocks, types a name, and clicks Create Stack
- **THEN** `stack_menu_server()`'s reactive fires with the spec
- **AND** the handler's single `observeEvent()` on it runs

#### Scenario: stack_sidebar_body is gone

- **WHEN** `blockr.dock` is checked after this change
- **THEN** there is no `stack_sidebar_body()` definition in `R/`
- **AND** no `R/` file references it
