## ADDED Requirements

### Requirement: `block_browser_ui()` builder

The block browser SHALL be a Shiny module. `blockr.ui` SHALL export `block_browser_ui(id, board, target = NULL)` returning an `htmltools::tag` with `block_browser_dep()` attached, where `id` is the module id (the `*_ui()` call site passes `NS(id)("...")` from the parent). The flow is selected by `target` (see the target-descriptor requirement): `NULL` is *add*, `append_to(id)` is *append*, `prepend_to(id)` is *prepend*. The function MUST render one `.blockr-block-browser-card` per registered block (from `available_blocks()`) grouped under one `.blockr-block-browser-category` per distinct category in `blks_metadata()$category`. Blocks with a missing or empty `category` SHALL render under a `"Uncategorized"` category.

The root element's `id` SHALL be `NS(id)("commit")` - this is the Shiny input id the `blockr.ui.blockBrowser` input binding reports against, and what `block_browser_server(id)` reads as `input$commit`. The root SHALL carry a `data-mode` attribute set to the resolved flow (`"add"` / `"append"` / `"prepend"`). When the target is a `prepend_to()`, the root SHALL also carry `data-target-arity` set to the target block's input arity (an integer for finite arities, or `"inf"` for variadic / `NA`).

The function SHALL NOT create per-field Shiny `inputId`s for the per-card form (`id`, `title`, `link_id`, `block_input`, `target_input`). Those fields are real DOM `<input>` / `<select>` elements with stable ids namespaced by module id + block type (e.g. `NS(id)(paste0(block_type, "_id"))`), read by the bundled JS when a block is added rather than wired through Shiny's input system.

#### Scenario: One card per registered block

- **WHEN** a caller invokes `block_browser_ui(id, board)` with at least one registered block
- **THEN** the rendered tag tree contains exactly `length(available_blocks())` elements with class `blockr-block-browser-card`
- **AND** each card carries `data-block-type`, `data-name`, `data-description`, `data-package`, `data-category`

#### Scenario: Cards grouped by category

- **WHEN** the registry has blocks across multiple categories
- **THEN** the rendered tag tree contains one `.blockr-block-browser-category` per distinct category
- **AND** each category container has a `data-category` attribute matching the category string

#### Scenario: Blocks with missing description render safely

- **WHEN** `blks_metadata()` returns `NA` or `""` for a block's `description`
- **THEN** the corresponding card still renders (no error, no skipped row)
- **AND** the card's `data-description` attribute is the empty string

#### Scenario: Commit-input id and mode are stamped on the root

- **WHEN** `block_browser_ui("mod_a", board, append_to("src"))` is rendered
- **THEN** the root `.blockr-block-browser` element has `id="mod_a-commit"` and `data-mode="append"`

#### Scenario: Target arity is stamped for prepend

- **WHEN** `block_browser_ui(id, board, prepend_to("merge_xyz"))` is rendered AND `"merge_xyz"` has input arity 2
- **THEN** the root has `data-target-arity="2"`

- **WHEN** the target has variadic (`NA`) arity
- **THEN** the root has `data-target-arity="inf"`

- **WHEN** the flow is add (`NULL` target) or an `append_to()`
- **THEN** the root has no `data-target-arity` attribute

#### Scenario: target surfaces context for append / prepend

- **WHEN** `block_browser_ui(id, board, append_to("block_a"))`
- **THEN** the panel header includes a `.blockr-block-browser-context` subtitle referencing the human name of `block_a`

- **WHEN** the target is `NULL` (add)
- **THEN** there is no `.blockr-block-browser-context` element

#### Scenario: Per-card form fields are not registered as Shiny inputs

- **WHEN** `block_browser_ui(id, board, append_to("src"))` is rendered
- **THEN** no element inside any `.blockr-block-browser-card-advanced` carries the `shiny-input-*` / `shiny-bound-input` classes
- **AND** the only Shiny-registered input owned by the browser is the root's `NS(id)("commit")`

### Requirement: Target descriptor (`append_to()` / `prepend_to()`)

`blockr.ui` SHALL export `append_to(block_id)` and `prepend_to(block_id)` constructors returning a `bb_target` descriptor that carries both the flow (`"append"` / `"prepend"`) and the block id the new block attaches to. `block_browser_ui()`'s `target` argument SHALL accept `NULL` (an add) or a `bb_target`, and SHALL reject any other value. This makes invalid flow/trigger combinations unrepresentable - there is no way to express "append with no source" or "add with a trigger".

#### Scenario: Constructors carry the flow and id

- **WHEN** `append_to("src")` is called
- **THEN** it returns a `bb_target` whose flow is `"append"` and whose id is `"src"`

- **WHEN** `prepend_to("merge1")` is called
- **THEN** it returns a `bb_target` whose flow is `"prepend"` and whose id is `"merge1"`

#### Scenario: Constructors validate the block id

- **WHEN** `append_to("")` or `prepend_to(c("a", "b"))` is called
- **THEN** an error is raised

#### Scenario: block_browser_ui rejects a non-target value

- **WHEN** `block_browser_ui(id, board, target = "src")` is called (a bare string, not a `bb_target`)
- **THEN** an error is raised

### Requirement: `block_browser_dep()` dependency

`blockr.ui` SHALL export `block_browser_dep()` returning an `htmltools::htmlDependency` referencing the bundled CSS (`inst/assets/css/blockr-block-browser.css`) and JS (`inst/assets/js/blockr-block-browser.js`). `block_browser_ui()` MUST attach this dependency automatically via `htmltools::attachDependencies()`.

#### Scenario: Dependency files exist and are referenced

- **WHEN** a caller invokes `block_browser_dep()`
- **THEN** the returned object is an `html_dependency` with `name` `"blockr-block-browser"`, `stylesheet` `"css/blockr-block-browser.css"`, `script` `"js/blockr-block-browser.js"`
- **AND** both files exist on disk under `system.file("assets", package = "blockr.ui")`

#### Scenario: `block_browser_ui()` attaches the dependency

- **WHEN** the result of `block_browser_ui(id, board)` is passed through `htmltools::findDependencies()`
- **THEN** the resolved dependencies include `blockr-block-browser`

### Requirement: Client-side search filters cards in place

The bundled JS SHALL listen for `input` events on `.blockr-block-browser-search` and hide cards whose `data-name + data-description + data-package + data-category` does NOT contain the (case-insensitive) search string. Empty category sections SHALL collapse via CSS. There SHALL NOT be a server round-trip for search.

#### Scenario: Substring search hides non-matching cards

- **WHEN** a user types `"data"` into `.blockr-block-browser-search`
- **THEN** cards whose metadata contains `"data"` (case-insensitive) remain visible
- **AND** non-matching cards get `.hidden`
- **AND** category headers whose cards are all hidden are themselves hidden

#### Scenario: Empty search restores all cards

- **WHEN** a user clears the search input
- **THEN** every card and category header becomes visible again

### Requirement: Per-card expand reveals the configuration form

Each card SHALL have a `.blockr-block-browser-card-chevron` element. Clicking the chevron SHALL toggle `.card-expanded` on its card, revealing the per-card advanced form (`id`, `title`, and the mode-appropriate `link_id` / `block_input` / `target_input`) and an in-card add button. The chevron toggle is symmetric (a second click collapses) and MUST NOT add the block.

`block_browser_ui()` SHALL render only the fields the resolved flow needs: `id` and `title` always; `link_id` for append and prepend; `block_input` for append; `target_input` for prepend into a target with more than one input slot. The inapplicable fields SHALL be absent from the DOM (not rendered-then-hidden), so no per-mode CSS hiding is required. The card's description SHALL be clamped while collapsed and shown in full when the card is expanded.

#### Scenario: Chevron toggles the form without adding

- **WHEN** a collapsed card's chevron is clicked
- **THEN** the card gains `.card-expanded` and its advanced form becomes visible
- **AND** no commit event is fired

- **WHEN** the chevron is clicked again
- **THEN** the card loses `.card-expanded`

#### Scenario: Flow-specific fields

- **WHEN** the flow is add (`NULL` target)
- **THEN** the advanced form renders `id` and `title` only (no `link_id` / `block_input` / `target_input` fields)

- **WHEN** the flow is an `append_to()`
- **THEN** the form also renders `link_id` and `block_input`

- **WHEN** the flow is a `prepend_to()` whose target arity is finite and greater than 1
- **THEN** the form renders `link_id` and `target_input`

- **WHEN** the flow is a `prepend_to()` whose target arity is 1 or variadic (`inf`)
- **THEN** the `target_input` field is not rendered

### Requirement: Adding a block publishes a single-block commit via an input binding

`blockr.ui` SHALL ship a `Shiny.InputBinding` registered as `blockr.ui.blockBrowser` and bound to `.blockr-block-browser`. The browser's value is the block the user chose to add: adding is an event (not persistent state), so there SHALL NOT be ad-hoc `Shiny.setInputValue` calls - the binding owns the value channel via `getValue` / `subscribe`, and reserves `receiveMessage` for the R->JS direction (used by a later AI-search card filter).

Clicking a card's body (outside the chevron and outside the advanced form) SHALL add the block immediately using the card's current (default) field values. Clicking the in-card add button (`.blockr-block-browser-card-add`) SHALL add the block using the card's current (possibly edited) field values. Clicking inside the advanced form other than the add button SHALL NOT add the block.

Adding a block SHALL set the binding's value to a single-block spec and fire its change event so Shiny re-reads `getValue`:

```json
{ "type": "...", "id": "...", "title": "...", "link_id": "...", "block_input": "...", "target_input": "...", "nonce": 1 }
```

`type` is the card's `data-block-type` (the registry constructor name). Fields not applicable to the current mode (hidden via CSS) SHALL be `null` (which R reads as `NULL`). An empty `title` SHALL be `null`. The `nonce` is a monotonically increasing counter that guarantees every add registers as a fresh change (so adding the same block twice still fires an event); `block_browser_server()` strips it before handing the spec to consumers. The value is reachable as `input$commit` inside the module (root id `NS(id)("commit")`). There SHALL NOT be any other Shiny input owned by the browser, and there SHALL NOT be a multi-block / `connection` wrapper.

#### Scenario: Card-body click adds with defaults

- **WHEN** a user clicks a card's header (a dataset block) in the add flow
- **THEN** `input[["<root id>"]]` becomes an object whose `type` is `"dataset_block"` and whose `id` is the card's non-empty default id
- **AND** the card does NOT gain `.card-expanded`
- **AND** adding the same card again re-fires the event (the `nonce` changes)

#### Scenario: In-card add button adds with edits

- **WHEN** a user expands a card, edits its `id` field, and clicks the in-card add button
- **THEN** the published spec's `id` equals the edited value
- **AND** the spec's `type` is the card's block type

#### Scenario: Clicking a form input does not add

- **WHEN** a user expands a card and clicks its `id` input field
- **THEN** no commit event is fired

#### Scenario: Inapplicable fields are null

- **WHEN** a block is added in the add flow
- **THEN** the published spec has `link_id = null`, `block_input = null`, `target_input = null`

### Requirement: `block_browser_server()` returns the committed block

`blockr.ui` SHALL export `block_browser_server(id)`, a Shiny module server matching the `block_browser_ui(id, ...)` UI. It SHALL return a `reactive` that fires once per add with the committed block spec (`list(type, id, title, link_id, block_input, target_input)`), with the internal `nonce` stripped. The reactive SHALL be driven by the binding's `input$commit` (so it re-fires even when the same block is added twice).

#### Scenario: Server returns the spec without the nonce

- **WHEN** the binding sets `input$commit` to `list(type = "dataset_block", id = "foo", nonce = 1)` (other fields `NULL`)
- **THEN** `block_browser_server("browser")`'s returned reactive yields a list with `type = "dataset_block"`, `id = "foo"`, and no `nonce`

#### Scenario: Repeat adds re-fire the reactive

- **WHEN** the same block is added twice (two `input$commit` updates with different `nonce`)
- **THEN** the returned reactive fires twice

### Requirement: Suggested ids are unique against the board

`block_browser_ui()` SHALL generate the default `id` for each card with `blockr.core::rand_names()` seeded by the board's existing block ids, and the default `link_id` seeded by the board's existing link ids, so the suggested ids are unique among the cards and do not collide with ids already on the board. When `board` is `NULL`, ids are generated without a seed.

Because the caller re-renders the browser with the updated board after each add, repeated adds of the same block (e.g. with a pinned sidebar) SHALL keep proposing fresh, non-colliding ids.

#### Scenario: Default ids avoid existing board ids

- **WHEN** the board already contains blocks with ids `"aaa"` and `"bbb"` and `block_browser_ui(id, board)` is rendered
- **THEN** no card's default `id` value equals `"aaa"` or `"bbb"`
- **AND** the default ids are unique across cards

### Requirement: Category colour coding

Each card's icon tile SHALL be tinted according to its category using the Okabe-Ito palette (matching `blockr.dock`'s `blk_color()`), and the category section heading SHALL echo the same colour. The colour mapping SHALL be driven entirely by the `data-category` attribute in the bundled CSS (no inline styles); unknown categories fall back to the default neutral styling.

#### Scenario: Icon is tinted by category

- **WHEN** a card with `data-category="input"` is rendered
- **THEN** the bundled CSS tints its `.blockr-block-browser-card-icon` with the input-category colour (distinct from a `data-category="plot"` card's icon)

### Requirement: Drop-in replacement for `block_sidebar_body()` add / append / prepend modes

`blockr.dock`'s action handlers (`add_block_action`, `append_block_action`, `prepend_block_action`) SHALL mount the block browser as a module: `block_browser_server(<sub_id>)` once (returning the committed-block reactive), and on the trigger `show_sidebar(ui = blockr.ui::block_browser_ui(session$ns(<sub_id>), board$board, target))` where `target` is `NULL` (add), `append_to(trigger())`, or `prepend_to(trigger())`. The action handler SHALL `observeEvent()` the returned reactive to build the block + link(s), replacing the per-field observers (`input$<mode>_block_selection`, `input$<mode>_block_id`, `input$<mode>_block_name`, `input$<mode>_link_id`, `input$<mode>_block_input`, `input$<mode>_block_confirm`).

#### Scenario: Add fires a single observer in the action handler

- **WHEN** a user opens the sidebar via `add_block_action`'s trigger and clicks a block card
- **THEN** the reactive returned by `block_browser_server()` fires with the chosen block's spec, and the handler's single `observeEvent()` on it runs

#### Scenario: Per-field observers from the old design are gone

- **WHEN** the action handler is in the post-migration shape
- **THEN** the only browser-driven input is the block browser module's `input$commit`, consumed via the reactive returned by `block_browser_server()`

#### Scenario: Stack / link / edit flows are unaffected

- **WHEN** `add_stack_action`, `edit_stack_action`, or `add_link_action` runs
- **THEN** they continue to use `stack_sidebar_body` / `link_sidebar_body` (this change does NOT touch them)
