## MODIFIED Requirements

### Requirement: `block_browser_ui()` builder

`blockr.ui` SHALL export `block_browser_ui(id, board = NULL, target = NULL)` returning an `htmltools::tag` with `block_browser_dep()` attached, where `id` is the module id. The flow is selected by `target` (see the target-descriptor requirement): `NULL` is *add*, `append_to(id)` is *append*, `prepend_to(id)` is *prepend*. The function MUST render one `.blockr-block-browser-card` per registered block (from `available_blocks()`) grouped under one `.blockr-block-browser-category` per distinct category, with blocks lacking a category under `"Uncategorized"`. The root element's `id` SHALL be `NS(id)("commit")` and carry `data-mode`; for *prepend* it SHALL also carry `data-target-arity`.

The static markup SHALL NOT bake a board-seeded default block or link id into the per-card form. A card's default id is resolved at commit time (server-side, against the live board) or generated client-side; it is not a function of board state baked into the server-rendered HTML. Consequently the rendered markup for the *add* flow (`target = NULL`) SHALL be independent of board state and MAY be rendered once and reused across opens.

The `board` argument is used only by the *prepend* flow (the target block's currently-free input ports, for the target-input picker and `data-target-arity`) and the *append* / *prepend* context subtitle. For the *add* flow `board` is unused and MAY be `NULL`.

#### Scenario: Add-flow markup is independent of board state

- **WHEN** `block_browser_ui(id)` is rendered against two different boards (or `board = NULL`) for the *add* flow
- **THEN** the produced card markup is identical (no board-derived default id appears in the static HTML)

#### Scenario: One card per registered block

- **WHEN** a caller invokes `block_browser_ui(id)` with at least one registered block
- **THEN** the rendered tag tree contains exactly `length(available_blocks())` elements with class `blockr-block-browser-card`
- **AND** each card carries `data-block-type`, `data-name`, `data-description`, `data-package`, `data-category`

### Requirement: `block_browser_server()` returns the committed block

`blockr.ui` SHALL export `block_browser_server(id, board = NULL, target = NULL)`, a Shiny module server matching `block_browser_ui()`. `board` MAY be a `reactive` returning the current board (used to validate ids, build the block, and resolve the link port) or `NULL`; `target` selects the flow and MAY be a reactive. It SHALL return a `reactive` that fires once per add with a ready-to-apply value, with the internal `nonce` stripped and driven by the binding's `input$commit` (so it re-fires on repeat adds):

- *add* (`target` is `NULL`): a `blockr.core` `blocks` object holding one id-keyed block, created from the committed `type` and named from the committed `title`.
- *append* / *prepend*: a list `list(blocks = <blocks>, links = <links>)`, where `links` is one id-keyed `blockr.core` link with its input port already resolved. Resolution uses the explicit picked port when present, else the first free named slot, else a fresh integer slot for a variadic target, computed from the live board with `blockr.core` primitives only (the same resolution `link_menu_server()` reproduces).

When a `board` reactive is supplied the server SHALL validate the committed block id (and, for *append* / *prepend*, the link id): on an empty or duplicate id it SHALL notify the user and NOT fire the returned reactive, mirroring `link_menu_server()` / `stack_menu_server()`.

#### Scenario: Add returns a ready blocks object

- **WHEN** the binding commits `list(type = "dataset_block", id = "foo", title = "", nonce = 1)` with `target = NULL` and a `board` reactive
- **THEN** the returned reactive yields a `blockr.core` `blocks` object named `"foo"` whose single element is a `dataset_block`

#### Scenario: Append returns a ready block and link with resolved port

- **WHEN** an *append* commit fires with no explicit `block_input` against a single-input target
- **THEN** the returned value is `list(blocks, links)`, `links` is one id-keyed link from the source to the new block, and its `input` is the block's resolved free port

#### Scenario: Duplicate id is rejected

- **WHEN** a `board` reactive is supplied and the committed id already exists on the board
- **THEN** the server notifies and the returned reactive does not fire

### Requirement: Suggested ids are unique against the board

The block browser SHALL offer a unique default block id (and, for *append* / *prepend*, a unique default link id) that does not collide with the board's existing block / link ids. Because the default is no longer baked into the server-rendered markup (see the `block_browser_ui()` requirement), uniqueness SHALL be guaranteed at commit time against the live board: when the committed id is empty or collides, the server resolves a fresh unique id (e.g. via `blockr.core::rand_names()` seeded with the current board ids) before building the returned object. Repeated adds against a pinned panel therefore yield distinct ids without re-rendering the panel.

#### Scenario: Repeated adds yield distinct ids without re-render

- **WHEN** the same card is committed twice against a pinned (not re-rendered) panel
- **THEN** the two returned `blocks` objects carry distinct, board-unique ids

## ADDED Requirements

### Requirement: Add browser is mountable once and opened without re-render

Because the *add* flow markup is board-independent, a consumer SHALL be able to pre-render the add block browser once via `sidebar_ui(id, ui = block_browser_ui(ns))` and open it with `show_sidebar(id)` (no `ui`) or the `data-blockr-sidebar-target` client trigger, with no per-open `renderTags()` cost. `blockr.dock` SHALL adopt this for the add action (a dedicated sidebar mount in `board-ui.R`), keeping the *append* / *prepend* / link / stack flows on the dynamic `show_sidebar(id, ui = ...)` path since their content is trigger-specific.

#### Scenario: Re-opening the add browser does not rebuild it

- **WHEN** the pre-rendered add browser sidebar is opened, closed, and opened again with no board change
- **THEN** the card list is not re-rendered between opens (the same pre-rendered DOM is shown)
