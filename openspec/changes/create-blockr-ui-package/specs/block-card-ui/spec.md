## ADDED Requirements

### Requirement: block_card S3 generic

`blockr.ui` SHALL export `block_card(blk, blk_id, ...)` as an S3 generic dispatching on the block class, plus a `block_card.default(blk, blk_id, plugin, board, board_ns, ctrl = NULL, ...)` method that renders the chrome currently produced by `blockr.dock`'s sparkle UI: header (title, info popover, dropdown menu, toggles), body (block-specific UI, expression UI, ctrl panel slot), and footer (validation summary, issues toggle).

#### Scenario: Default method dispatches and renders

- **WHEN** a caller invokes `block_card(blk, "blk_1", plugin, board, board_ns)` where `blk` has no class-specific method
- **THEN** S3 dispatch resolves to `block_card.default()`
- **AND** the returned tag is a `<div>` carrying the `.blockr-card` (or equivalent) class
- **AND** the tag tree contains a card title, dropdown, body, and footer in that order

#### Scenario: Subclass override is respected

- **WHEN** a downstream package registers `block_card.data_block()` and the caller passes a `data_block` instance
- **THEN** S3 dispatch resolves to `block_card.data_block()` not the default
- **AND** the call is performed without modifying `blockr.ui`

### Requirement: Block card subcomponents

`blockr.ui` SHALL export the helpers that compose a block card so consumers and downstream methods can reuse them without duplicating markup: `block_card_title(block, id, info)`, `block_card_toggles(blk, ns, ctrl_meta = NULL)`, `block_card_dropdown(ns, info, blk_id)`, `block_card_content(ns, expr_ui, block_ui, ctrl_ui = NULL)`, `build_ctrl_panel(...)`, and the `ctrl_button` attribute system.

#### Scenario: Card title renders the block name and info popover

- **WHEN** a developer calls `block_card_title(block, "blk_1", info = "Some help text")`
- **THEN** the returned tag contains the block name as visible text
- **AND** contains an info icon whose popover content is `"Some help text"`

### Requirement: Edit-block module

`blockr.ui` SHALL export the edit-block server module: `edit_block_ui(id, blk, blk_id, expr_ui, block_ui, ...)`, `edit_block_server(callbacks = list())`, `edit_block_validator(x)`, `update_blk_cond_observer(...)`, and `create_issues_ui(...)`. The module's reactive contract MUST be unchanged from its pre-migration state in `blockr.dock`.

#### Scenario: edit_block_server returns a list of reactives

- **WHEN** a developer wraps a block with `edit_block_server()` inside a `moduleServer()`
- **THEN** the returned object is a list whose names include the same reactive names exposed by the pre-migration `blockr.dock` implementation (covered by ports of the existing `blockr.dock` tests)

#### Scenario: edit_block_validator surfaces issues

- **WHEN** an invalid block expression is passed to `edit_block_validator(x)`
- **THEN** the returned object exposes the issues via the same shape consumed by `create_issues_ui()`

### Requirement: Block-display helpers

`blockr.ui` SHALL export the block-display helpers used to build cards and selectize options: `registry_metadata(blocks = list_blocks())`, `blks_metadata(blocks)`, `blk_color(category)`. Helpers that are properly block-data accessors (`block_inputs()`, `block_arity()`, `block_name()`, …) SHALL stay in `blockr.core`; `blockr.ui` only owns helpers that build display strings, icons, or colours.

#### Scenario: blk_color is total over registered categories

- **WHEN** `blk_color(c)` is called with any category present in `registry_metadata()$category`
- **THEN** the returned value is a non-NA character of length 1 representing a CSS colour

#### Scenario: registry_metadata reads from the registry

- **WHEN** a developer registers a new block via `blockr.core::register_block(...)` and then calls `registry_metadata()`
- **THEN** the returned data frame includes a row for the new block

### Requirement: Default block_ui.board method

`blockr.ui` SHALL provide the default `block_ui.board(id, x, edit_ui, blocks = NULL, ..., plugins = board_plugins(x), session = shiny::getDefaultReactiveDomain())` method so non-dock apps can render blocks on a plain `board` without DockView. The method MUST iterate `blocks` and call `block_card()` per block; the resulting tag list MUST be returnable directly into a Shiny UI without any dock-specific chrome.

#### Scenario: Plain board renders cards without DockView

- **WHEN** an app constructs a plain `blockr.core::new_board(blocks)` and calls `block_ui(id, board, edit_ui, blocks)`
- **THEN** the returned tag tree contains one `block_card()` per block
- **AND** does not contain any DockView-specific class (e.g. `dv-*`)

#### Scenario: dock_board composes the default via NextMethod

- **WHEN** `blockr.dock::block_ui.dock_board()` calls `NextMethod()`
- **THEN** the underlying default `block_ui.board()` is invoked first
- **AND** the dock method then wraps each card with dock-panel chrome
