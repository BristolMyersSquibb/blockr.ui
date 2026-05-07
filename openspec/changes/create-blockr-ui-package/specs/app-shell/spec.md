## ADDED Requirements

### Requirement: app_shell S3 generic

`blockr.ui` SHALL export `app_shell(id, x, ...)` as an S3 generic dispatching on the board class `x`, plus an `app_shell.default(id, x, ..., navbar = NULL, sidebar = NULL, content = NULL, deps = NULL)` method that renders a full-viewport flex-column layout with four ordered slots: `deps` (any `htmlDependency` objects to attach), `navbar` (top), `sidebar` (slide-in panel), `content` (the main canvas). The default method MUST set up the CSS variable `--blockr-sidebar-width` on the body so pinned-mode reflow works.

#### Scenario: Default method renders four slots

- **WHEN** a caller invokes `app_shell("app", board, navbar = nav, sidebar = sb, content = main, deps = list(dep))`
- **THEN** the returned tag contains, in DOM order: dependency tags, the navbar slot, the sidebar slot, the content slot
- **AND** the root element has a class that triggers the full-viewport layout from `blockr-shell.css`

#### Scenario: Subclass override is respected

- **WHEN** a downstream package registers `app_shell.canvas_board()` and a caller passes a `canvas_board`
- **THEN** S3 dispatch resolves to `app_shell.canvas_board()` not the default

### Requirement: blockr_navbar S3 generic

`blockr.ui` SHALL export `blockr_navbar(id, x, ...)` as an S3 generic dispatching on the board class, plus a `blockr_navbar.default(id, x, ..., left = NULL, right = NULL, settings_id = NULL)` method that renders a `<nav class="blockr-navbar">` with `.blockr-navbar-left` and `.blockr-navbar-right` slots. When `settings_id` is non-NULL, the default method MUST also render a settings gear button inside `.blockr-navbar-right` with `data-blockr-action="settings"` and the `.blockr-navbar-icon-btn` class. The button MUST NOT carry any Bootstrap `data-bs-*` attribute; its click handler is wired in JS to call `sidebar_set_content(settings_id, settings_trigger())`.

#### Scenario: Settings gear is rendered when settings_id is set

- **WHEN** a caller invokes `blockr_navbar("app", board, left = NULL, right = views_tag, settings_id = "main_sidebar")`
- **THEN** the returned tag contains a button with `data-blockr-action="settings"` inside the `.blockr-navbar-right` slot
- **AND** the button has no `data-bs-toggle` attribute

#### Scenario: Settings gear is omitted when settings_id is NULL

- **WHEN** a caller invokes `blockr_navbar("app", board)` with no `settings_id`
- **THEN** the returned tag contains no button with `data-blockr-action="settings"`

#### Scenario: Left and right slots are rendered in order

- **WHEN** a caller passes both `left` and `right` slots
- **THEN** the rendered DOM places `.blockr-navbar-left` before `.blockr-navbar-right`

### Requirement: options_ui S3 generic

`blockr.ui` SHALL export `options_ui(id, x, ...)` as an S3 generic dispatching on the board class, plus an `options_ui.default(id, x, options, ...)` method that renders the board's option categories as a Bootstrap accordion (one panel per category), using `blockr.core::board_option_ui()` per option. The default method MUST return a plain `tagList` (or accordion tag tree) — it MUST NOT wrap the accordion in any panel chrome (offcanvas, modal, card), because it is rendered into the sidebar body.

#### Scenario: Returns an accordion, not an offcanvas

- **WHEN** a caller invokes `options_ui("app", board, options)` with two option categories
- **THEN** the returned tag tree contains an accordion with two panels (one per category)
- **AND** does not contain any `.offcanvas` element

#### Scenario: Option categories render via blockr.core

- **WHEN** the default method renders an option category
- **THEN** it dispatches to `blockr.core::board_option_ui()` for the per-option markup unchanged

### Requirement: dock_board uses the default page-chrome generics

The post-migration `blockr.dock::board_ui.dock_board()` SHALL be a thin assembly that calls `blockr.ui::app_shell()`, `blockr.ui::blockr_navbar()`, `blockr.ui::sidebar_ui()` and only injects dock-specific content (`view_nav_ui()`, `dock_outputs_ui()`, `dock_pool_ui()`). It MUST NOT register `app_shell.dock_board()` / `blockr_navbar.dock_board()` / `options_ui.dock_board()` methods at v0; the default methods SHALL be sufficient.

#### Scenario: No dock-specific shell methods at v0

- **WHEN** a maintainer runs `methods("app_shell")` after `blockr.dock` is loaded
- **THEN** the result lists only `app_shell.default` (and any methods registered by other downstream packages, but not `app_shell.dock_board`)

#### Scenario: board_ui.dock_board composes the shell

- **WHEN** `board_ui.dock_board()` is called for a `dock_board`
- **THEN** the returned tag tree includes `app_shell()`'s root, the navbar (with view nav in the right slot), the sidebar, and within `content` both `dock_outputs_ui()` and `dock_pool_ui()`

### Requirement: Hidden block / extension pool replaces the staging offcanvases

`blockr.dock` SHALL provide `dock_pool_ui(id)` that returns a single hidden `<div id="<id>-block_pool" class="blockr-dock-pool" hidden aria-hidden="true">`. After migration, `hide_block_ui()` and `hide_ext_ui()` MUST move DOM nodes into this pool element instead of into the offcanvas body. The two prior staging Bootstrap offcanvases (`blocks_offcanvas`, `exts_offcanvas`) MUST NOT be present in the rendered page.

#### Scenario: Pool element is in the DOM and hidden

- **WHEN** `board_ui.dock_board()` renders a board
- **THEN** the rendered DOM contains exactly one element matching `#<id>-block_pool.blockr-dock-pool`
- **AND** that element has `aria-hidden="true"` and is not visible to users

#### Scenario: Staging offcanvases are gone

- **WHEN** `board_ui.dock_board()` renders a board after Phase 1
- **THEN** the rendered DOM contains no element with id ending in `blocks_offcanvas` or `exts_offcanvas`
