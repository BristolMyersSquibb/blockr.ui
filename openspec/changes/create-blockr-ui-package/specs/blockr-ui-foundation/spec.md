## ADDED Requirements

### Requirement: Package boundary

The `blockr.ui` package SHALL stand as an independent R package with its own `DESCRIPTION`, `NAMESPACE`, `pkgdown` site, `NEWS.md`, and semantic version. Its `Imports` MUST be limited to `blockr.core`, `shiny`, `htmltools`, and `rlang`. It MUST NOT depend on `dockViewR`, on `bslib`-only Bootstrap helpers (e.g. `bslib::offcanvas`), or on any code in `blockr.dock`.

#### Scenario: Allowed runtime dependencies

- **WHEN** a maintainer reads `blockr.ui`'s `DESCRIPTION` `Imports`
- **THEN** every listed package is one of `blockr.core`, `shiny`, `htmltools`, `rlang` (plus their indirect deps)
- **AND** `dockViewR` does not appear anywhere in `Imports`, `Suggests`, or `LinkingTo`
- **AND** `bslib` does not appear in `Imports`

#### Scenario: No upward dependency on blockr.dock

- **WHEN** a maintainer greps `blockr.ui/R/` and `blockr.ui/inst/` for `blockr.dock`
- **THEN** no source file references `blockr.dock::` or `library(blockr.dock)` or `requireNamespace("blockr.dock")`

### Requirement: Design tokens and base CSS

`blockr.ui` SHALL ship the cross-cutting design tokens (`--blockr-*` palettes for grey and blue, shadows, font sizes, font weights, semantic colour aliases) and the base CSS classes used across blockr-flavoured apps. The base classes MUST include typography (`.blockr-title`, `.blockr-subtitle`, `.blockr-section-header`, `.blockr-label`, `.blockr-helper`, `.blockr-meta`), button overrides (`.btn-default`, `.btn-light`, `.btn-outline-default`, `.btn-primary`, `.btn-secondary`, `.btn-link`), form overrides (`.form-control`, `.form-select`, `.form-label`, `.shiny-input-container`), error styling (`.blockr-error`, `.blockr-error-icon`), the issues toggle (`.blockr-issues-toggle`), the inline-edit input (`.blockr-inline-edit`, `.blockr-title-edit`), and popover / tooltip overrides. These styles MUST load via a single `htmlDependency` returned by `blockr_ui_dep()`.

#### Scenario: Tokens available in any consuming app

- **WHEN** an app attaches `blockr_ui_dep()` to its UI
- **THEN** `getComputedStyle(document.documentElement).getPropertyValue('--blockr-grey-500')` is a non-empty colour value
- **AND** `getComputedStyle(document.documentElement).getPropertyValue('--blockr-shadow-sm')` is a non-empty shadow value

#### Scenario: Base classes available

- **WHEN** an app attaches `blockr_ui_dep()` and renders `<h2 class="blockr-title">Hello</h2>`
- **THEN** the rendered element receives the `.blockr-title` typography rules from `blockr-base.css`

#### Scenario: Single dependency entry point

- **WHEN** a consumer calls `blockr_ui_dep()`
- **THEN** it returns one `htmlDependency` whose `stylesheet` field references `blockr-tokens.css`, `blockr-base.css`, `blockr-shell.css`, and `blockr-block.css`

### Requirement: Generic UI helpers

`blockr.ui` SHALL export the following cross-cutting UI helpers, each unchanged from their pre-migration `blockr.dock` semantics:

- selectize widgets: `block_input_select(block, block_id, links, mode = c("create", "update", "inputs"), ...)`, `block_registry_selectize(id, blocks = list_blocks())`, `board_select(id, blocks, selected = NULL, ...)`
- selectize JS rendering: `js_blk_selectize_render()`
- button helpers: `toggle_button(opt_id, tog_id)`, `confirm_button(...)`
- focus helper: `auto_focus_script(id)`

`off_canvas()` and `move_dom_element()` (with the `move-element` JS handler) are NOT exported from `blockr.ui` at v0; they remain in `blockr.dock` since they have no v0 consumer in `blockr.ui`.

#### Scenario: Selectize widgets render

- **WHEN** a developer calls `block_registry_selectize("foo")` inside a Shiny UI
- **THEN** the returned tag is a `selectizeInput` whose options are derived from the block registry
- **AND** the rendered `<select>` element uses the JS rendering function `js_blk_selectize_render()`

### Requirement: htmlDependency builders

`blockr.ui` SHALL expose two dependency builders: `blockr_ui_dep()` (tokens + base + shell + block CSS) and `sidebar_dep()` (sidebar CSS + JS, see the `sidebar` capability spec). Fine-grained per-surface dep builders are NOT introduced at v0 — consumers who need only a subset attach `blockr_ui_dep()` and accept the bundled CSS.

#### Scenario: Dependencies resolve in correct load order

- **WHEN** a consumer attaches both `blockr.ui::blockr_ui_dep()` and `blockr.dock`'s own dependency that lists `blockr.ui` as a transitive dep
- **THEN** `htmltools::resolveDependencies()` orders `blockr.ui`'s dependency before `blockr.dock`'s in the rendered `<head>`

### Requirement: Runnable demo app

`blockr.ui` SHALL ship at least one runnable Shiny demo app under `inst/examples/blockr-ui-demo/app.R` that exercises the default `block_ui.board`, `app_shell()`, `blockr_navbar()`, `options_ui()`, sidebar primitive, and block browser end-to-end **against a plain `blockr.core::new_board()`** (NOT a `dock_board`). The app MUST be launchable both via `shiny::runApp(system.file("examples", "blockr-ui-demo", package = "blockr.ui"))` and via an exported helper `run_demo(name = "blockr-ui-demo")`. The app serves the package's smoke-test surface (it must run cleanly with `library(blockr.ui)` only — no `library(blockr.dock)`) and is the basis for chromote-driven integration tests.

#### Scenario: Demo runs without blockr.dock attached

- **WHEN** a developer runs `blockr.ui::run_demo()` in a fresh R session that has not loaded `blockr.dock`
- **THEN** the app starts and listens on a port without errors
- **AND** the rendered page contains the navbar, the sidebar (closed by default), and at least one rendered `block_card` from the default `block_ui.board` method
- **AND** no DockView (`dv-*`) class appears anywhere in the rendered DOM

#### Scenario: Demo exercises the sidebar end-to-end

- **WHEN** the demo is running and the user clicks the navbar's "+" button
- **THEN** the sidebar opens (`input[["main_sidebar"]]$open` becomes `TRUE`) and `input[["main_sidebar"]]$content` becomes `"add_block"`
- **WHEN** the user clicks a quick-add button on any block card in the browser
- **THEN** a new block card appears in the canvas and the board's block list grows by one

### Requirement: Backwards-compatible re-exports in blockr.dock

When a previously-`@export`ed symbol moves from `blockr.dock` to `blockr.ui`, `blockr.dock` SHALL keep a thin shim that calls the `blockr.ui` implementation and emits `lifecycle::deprecate_warn()` on first call. The shim MUST live for at least one deprecation cycle. Symbols affected at minimum: `block_card`, `block_input_select`, `block_registry_selectize`, `board_select`. Symbols that do not move (`off_canvas`, `move_dom_element`) need no shim. Symbols that disappear entirely (`block_modal`, `link_modal`, `stack_modal`, `css_modal`, `css_modal_advanced`) are NOT re-exported.

#### Scenario: Calling a moved export from blockr.dock

- **WHEN** an app calls `blockr.dock::block_card(...)` after the migration
- **THEN** the call returns the same value as `blockr.ui::block_card(...)`
- **AND** a `lifecycle::deprecate_warn()` warning is emitted on the first call within the session, naming the new home `blockr.ui::block_card()`

#### Scenario: Deleted modal builder is not re-exported

- **WHEN** an app calls `blockr.dock::block_modal(...)` after Phase 4
- **THEN** the call errors with `Error: object 'block_modal' not found` (or equivalent) rather than emitting a deprecation warning
