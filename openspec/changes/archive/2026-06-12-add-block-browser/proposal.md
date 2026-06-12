## Why

The `add-sidebar-primitive` change lifted the modal body for add / append / prepend block actions into the new sidebar unchanged: a vertical stack of `block_registry_selectize()` + `textInput("name")` + a collapsible advanced section with the block ID and link fields.

`block_registry_selectize()` already renders rich rows - icon, name, description, category, package badge - via a custom selectize render. That worked well *in a modal*, where vertical space is at a premium and a closed dropdown is the only honest way to fit "pick one of N" alongside the other form fields. The sidebar relaxes that constraint: it has the full viewport height to play with, so the same metadata can be shown for many blocks at once instead of being hidden behind one open-the-dropdown click.

The friction is shape, not data:

- A closed selectize hides every option until the user opens it; with ~30 blocks the open dropdown then scrolls.
- Categories exist in the data but the dropdown ordering doesn't surface them as headings - users can't skim "all the input blocks" at a glance.
- The block ID and link fields sit at the same visual level as the choice of block, so the panel reads as a form to fill, not a chooser to browse.

This change replaces `block_sidebar_body()` for the add / append / prepend flows with a **block browser**: a card list grouped by category, a search bar that filters in place, and a per-card expand for the id / title / link / port options. The data shown per card is the same metadata `block_registry_selectize()` already exposes; only the layout changes.

The browser is deliberately **single-shot**: clicking a card adds that one block immediately with sensible defaults, and opening a card lets the user tweak the id / title / link first and then add. Repeated adds of the same block are achieved by clicking the card again (each suggested id is generated to be unique against the board), not by a multi-select. An earlier iteration explored multi-select with parallel/sequential connection modes and a reorderable chip tray; that proved to make the component do too much for the value, and is parked for a future revisit (see Deferred work in `design.md`).

Stack / link / edit-stack flows are out of scope: their UX questions are different and need their own design pass.

## What Changes

- **NEW** The block browser is a **Shiny module**: `blockr.ui::block_browser_ui(id, board, target = NULL)` renders the card-list-with-search body (replacing `block_sidebar_body()` for these three flows) and `blockr.ui::block_browser_server(id)` returns a `reactive` firing once per add with the block to create. Clicking a card adds it; opening a card (chevron) reveals the id / title / link / port form plus an in-card add button. The flow is chosen by `target` - `NULL` (add), `blockr.ui::append_to(id)`, or `blockr.ui::prepend_to(id)` - which collapses the old `mode` + `trigger_id` pair into one well-typed argument so illegal combinations (append with no source, add with a trigger) are unrepresentable. (`ns` is no longer a parameter - the old fragment was a half-module; it now follows the standard `ui(id)` / `server(id)` pattern.)
- **NEW** `blockr.ui::append_to(block_id)` / `blockr.ui::prepend_to(block_id)` - small `bb_target` constructors used for the `target` argument.
- **NEW** `blockr.ui::block_browser_dep()` - `htmlDependency` carrying the bundled CSS and JS for the card list, search filter, per-card expand, and single-block commit.
- **NEW** A `Shiny.InputBinding` (`blockr.ui.blockBrowser`) backs the value channel - no ad-hoc `Shiny.setInputValue`. The root element's `id` is `NS(id)("commit")`; the binding's value is the block spec `list(type, id, title, link_id, block_input, target_input, nonce)` (mode-inapplicable fields `NULL`; `nonce` an internal counter so repeat adds re-fire). `block_browser_server()` reads it as `input$commit`, strips the nonce, and returns the clean spec. The binding reserves `receiveMessage` for the R→JS card filter the AI-search follow-up will use.
- **NEW** Unique-id seeding: the suggested `id` and `link_id` defaults are generated with `blockr.core::rand_names()` seeded by the board's existing block / link ids, so repeated adds (with the sidebar pinned and the board re-rendered between adds) keep proposing fresh ids.
- **NEW** Per-category icon tinting (Okabe-Ito palette) and category-header colour, all CSS-only via `data-category`.
- **MODIFIED (in `blockr.dock`)** the add / append / prepend action handlers mount the browser module (`block_browser_server()` + `block_browser_ui()` via `show_sidebar`) and `observeEvent()` the returned reactive instead of the per-field input stream.

## What's *not* in this change

- Multi-select, the parallel/sequential connection toggle, order badges, the arity-aware cap, `target_input` slot arbitration across instances, and the reorderable chip tray. These were explored and parked (see Deferred work in `design.md`).
- A per-card quantity stepper (e.g. "3 dataset blocks at once"). Repeated single clicks cover the common case; a stepper is parked with the multi-select exploration.
- AI-assisted block search. The current search is a case-insensitive substring match. Replacing the search bar with a `{shinychat}`-based assistant that maps a natural-language question to candidate blocks via registry-querying tools is the intended **next** piece of work - designed-for but out of scope here (see Deferred work).
- Stack and link flows (`add_stack_action`, `edit_stack_action`, `add_link_action`).
- Block metadata schema changes in `blockr.core`.
- A move of `blks_metadata()` from `blockr.dock` to `blockr.ui`.

## Capabilities

### New Capabilities

- `block-browser`: the single-select card-list block picker (`block_browser_ui()`, `block_browser_dep()`) and the JS that wires search / category sections / per-card expand / click-to-add / in-card add button / single-block commit.

### Modified Capabilities

None in `blockr.ui`. In `blockr.dock`, three existing action handlers are modified to mount the browser module and observe its committed-block reactive.

## Phases as separate PRs

Two PRs because the work spans two repos.

1. **PR 1 (`blockr.ui`):** `block_browser_ui()` + `block_browser_dep()` with bundled CSS / JS, click-to-add + in-card add button, unique-id seeding, per-card expand, unit tests, shinytest2 test, vignette, and a runnable example.
2. **PR 2 (`blockr.dock`):** action-handler adoption. Mount the browser module, observe its committed-block reactive, update tests.

## Impact

- **Affected packages.** `blockr.ui` gains the block-browser capability. `blockr.dock` keeps its `Imports: blockr.ui` and rewrites three action handlers.
- **Affected dependencies.** None beyond what the sidebar primitive already pulls in.
- **Affected APIs.** `block_sidebar_body()` keeps `mode = "edit"`. For add / append / prepend the action handlers call `blockr.ui::block_browser_ui()`; the legacy form-shaped body for those modes is removed (nothing in-tree calls it). The per-field inputs (`<mode>_block_selection`, etc.) are removed in favour of the block browser module (`block_browser_server()` returns the committed block). Out-of-tree consumers migrate via NEWS.
- **Affected downstream.** Extensions get the new browser for free.
