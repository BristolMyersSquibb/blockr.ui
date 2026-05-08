## Why

`blockr.dock` has accumulated layers of code that have nothing to do with docking: design tokens, base CSS, the block card with its sparkle UI, page-chrome elements (navbar, options accordion), modal builders, selectize widgets, button helpers, and Bootstrap offcanvases used as invisible DOM staging areas. Today every extension that wants the same look-and-feel (`blockr.ai`, `blockr.io`, `blockr.sdtm`, `blockr.dplyr`, `blockr.quarto`, `blockr.dag`) must depend on `blockr.dock` even when it never touches a dock; non-dock renderers and embedded apps have no clean place to land.

The plan is to lower `blockr.dock`'s codebase by extracting that cross-cutting UI surface into `blockr.ui` (which sits between `blockr.core` and the renderers in the dependency graph) — one piece at a time, each piece its own `openspec` change and its own pull request. A previous attempt tried to do everything at once and ran into too many moving parts, fragile namespacing, and an API that drifted too far from current `blockr.dock` usage. We're restarting in small, individually shippable increments.

This change is the first such increment. It ships a **sidebar primitive** in `blockr.ui` and migrates `blockr.dock`'s existing modal flows (`block_modal()`, `link_modal()`, `stack_modal()`, settings offcanvas) onto it. Modals are the most painful UX pattern in the current `blockr.dock` build cadence — dismiss-click-fill-confirm-dismiss interrupts every iteration — so they're the right starting point for the extraction. A persistent right-side panel (prototyped earlier on `blockr.dock@feat/sidebar-s3-dispatch`, deployed at <https://blockr.cloud/app/empty>) tested well with users. Replacing modals with a sidebar:

- removes a Bootstrap dependency from the mutation flows,
- gives `blockr.ui` its first concrete consumer, and
- proves out the "extract one piece at a time" pattern that subsequent changes will follow.

The API is intentionally tiny and modelled on Shiny's `showModal()` / `removeModal()`: two server helpers (`show_sidebar()` / `hide_sidebar()`) plus a UI builder and a dependency. The diff at every `blockr.dock` call site is approximately `showModal(modalDialog(body))` → `show_sidebar(id, body)` and `removeModal()` → `hide_sidebar(id)`. No S3 generic for content, no trigger types, no content registry, no nested `moduleServer`s, no session walking. The previous attempt's pain points are explicitly avoided.

## What Changes

- **NEW** `blockr.ui::sidebar_ui(id, ...)` — a Bootstrap-free, `bslib`-free slide-in panel container. Mounted once per app; the body is empty initially.
- **NEW** `blockr.ui::show_sidebar(id, ui, title = NULL)` — pre-renders `ui` via `htmltools::renderTags()`, ships HTML + dependencies + title to the client through `session$rootScope()$sendInputMessage(id, ...)`; the panel's `Shiny.InputBinding` receives the message, unbinds existing body bindings, installs new dependencies via `Shiny.renderDependencies()`, replaces children, runs `Shiny.initializeInputs()` and `Shiny.bindAll()` over the new body, and opens the panel. Mirrors `shiny::showModal()`.
- **NEW** `blockr.ui::hide_sidebar(id)` — unbinds the panel body and removes the open class via the same input-message path. Mirrors `shiny::removeModal()`.
- **NEW** `blockr.ui::sidebar_dep()` — `htmlDependency` carrying the panel's CSS and JS (a single `Shiny.InputBinding` whose `receiveMessage` handles show / hide, plus tiny class-toggling logic for the pin button, close button, Esc-to-close, outside-click-to-close, and focus trap). Attached automatically by `sidebar_ui()`.
- **NEW** Shiny input binding registered against `.blockr-sidebar` exposing `list(open, pinned)`. Server-side code can read `input[[id]]$open` and `input[[id]]$pinned` to react to user-driven open / close (Esc, X button, pin toggle) without round-tripping through R. Useful for example to coordinate an auto-open recipe: when an app first loads and the board is empty, the renderer can call `show_sidebar()` with an "Add your first block" hint UI, while still respecting a later user dismissal via `input[[id]]$open == FALSE`.
- **MODIFIED (in `blockr.dock`)** the action handlers `add_block_action()`, `append_block_action()`, `prepend_block_action()`, `add_link_action()`, `add_stack_action()`, `edit_stack_action()` switch from `showModal()` / `removeModal()` to `blockr.ui::show_sidebar()` / `blockr.ui::hide_sidebar()`. The existing modal-body builders (`block_modal`, `link_modal`, `stack_modal`) keep their bodies but lose the `modalDialog()` wrapper — their input ids and form structure are unchanged, so the action handlers' `observeEvent(input$add_block_confirm, ...)` etc. keep working as-is.
- **MODIFIED (in `blockr.dock`)** `board_ui.dock_board()` mounts a sidebar via `blockr.ui::sidebar_ui(NS(id, "main_sidebar"))` and the navbar's settings gear opens it with the existing options-accordion content (no more Bootstrap settings offcanvas).

## What's *not* in this change

The longer-term direction is to keep extracting cross-cutting UI from `blockr.dock` into `blockr.ui`, each as a separate `openspec` change / pull request. This change explicitly does **not** include any of the following — each is a candidate for a follow-up:

- Design tokens / base CSS extraction (`--blockr-*`, typography, button overrides). The sidebar ships its own minimal CSS for now.
- Block card S3 generic, edit-block plugin extraction, block-display helpers (`block_card()`, `edit_block_ui/server/validator`, `blks_metadata`, `blk_color`).
- Page-chrome generics (`app_shell()`, `blockr_navbar()`, `options_ui()`).
- Selectize widgets (`block_input_select()`, `block_registry_selectize()`, `board_select()`).
- Modal-only CSS helpers (`css_modal()`, `css_modal_advanced()`) — kept private in `blockr.dock` for the view-management modals (add view / rename view) that are NOT in scope here.
- Dock-pool replacement for the staging Bootstrap offcanvases (`blocks_offcanvas`, `exts_offcanvas`).
- Block-browser UX (cards by category, search, quick-add, detailed-add accordion). The current modal forms are reused 1:1 inside the sidebar.
- Sidebar content registry / S3 generic / trigger constructors. Callers compose `ui` directly with whatever shiny tags they want, exactly like `modalDialog()`.
- Demo app + chromote integration tests.

Each of those will land as its own change with its own proposal / design / tasks once this one ships and stabilises.

## Capabilities

### New Capabilities

- `sidebar`: The sidebar UI builder (`sidebar_ui()`), its dependency (`sidebar_dep()`), and the two server helpers (`show_sidebar()`, `hide_sidebar()`) plus the JS handler that processes them. That's it.

### Modified Capabilities

None. `blockr.ui`'s only existing artefact is the package skeleton.

## Phases as separate PRs

Implementation is sequenced as two independently shippable PRs:

1. **Phase 1 (PR 1):** Sidebar primitive in `blockr.ui` — `sidebar_ui()`, `sidebar_dep()`, `show_sidebar()`, `hide_sidebar()`, JS handler, CSS. `R CMD check` clean. `blockr.dock` untouched. A non-dock Shiny app can mount and drive the sidebar.
2. **Phase 2 (PR 2):** `blockr.dock` adopts the sidebar — six action handlers switched, settings gear switched, `board_ui.dock_board()` mounts the sidebar. The modal-body builders stay but lose the `modalDialog()` wrapper; the settings Bootstrap offcanvas is removed. `R CMD check` clean on both packages, all existing tests pass.

## Future change ideas (NOT in this change)

The longer-term direction is to lower `blockr.dock`'s codebase by lifting cross-cutting UI into `blockr.ui`. Each of these is a separate `openspec` change, sequenced when the prior one has shipped and stabilised:

- Extract design tokens + base CSS from `blockr.dock` into a `blockr.ui` foundation capability.
- Extract the block card (`block_card()` S3 generic + edit-block plugin + block-display helpers) into a `blockr.ui` block-card-ui capability.
- Extract page-chrome generics (`app_shell()`, `blockr_navbar()`, `options_ui()`) into a `blockr.ui` app-shell capability.
- Extract selectize widgets (`block_input_select()`, etc.) into the `blockr.ui` foundation.
- Replace the staging Bootstrap offcanvases (`blocks_offcanvas`, `exts_offcanvas`) with a plain hidden `<div>` block pool in `blockr.dock`.
- Block-browser UX (cards by category, search, quick-add) replacing the form-style add-block sidebar content.

Each of those is a self-contained PR pair (one in `blockr.ui`, one in `blockr.dock`) with the same shape as this change: ship the new home in `blockr.ui` first, then have `blockr.dock` adopt it.

## Impact

- **Affected packages.** `blockr.ui` gains the sidebar capability. `blockr.dock` adds `blockr.ui` to its `Imports` and switches its modal flows + settings offcanvas.
- **Affected dependencies.** New dep in `blockr.ui`: nothing beyond Phase 0 (`blockr.core`, `htmltools`, `rlang`, `shiny`). New dep in `blockr.dock`: `blockr.ui`.
- **Affected APIs.** None of `blockr.dock`'s public exports are removed by this change. Modal builders (`block_modal`, `link_modal`, `stack_modal`, `css_modal_advanced`) were internal. No `lifecycle::deprecate_warn()` shims needed.
- **Affected downstream.** Extensions (`blockr.ai`, `blockr.io`, `blockr.sdtm`, `blockr.dplyr`, `blockr.quarto`, `blockr.dag`, `blockr-demo-*`) continue to work unchanged. Triggers fired by extensions (e.g. `blockr.dag`'s right-click "append node") flow through `blockr.dock`'s action handlers, which now open the sidebar instead of a modal — no extension code change required.
- **Out of scope.** See "What's *not* in this change" above.
