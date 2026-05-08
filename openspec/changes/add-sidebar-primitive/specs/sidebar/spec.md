## ADDED Requirements

### Requirement: sidebar_ui builder

`blockr.ui` SHALL export `sidebar_ui(id, side = c("right", "left"), width = "360px")` returning a `<div class="blockr-sidebar">` with header (title slot, pin button, close button) and body slots, plus `data-side` and `aria-hidden` attributes. The element MUST NOT use any Bootstrap `.offcanvas` or `.modal` classes or `data-bs-*` attributes. The function SHALL attach `sidebar_dep()` via `htmltools::attachDependencies()` so callers do not need to attach the dependency separately.

#### Scenario: Markup is Bootstrap-free

- **WHEN** a caller invokes `sidebar_ui("main_sidebar")`
- **THEN** the returned tag tree contains no element with class containing `offcanvas` or `modal`
- **AND** no element carries any attribute starting with `data-bs-`

#### Scenario: Initial state attributes

- **WHEN** a caller invokes `sidebar_ui("main_sidebar", side = "left", width = "400px")`
- **THEN** the root element has `id="main_sidebar"`, `data-side="left"`, `style` containing `width: 400px`
- **AND** initial `aria-hidden` is `"true"` (closed by default)

#### Scenario: Dependency attached automatically

- **WHEN** the result of `sidebar_ui("main_sidebar")` is passed through `htmltools::findDependencies()`
- **THEN** the resolved dependencies include `blockr-sidebar` (the dep produced by `sidebar_dep()`)

### Requirement: Server-side show / hide helpers

`blockr.ui` SHALL export `show_sidebar(id, ui, title = NULL, session = shiny::getDefaultReactiveDomain())` and `hide_sidebar(id, session = shiny::getDefaultReactiveDomain())`. These mirror Shiny's `showModal()` / `removeModal()` in spirit.

Both helpers MUST dispatch through the calling session's root scope (`session$rootScope()$sendInputMessage(id, ...)`) so the panel id is treated as an absolute DOM id and is NOT prefixed by the calling module's namespace. The id passed at call time MUST match the id that was passed to `sidebar_ui()` at mount time.

`show_sidebar()` MUST pre-render `ui` via `htmltools::renderTags()` and emit a single input message with `list(action = "show", html = rendered$html, dependencies = <resolved deps>, title = title)`. `hide_sidebar()` MUST emit `list(action = "hide")`. There is no custom-message handler: the input message is delivered to the panel's `Shiny.InputBinding` via its `receiveMessage(el, data)` method.

#### Scenario: show_sidebar ships HTML and resolved dependencies

- **WHEN** server code calls `show_sidebar("main_sidebar", ui = some_tag_tree, title = "Add block")`
- **THEN** the root session emits one `sendInputMessage` call with `inputId = "main_sidebar"`
- **AND** the message payload's `action` is `"show"`, `html` is character (the rendered HTML), `dependencies` is a (possibly empty) list of resolved web dependencies
- **AND** `title` in the payload is `"Add block"`

#### Scenario: hide_sidebar is a single-purpose hide message

- **WHEN** server code calls `hide_sidebar("main_sidebar")`
- **THEN** the root session emits one `sendInputMessage` call with `inputId = "main_sidebar"` and payload `list(action = "hide")`

#### Scenario: Calling session is the modal-equivalent default

- **WHEN** `show_sidebar()` is called from inside a `moduleServer` body without an explicit `session` argument
- **THEN** the message is dispatched via `getDefaultReactiveDomain()` (the calling module's session) — exactly as `shiny::showModal()` does
- **AND** the call walks to `session$rootScope()` before invoking `sendInputMessage`, so the panel id is the absolute DOM id chosen at `sidebar_ui()` time and is NOT prepended with the calling module's namespace

### Requirement: Client-side handling via the InputBinding

The panel's `Shiny.InputBinding` (registered against `.blockr-sidebar`) MUST implement `receiveMessage(el, data)` that switches on `data.action`. There SHALL NOT be a separate `Shiny.addCustomMessageHandler(...)` registration: the same binding owns both directions of the R↔JS conversation.

On `data.action == "show"`, `receiveMessage` MUST execute the following sequence in order on the panel's `.blockr-sidebar-body` element:

1. `Shiny.unbindAll(body)` — tear down stale Shiny bindings.
2. `Shiny.renderDependencies(data.dependencies)` if any — install new CSS / JS deps before rendering.
3. `body.replaceChildren(...$.parseHTML(data.html))` — swap the children.
4. `Shiny.initializeInputs(body)` — initialise input default values.
5. `Shiny.bindAll(body)` — wire up new bindings.
6. Set the panel's title slot to `data.title` (or empty if `null`); add the `.blockr-sidebar-open` class; set `aria-hidden="false"`; move keyboard focus to the first focusable element inside the body, remembering the previously-focused element so close can restore it.

On `data.action == "hide"`, `receiveMessage` MUST `Shiny.unbindAll(body)`, remove the `.blockr-sidebar-open` class, set `aria-hidden="true"`, and restore focus to the remembered previously-focused element.

#### Scenario: set-content unbinds before replacing

- **WHEN** `receiveMessage` fires with `data.action == "show"` and new HTML
- **THEN** `Shiny.unbindAll(body)` is invoked before `body.replaceChildren(...)`
- **AND** `Shiny.initializeInputs(body)` and `Shiny.bindAll(body)` are invoked after `replaceChildren`

#### Scenario: set-content installs new dependencies

- **WHEN** the message payload's `dependencies` array is non-empty
- **THEN** `Shiny.renderDependencies(dependencies)` is called between `unbindAll` and `replaceChildren` (or strictly before `bindAll`)

#### Scenario: hide unbinds the body

- **WHEN** `receiveMessage` fires with `data.action == "hide"`
- **THEN** `Shiny.unbindAll(body)` is invoked before the open class is removed
- **AND** the panel's `aria-hidden` is set to `"true"`

#### Scenario: No standalone custom-message handler is registered

- **WHEN** the bundled JS loads in a Shiny page
- **THEN** it does NOT register a `Shiny.addCustomMessageHandler` for sidebar control; all R-driven state changes flow through the InputBinding's `receiveMessage`

### Requirement: `mode = "overlay"` vs `mode = "push"`

`sidebar_ui()` SHALL accept a `mode` argument with values `"overlay"` (default) or `"push"`. The mode is set on the panel as `data-mode="<mode>"` and is independent of pin state (pin only affects dismissal).

In `"overlay"` mode, the panel slides over page content; the page layout is untouched. In `"push"` mode, the bundled JS sets `--blockr-sidebar-width` on `<html>` to the panel's pixel width and adds a `.blockr-html-pushed-<side>` class while the panel is open; the bundled CSS uses these to apply `padding-<side>: var(--blockr-sidebar-width)` on `<html>` (with `box-sizing: border-box`), so page content shifts aside instead of being covered. The class + variable live on `<html>` rather than `<body>` because bslib's page-fill layouts pin body to 100% of html and zero its padding inline — body-level padding/margin no longer constrains the visible viewport. When the panel closes, the class and width are removed.

The pin state lives entirely in the DOM (a class on the panel element). It is not exposed as a Shiny input value beyond what the binding's `getValue()` returns. Pin no longer drives body reflow — that is `mode`'s job.

#### Scenario: Push mode shifts page content aside while open

- **WHEN** a panel with `mode = "push"` and `side = "right"` opens
- **THEN** `document.documentElement` (the `<html>` element) has the class `blockr-html-pushed-right`
- **AND** `document.documentElement.style.getPropertyValue('--blockr-sidebar-width')` is the panel's pixel width

- **WHEN** that same panel closes
- **THEN** `document.documentElement` no longer has `blockr-html-pushed-right`
- **AND** `--blockr-sidebar-width` is `"0px"`

#### Scenario: Overlay mode never reflows

- **WHEN** a panel with `mode = "overlay"` (the default) is open
- **THEN** `document.documentElement` has neither `blockr-html-pushed-left` nor `blockr-html-pushed-right`
- **AND** `--blockr-sidebar-width` is `"0px"` (the panel overlays the content)

#### Scenario: Pin state does not affect page reflow

- **WHEN** an open panel's pin button is toggled
- **THEN** `document.documentElement.classList` and `--blockr-sidebar-width` are unchanged (reflow is governed by `data-mode`, not by pin)

### Requirement: Dismissal, keyboard support, and focus management

On open the panel SHALL move keyboard focus to the first focusable element inside the body and remember the previously-focused element. While the panel is open, Tab and Shift+Tab SHALL cycle focus among focusable descendants of the panel (focus trap). On close the panel SHALL restore focus to the previously-focused element.

While the panel is open AND not pinned, pressing `Escape` SHALL close it AND clicking outside the panel SHALL close it. While pinned, neither `Escape` nor an outside click SHALL close it - only the X button closes a pinned panel. The pin button is therefore the user's opt-in to "stay open while I work elsewhere on the page".

#### Scenario: Esc closes when not pinned

- **WHEN** the panel is open, not pinned, and the user presses `Escape`
- **THEN** the panel transitions to closed (`.blockr-sidebar-open` removed, `aria-hidden="true"`)

#### Scenario: Esc does not close when pinned

- **WHEN** the panel is open, pinned, and the user presses `Escape`
- **THEN** the panel remains open

#### Scenario: Outside click closes when not pinned

- **WHEN** the panel is open, not pinned, and the user clicks anywhere outside the panel
- **THEN** the panel transitions to closed (`.blockr-sidebar-open` removed, `aria-hidden="true"`)
- **AND** the click that opened the panel does NOT itself close it (the outside-click handler activates after the open completes)

#### Scenario: Outside click does not close when pinned

- **WHEN** the panel is open, pinned, and the user clicks outside the panel
- **THEN** the panel remains open

#### Scenario: Focus returns to the trigger on close

- **WHEN** focus is on a button, the user opens the panel, then closes it
- **THEN** keyboard focus returns to the same button

### Requirement: Shiny input binding exposes `{open, pinned}`

`blockr.ui` SHALL ship a `Shiny.InputBinding` registered against `.blockr-sidebar` whose `getValue()` returns a list-shaped value with named entries `open` (logical) and `pinned` (logical). The binding MUST emit a state update through Shiny on every state change — server-driven (`show_sidebar()` / `hide_sidebar()` via `receiveMessage`), Esc-to-close, X-button click, outside-click-to-close, pin toggle. There SHALL NOT be a `content` entry: R-side already knows what content was last set (it called `show_sidebar()`), and tracking it client-side is duplicate state. The binding's `receiveMessage(el, data)` is the sole entry point for R-driven state changes (per the previous requirement); the binding does NOT define `setValue()`.

The binding's input value SHALL be reachable as `input[[id]]` server-side, where `id` matches the DOM id passed to `sidebar_ui()`.

#### Scenario: input[[id]]$open reflects open state

- **WHEN** the panel is open
- **THEN** `input[["main_sidebar"]]$open` is `TRUE`

- **WHEN** the panel is closed
- **THEN** `input[["main_sidebar"]]$open` is `FALSE`

#### Scenario: input[[id]]$pinned reflects pinned state

- **WHEN** the user clicks the pin button on an open panel
- **THEN** `input[["main_sidebar"]]$pinned` becomes `TRUE` on the next reactive flush

#### Scenario: User-driven close updates input[[id]]$open

- **WHEN** the panel is open (regardless of how it was opened) and the user presses `Escape` (with the panel not pinned) OR clicks the X button
- **THEN** `input[["main_sidebar"]]$open` becomes `FALSE` on the next reactive flush, allowing R-side observers to react to user-initiated dismissal

### Requirement: Snapshot helper `sidebar_state(id, session)`

`blockr.ui` SHALL export `sidebar_state(id, session = shiny::getDefaultReactiveDomain())` returning a `list(open, pinned)` snapshot of the binding's current value. The helper MUST walk to `session$rootScope()` so callers from a deeply-nested module can read the absolute-id state without computing the namespacing themselves, and MUST `shiny::isolate()` the read so the calling consumer does not pick up a reactive dependency. When the binding has not yet reported a value (e.g. the panel was never opened), the helper SHALL return `list(open = FALSE, pinned = FALSE)`.

`blockr.ui` SHALL also export `keep_or_hide_sidebar(id, ui, title = NULL, session = ...)` as the convenience primitive for the chain-on-pinned action-handler flow: when `sidebar_state(id, session)$pinned` is `TRUE`, it dispatches via `show_sidebar(id, ui, title, session)` (re-render the form fresh); otherwise it dispatches via `hide_sidebar(id, session)`. The chaining decision SHALL live in `blockr.ui` rather than be duplicated in every consumer (e.g. `blockr.dock`).

```r
# Caller's confirm flow becomes a single call:
update(list(blocks = list(add = bk)))
blockr.ui::keep_or_hide_sidebar(
  sidebar_id,
  ui = block_sidebar_body(ns, board$board, mode = "add"),
  title = "Add new block"
)
```

#### Scenario: sidebar_state reads from the root session

- **WHEN** `sidebar_state("main_sidebar", session)` is called from a deeply-nested module's session-proxy
- **THEN** the helper walks to `session$rootScope()` and reads the absolute-id input value, returning the same `{open, pinned}` list that the root session sees

#### Scenario: sidebar_state defaults before the binding has reported

- **WHEN** `sidebar_state("main_sidebar")` is called before the panel has ever rendered (binding has never set a value)
- **THEN** the helper returns `list(open = FALSE, pinned = FALSE)` rather than erroring

#### Scenario: sidebar_state does not create a reactive dependency

- **WHEN** `sidebar_state("main_sidebar")` is called from within an `observeEvent` handler
- **THEN** subsequent changes to `input[["main_sidebar"]]` do NOT re-run the observer (the read is isolated)

#### Scenario: keep_or_hide_sidebar branches on the pinned state

- **WHEN** `keep_or_hide_sidebar(id, ui = fresh_body, title = "...")` is called and `sidebar_state(id)$pinned` is `TRUE`
- **THEN** the panel is re-shown via `show_sidebar(id, ui = fresh_body, title = "...")` so the user can chain another action

- **WHEN** `keep_or_hide_sidebar(id, ui = fresh_body, title = "...")` is called and `sidebar_state(id)$pinned` is `FALSE`
- **THEN** the panel is hidden via `hide_sidebar(id)`

### Requirement: Auto-open on empty board

R-side code SHALL be able to drive an "auto-open at session start when there's nothing on the board" pattern using only `show_sidebar()` and the `input[[id]]$open` value. No special opt-in argument on `sidebar_ui()` is required: callers compose the recipe themselves with the existing helpers.

#### Scenario: Renderer auto-opens an empty-board hint

- **WHEN** a renderer's session starts with `length(board_blocks(board)) == 0`
- **AND** the renderer's server function calls `blockr.ui::show_sidebar(id, hint_ui, title = "Add your first block")` early in `moduleServer`
- **THEN** the panel opens with the hint content
- **AND** if the user dismisses it (Esc, X), `input[[id]]$open` becomes `FALSE` and the renderer's observer can read that to avoid re-opening on subsequent reactive flushes (e.g. by tracking a `auto_opened` `reactiveVal()` flag set after the first show)

### Requirement: Drop-in replacement for `showModal()` / `removeModal()`

The API of `show_sidebar()` and `hide_sidebar()` SHALL be ergonomically equivalent to Shiny's `showModal()` / `removeModal()`: callers compose the panel body with arbitrary shiny tags (no S3 generic, no trigger types, no content registry), and the panel's session-handling matches a modal's so existing input observers attached to inputs inside the body keep working without change.

After this change ships, the migration in `blockr.dock` from each modal flow to the sidebar SHALL be a mechanical substitution of two function calls:

- `showModal(modal_dialog_wrapping(body))` becomes `blockr.ui::show_sidebar(sidebar_id, ui = body, title = ...)`.
- `removeModal()` becomes `blockr.ui::hide_sidebar(sidebar_id)`.

#### Scenario: Inputs inside the panel body land in the calling session's input

- **WHEN** an action handler in `blockr.dock` calls `show_sidebar(sidebar_id, ui = block_modal_body(session$ns, ...))` from inside its `moduleServer`
- **THEN** the panel body's inputs (e.g. `add_block_confirm`) are accessible as `input$add_block_confirm` from that same `moduleServer`'s body, exactly as they were when the body lived inside `modalDialog(...)` opened by `showModal()`

#### Scenario: No session walking required

- **WHEN** `show_sidebar()` / `hide_sidebar()` is called from a deeply-nested `moduleServer` (e.g. `blockr.dock`'s action handler, which is itself a `moduleServer` mounted inside `blockr.core::board_server()`'s `moduleServer`)
- **THEN** the message is delivered to the panel element identified by `id`, without the caller having to compute or walk to a parent session
- **AND** the migration code does NOT need to access `.subset2(session, "parent")`, `session$rootScope`, or any private shiny internals
