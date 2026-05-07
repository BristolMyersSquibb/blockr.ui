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

`blockr.ui` SHALL export `sidebar_show(id, ui, title = NULL, session = shiny::getDefaultReactiveDomain())` and `sidebar_hide(id, session = shiny::getDefaultReactiveDomain())`. These mirror Shiny's `showModal()` / `removeModal()` in spirit.

`sidebar_show()` MUST pre-render `ui` via `htmltools::renderTags()` and send a single Shiny custom message of type `"blockr.ui:sidebar"` with payload `list(action = "show", id = id, html = rendered$html, dependencies = htmltools::resolveDependencies(rendered$dependencies), title = title)`. `sidebar_hide()` MUST send `list(action = "hide", id = id)` with the same custom-message type.

The custom-message type is intentionally fixed (`"blockr.ui:sidebar"`) so messages aren't namespaced by the calling session — the JS handler reads the absolute element id from `data.id` directly. Callers do not need to walk session proxies or compute parent namespaces.

#### Scenario: sidebar_show ships HTML and resolved dependencies

- **WHEN** server code calls `sidebar_show("main_sidebar", ui = some_tag_tree, title = "Add block")`
- **THEN** the session emits one custom message of type `"blockr.ui:sidebar"`
- **AND** the message payload's `action` is `"show"`, `id` is `"main_sidebar"`, `html` is character (the rendered HTML), `dependencies` is a (possibly empty) list of `html_dependency` objects
- **AND** `title` in the payload is `"Add block"`

#### Scenario: sidebar_hide is a single-purpose hide message

- **WHEN** server code calls `sidebar_hide("main_sidebar")`
- **THEN** the session emits one custom message of type `"blockr.ui:sidebar"` with payload `list(action = "hide", id = "main_sidebar")`

#### Scenario: Calling session is the modal-equivalent default

- **WHEN** `sidebar_show()` is called from inside a `moduleServer` body without an explicit `session` argument
- **THEN** the message is dispatched via `getDefaultReactiveDomain()` (the calling module's session) — exactly as `shiny::showModal()` does
- **AND** the panel id passed in `data.id` is the absolute DOM id chosen by the caller; no session-namespacing is applied to it

### Requirement: Client-side custom-message handler

`blockr.ui` SHALL register a single Shiny custom-message handler keyed `"blockr.ui:sidebar"` (in `inst/assets/js/blockr-sidebar.js`). On `data.action == "show"`, the handler MUST execute the following sequence in order on the panel's `.blockr-sidebar-body` element:

1. `Shiny.unbindAll(body)` — tear down stale Shiny bindings.
2. `Shiny.renderDependencies(data.dependencies)` if any — install new CSS / JS deps before rendering.
3. `body.replaceChildren(...$.parseHTML(data.html))` — swap the children.
4. `Shiny.initializeInputs(body)` — initialise input default values.
5. `Shiny.bindAll(body)` — wire up new bindings.
6. Set the panel's title slot to `data.title` (or empty if `null`); add the `.blockr-sidebar-open` class; set `aria-hidden="false"`; move keyboard focus to the first focusable element inside the body, remembering the previously-focused element so close can restore it.

On `data.action == "hide"`, the handler MUST `Shiny.unbindAll(body)`, remove the `.blockr-sidebar-open` class, set `aria-hidden="true"`, and restore focus to the remembered previously-focused element.

#### Scenario: set-content unbinds before replacing

- **WHEN** the handler fires for `action == "show"` with new HTML
- **THEN** `Shiny.unbindAll(body)` is invoked before `body.replaceChildren(...)`
- **AND** `Shiny.initializeInputs(body)` and `Shiny.bindAll(body)` are invoked after `replaceChildren`

#### Scenario: set-content installs new dependencies

- **WHEN** the message payload's `dependencies` array is non-empty
- **THEN** `Shiny.renderDependencies(dependencies)` is called between `unbindAll` and `replaceChildren` (or strictly before `bindAll`)

#### Scenario: hide unbinds the body

- **WHEN** the handler fires for `action == "hide"`
- **THEN** `Shiny.unbindAll(body)` is invoked before the open class is removed
- **AND** the panel's `aria-hidden` is set to `"true"`

### Requirement: Pinned mode reflows the page via a CSS variable

The panel header SHALL include a pin toggle. When the panel is open AND pinned, the JS handler SHALL set `--blockr-sidebar-width` on `document.body` to the panel's pixel width. When the panel is closed OR not pinned, the variable SHALL be `0px`. Apps that want the page canvas to reflow beside the panel can use `margin-right: var(--blockr-sidebar-width, 0px)` (or equivalent) on their main container.

The pin state lives entirely in the DOM (a class on the panel element). It is not exposed as a Shiny input value at v0.

#### Scenario: --blockr-sidebar-width tracks state

- **WHEN** the panel transitions from closed to open while pinned
- **THEN** `document.body.style.getPropertyValue('--blockr-sidebar-width')` becomes the panel's pixel width (e.g. `"360px"`)

- **WHEN** the panel transitions from open-pinned to closed
- **THEN** `document.body.style.getPropertyValue('--blockr-sidebar-width')` becomes `"0px"`

#### Scenario: Open-but-not-pinned does not reflow

- **WHEN** the panel is open but not pinned
- **THEN** `--blockr-sidebar-width` is `"0px"` (the panel overlays the content rather than reflowing it)

### Requirement: Keyboard support and focus management

On open the panel SHALL move keyboard focus to the first focusable element inside the body and remember the previously-focused element. While the panel is open, Tab and Shift+Tab SHALL cycle focus among focusable descendants of the panel (focus trap). On close the panel SHALL restore focus to the previously-focused element.

While the panel is open AND not pinned, pressing `Escape` SHALL close it. While pinned, `Escape` SHALL NOT close it.

#### Scenario: Esc closes when not pinned

- **WHEN** the panel is open, not pinned, and the user presses `Escape`
- **THEN** the panel transitions to closed (`.blockr-sidebar-open` removed, `aria-hidden="true"`)

#### Scenario: Esc does not close when pinned

- **WHEN** the panel is open, pinned, and the user presses `Escape`
- **THEN** the panel remains open

#### Scenario: Focus returns to the trigger on close

- **WHEN** focus is on a button, the user opens the panel, then closes it
- **THEN** keyboard focus returns to the same button

### Requirement: Shiny input binding exposes `{open, pinned}`

`blockr.ui` SHALL ship a `Shiny.InputBinding` registered against `.blockr-sidebar` whose `getValue()` returns a list-shaped value with named entries `open` (logical) and `pinned` (logical). The binding MUST emit a state update through Shiny on every state change — server-driven (`sidebar_show()` / `sidebar_hide()`), Esc-to-close, X-button click, pin toggle. There SHALL NOT be a `content` entry: R-side already knows what content was last set (it called `sidebar_show()`), and tracking it client-side is duplicate state. There SHALL NOT be `setValue()` / `receiveMessage()` overrides on the binding: state changes from R go through the custom-message handler (`sidebar_show()` / `sidebar_hide()`), and the binding observes the resulting class flip just like any client-driven flip.

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

### Requirement: Auto-open on empty board

R-side code SHALL be able to drive an "auto-open at session start when there's nothing on the board" pattern using only `sidebar_show()` and the `input[[id]]$open` value. No special opt-in argument on `sidebar_ui()` is required: callers compose the recipe themselves with the existing helpers.

#### Scenario: Renderer auto-opens an empty-board hint

- **WHEN** a renderer's session starts with `length(board_blocks(board)) == 0`
- **AND** the renderer's server function calls `blockr.ui::sidebar_show(id, hint_ui, title = "Add your first block")` early in `moduleServer`
- **THEN** the panel opens with the hint content
- **AND** if the user dismisses it (Esc, X), `input[[id]]$open` becomes `FALSE` and the renderer's observer can read that to avoid re-opening on subsequent reactive flushes (e.g. by tracking a `auto_opened` `reactiveVal()` flag set after the first show)

### Requirement: Drop-in replacement for `showModal()` / `removeModal()`

The API of `sidebar_show()` and `sidebar_hide()` SHALL be ergonomically equivalent to Shiny's `showModal()` / `removeModal()`: callers compose the panel body with arbitrary shiny tags (no S3 generic, no trigger types, no content registry), and the panel's session-handling matches a modal's so existing input observers attached to inputs inside the body keep working without change.

After this change ships, the migration in `blockr.dock` from each modal flow to the sidebar SHALL be a mechanical substitution of two function calls:

- `showModal(modal_dialog_wrapping(body))` becomes `blockr.ui::sidebar_show(sidebar_id, ui = body, title = ...)`.
- `removeModal()` becomes `blockr.ui::sidebar_hide(sidebar_id)`.

#### Scenario: Inputs inside the panel body land in the calling session's input

- **WHEN** an action handler in `blockr.dock` calls `sidebar_show(sidebar_id, ui = block_modal_body(session$ns, ...))` from inside its `moduleServer`
- **THEN** the panel body's inputs (e.g. `add_block_confirm`) are accessible as `input$add_block_confirm` from that same `moduleServer`'s body, exactly as they were when the body lived inside `modalDialog(...)` opened by `showModal()`

#### Scenario: No session walking required

- **WHEN** `sidebar_show()` / `sidebar_hide()` is called from a deeply-nested `moduleServer` (e.g. `blockr.dock`'s action handler, which is itself a `moduleServer` mounted inside `blockr.core::board_server()`'s `moduleServer`)
- **THEN** the message is delivered to the panel element identified by `id`, without the caller having to compute or walk to a parent session
- **AND** the migration code does NOT need to access `.subset2(session, "parent")`, `session$rootScope`, or any private shiny internals
