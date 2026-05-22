## ADDED Requirements

### Requirement: sidebar_ui builder

`blockr.ui` SHALL export `sidebar_ui(id, ui = NULL, title = NULL, side = c("right", "left"), width = "360px", mode = c("overlay", "push"))` returning a `<div class="blockr-sidebar">` with header (title slot, pin button, close button) and body slots, plus `data-side` and `aria-hidden` attributes. The element MUST NOT use any Bootstrap `.offcanvas` or `.modal` classes or `data-bs-*` attributes. The function SHALL attach `sidebar_dep()` via `htmltools::attachDependencies()` so callers do not need to attach the dependency separately.

A panel can be driven in two complementary ways:

1. **Dynamic content (modal-like).** `ui` is left at its default `NULL` and the body slot ships empty. The server populates it on demand by calling `show_sidebar(id, ui, title)`. This is the original use case and remains the right model for one-off content that varies per open (forms, transient action panels).

2. **Static content (offcanvas-like).** `ui` (and optionally `title`) is supplied at UI-build time. The body slot is pre-rendered via `htmltools::renderTags()` so HTML dependencies travel with the content, the title is set on the header, and the panel is ready to open with **zero server round-trip**. Opens are wired client-side via the data-attribute trigger described in a separate requirement below. A subsequent `show_sidebar(id, ui = ...)` still works and replaces the pre-rendered body if the consumer later wants dynamic behaviour.

The two modes are mutually compatible on the same DOM mount: pre-rendered content can be swapped out by a later `show_sidebar()`, and a pre-rendered panel can still be opened from the server (see the updated `show_sidebar()` contract where `ui` becomes optional).

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

#### Scenario: Empty body by default

- **WHEN** a caller invokes `sidebar_ui("main_sidebar")` (i.e. `ui = NULL`)
- **THEN** the `.blockr-sidebar-body` slot has no child elements
- **AND** the `.blockr-sidebar-title` slot is empty

#### Scenario: Pre-rendered body content

- **WHEN** a caller invokes `sidebar_ui("settings_sidebar", ui = tagList(h3("Hi"), p("there")), title = "Board options")`
- **THEN** the `.blockr-sidebar-body` slot contains the rendered HTML for `tagList(h3("Hi"), p("there"))`
- **AND** the `.blockr-sidebar-title` slot's text content is `"Board options"`
- **AND** any HTML dependencies attached to `ui` are carried with the tag tree so `htmltools::findDependencies()` resolves them alongside `blockr-sidebar`
- **AND** the panel remains closed (`aria-hidden="true"`, no `.blockr-sidebar-open` class) until a trigger (data-attribute or `show_sidebar()`) opens it

#### Scenario: show_sidebar replaces pre-rendered content

- **WHEN** a panel was mounted with `sidebar_ui("settings_sidebar", ui = static_body)` and the server later calls `show_sidebar("settings_sidebar", ui = fresh_body)`
- **THEN** the body slot's children are replaced with the rendered HTML of `fresh_body`, identical in behaviour to a panel that had been mounted with `ui = NULL`

### Requirement: Server-side show / hide helpers

`blockr.ui` SHALL export `show_sidebar(id, ui = NULL, title = NULL, session = shiny::getDefaultReactiveDomain())` and `hide_sidebar(id, session = shiny::getDefaultReactiveDomain())`. These mirror Shiny's `showModal()` / `removeModal()` in spirit, with `show_sidebar()` extended to also act as "just open" when the body is already populated.

Both helpers MUST dispatch through the calling session's root scope (`session$rootScope()$sendInputMessage(id, ...)`) so the panel id is treated as an absolute DOM id and is NOT prefixed by the calling module's namespace. The id passed at call time MUST match the id that was passed to `sidebar_ui()` at mount time.

`show_sidebar()`'s behaviour depends on whether `ui` is supplied:

- **`ui` non-NULL**: MUST pre-render `ui` via `htmltools::renderTags()` and emit a single input message with `list(action = "show", html = rendered$html, dependencies = <resolved deps>, title = title)`. The client replaces the body and opens.
- **`ui = NULL`**: MUST emit `list(action = "show", title = title)` with NO `html` and NO `dependencies` fields. The client opens the panel without touching its body, so consumers that pre-rendered content via `sidebar_ui(id, ui = ...)` (or that left the previous body in place from an earlier show) can open from the server without re-shipping HTML. When `title` is also `NULL`, the existing header title (if any) is left untouched; when `title` is supplied, the header is updated.

`hide_sidebar()` MUST emit `list(action = "hide")`. There is no custom-message handler: the input message is delivered to the panel's `Shiny.InputBinding` via its `receiveMessage(el, data)` method.

#### Scenario: show_sidebar ships HTML and resolved dependencies

- **WHEN** server code calls `show_sidebar("main_sidebar", ui = some_tag_tree, title = "Add block")`
- **THEN** the root session emits one `sendInputMessage` call with `inputId = "main_sidebar"`
- **AND** the message payload's `action` is `"show"`, `html` is character (the rendered HTML), `dependencies` is a (possibly empty) list of resolved web dependencies
- **AND** `title` in the payload is `"Add block"`

#### Scenario: hide_sidebar is a single-purpose hide message

- **WHEN** server code calls `hide_sidebar("main_sidebar")`
- **THEN** the root session emits one `sendInputMessage` call with `inputId = "main_sidebar"` and payload `list(action = "hide")`

#### Scenario: show_sidebar opens without re-shipping content

- **WHEN** server code calls `show_sidebar("settings_sidebar")` (no `ui`, no `title`) on a panel whose body was pre-rendered at `sidebar_ui()` time
- **THEN** the root session emits one `sendInputMessage` call with `inputId = "settings_sidebar"` and payload `list(action = "show", title = NULL)`
- **AND** the payload has NO `html` field and NO `dependencies` field
- **AND** the client opens the panel without touching the body content already in the DOM

#### Scenario: show_sidebar opens and updates only the title

- **WHEN** server code calls `show_sidebar("settings_sidebar", title = "Updated")` on a panel with pre-rendered body
- **THEN** the payload is `list(action = "show", title = "Updated")` with no `html` / `dependencies` fields
- **AND** the client updates the header title text without touching the body

#### Scenario: Calling session is the modal-equivalent default

- **WHEN** `show_sidebar()` is called from inside a `moduleServer` body without an explicit `session` argument
- **THEN** the message is dispatched via `getDefaultReactiveDomain()` (the calling module's session) — exactly as `shiny::showModal()` does
- **AND** the call walks to `session$rootScope()` before invoking `sendInputMessage`, so the panel id is the absolute DOM id chosen at `sidebar_ui()` time and is NOT prepended with the calling module's namespace

### Requirement: Client-side handling via the InputBinding

The panel's `Shiny.InputBinding` (registered against `.blockr-sidebar`) MUST implement `receiveMessage(el, data)` that switches on `data.action`. There SHALL NOT be a separate `Shiny.addCustomMessageHandler(...)` registration: the same binding owns both directions of the R↔JS conversation.

On `data.action == "show"`, the handler MUST branch on whether `data.html` is present:

- **`data.html` present (content swap)**: execute the following sequence in order on the panel's `.blockr-sidebar-body` element:
  1. `Shiny.unbindAll(body)` to tear down stale Shiny bindings.
  2. `Shiny.renderDependencies(data.dependencies)` if any, to install new CSS / JS deps before rendering.
  3. `body.replaceChildren(...$.parseHTML(data.html))` to swap the children.
  4. `Shiny.initializeInputs(body)` to initialise input default values.
  5. `Shiny.bindAll(body)` to wire up new bindings.
- **`data.html` absent (open-only)**: skip the unbind / dependencies / replace / init / bind sequence entirely. The existing body and its bindings (whether installed by a previous show or by `sidebar_ui(ui = ...)` at page-build time) remain intact.

After the body branch, in both cases: if `data.title` is present (non-`undefined`), set the panel's title slot to `data.title` (or empty string if `null`); when `data.title` is absent, leave the title slot unchanged. Then add the `.blockr-sidebar-open` class, set `aria-hidden="false"`, and move keyboard focus to the first focusable element inside the body, remembering the previously-focused element so close can restore it.

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

#### Scenario: Open-only show preserves existing body

- **WHEN** `receiveMessage` fires with `data.action == "show"` and no `html` field on a panel whose body already contains content (from `sidebar_ui(ui = ...)` or a previous show)
- **THEN** `Shiny.unbindAll` / `Shiny.renderDependencies` / `replaceChildren` / `Shiny.initializeInputs` / `Shiny.bindAll` are NOT invoked
- **AND** the panel opens (`.blockr-sidebar-open` class added, `aria-hidden="false"`) with its existing body intact
- **AND** existing inputs inside the body remain bound to Shiny

### Requirement: JS-triggered open via `data-blockr-sidebar-target`

Any element in the page MAY act as a client-side open trigger for a mounted panel by carrying a `data-blockr-sidebar-target="<id>"` attribute, where `<id>` matches the DOM id of a `sidebar_ui()`. The bundled JS SHALL listen for `click` events on the document and, when the event target (or any of its ancestors via `.closest()`) carries the attribute, toggle the panel's open state without an R round-trip.

Toggle semantics: if the target panel is closed, clicking opens it; if it is open, clicking closes it. The same dismissal rules apply regardless of how the panel was opened (Esc / outside-click / X button still work when not pinned, etc.).

When opening via this trigger, the JS MUST run the same "open" steps as the open-only branch of `receiveMessage` (add the open class, set `aria-hidden="false"`, refresh body reflow, attach the outside-click handler, dispatch the state event, move focus). It MUST NOT touch the body or the title; the consumer is expected to have populated those via `sidebar_ui(ui = ..., title = ...)`.

Because every state change still dispatches `blockr-sidebar:state`, the binding's `getValue()` and `input[[id]]$open` continue to reflect reality regardless of whether the open was R-driven or JS-driven. Server-side observers can react to the resulting `$open == TRUE` if they need to.

#### Scenario: Click on a target element opens the matching panel

- **WHEN** a page mounts `sidebar_ui("settings_sidebar", ui = static_body, title = "Board options")` and a button with `data-blockr-sidebar-target="settings_sidebar"`
- **AND** the user clicks the button
- **THEN** the panel `#settings_sidebar` receives the `.blockr-sidebar-open` class and `aria-hidden="false"`
- **AND** the panel's body content is unchanged (no `unbindAll` / `replaceChildren` / `bindAll`)
- **AND** keyboard focus moves to the first focusable element inside the body
- **AND** no `sendInputMessage` is issued by R (open is purely client-side)

#### Scenario: Click on a target element with the panel already open closes it

- **WHEN** the panel is open and the user clicks an element with `data-blockr-sidebar-target="settings_sidebar"`
- **THEN** the panel closes (same path as the X button / Esc / outside-click)

#### Scenario: Trigger nested inside a larger clickable region

- **WHEN** the trigger attribute is on an ancestor (e.g. a `<button>` wrapping an `<i>` icon) and the user clicks the `<i>`
- **THEN** the click handler still finds the trigger via `event.target.closest("[data-blockr-sidebar-target]")`
- **AND** the matching panel toggles

#### Scenario: input[[id]]$open reflects JS-driven opens

- **WHEN** a JS-driven open fires via the data-attribute trigger
- **THEN** `input[["settings_sidebar"]]$open` becomes `TRUE` on the next reactive flush (the `blockr-sidebar:state` event still fires, so the input binding still updates)

### Requirement: `mode = "overlay"` vs `mode = "push"`

`sidebar_ui()` SHALL accept a `mode` argument with values `"overlay"` (default) or `"push"`. The mode is set on the panel as `data-mode="<mode>"` and is independent of pin state (pin only affects dismissal).

In `"overlay"` mode, the panel slides over page content; the page layout is untouched. In `"push"` mode, the bundled JS adds a `.blockr-html-pushed-<side>` class on `<html>` while the panel is open and sets a side-specific CSS custom property — `--blockr-sidebar-width-left` or `--blockr-sidebar-width-right` — to the panel's pixel width. The bundled CSS uses these to apply `padding-<side>: var(--blockr-sidebar-width-<side>)` on `<html>` (with `box-sizing: border-box`), so page content shifts aside instead of being covered. The class + variables live on `<html>` rather than `<body>` because bslib's page-fill layouts pin body to 100% of html and zero its padding inline — body-level padding/margin no longer constrains the visible viewport.

Two push panels on opposite sides MAY be open simultaneously. Each side contributes its own padding via its own variable so the page content is constrained between them rather than disappearing under whichever side opened second. For backwards compatibility with single-side consumers, the legacy `--blockr-sidebar-width` (without side suffix) is also set to the open side's width when exactly one push panel is open; it is `"0px"` when no push panel is open. When both sides are open, consumers reading the legacy variable see only the right side's width and SHOULD switch to the side-specific variables.

When a panel closes, its side's class is removed from `<html>` and its side-specific variable is set to `"0px"`. The opposite side, if still open, is unaffected.

The pin state lives entirely in the DOM (a class on the panel element). It is not exposed as a Shiny input value beyond what the binding's `getValue()` returns. Pin no longer drives body reflow — that is `mode`'s job.

#### Scenario: Push mode shifts page content aside while open

- **WHEN** a panel with `mode = "push"` and `side = "right"` opens
- **THEN** `document.documentElement` has the class `blockr-html-pushed-right`
- **AND** `document.documentElement.style.getPropertyValue('--blockr-sidebar-width-right')` is the panel's pixel width
- **AND** the legacy `--blockr-sidebar-width` equals the same value

- **WHEN** that same panel closes
- **THEN** `document.documentElement` no longer has `blockr-html-pushed-right`
- **AND** both `--blockr-sidebar-width-right` and `--blockr-sidebar-width` are `"0px"`

#### Scenario: Two push panels on opposite sides coexist

- **WHEN** a `mode = "push"`, `side = "left"` panel is already open AND a `mode = "push"`, `side = "right"` panel opens
- **THEN** `document.documentElement` has BOTH `blockr-html-pushed-left` AND `blockr-html-pushed-right`
- **AND** `--blockr-sidebar-width-left` and `--blockr-sidebar-width-right` are each set to their panel's pixel width
- **AND** the page content's `<html>` content area is shrunk on both sides (constrained between the two panels), not hidden under either

- **WHEN** the right panel then closes (the left remains open)
- **THEN** `document.documentElement` has only `blockr-html-pushed-left`
- **AND** `--blockr-sidebar-width-right` is `"0px"`
- **AND** `--blockr-sidebar-width-left` is unchanged from before the right side closed

#### Scenario: Overlay mode never reflows

- **WHEN** a panel with `mode = "overlay"` (the default) is open
- **THEN** `document.documentElement` has neither `blockr-html-pushed-left` nor `blockr-html-pushed-right`
- **AND** all three width variables (`--blockr-sidebar-width`, `--blockr-sidebar-width-left`, `--blockr-sidebar-width-right`) are `"0px"` (the panel overlays the content)

#### Scenario: Pin state does not affect page reflow

- **WHEN** an open panel's pin button is toggled
- **THEN** `document.documentElement.classList` and the width variables are unchanged (reflow is governed by `data-mode`, not by pin)

### Requirement: Dismissal, keyboard support, and focus management

On open the panel SHALL move keyboard focus to the first focusable element inside the body and remember the previously-focused element. While the panel is open, Tab and Shift+Tab SHALL cycle focus among focusable descendants of the panel (focus trap). On close the panel SHALL restore focus to the previously-focused element.

When that first focusable element belongs to a selectize control (i.e. lives inside a `.selectize-control` wrapping a `<select class="selectized">` whose `.selectize` JS API is attached), the panel SHALL additionally call `select.selectize.focus()` so the dropdown opens, matching the keyboard UX previously wired per-form via `shown.bs.modal` listeners. Plain `HTMLElement.focus()` is insufficient for selectize because it lands on the visible `.selectize-input` div without triggering open-on-focus. Selectize awareness lives in the primitive so every consumer benefits without per-form opt-in.

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

#### Scenario: Selectize dropdown opens automatically on show

- **WHEN** the panel opens with a body whose first focusable element is a selectized `<select>` (e.g. `block_registry_selectize(...)`)
- **THEN** keyboard focus lands inside the selectize control
- **AND** the selectize dropdown is open (`select.selectize.isOpen` is `TRUE`), without the form needing its own `shown.bs.*` or panel-open listener

#### Scenario: Non-selectize first focusable is left alone

- **WHEN** the panel opens with a body whose first focusable element is an ordinary `<input>` or `<button>` (not inside a `.selectize-control`)
- **THEN** keyboard focus lands on that element via `HTMLElement.focus()` and no selectize API is invoked

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
