## ADDED Requirements

### Requirement: sidebar_ui builder

`blockr.ui` SHALL export `sidebar_ui(id, side = "right", width = "360px", pinned = FALSE)` that returns a `<div class="blockr-sidebar">` element with header (title, pin button, close button) and body slots, and `data-side` / `data-content-type` / `aria-hidden` attributes. The element MUST NOT use any Bootstrap `.offcanvas` or `.modal` classes or `data-bs-*` attributes. The function SHALL also attach `sidebar_dep()` so a caller does not need to attach the dependency separately.

#### Scenario: Markup is Bootstrap-free

- **WHEN** a caller invokes `sidebar_ui("main_sidebar")`
- **THEN** the returned tag tree contains no element with class containing `offcanvas` or `modal`
- **AND** no element carries any attribute starting with `data-bs-`

#### Scenario: Initial state attributes

- **WHEN** a caller invokes `sidebar_ui("main_sidebar", side = "left", width = "400px", pinned = TRUE)`
- **THEN** the root element has `data-side="left"`, `style` containing `width: 400px`, and class `blockr-sidebar-pinned`
- **AND** initial `aria-hidden` is `"true"` (closed by default)

### Requirement: Shiny input binding exposes sidebar state

`blockr.ui` SHALL ship a Shiny `InputBinding` registered as `"blockr.sidebar"` against `.blockr-sidebar` whose `getValue()` returns a list-shaped value with named entries `open` (logical), `pinned` (logical), and `content` (character or NULL). The binding MUST emit a state update through Shiny on every state change (open/close, pin/unpin, content swap, Esc close).

#### Scenario: input[[id]] reflects open state

- **WHEN** the sidebar is open and not pinned
- **THEN** `input[["main_sidebar"]]$open` is `TRUE`, `input[["main_sidebar"]]$pinned` is `FALSE`

#### Scenario: input[[id]] reflects pinned state

- **WHEN** the user clicks the pin button on an open sidebar
- **THEN** `input[["main_sidebar"]]$pinned` becomes `TRUE` on the next reactive flush

#### Scenario: input[[id]] reflects content type

- **WHEN** the server calls `sidebar_set_content("main_sidebar", add_block_trigger())`
- **THEN** `input[["main_sidebar"]]$content` is `"add_block"` after the next reactive flush

### Requirement: Custom message handlers for sidebar control

`blockr.ui` SHALL register the following Shiny custom message handlers on the sidebar binding's element: `sidebar-open` (show), `sidebar-close` (hide), `sidebar-toggle` (flip), `sidebar-set-content` (replace body and open if not already). The `set-content` handler MUST unbind existing Shiny inputs / outputs in the body before replacing children, install any dependencies in the message payload via `Shiny.renderDependencies()`, then run `Shiny.initializeInputs()` and `Shiny.bindAll()` on the new body.

#### Scenario: set-content unbinds before replacing

- **WHEN** the `set-content` handler fires with new HTML
- **THEN** `Shiny.unbindAll(body)` is invoked before `body.replaceChildren(...)`
- **AND** `Shiny.initializeInputs(body)` and `Shiny.bindAll(body)` are invoked after

#### Scenario: set-content installs new dependencies

- **WHEN** the message payload contains a `dependencies` array
- **THEN** `Shiny.renderDependencies(dependencies)` is called before `Shiny.bindAll(body)`

#### Scenario: set-content auto-opens the sidebar

- **WHEN** the sidebar is closed and `sidebar-set-content` arrives
- **THEN** the sidebar is opened (class `blockr-sidebar-open` added) and `aria-hidden` becomes `"false"`

### Requirement: R helpers for sidebar control

`blockr.ui` SHALL export `sidebar_open(id, session = shiny::getDefaultReactiveDomain())`, `sidebar_close(id, session = ...)`, `sidebar_toggle(id, session = ...)`, and `sidebar_set_content(id, trigger, ..., session = ...)`. Each MUST send a single Shiny custom input message to `id` with the corresponding action. `sidebar_set_content()` MUST validate that `trigger` inherits from `"sidebar_trigger"` and MUST forward extra `...` arguments to the `sidebar_content()` method.

#### Scenario: sidebar_open sends the open message

- **WHEN** server code calls `sidebar_open("main_sidebar")` with a connected session
- **THEN** the session sends a custom input message to `"main_sidebar"` with `list(action = "open")`

#### Scenario: sidebar_set_content forwards extra args to the method

- **WHEN** server code calls `sidebar_set_content("main_sidebar", add_block_trigger(), board = my_board)`
- **THEN** `sidebar_content.add_block(trigger, board = my_board)` is invoked
- **AND** the message payload includes the rendered HTML and any required dependencies

#### Scenario: sidebar_set_content rejects non-trigger objects

- **WHEN** server code calls `sidebar_set_content("main_sidebar", list(foo = 1))`
- **THEN** the call errors before any message is sent, with a class indicating the trigger contract was violated

### Requirement: Pinned mode reflows the page

When the sidebar is open AND pinned, the rendered DOM SHALL set `--blockr-sidebar-width` on the document body to the panel's pixel width. When the sidebar is closed OR not pinned, the variable SHALL be set to `0px`. App-shell CSS SHALL inset the `content` slot by `var(--blockr-sidebar-width)` so the canvas reflows beside the panel rather than being overlaid.

#### Scenario: --blockr-sidebar-width tracks state

- **WHEN** the sidebar transitions from closed to open while pinned
- **THEN** `document.body.style.getPropertyValue('--blockr-sidebar-width')` becomes the panel's pixel width (e.g. `"360px"`)

- **WHEN** the sidebar transitions from open-pinned to closed
- **THEN** `document.body.style.getPropertyValue('--blockr-sidebar-width')` becomes `"0px"`

#### Scenario: Open but not pinned does not reflow

- **WHEN** the sidebar is open but not pinned
- **THEN** `--blockr-sidebar-width` is `"0px"` (the panel overlays the content rather than reflowing it)

### Requirement: Keyboard support and focus management

The sidebar SHALL move focus to the first focusable element in the body on open, trap focus within the panel while open (Tab and Shift+Tab cycle within focusable descendants), and return focus to the previously-focused element on close. While the sidebar is open and not pinned, pressing `Escape` SHALL close it. While pinned, `Escape` SHALL NOT close it.

#### Scenario: Esc closes when not pinned

- **WHEN** the sidebar is open, not pinned, and the user presses `Escape`
- **THEN** the sidebar transitions to closed (class `blockr-sidebar-open` removed, `aria-hidden="true"`)

#### Scenario: Esc does not close when pinned

- **WHEN** the sidebar is open, pinned, and the user presses `Escape`
- **THEN** the sidebar remains open

#### Scenario: Focus returns to the trigger on close

- **WHEN** focus is on a navbar button, the user opens the sidebar, then closes it
- **THEN** keyboard focus returns to the same navbar button

### Requirement: sidebar_content S3 dispatch

`blockr.ui` SHALL export `sidebar_content <- function(x, ...) UseMethod("sidebar_content")` that dispatches on a sidebar trigger object's class. Built-in methods MUST cover: `sidebar_content.add_block`, `sidebar_content.append_block`, `sidebar_content.prepend_block`, `sidebar_content.add_link`, `sidebar_content.create_stack`, `sidebar_content.edit_stack`, `sidebar_content.settings`. Each method SHALL return a `shiny.tag` (or `tagList`) suitable for injection into the sidebar body.

#### Scenario: Default dispatch covers all built-in triggers

- **WHEN** a developer constructs each of `add_block_trigger()`, `append_block_trigger(src)`, `prepend_block_trigger(tgt)`, `add_link_trigger()`, `create_stack_trigger()`, `edit_stack_trigger(stk)`, `settings_trigger()` and calls `sidebar_content()` on each
- **THEN** every call dispatches successfully and returns an object that inherits from `"shiny.tag"` or is a `tagList`

#### Scenario: Extension hook works without modifying blockr.ui

- **WHEN** an extension package exports `sidebar_content.my_thing` for its own trigger class via `S3method(sidebar_content, my_thing)`
- **AND** an app calls `sidebar_set_content(id, structure(list(), class = c("my_thing", "sidebar_trigger")))`
- **THEN** the extension's method is invoked and its returned tag is rendered in the sidebar body

### Requirement: Sidebar trigger constructors

`blockr.ui` SHALL export the following user-facing trigger constructors, each producing an S3 object whose class vector ends with `"sidebar_trigger"`: `new_sidebar_trigger(class, ...)` (low-level), `add_block_trigger(...)`, `append_block_trigger(source, ...)`, `prepend_block_trigger(target, ...)`, `add_link_trigger(source = NULL, ...)`, `create_stack_trigger(...)`, `edit_stack_trigger(stack, ...)`, `settings_trigger(...)`.

#### Scenario: Constructor produces a valid trigger object

- **WHEN** a developer calls `add_block_trigger()`
- **THEN** the returned object inherits from both `"add_block"` and `"sidebar_trigger"`

#### Scenario: Trigger forwards args to the method

- **WHEN** a developer calls `append_block_trigger(source = "blk_1")`
- **THEN** the returned object's `source` element is `"blk_1"`
- **AND** `sidebar_content()` dispatches to `sidebar_content.append_block(trigger, ...)` with that `source` available

### Requirement: Block browser content

`blockr.ui` SHALL export `block_browser_ui(id, board)` and `block_browser_server(id, board, on_select)` that together render a cards-by-category browser used by `sidebar_content.add_block` (and by extension `append_block` / `prepend_block`). The browser MUST: render one card per registered block with name, description, icon, package badge, and category badge; group cards by category; provide a search box that filters across name, description, category, and package with live update; expose a quick-add affordance (single click); expose a detailed-add accordion with custom name, ID, and input selection (using `block_input_select()` from `blockr.ui`'s foundation capability).

#### Scenario: Cards render per registered block

- **WHEN** `block_browser_ui(id, board)` is rendered for a registry containing 3 blocks across 2 categories
- **THEN** the rendered DOM contains 3 cards
- **AND** the cards are grouped under 2 category headings

#### Scenario: Search filters live

- **WHEN** the user types `"join"` in the search box
- **THEN** only cards whose name, description, category, or package contains `"join"` (case-insensitive) remain visible
- **AND** empty categories are hidden

#### Scenario: Quick-add fires on_select

- **WHEN** the user clicks a card's quick-add button
- **THEN** `block_browser_server()`'s `on_select` callback is invoked with the registered block's constructor and default args

#### Scenario: Keyboard navigation

- **WHEN** focus is on a card and the user presses `ArrowRight` / `ArrowDown` / `ArrowLeft` / `ArrowUp`
- **THEN** focus moves to the appropriate adjacent card
- **WHEN** focus is on a card and the user presses `Enter`
- **THEN** quick-add is triggered for that card
- **WHEN** focus is anywhere in the browser and the user presses `Escape`
- **AND** the sidebar is not pinned
- **THEN** the sidebar closes

### Requirement: Sidebar replaces modal flows

After migration, the action handlers that today open Bootstrap modals SHALL instead call `sidebar_set_content()` with the appropriate trigger. Specifically:

- `blockr.dock::add_block_action()` / `append_block_action()` / `prepend_block_action()` MUST call `sidebar_set_content(sidebar_id, add_block_trigger() | append_block_trigger(source) | prepend_block_trigger(target))`.
- `blockr.dock::add_link_action()` MUST call `sidebar_set_content(sidebar_id, add_link_trigger(source))`.
- `blockr.dock::add_stack_action()` / `edit_stack_action()` MUST call `sidebar_set_content(sidebar_id, create_stack_trigger() | edit_stack_trigger(stack))`.
- The settings gear button on the navbar MUST call `sidebar_set_content(sidebar_id, settings_trigger())`.

After Phase 4, `blockr.dock` SHALL contain no calls to `block_modal()`, `link_modal()`, `stack_modal()`, `css_modal()`, or `css_modal_advanced()`, and the corresponding source files / functions MUST be deleted.

#### Scenario: add_block_action no longer calls showModal

- **WHEN** the trigger reactive of `add_block_action()` fires after Phase 3
- **THEN** the action calls `sidebar_set_content(sidebar_id, add_block_trigger(), board = board$board)` instead of `showModal(block_modal(...))`
- **AND** the action does not call `removeModal()`

#### Scenario: Modal builders are deleted at Phase 4

- **WHEN** a maintainer greps `blockr.dock/R/` after Phase 4
- **THEN** no file defines `block_modal`, `link_modal`, `stack_modal`, `css_modal`, or `css_modal_advanced`

#### Scenario: Pinned-mode multi-add round-trip

- **WHEN** the user opens the sidebar, pins it, and clicks quick-add three times in succession
- **THEN** three blocks are added to the board in order
- **AND** the sidebar remains open between adds without re-opening
