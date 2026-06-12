## ADDED Requirements

### Requirement: `sidebar_menu_action()` factory

`blockr.dock` SHALL provide an internal `sidebar_menu_action()` factory that
returns an action constructor of the existing shape `function(trigger,
board, update, ...)`, suitable for listing in `board_actions.dock_board()`
without any change to that generic or to `register_action()`.

The factory SHALL own the scaffold shared by every sidebar-menu action and
expose the per-handler differences as parameters: `id` (the action id),
`server` (a thunk returning the committed reactive for a given module id),
`ui` (a builder `function(ns_id, board, trigger)` returning the panel tag),
`title` (a character scalar OR a `function(trigger)` returning one),
`on_commit` (the commit handler), an optional `after_commit` rebuild hook,
and `module_id` (defaulting to `"menu"`).

Within the produced action server the factory SHALL: derive `sidebar_id`
as `NS(isolate(board$board_id), "actions_sidebar")`; mount the menu module
exactly once via `server(module_id)`; build a `*_ui()` closure that calls
`ui(session$ns(module_id), board$board, trigger())`; on `trigger()` show
the sidebar with the resolved title and the built UI; and observe the
committed reactive (see the commit-dispatch requirement). The server SHALL
return `NULL`, satisfying the `new_action()` contract.

All arguments captured by the returned closures SHALL be `force()`d so that
calling the factory multiple times (once per handler) produces independent
actions with no lazy-evaluation aliasing.

#### Scenario: Produces a valid action constructor

- **WHEN** `sidebar_menu_action(id = "add_stack_action", ...)` is called
- **THEN** it returns a value satisfying the `new_action()` contract whose
  `action_id()` is `"add_stack_action"`
- **AND** the value is usable directly as an entry in
  `board_actions.dock_board()` with no change to that generic

#### Scenario: Constant vs. dynamic title

- **WHEN** `title` is the string `"Create new stack"` AND the action's
  `trigger()` fires
- **THEN** the sidebar is shown with title `"Create new stack"`
- **WHEN** instead `title` is `function(trigger) paste0("Edit stack ", trigger)`
  AND `trigger()` resolves to `"s1"`
- **THEN** the sidebar is shown with title `"Edit stack s1"`

#### Scenario: Module mounted once and namespaced

- **WHEN** the produced action server starts with `module_id = "menu"`
- **THEN** `server("menu")` is invoked exactly once to obtain the committed
  reactive
- **AND** the `ui` builder receives `session$ns("menu")` as its first
  argument, so the panel root resolves to the `menu-commit` input the
  module server reads

### Requirement: Commit dispatch and rebuild gating

The action server produced by `sidebar_menu_action()` SHALL observe the
committed reactive and, on each fire, invoke `on_commit(spec, board$board,
update, session, trigger)`. The factory SHALL trigger the post-commit
rebuild ONLY when `on_commit` returns `TRUE`; when `on_commit` returns
`FALSE` (a validator short-circuited), no rebuild SHALL occur and no
`update()` SHALL have been issued.

`on_commit` SHALL be the sole owner of domain validation, object
construction, and the `update()` call; the factory SHALL NOT inspect or
reshape the committed spec.

#### Scenario: Failed validation short-circuits without update or rebuild

- **WHEN** the committed spec carries a duplicate id AND `on_commit`'s
  validator rejects it (returns `FALSE`)
- **THEN** `update()` is not called (its payload stays empty)
- **AND** the post-commit rebuild hook does not run

#### Scenario: Successful commit updates then rebuilds

- **WHEN** the committed spec is valid AND `on_commit` issues `update(...)`
  and returns `TRUE`
- **THEN** the `update()` payload reflects the built object
- **AND** the `after_commit` rebuild hook runs exactly once

### Requirement: Post-commit rebuild strategies preserved

The factory SHALL support, via the `after_commit` hook, the three rebuild
behaviours the existing handlers use, with their current semantics
unchanged:

- **Synchronous keep-or-hide** (block add / append / prepend): call
  `blockr.ui::keep_or_hide_sidebar()` immediately after a successful commit.
- **Deferred keep-or-hide** (stack create / edit): register
  `session$onFlushed(once = TRUE, ...)` and call
  `blockr.ui::keep_or_hide_sidebar()` there, so the rebuilt UI reads the
  post-`update()` board state.
- **Deferred hide-or-pool-update** (link add): register
  `session$onFlushed(once = TRUE, ...)`, recompute
  `blockr.ui::link_eligible_pools(board$board, trigger())`, and either call
  `blockr.ui::hide_sidebar()` when both pools are empty or
  `session$sendInputMessage(session$ns("menu-commit"), <pool-update>)`
  otherwise. This strategy SHALL NOT call `keep_or_hide_sidebar()`.

The default `after_commit` (when none is supplied) SHALL be the deferred
keep-or-hide strategy.

#### Scenario: Stack flow rebuilds after the flush

- **WHEN** a stack action commits successfully
- **THEN** `keep_or_hide_sidebar()` is invoked from within an
  `onFlushed(once = TRUE)` callback (after the board mutation is visible),
  not synchronously inside the commit observer

#### Scenario: Link flow pushes a pool-update instead of rebuilding

- **WHEN** a link action commits successfully AND at least one direction
  still has eligible candidates
- **THEN** `keep_or_hide_sidebar()` is NOT called
- **AND** a `pool-update` message is sent to `session$ns("menu-commit")`
- **WHEN** instead both pools are empty after the commit
- **THEN** `hide_sidebar()` is called

### Requirement: `remove_action()` factory

`blockr.dock` SHALL provide an internal `remove_action(id, kind)` factory
that returns an action constructor whose server observes `trigger()` and
calls `update(list(<kind> = list(rm = trigger())))`, where `kind` is one of
`"blocks"`, `"links"`, or `"stacks"`. The three handlers
`remove_block_action`, `remove_link_action`, and `remove_stack_action`
SHALL be defined as calls to this factory, preserving their current ids and
behaviour.

#### Scenario: Remove handler emits the rm delta for its kind

- **WHEN** `remove_action("remove_link_action", "links")` is mounted AND its
  `trigger()` resolves to `"ab"`
- **THEN** `update()` receives `list(links = list(rm = "ab"))`

### Requirement: Shared id validator

`blockr.dock` SHALL provide a single internal `require_valid_id(id,
existing, what, session)` helper returning `FALSE` (after a warning
`notify()`) when `id` is `NULL`, empty, or already present in `existing`,
and `TRUE` otherwise. The warning message SHALL read `"Please choose a
valid <what> ID."`. The id-collision validators for blocks, links, and
stacks SHALL route through this single helper, and the duplicate
`valid_link_id()` definition SHALL be removed (exactly one definition of
the id-validation logic remains in the package).

#### Scenario: Duplicate id is rejected with the kind-specific message

- **WHEN** `require_valid_id("ab", existing = c("ab"), what = "link", session)`
  is called
- **THEN** it returns `FALSE`
- **AND** a warning notification with text containing `"valid link ID"` is
  emitted

#### Scenario: Fresh id passes

- **WHEN** `require_valid_id("xy", existing = c("ab"), what = "block", session)`
  is called
- **THEN** it returns `TRUE` and no notification is emitted

### Requirement: Shared input-port resolution

`blockr.dock` SHALL provide a single internal `resolve_input_port(block,
block_id, links, explicit)` helper that returns `explicit` when it is a
non-empty string, and otherwise the first free named slot via
`block_input_select(block, block_id, links, mode = "inputs")[1L]` (which
yields a freshly generated slot for variadic blocks). The append, prepend,
and add-link handlers SHALL resolve the target port through this helper
instead of inlining the logic.

#### Scenario: Explicit port wins

- **WHEN** `resolve_input_port(blk, "m", links, explicit = "y")` is called
  for an arity-2 block
- **THEN** it returns `"y"`

#### Scenario: Falls back to the first free slot

- **WHEN** `resolve_input_port(blk, "b", links, explicit = NULL)` is called
  for an arity-1 block whose only input is `"data"` and which has no links
- **THEN** it returns `"data"`

### Requirement: Behaviour-preserving refactor

This change SHALL NOT alter any externally observable behaviour of the nine
action handlers: the committed-spec handling, the `update()` payload shapes,
the sidebar titles, the validation warning messages, and the post-commit
rebuild behaviour SHALL be identical before and after. No new package
exports SHALL be introduced (the factory and helpers are unexported). The
existing `testServer` suites `test-action-block.R`, `test-action-link.R`,
and `test-action-stack.R` SHALL pass unchanged, and `devtools::check()` and
`lintr::lint_package()` (under `linters_with_defaults(object_name_linter =
NULL)`) SHALL be clean.

#### Scenario: Existing action test suites pass unedited

- **WHEN** the refactor is complete AND the three action `testServer` suites
  are run without modification
- **THEN** all of them pass

#### Scenario: No new exports

- **WHEN** the package `NAMESPACE` is regenerated after the refactor
- **THEN** it gains no new exported symbols relative to before the change
