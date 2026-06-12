## Why

After `add-block-browser` (#5), `add-stack-menu` (#7), and `add-link-menu`
brought the add / append / prepend block, add / edit stack, and add link
flows onto the shared card-list sidebar pattern, the six action handlers in
`blockr.dock` have converged on a near-identical structure. They were
ported one change at a time, so the convergence happened by accretion, not
by design. Each handler now re-implements the same scaffold (derive the
sidebar id, mount the menu module once, build a `*_ui()` closure, show on
trigger, observe the committed reactive, validate, build, `update()`,
rebuild the sidebar), and the leaf-level helpers (id validators, port
resolution, wrap-and-name) are copy-pasted across files. The link-menu
adoption even left `valid_link_id()` defined **twice** (`R/action-block.R`
and `R/action-link.R`). Now that all flows are on the same pattern, this
is the natural moment to factor the shared scaffold into one reusable
internal API before the duplication ossifies.

## What Changes

- **NEW (internal)** `sidebar_menu_action()`: a higher-order factory in
  `blockr.dock` that owns the shared action scaffold (sidebar id,
  mount-the-module-once, `*_ui()` closure, show-on-trigger, and the
  `observeEvent(committed(), ...)` wrapper). The six menu handlers
  (`add_block_action`, `append_block_action`, `prepend_block_action`,
  `add_link_action`, `add_stack_action`, `edit_stack_action`) become thin
  calls into it, supplying only their variation points: the menu module
  pair, the `*_ui()` extra arguments, the sidebar title (constant or a
  function of the trigger), the commit handler, and the post-commit rebuild
  strategy.
- **NEW (internal)** `remove_action(id, kind)`: a factory for the three
  trivial `remove_*_action` handlers, which are byte-identical except the
  `update()` key (`blocks` / `links` / `stacks`).
- **NEW (internal)** `require_valid_id()`: collapses the three id-collision
  validators (`valid_block_id`, `valid_link_id`, `valid_stack_id`) into one,
  and removes the duplicate `valid_link_id()` definition.
- **NEW (internal)** `resolve_input_port()`: the "explicit pick, else first
  free / fresh variadic slot via `block_input_select(mode = "inputs")`"
  block currently repeated in the append / prepend / add-link handlers.
- **MODIFIED** All six menu handlers + three remove handlers in
  `R/action-block.R`, `R/action-link.R`, `R/action-stack.R` are rewritten
  as thin wrappers. `R/action-block.R`'s and `R/action-link.R`'s private
  helpers are consolidated.
- **NO behaviour change.** The committed-spec shapes, the `update()`
  payloads, the sidebar titles, the validation messages, and the
  post-commit rebuild behaviour (sync vs. deferred vs. live pool-update)
  are all preserved exactly. The existing `testServer` suites
  (`test-action-block.R`, `test-action-link.R`, `test-action-stack.R`) are
  the regression oracle and MUST pass unchanged.

## Capabilities

### New Capabilities
- `dock-action-handlers`: the reusable internal scaffold for
  sidebar-menu-backed board actions in `blockr.dock`. It bundles the
  `sidebar_menu_action()` factory and its variation contract, the
  `remove_action()` factory, and the shared `require_valid_id()` /
  `resolve_input_port()` helpers. Defines the structure every menu action
  handler is expressed against and the behaviour each must preserve.

### Modified Capabilities
<!-- None. This is an internal blockr.dock refactor; the blockr.ui
     block-browser / stack-menu / link-menu module contracts (committed-spec
     shapes, pool-update protocol) are untouched. -->

## Impact

- **Affected packages.** `blockr.dock` only. No change to `blockr.ui` or
  `blockr.core`.
- **Affected code.** `R/action-block.R`, `R/action-link.R`,
  `R/action-stack.R` rewritten as thin wrappers; a new `R/action-menu.R`
  (or similar) holds the factory + shared helpers.
- **Affected APIs.** None public. The new helpers are unexported
  (`@keywords internal` / no `@export`), consistent with the team's
  minimal-scope preference. Exporting `sidebar_menu_action()` for
  out-of-tree extension authors (extensions can ship their own actions via
  `board_actions.dock_extension`) is flagged as an open question and
  deferred until a real consumer exists.
- **Affected dependencies.** None.
- **Affected behaviour.** None. Pure refactor, verified by the unchanged
  test suites and a clean `devtools::check()` / `lintr`.
- **Related (out of scope).** `add-link-menu`'s `design.md` already defers a
  separate `blockr.ui`-side "card-list primitive" factor-out (shared card
  CSS / JS / R builders across block-browser, stack-menu, link-menu). That
  is a different redundancy in a different package and is not absorbed here.
