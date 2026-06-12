## Why

The stack menu and link menu render **board instances** (existing blocks,
links, stacks), so their content is a function of the current board. But
they are rendered as a one-time snapshot: `*_menu_ui(id, board, ...)` is
called once when the sidebar opens, and nothing keeps it current. When the
board changes while the sidebar is pinned open, the menu goes stale:

- **Stack menu.** Open "Create stack", pin the sidebar, then remove a block
  from the board — the removed block is still offered as a candidate card.
- **Link menu.** Two blocks are linked, so the target has no free input and
  the OUTGOING pool is empty. Pin the sidebar, then remove the existing
  link — the target frees up but the menu still shows its empty-state. Or
  remove a block entirely — its card should disappear from the menu.

The block browser is immune because it renders registry **types**, which
don't depend on board state. Stack and link don't have that luxury.

A first attempt fixed this in `blockr.dock` by having each action
`observeEvent(board$board, ...)` and imperatively reconcile the open menu:
ownership bookkeeping (`claim_sidebar` / `sidebar_owner`) to avoid
clobbering a sibling action's form, `rendered_ids` staleness tracking, and
a grow-vs-shrink branch (full re-render when a card must appear,
`pool-update` when one must disappear). Three mechanisms, in the consumer
layer, about to be copy-pasted into the stack action. The root cause is
treating a board-dependent view as a snapshot the *consumer* patches, when
it should be a **reactive component that keeps itself current**.

## What Changes

- **MODIFIED (`blockr.ui`)** `link_menu_server(id, board)` and
  `stack_menu_server(id, board)` gain a `board` argument: a **reactive**
  returning the current board. The module `observe()`s it and pushes a
  self-scoped sync diff to its own binding whenever the board changes -
  inserting newly-eligible/new-instance cards, removing vanished ones,
  toggling per-card eligibility, and refreshing per-card input-port options
  - **without** re-rendering the panel, so scroll position, card-expansion,
  the search query, and in-progress per-card / panel-level inputs are
  preserved. `board` defaults to `NULL` for backward compatibility (no
  argument = today's static snapshot behaviour).
- **MODIFIED (`blockr.ui`)** The link menu's `pool-update`
  `receiveMessage` protocol is generalised to a `menu:sync` reconciliation
  that can **add** card nodes (not just hide/show ones already in the DOM),
  using card markup carried in the message. The stack menu gains the same
  reconciliation. The shared client logic lives alongside the existing card
  helpers on `window.BlockrUI`.
- **MODIFIED (`blockr.dock`)** `add_link_action`, `add_stack_action`, and
  `edit_stack_action` pass `reactive(board$board)` into the menu server and
  **delete** all board-sync orchestration: the per-action
  `observeEvent(board$board, ...)`, `rendered_ids`, `record_render`,
  `push_pool_update`, the deferred post-commit `pool-update` /
  `keep_or_hide_sidebar` rebuilds, and the entire `claim_sidebar` /
  `sidebar_owner` / `sidebar_owner_store` apparatus in `R/action-utils.R`
  (a self-scoped module diff cannot clobber a sibling's slot, so ownership
  tracking is no longer needed).
- **NEW BEHAVIOUR.** A pinned stack/link menu now tracks board mutations
  made anywhere (context-menu block/link removal, other actions, undo): new
  candidates appear, gone ones disappear, freed inputs re-open, all live.
- **MODIFIED (`blockr.ui`)** — incorporating PR #172 review feedback
  ("validators could live in blockr.ui; pass `board` and `session` to the
  stack menu server"). The instance-menu servers also gain a `session`
  argument and take ownership of **validation**: the committed reactive
  fires only with an already-validated payload, having checked id-uniqueness
  against the board, name presence, and (stack) hex-colour format, and
  having surfaced the failure via `notify()` itself. The committed value is
  enriched from a raw spec to a **validated result** so the consumer no
  longer re-validates. Because `blockr.ui` depends on `blockr.core` (not
  `blockr.dock`), the stack menu returns a `blockr.core` `stack` / validated
  spec, NOT a `dock_stack`; the dock performs the thin `dock_stack` upgrade
  (open question: inject `new_dock_stack` so the menu can return it
  directly).
- **MODIFIED (`blockr.dock`)** The stack / link commit handlers shrink to
  "take the validated result → (stack) upgrade to `dock_stack` → `update()`".
  The dock-side validators (`valid_stack_id` / `valid_stack_name` /
  `valid_stack_color`, the id-collision link/block validators) are removed
  in favour of the blockr.ui-owned validation. This reconciles with
  `refactor-dock-action-handlers`: its `require_valid_id()` consolidation
  shrinks to at most the block-browser id case (the block browser is
  board-independent), since stack/link validation now lives upstream.

## Capabilities

### New Capabilities
- `menu-board-sync`: the board-reactive contract for the `blockr.ui`
  instance-backed menus - the `board` reactive argument on
  `*_menu_server()`, the module-owned `observe(board())` that recomputes the
  menu's desired card set + eligibility and emits a self-scoped `menu:sync`
  message, and the client-side reconciliation that adds / removes / retunes
  cards in place without re-rendering or disturbing user state.

### Modified Capabilities
<!-- The link-menu and stack-menu capabilities (specs/link-menu,
     specs/stack-menu) are extended by `menu-board-sync` rather than
     re-specified here; their committed-spec shapes are unchanged. Those
     specs are not yet archived into openspec/specs/, so this change adds
     the new capability and references them. -->

## Impact

- **Affected packages.** `blockr.ui` (menu servers + JS reconciliation) and
  `blockr.dock` (drop sync + ownership, pass the board reactive).
- **Affected APIs.** `link_menu_server()` / `stack_menu_server()` gain a
  `board = NULL` argument (additive, backward-compatible). The dock-internal
  `claim_sidebar()` / `sidebar_owner()` helpers are removed.
- **Affected behaviour.** Pinned instance-menus now stay in sync with the
  board. The committed-spec shapes, eligibility rules, and empty-state
  semantics are unchanged.
- **Affected dependencies.** None.
- **Sequencing.** Lands before (or alongside) `refactor-dock-action-handlers`:
  removing the per-action board-sync code shrinks exactly what that refactor
  would otherwise factor out, so the post-commit `after_commit` strategies in
  the factory simplify (the link flow no longer needs a bespoke pool-update
  hook - the module's board observer absorbs post-commit refresh too).
- **Related (out of scope).** The `blockr.ui` card-list primitive factor-out
  (deferred in `add-link-menu`) and the `dock-action-handlers` scaffold
  refactor remain separate changes.
