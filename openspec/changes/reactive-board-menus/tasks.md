> Status: stack (blockr.ui #9 / blockr.dock #172) and link (blockr.ui #10 /
> blockr.dock #183) shipped. Notes below record where the implementation
> deviated from the original plan.

## 1. blockr.ui - shared client reconciliation

- [x] 1.1 Generalise the link menu's `pool-update` handler into a
  `menu:sync` reconciliation on `window.BlockrUI`: remove cards gone from the
  payload, insert cards present in the payload but absent from the DOM (from
  carried `html`, into the right category / direction section), retune
  surviving cards (port `<select>` rebuild preserving a valid selection),
  recompute empty-state.
  Note: `BlockrUI.cardSync(cats, cards)` does the structural add / remove and
  takes a categories *container* (so the link menu drives it once per
  direction section, creating / dropping a section as its pool grows /
  drains); eligibility is expressed by card *presence* (cards = the eligible
  set) rather than a `.hidden` class. `pool-update` is kept as a legacy
  alias.
- [x] 1.2 Guarantee the reconciliation never touches scroll position,
  per-card expansion, the search input, or user-edited panel-level / per-card
  inputs. (Verified end-to-end via chromote for both menus.)
- [x] 1.3 Point both the `blockr.ui.linkMenu` and `blockr.ui.stackMenu`
  bindings' `receiveMessage` at the `menu:sync` handler (link keeps accepting
  the legacy `pool-update` type as an alias).

## 2. blockr.ui - board-reactive servers

- [x] 2.1 Add `board = NULL` to `link_menu_server()` and
  `stack_menu_server()` (plus `anchor` on the link server, `target` on the
  stack server).
  Note: no `session` argument was needed - the module's own `moduleServer`
  session is used for `blockr.core::notify()` and `sendInputMessage`.
- [x] 2.2 When `board` is a reactive, `observeEvent(board(), ...,
  ignoreInit = TRUE)` recomputes the desired card set and emits `menu:sync`
  to the bare `"commit"` input (the module session namespaces it; passing
  `session$ns("commit")` would double-prefix - a bug caught in testing). The
  anchor / target arrive as reactive args.
- [x] 2.3 Reuse `link_eligible_pools()` for the link side; the stack side
  reuses the existing `resolve_stack_target()` / `stack_eligible_blocks()`
  (no new helper needed).
- [x] 2.4 Build the `menu:sync` payload from the R card builders
  (`link_block_card()` / `stack_block_card()`) so new-card `html` matches the
  initial render exactly (no parallel JS template). `link_card_meta()` was
  extracted so the initial render and the payload share one meta builder.

## 3. blockr.ui - menu-owned validation + validated return (PR #172)

- [x] 3.1 Validation moved into the committed reactive of both servers: stack
  = id-uniqueness vs `board_stack_ids()` (create) + name presence + hex
  colour; link = id-uniqueness vs `board_link_ids()`. On failure the module
  `blockr.core::notify()`s and `req(FALSE)`s, so the committed reactive never
  fires invalid. Reuses `blockr.core::is_string()` / `notify()`.
  Note: validation is gated on a board reactive being supplied; `board = NULL`
  keeps the un-validated snapshot behaviour (back-compat).
- [x] 3.2 Decision 7 resolved: both menus return a **validated spec** (the
  existing list shape, now guaranteed valid), NOT a constructed domain
  object. The consumer still builds `new_link()` / `new_dock_stack()` - this
  avoids the `dock_stack`-colour layering inversion and the create/edit
  asymmetry. `ctor =` injection was not taken.
- [x] 3.3 Unit tests added: stack + link validation-rejection paths and
  `*_sync_payload()` (cards add / drop across a board change, direction +
  html carried).
  Note: end-to-end behaviour (insert / remove without re-render, inputs
  preserved) was verified via chromote rather than committed shinytest2 -
  shinytest2 coverage is a possible follow-up.

## 4. blockr.dock - drop sync + ownership, thin the handlers

- [x] 4.1 `add_link_action` passes `board` + `anchor` reactives;
  `add_stack_action` / `edit_stack_action` pass `board` (+ `target` on edit).
  No `session` passed (see 2.1).
- [x] 4.2 The committed `add_link_action` had a deferred post-commit
  pool-update (`link_eligible_pools` recompute + `sendInputMessage`); it is
  removed - the menu's own board observer now refreshes on the commit's board
  change.
  Note: the `rendered_ids` / `record_render` / `push_pool_update` /
  `observeEvent(board$board)` prototype was never committed (it was the
  "smell" - stashed and discarded), so there was nothing to delete there.
- [ ] 4.3 NOT done as written. `keep_or_hide_sidebar` is **kept** in the
  stack handlers: it owns the commit lifecycle (pinned -> re-render with a
  fresh seeded id; unpinned -> close), while the board observer handles
  external changes. Deleting it would need `menu:sync` to also re-seed the
  stack id. The link handler instead closes only when unpinned (no
  re-render), letting the board observer drive the pinned multi-link session.
- [x] 4.4 N/A - `claim_sidebar()` / `sidebar_owner()` / `sidebar_owner_store()`
  lived only in the discarded prototype; they were never committed, so the
  shipped handlers carry no ownership apparatus.
- [x] 4.5 Handlers shrunk to thin adapters: link resolves the target port
  then `update(links = add new_link(...))`; stack builds `new_dock_stack()`
  then `update(stacks = ...)`. Dock-side `valid_stack_*` removed;
  `valid_link_id` removed from `action-link.R` (it stays in `action-block.R`
  for the append / prepend block flow, which this change does not touch).
- [x] 4.6 `test-action-link.R` / `test-action-stack.R` updated (duplicate id
  now rejected upstream by the menu; no dock-side validation asserted). The
  prototype's "removing a link re-renders the open menu" test was never
  committed.

## 5. Verify + reconcile

- [ ] 5.1 Full dock right-click manual smoke (DAG repro app) left to the
  maintainer's review. The blockr.ui mechanism is verified end-to-end via
  chromote: stack (remove block -> card drops, selection preserved; add block
  -> card inserted) and link (remove link -> freed target's card inserted;
  remove block -> card removed), no re-render.
- [x] 5.2 `lintr` clean on all changed files (CI config); test suites green
  (blockr.ui 280, blockr.dock 624). `devtools::check()` run clean on
  blockr.dock for the stack PR (0/0, timestamp + `CLAUDE.local.md` notes); a
  full check on the link branches is worth a final pass before merge.
- [ ] 5.3 Reconcile `refactor-dock-action-handlers`: stack/link validation
  now lives in `blockr.ui`, so its `require_valid_id()` shrinks to at most the
  block-browser id case. Pending.
- [x] 5.4 NEWS updated in both packages for the stack and link changes.
- [x] 5.5 `openspec validate reactive-board-menus --strict` clean.
