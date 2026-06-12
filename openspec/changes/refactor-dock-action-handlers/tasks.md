## 1. Shared scaffold + helpers (`R/action-menu.R`)

- [ ] 1.1 Create `R/action-menu.R` and add `'action-menu.R'` to the
  `Collate` field in `DESCRIPTION` (before `action-block.R` /
  `action-link.R` / `action-stack.R`).
- [ ] 1.2 Implement `sidebar_menu_action(id, server, ui, title, on_commit,
  after_commit = NULL, module_id = "menu")`: derive `sidebar_id`, mount the
  module once via `server(module_id)`, build the `*_ui()` closure over
  `ui(session$ns(module_id), board$board, trigger())`, resolve `title`
  (string or `function(trigger)`), show on `trigger()`, and observe the
  committed reactive - running `on_commit(...)` then the rebuild hook only
  when `on_commit` returns `TRUE`. `force()` every captured argument; return
  `NULL`.
- [ ] 1.3 Implement the rebuild hooks: `default_after_commit` (deferred
  `onFlushed(once = TRUE)` → `keep_or_hide_sidebar`) and a
  `sync_after_commit` (immediate `keep_or_hide_sidebar`). Give all hooks one
  signature `function(session, sidebar_id, board, trigger, ui_fn, title_fn)`.
- [ ] 1.4 Implement `require_valid_id(id, existing, what, session)` with the
  message `"Please choose a valid <what> ID."`.
- [ ] 1.5 Implement `resolve_input_port(block, block_id, links, explicit =
  NULL)` returning `explicit` when non-empty else
  `block_input_select(block, block_id, links, mode = "inputs")[1L]`.
- [ ] 1.6 Implement `remove_action(id, kind)` returning an action that
  observes `trigger()` and calls `update(list(<kind> = list(rm =
  trigger())))`.

## 2. Rewrite the block handlers (`R/action-block.R`)

- [ ] 2.1 Re-express `add_block_action` as a `sidebar_menu_action()` call:
  `server = function(m) blockr.ui::block_browser_server(m)`, `ui =
  function(ns, board, trg) blockr.ui::block_browser_ui(ns, board)`, `title =
  "Add new block"`, `module_id = "browser"`, `after_commit =
  sync_after_commit`. Move its validate/build/`update()` body into the
  `on_commit` (returning `TRUE`/`FALSE`); route id checks through
  `require_valid_id(..., "block", ...)`.
- [ ] 2.2 Re-express `append_block_action` likewise with `ui` passing
  `blockr.ui::append_to(trg)` and an `on_commit` that builds block + link;
  resolve the port via `resolve_input_port(new_blk, spec$id, links,
  spec$block_input)`; id + link-id checks via `require_valid_id`.
- [ ] 2.3 Re-express `prepend_block_action` likewise with
  `blockr.ui::prepend_to(trg)` and `resolve_input_port(board_blocks[[trg]],
  trg, links, spec$target_input)`.
- [ ] 2.4 Re-express `remove_block_action` as `remove_action(
  "remove_block_action", "blocks")`.
- [ ] 2.5 Delete the now-dead `valid_block_id`, the local `valid_link_id`
  copy, and the inlined port-resolution blocks; keep
  `build_block_from_spec`. Run `test-action-block.R` - must pass unchanged.

## 3. Rewrite the link handler (`R/action-link.R`)

- [ ] 3.1 Re-express `add_link_action` as a `sidebar_menu_action()` call:
  `server`/`ui` from `link_menu_*`, `ui` passing `anchor = trg`, `title =
  function(trg) paste0("Connect ", trg)`, and an `after_commit` closure that
  defers, recomputes `link_eligible_pools()`, and either `hide_sidebar()` or
  sends the `menu-commit` pool-update (NOT `keep_or_hide_sidebar`).
- [ ] 3.2 Move the link `on_commit` body in: `require_valid_id(spec$link_id,
  board_link_ids(board), "link", session)`, `resolve_input_port(trg_blk,
  spec$target, links, spec$block_input)`, build + `update()`, return `TRUE`.
- [ ] 3.3 Re-express `remove_link_action` as `remove_action(
  "remove_link_action", "links")`. Delete the duplicate `valid_link_id`.
  Run `test-action-link.R` - must pass unchanged.

## 4. Rewrite the stack handlers (`R/action-stack.R`)

- [ ] 4.1 Re-express `add_stack_action` via `sidebar_menu_action()` with the
  default (deferred keep-or-hide) `after_commit`; route the id check through
  `require_valid_id(..., "stack", ...)`; keep `valid_stack_name` /
  `valid_stack_color` calls in the `on_commit`.
- [ ] 4.2 Re-express `edit_stack_action` with `title = function(trg)
  paste0("Edit stack ", trg)`, `ui` passing `target = trg`, default
  `after_commit`.
- [ ] 4.3 Re-express `remove_stack_action` as `remove_action(
  "remove_stack_action", "stacks")`. Keep `valid_stack_name` /
  `valid_stack_color`; drop `valid_stack_id` in favour of
  `require_valid_id`. Run `test-action-stack.R` - must pass unchanged.

## 5. Verify

- [ ] 5.1 `devtools::document()` then confirm `NAMESPACE` gains no new
  exports relative to the pre-refactor state.
- [ ] 5.2 `devtools::test()` - the three action suites and the full suite
  pass with no test edits.
- [ ] 5.3 `devtools::check()` clean (0/0, modulo the timestamp /
  `CLAUDE.local.md` NOTEs) under a UTF-8 locale.
- [ ] 5.4 `lintr::lint_package()` clean under the CI config
  (`linters_with_defaults(object_name_linter = NULL)`) - no new hits in the
  rewritten files.
- [ ] 5.5 NEWS bullet in `blockr.dock` noting the internal action-handler
  consolidation (no user-facing behaviour change).
- [ ] 5.6 `openspec validate refactor-dock-action-handlers --strict` clean.
