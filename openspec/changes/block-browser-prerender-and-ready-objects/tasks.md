## 1. `blockr.ui`: board-independent UI + ready-object server

- [x] 1.1 In `R/block-browser.R`, stop seeding board-derived default block /
  link ids into the per-card markup (`browser_block_metas()` /
  `card_advanced()`). The card's id field ships with a client-side or empty
  default; the server resolves the real unique id at commit. Make
  `block_browser_ui(id, board = NULL, target = NULL)` so the *add* call site
  needs no board.
- [x] 1.2 Add an internal `resolve_free_input()`-equivalent (or lift /
  share the one in `R/link-menu.R`) so the block browser resolves the link
  port from the live board with `blockr.core` primitives only: explicit
  pick, else first free named slot, else a fresh integer slot for variadic
  targets.
- [x] 1.3 Add an internal block builder: `create_block(spec$type)` named
  from `spec$title`, with a board-unique id (`rand_names()` seeded from the
  live board's block ids when the committed id is empty / colliding).
- [x] 1.4 Rework `block_browser_server(id, board = NULL, target = NULL)` to
  return ready objects: a `blocks` object for *add*; `list(blocks, links)`
  for *append* / *prepend* with the resolved port. Strip the `nonce`; keep
  the `input$commit` event drive.
- [x] 1.5 When `board` is a reactive, validate the committed block id (and
  link id for append / prepend): notify + `req(FALSE)` on empty / duplicate,
  mirroring `validate_link_spec()` / the stack-menu validator.
- [x] 1.6 Update roxygen (`@param board`, `@param target`, `@return`) for
  both functions; regenerate `man/`.

## 2. `blockr.ui`: tests + examples

- [x] 2.1 Update `tests/testthat/test-block-browser.R`: the server now
  returns `blocks` / `list(blocks, links)`. Cover add (blocks object),
  append (blocks + link, resolved port, explicit port), prepend (arity-1
  implicit slot, arity-2 explicit `target_input`), and duplicate-id
  rejection with a board reactive.
- [x] 2.2 Add a test asserting the *add* markup is identical across two
  boards (board-independence), and that two commits against a pinned panel
  yield distinct board-unique ids.
- [x] 2.3 Update `inst/examples/block-browser/app.R` to the new contract
  (pass a `board` reactive; consume ready objects) and pre-render the add
  browser once via `sidebar_ui(ui = block_browser_ui(...))`.
- [x] 2.4 Update the block-browser shinytest2 suite for the pre-rendered
  open path (open / close / reopen with no re-render) where it asserts on
  the rendered panel.

## 3. `blockr.dock`: pre-render mount + thin handlers

- [x] 3.1 In `R/board-ui.R`, add a dedicated `sidebar_ui(NS(id,
  "add_block_sidebar"), ui = blockr.ui::block_browser_ui(NS(id, "add_browser")))`
  mount (rendered once); wire its open trigger via `data-blockr-sidebar-target`
  or `show_sidebar(id)`.
- [x] 3.2 Rewrite `add_block_action()` in `R/action-block.R` as a thin
  adapter: mount `block_browser_server(m, board = reactive(board$board))`
  once, and on commit `update(list(blocks = committed()))`. No
  `show_sidebar(ui = ...)`, no `build_block_from_spec()`.
- [x] 3.3 Rewrite `append_block_action()` / `prepend_block_action()` as thin
  adapters consuming `list(blocks, links)` from
  `block_browser_server(m, board = reactive(board$board), target =
  reactive(append_to/prepend_to(trigger())))`, applying both via `update()`.
  Remove the dock-side `block_input_select()` + `new_link()` assembly and the
  `valid_block_id()` / `valid_link_id()` pre-flight from these handlers.
- [x] 3.4 Remove now-dead dock helpers (`build_block_from_spec()`, and the
  block-flow uses of `block_input_select()`) if nothing else references
  them; keep helpers still used by link / stack flows.
- [x] 3.5 Update the dock action `testServer` suites
  (`test-action-block.R`) to drive `input$commit` and assert the ready
  objects flow through `update()` unchanged.

## 4. Reconcile + validate

- [x] 4.1 Reconcile `refactor-dock-action-handlers`: the block handlers now
  receive ready objects, so its `resolve_input_port()` and the block-flow
  scaffold shrink; update that change's design / tasks to match (or note the
  overlap is absorbed here).
- [x] 4.2 NEWS in both packages: `blockr.ui` (`block_browser_server()` now
  returns ready `blocks` / `links` objects and takes `board` / `target`;
  `block_browser_ui()` add-flow markup is board-independent and
  pre-renderable); `blockr.dock` (add browser pre-rendered once; add /
  append / prepend handlers are thin adapters).
- [x] 4.3 `devtools::check()` + `lintr::lint_package()` clean on `blockr.ui`.
- [x] 4.4 `devtools::check()` + `lintr::lint_package()` clean on
  `blockr.dock` after the handler rewrite.
- [ ] 4.5 Manual smoke test of the dock + dag example: open add browser
  (pre-rendered, no re-render on reopen), add a block; right-click → append
  / prepend, add with default and edited port; repeated pinned adds yield
  distinct ids.
