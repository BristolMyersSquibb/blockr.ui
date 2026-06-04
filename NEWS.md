# blockr.ui 0.0.0.9000

* New `link_menu_ui()` / `link_menu_server()` / `link_menu_dep()`
  module: a bidirectional card-list link picker. Cards represent both
  OUTGOING ("CONNECT TO") and INCOMING ("CONNECT FROM") candidates
  for a fixed `anchor` block, gated by the anchor's free-input
  capacity. Single-shot click-to-add, per-card chevron-revealed
  advanced form (`link_id` + `block_input` when the target end has
  arity > 1). The binding's `receiveMessage` accepts a `pool-update`
  payload so consumers can keep the menu open across multiple link
  commits in a session; just-wired cards drop client-side without
  re-rendering. `link_eligible_pools(board, anchor)` is exported so
  consumers recompute the post-commit pool against the same
  eligibility logic the menu uses for its initial render.
  `link_menu_server()` gains `board` (reactive) and `anchor` arguments:
  it owns link-id validation (via `blockr.core::notify()`) and, when
  passed a board reactive, keeps an open menu in sync with the board via
  a `menu:sync` diff that supersedes `pool-update` - it can now also
  *add* a card that became eligible (e.g. after a link / block was
  removed elsewhere), not just hide ones already rendered, all without
  re-rendering.
* New `stack_menu_ui()` / `stack_menu_server()` / `stack_menu_dep()`
  module: a multi-select card-list block picker for stacks, with an
  inline hue / lightness slider + hex colour picker and a panel-level
  form for the stack name / color / id. `target = NULL` is the create
  flow; `target = "<stack_id>"` selects the edit flow. Mirrors
  `block_browser_ui()`'s `target` argument shape.
  `stack_menu_server()` gains `board` (reactive) and `target`
  arguments: it now owns validation of the committed spec
  (id / name / colour, via `blockr.core::notify()`) and, when passed a
  board reactive, keeps an open menu in sync with the board - cards are
  added / removed live via a `menu:sync` diff with no re-render, so
  scroll, selection, and in-progress inputs are preserved. The committed
  reactive now returns a `blockr.core` `stacks` object (one id-keyed
  stack built via `new_stack()`, colour carried as an attribute) rather
  than a raw list, so a consumer applies it without reshaping.
* New exported `is_hex_color()` helper (`#rgb` / `#rrggbb`) so
  consumers validate colours against the same rule the stack menu uses.
* Initial package scaffold.
