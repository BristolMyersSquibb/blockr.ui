# Changelog

## blockr.ui 0.0.0.9000

- The block browser, sidebar, link menu and stack menu are gone, along
  with `is_hex_color()` – 19 of the 28 exports. Package blockr.dock
  vendored a copy of all four in July 2025 and has developed on them
  since (a link edit form, a native colour input, reworked panel
  ownership, and a fifth `sidebar-inputs` member that never existed
  here), while these copies had not been touched since June. Nothing
  outside this package used them, and both copies styled the same class
  names, so whichever stylesheet loaded last decided the result.
  Coverage of the surviving copy’s client behaviour was extended first,
  in BristolMyersSquibb/blockr.dock#440.

- The `Suggests` on blockr.dock goes with them, removing the last
  non-CRAN entry from `Remotes:` and simplifying a CRAN release.

- The
  [`theme_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/theme_dep.md)
  dependency carries the shared blockr stylesheet: the `--blockr-*`
  design tokens in a `:root` block, and the unscoped Bootstrap theme
  layer - typography, labels, form controls, selectize, buttons,
  tooltips, popovers and the DataTables chrome - that blockr apps have
  so far picked up from `blockr.dock`. A host attaches it once from its
  own UI. Nothing in this package attaches it for you, and the token
  block on its own styles nothing.

- `html_table_display` wires the HTML table preview into blockr.core’s
  `tabular_display` seam (blockr.core \>= 0.1.4). Apps opt in with
  `options(blockr.tabular_display = blockr.ui::html_table_display)` to
  preview data, parser and transform block results through the
  paginated, sortable HTML table rather than the default minimal
  preview. This supersedes the never-read `blockr.html_table_preview`
  option.

- `link_menu_server()` and the block browser now resolve a variadic
  target’s new-link input to an empty (positional) slot rather than a
  generated integer name (`"1"`, `"2"`, …). This aligns with
  blockr.core’s name-or-position variadic input model, where an integer
  input is a *named* argument - so the old convention quietly named what
  should be positional.

- `sidebar_ui()` panels now re-bind their body inputs/outputs on open
  (`Shiny.bindAll`). `hidePanel` unbinds on close, so a pre-rendered
  panel opened via `show_sidebar(id)` with no `ui` (no body swap) stayed
  unbound after its first close and silently stopped emitting. This
  broke the add / append block browser, which could only commit once.

- `block_browser_server(id, board, target)` now returns a ready-to-apply
  value instead of a raw spec: a `blockr.core` `blocks` object for the
  add flow, or `list(blocks, links)` for append / prepend with the
  link’s input port resolved menu-side. It builds the block and
  validates the committed ids when given a `board` reactive, matching
  `link_menu_server()` / `stack_menu_server()`. `block_browser_ui()` no
  longer bakes board-seeded default ids into the markup, so the add-flow
  panel is independent of board state and can be pre-rendered once and
  opened without re-rendering. `append_to()` / `prepend_to()` now accept
  a `NULL` block id (a source-less descriptor), so an append / prepend
  panel can likewise be pre-rendered once with the source / target
  supplied server-side at commit. (Breaking change for the
  committed-value shape.)

- New `link_menu_ui()` / `link_menu_server()` / `link_menu_dep()`
  module: a bidirectional card-list link picker. Cards represent both
  OUTGOING (“CONNECT TO”) and INCOMING (“CONNECT FROM”) candidates for a
  fixed `anchor` block, gated by the anchor’s free-input capacity.
  Single-shot click-to-add, per-card chevron-revealed advanced form
  (`link_id` + `block_input` when the target end has arity \> 1). The
  binding’s `receiveMessage` accepts a `pool-update` payload so
  consumers can keep the menu open across multiple link commits in a
  session; just-wired cards drop client-side without re-rendering.
  `link_eligible_pools(board, anchor)` is exported so consumers
  recompute the post-commit pool against the same eligibility logic the
  menu uses for its initial render. `link_menu_server()` gains `board`
  (reactive) and `anchor` arguments: it owns link-id validation (via
  [`blockr.core::notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.html))
  and, when passed a board reactive, keeps an open menu in sync with the
  board via a `menu:sync` diff that supersedes `pool-update` - it can
  now also *add* a card that became eligible (e.g. after a link / block
  was removed elsewhere), not just hide ones already rendered, all
  without re-rendering.

- New `stack_menu_ui()` / `stack_menu_server()` / `stack_menu_dep()`
  module: a multi-select card-list block picker for stacks, with an
  inline hue / lightness slider + hex colour picker and a panel-level
  form for the stack name / color / id. `target = NULL` is the create
  flow; `target = "<stack_id>"` selects the edit flow. Mirrors
  `block_browser_ui()`’s `target` argument shape. `stack_menu_server()`
  gains `board` (reactive) and `target` arguments: it now owns
  validation of the committed spec (id / name / colour, via
  [`blockr.core::notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.html))
  and, when passed a board reactive, keeps an open menu in sync with the
  board - cards are added / removed live via a `menu:sync` diff with no
  re-render, so scroll, selection, and in-progress inputs are preserved.
  The committed reactive now returns a `blockr.core` `stacks` object
  (one id-keyed stack built via `new_stack()`, colour carried as an
  attribute) rather than a raw list, so a consumer applies it without
  reshaping.

- New exported `is_hex_color()` helper (`#rgb` / `#rrggbb`) so consumers
  validate colours against the same rule the stack menu uses.

- Initial package scaffold.
