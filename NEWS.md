# blockr.ui (development version)

* The block's mark is drawn here, in the design system's four sizes (#65).
  In R, `block_mark()` builds it from a block's glyph and category as the
  `.blockr-mark` class: 24px, or 32, 20 and 16px through its `--32`, `--20`
  and `--16` modifiers, with the glyph half the mark plus 2px and the corners
  a quarter of it. For a place that needs an image, such as the DAG's canvas,
  `block_mark_svg()` draws the same mark as an SVG or its `data:` URI. The
  category colours are tokens, `--blockr-category-<category>`, with the
  spec's fixed Okabe-Ito values, and blockr.ui owns them in place of
  blockr.dock's `blk_color()`. In R, `category_color()` reads them. A
  `Blockr.menu` row's `mark` takes the block's `category` and draws the same
  mark at 24px, in place of the menu's own `.blockr-menu__mark` class and its
  `--blockr-menu-mark` property.

* The new `small_icon()` draws the design system's small icons in markup
  built in R (#64), from the list `Blockr.icons` is now built from:
  `controls_dep()` writes the list into the page ahead of `blockr-ui.js`,
  so R and JavaScript draw one copy of each. The icons are no longer part
  of `blockr-ui.js`, so a page that loads that file on its own, as a test
  harness might, has none. Every icon is hidden from screen readers, and
  the plus is a 1.25px stroke now, as blockr.dock's views menu draws it, in
  place of Bootstrap's filled path. The set gains `grip`, the drag handle
  of that menu's page rows.

* The warning border, `--blockr-color-border-warning`, is amber-600
  (`#d97706`) in the light scheme, a new step of the amber ramp, up from
  amber-500: a warning border, and a status dot drawn in one, now clears 3:1
  on white (WCAG 1.4.11). A test holds icons and status borders to 3:1 on the
  surface in both schemes.

* Escape and a click outside go through one dismiss stack, `Blockr.layer()`
  (#49). Every control that opens something registers it as a layer: the
  Select's list and its expanded tags, the code field's completions, both
  menus of actions, the tooltip, a dirty `Blockr.textCommit()` field and the
  gear tray. Escape closes the top layer only, and a click, read on
  pointerdown, closes the layers above the one it lands in, so a Select in
  the gear tray or a menu over a modal closes alone. While a tooltip shows,
  the next Escape hides it and leaves what is under it open. The
  `Blockr.onDocClick()` registry is gone; a package's own floating UI
  registers with `Blockr.layer()` instead.

* The design system's menu of actions, `Blockr.menu()`, joins
  `controls_dep()`: rows with an icon, label and meta text, dividers, group
  titles, a head (a name with a badge and a line of text), current, disabled
  and destructive rows, placed with `Blockr.place()` and driven from the
  keyboard. A trigger is wired with `Blockr.menu.bind()`. Its classes also
  style menus built elsewhere.

* A `Blockr.tooltip` line can carry a `badge`.

* The new `action_menu()` builds a menu of actions opened by a button:
  downloads, Rename, Remove. Its rows are `menu_item()`s wrapping a
  `downloadLink()`, an `actionLink()` or any link or button, with
  `menu_section()` titles and `menu_divider()` rules between them. A row
  does one thing and the menu closes; unlike `Blockr.Select.menu()` it sets
  no value. Open, the list sits on the page body, so no panel's overflow
  clips it.

* The new `tool_button()` is the design system's 26px icon button, named
  by a tooltip.

* An element built in R can carry its tooltip as a `data-blockr-tooltip`
  attribute; `Blockr.tooltip` shows it as the light card, in place of the
  native `title` box.

* Text marked `data-blockr-editable` shows the text cursor and a tooltip
  naming the gesture, "Double-click to edit" unless the attribute names
  another.

* The new `shortcut()` writes a keyboard hint once for every platform:
  `shortcut("Mod+Shift+S")` reads ⌘⇧S on a Mac and Ctrl+Shift+S elsewhere.
  It fits the meta slot of a `menu_item()`.

* The table preview draws its header in the table style: the sort cue sits
  on the name's line, a numeric column's header sits over its numbers, and
  a sorted header states its order in a tooltip and in `aria-sort`. A label
  or value cut off by its column shows whole in the light card rather than
  a native `title`.

* With `theme_dep()` in the page, Bootstrap's buttons take the design
  system's kinds: `.btn-primary` is the main button, `.btn-default` (Shiny's
  `actionButton()` and `downloadButton()`), `.btn-secondary` and
  `.btn-light` the secondary one, `.btn-link` the quiet one and
  `.btn-danger` the destructive one. Markup built for blockr uses the
  `.blockr-btn` classes, in three sizes.

* The design tokens are rewritten around the vocabulary of the design spec:
  a colour palette, meaning tokens for text, backgrounds, borders and status,
  type, radii, control heights and shadows. Every name defined before keeps
  its old value.

* The tokens include a stacking scale, `--blockr-z-sticky` up to
  `--blockr-z-toast`, at Bootstrap's own z-index values. Anything that floats
  over the page takes one of these layers; inside a component, a z-index only
  orders siblings, from -1 to 3.

* The `theme_dep()` dependency also attaches `blockr-tokens-dark.css`, which
  restates the tokens under `data-bs-theme="dark"`, the attribute that
  blockr.core's dark-mode board option sets. In dark, Bootstrap's body
  background and text colour follow the tokens too.

* The `theme_dep()` function returns two dependencies in a `tagList()`, the
  tokens and the theme layer, so a component can carry the tokens without
  restyling the rest of the page.

* The `controls_dep()` dependency carries the controls blockr blocks are
  built from, moved here from blockr.dplyr: `Blockr.Select`, the Enter
  button, the required-empty cue, the checkbox, the segmented control, the
  gear tray and `Blockr.place`, with the stylesheets they draw with. It
  brings the tokens but not the theme layer, which an app attaches with
  `theme_dep()`. The dependency names are the ones blockr.dplyr used, so a
  page carries one copy of each; while blockr.dplyr still ships its own
  copies, that copy is blockr.dplyr's, as htmltools keeps the higher version
  of a name.

* `controls_dep()` also carries `Blockr.Input`, the code field with column
  and function completions, moved here from blockr.dplyr under its old
  dependency names (`blockr-input-js`, `blockr-input-css`).

# blockr.ui 0.0.1

* The `shiny_has_perf_dep()` dependency strips a redundant `:has(> *)` guard
  out of the rule Shiny uses to fade a pass-through `uiOutput()` while it
  recalculates. That guard sits in non-subject position with a universal
  subject, which makes Chrome restyle the whole document on every DOM
  insertion: 107ms per insertion on a 40-block dock board against 3ms
  without it, growing linearly with element count, and paid by every block
  re-render and every keystroke in a picker. Removing it is provably
  behaviour-neutral, since reaching the subject already proves the guarded
  container has an element child, so both the `display: contents`
  pass-through and the fade survive. Attach it once at the page level.

* One `var()` fallback had drifted from the token it backs: the
  `.blockr-type-label` rule wrote `#b0b7c3` where
  `--blockr-color-text-subtle` resolves to `#9ca3af`, which the two other
  readers of that token in the same file already spelled correctly. It
  rendered the wrong grey in any host that does not attach `theme_dep()`.

* The block browser, sidebar, link menu and stack menu are gone, along
  with `is_hex_color()` -- 19 of the 28 exports. Package blockr.dock
  vendored a copy of all four in July 2025 and has developed on them
  since (a link edit form, a native colour input, reworked panel
  ownership, and a fifth `sidebar-inputs` member that never existed
  here), while these copies had not been touched since June. Nothing
  outside this package used them, and both copies styled the same class
  names, so whichever stylesheet loaded last decided the result.
  Coverage of the surviving copy's client behaviour was extended first,
  in BristolMyersSquibb/blockr.dock#440.

* The `Suggests` on blockr.dock goes with them, removing the last
  non-CRAN entry from `Remotes:` and simplifying a CRAN release.

* The `theme_dep()` dependency carries the shared blockr stylesheet: the
  `--blockr-*` design tokens in a `:root` block, and the unscoped
  Bootstrap theme layer - typography, labels, form controls, selectize,
  buttons, tooltips, popovers and the DataTables chrome - that blockr
  apps have so far picked up from `blockr.dock`. A host attaches it once
  from its own UI. Nothing in this package attaches it for you, and the
  token block on its own styles nothing.

* `html_table_display` wires the HTML table preview into blockr.core's
  `tabular_display` seam (blockr.core >= 0.1.4). Apps opt in with
  `options(blockr.tabular_display = blockr.ui::html_table_display)` to
  preview data, parser and transform block results through the
  paginated, sortable HTML table rather than the default minimal
  preview. This supersedes the never-read `blockr.html_table_preview`
  option.

* `link_menu_server()` and the block browser now resolve a variadic
  target's new-link input to an empty (positional) slot rather than a
  generated integer name (`"1"`, `"2"`, ...). This aligns with
  blockr.core's name-or-position variadic input model, where an integer
  input is a *named* argument - so the old convention quietly named what
  should be positional.

* `sidebar_ui()` panels now re-bind their body inputs/outputs on open
  (`Shiny.bindAll`). `hidePanel` unbinds on close, so a pre-rendered
  panel opened via `show_sidebar(id)` with no `ui` (no body swap) stayed
  unbound after its first close and silently stopped emitting. This broke
  the add / append block browser, which could only commit once.

* `block_browser_server(id, board, target)` now returns a ready-to-apply
  value instead of a raw spec: a `blockr.core` `blocks` object for the
  add flow, or `list(blocks, links)` for append / prepend with the link's
  input port resolved menu-side. It builds the block and validates the
  committed ids when given a `board` reactive, matching
  `link_menu_server()` / `stack_menu_server()`. `block_browser_ui()` no
  longer bakes board-seeded default ids into the markup, so the add-flow
  panel is independent of board state and can be pre-rendered once and
  opened without re-rendering. `append_to()` / `prepend_to()` now accept
  a `NULL` block id (a source-less descriptor), so an append / prepend
  panel can likewise be pre-rendered once with the source / target
  supplied server-side at commit. (Breaking change for the
  committed-value shape.)

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
