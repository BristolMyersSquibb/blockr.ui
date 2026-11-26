# blockr.ui 0.2.0

* The entry `@blockr/ui/icons` offers every icon of Lucide's package `lucide`
  1.75.0, under the names that `lucide` exports, with their aliases and the
  type `IconNode` (#113).
  It leaves out `icons`, the object that holds all of them, and the functions
  of `lucide`, such as `createIcons()`.
  The component `Icon` of `@blockr/ui` draws an icon, 14 pixels wide unless
  given a `size`, in the colour of the text around it.
  It hides the icon from screen readers unless given a `label`.
  The function `iconMarkup()` writes the same drawing as SVG markup, which is
  what a board action's icon is.
  The package `@blockr/ui` in `inst/types` names `lucide` as its dependency, so
  npm installs `lucide` with it.

* The build preset bundles the icons that a module imports from
  `@blockr/ui/icons`, for which the page's import map names no entry (#113).
  A package bundles them into its vendor file, since the vendor check fails a
  build that puts them into one of the package's own files.
  The preset fails a build that imports one of Lucide's packages itself, such
  as `lucide` or `lucide-react`, with an error that names `@blockr/ui/icons`.

* The vignette "Migration notes for app scripts" lists each change that an app
  script of blockr.ui 0.0.1 needs to run on version 0.2.0 (#117).
  It is written so that an AI can apply it to a script.
  A script drops the line that picks the table preview, and core's plain table
  draws every table output in its place.

* The widgets, the R helpers and the table preview are gone, with the
  stylesheets, icons and dependencies that they draw with (#107).
  The widgets on `window.Blockr`, such as `Blockr.Select` and `Blockr.menu()`,
  go with their types.
  The R helpers that draw into Shiny UIs go: `action_menu()` with
  `menu_item()`, `menu_section()` and `menu_divider()`, `block_mark()` with
  `block_mark_svg()` and `category_color()`, `dropdown()`, `gear_tray()` with
  `tray_section()`, `shortcut()`, `small_icon()` and `tool_button()`.
  The table preview goes with `html_table_display`, `html_table_render()`,
  `html_table_result()`, `table_page()`, `build_html_table()`,
  `column_widths_px()`, `apply_table_sort()`, `table_preview_dep()` and
  `table_preview_css()`.
  The dependencies `theme_dep()`, `controls_dep()`, `base_dep()`,
  `shiny_has_perf_dep()` and `shiny_input_batch_dep()` go, with the design
  tokens and the theme and base stylesheets.
  The small icons and the design system's article go as well, while the files
  of Open Sans stay.
  The package no longer imports blockr.core, dplyr, htmltools, pillar, rlang or
  shiny.

* The package ships the one copy of React and of React Aria that a page runs,
  which its client's build writes into `inst/js` from TypeScript in `srcts/`
  (#108).
  React's file holds React and ReactDOM, and an ES module beside it stands for
  each of React's entry points, such as `react.js`.
  React Aria's files hold the whole of `react-aria-components`, `react-aria`
  and `react-stately`, each as an entry of its own, with the code that they
  share in files of its own.
  The file of the components, `blockr-ui.js`, holds the package's own code.
  The build lists the entries of the page's import map in
  `inst/js/import-map.json`, and what each file imports up front in
  `inst/js/preload.json`.

* The package ships the build preset `@blockr/ui/build`, with which a package
  builds its modules against blockr.ui (#108).
  It ships in `inst/types`, as the npm package `@blockr/ui`, where core's
  `install_js_types()` finds it.
  The preset leaves each import of React's entry points, of `@blockr/ui` and of
  React Aria's three packages an import, which the page's import map resolves.
  It fails a build that imports an entry point of React that the import map
  does not name, a package under `@react-aria/` or `@react-stately/`, or an
  entry point inside React Aria's packages, such as
  `react-aria-components/Table`.
  It fails a build whose React Aria packages are at other versions than
  blockr.ui's files hold.
  Beside the preset, `@blockr/ui/build` holds `staticImports()` and the vendor
  check, as core's preset does.

* The package's stylesheet, `inst/js/blockr-ui.css`, holds the theme
  variables under the prefix `bk`, such as `--bk-color-popover` and
  `--bk-spacing`, each with its default for light mode and for dark mode
  (#109).
  Every UI draws with them, and a frontend restyles the page by setting them.
  The defaults keep the look of the design tokens of blockr.ui 0.0.1.
  They sit in Tailwind's theme layer, so that a frontend's values, which it
  sets outside any layer, win over them whichever stylesheet loads first.
  What both modes share sits under `:root`, and the colours and the shadows of
  each mode under `:root[data-bk-mode="light"]` and
  `:root[data-bk-mode="dark"]`.
  The stylesheet also writes the faces of Open Sans, the default of
  `--bk-font-sans`, in place of `inst/assets/fonts/open-sans.css`.
  The files of Open Sans move from `inst/assets/fonts` into `inst/js`, beside
  the stylesheet, and `inst/js/LICENSES` holds their licence.

* The build preset ships its Tailwind half as `@blockr/ui/preset.css`, which a
  package's stylesheet imports after Tailwind's theme and utilities, under the
  package's own prefix (#109).
  It maps each name of Tailwind's theme that has a theme variable onto that
  variable, so that core's `bc:bg-popover` writes
  `background-color: var(--bk-color-popover)`.
  It derives every step of the radius scale from `--bk-radius`, and keys the
  dark variant on `data-bk-mode="dark"` on the page's root.
  It drops Tailwind's palette, but for black and white.
  It drops as well the steps of the faces, the sizes of text, the radii and the
  shadows that the theme variables lack, so that a class of such a step, such
  as `bc:shadow-2xl`, writes nothing.

* The package declares the list of theme variables,
  which is what its stylesheet provides every other package (#110).
  The list holds the names of the theme variables, such as
  `--bk-color-primary`, the font Open Sans and the mode attribute,
  `data-bk-mode`.
  The build preset `@blockr/ui/build` exports it as `themeVariables`,
  and the build writes it into `inst/js/theme-variables.json`.
  The new R function `theme_variables()` reads that file
  and returns the names of the theme variables.
  Adding to the list takes a minor release of blockr.ui,
  and removing or renaming takes a major.
  The package now imports jsonlite.

* The build preset holds `themeCheck()`, which a package's build runs on the
  stylesheets that it writes and on its code (#110).
  It fails a build whose stylesheet reads a variable under `--bk-` that the
  list of theme variables lacks,
  or whose stylesheet another version of Tailwind CSS than 4.3.3 wrote.
  It fails a build whose code names a colour of Tailwind's palette,
  or a step of the faces, the sizes of text, the radii or the shadows that the
  theme variables lack, under any prefix, since such a class writes nothing.
  It fails another package's build whose code names a class under `bk:`.
  The build of blockr.ui runs it on its own stylesheet,
  and on the files that Tailwind scans for it.

* The new R function `new_blockr_theme()` makes an app's theme (#112).
  The theme holds values for some of the theme variables in each mode,
  under their full names, and the files of the fonts that it brings.
  The function refuses a name that `theme_variables()` lacks, naming it,
  and a value that holds `}` or `<`.
  It returns an object of class `blockr_theme`,
  and the new function `is_blockr_theme()` tells whether an object is one.
  The method of `format()` for the class returns the text of the theme's
  rules, one for each mode.
  Core writes that text into the page's head.
  Each rule names `:root` twice beside the mode attribute,
  so that the app's values win over the frontend's and over blockr.ui's
  defaults.

* A frontend gives its values as a theme too, which `new_blockr_theme()` makes,
  and which the generic `frontend_theme()` of blockr.core returns (#111).
  Core writes the frontend's theme into the page's head before the app's
  theme, so that the app's values win by coming later.
  The new R function `unset_variables()` takes a theme and gives, for light
  mode and for dark mode, the theme variables that the theme sets no value
  for.
  For a frontend's theme, those are the defaults of blockr.ui that the
  frontend keeps, which the frontend's tests list.
  So a theme variable that a later blockr.ui adds fails those tests.

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
