# blockr.ui 0.0.0.9000

* New `stack_menu_ui()` / `stack_menu_server()` / `stack_menu_dep()`
  module: a multi-select card-list block picker for stacks, with an
  inline hue / lightness slider + hex colour picker and a panel-level
  form for the stack name / color / id. `target = NULL` is the create
  flow; `target = "<stack_id>"` selects the edit flow. Mirrors
  `block_browser_ui()`'s `target` argument shape.
* Initial package scaffold.
