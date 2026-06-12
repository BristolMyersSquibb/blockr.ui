## Why

Two problems with the block browser, one performance and one consistency,
share a root cause and a fix.

1. **The block browser is re-rendered on every action-sidebar open**
   (blockr.dock#224). The dock rebuilds the full card list and ships it on
   each open via `show_sidebar(id, ui = browser_ui())`, and again after each
   add via `keep_or_hide_sidebar()`. Measured on `blockr.ui` with the 10
   core blocks: building the tags is ~5.7 ms, and build + `renderTags()`
   (what `show_sidebar()` actually pays) is ~22 ms per open. It is the same
   static card markup every time and scales linearly with the registry, so
   a 50-100 block app projects to ~110-220 ms re-rendered on every open. The
   settings sidebar already shows the right pattern: `sidebar_ui(ui = ...)`
   pre-renders the body once and `data-blockr-sidebar-target` opens it
   client-side with no round-trip and no re-render.

2. **The block browser does not hand the dock a plug-and-play result.**
   `link_menu_server()` and `stack_menu_server()` return ready-to-apply
   `blockr.core` objects (a `links` / `stacks` object) that the dock applies
   verbatim. `block_browser_server()` instead returns a raw spec, so the
   append / prepend / add handlers re-implement block construction
   (`build_block_from_spec()`) and, for append / prepend, port resolution
   (`block_input_select()`) and link assembly (`new_link()`). That logic is
   duplicated dock-side and diverges from the menu pattern the other two
   flows already follow.

The fix for both is the same shift the menus already made: a
board-independent UI that can be rendered once, plus a board-aware server
that builds the final objects at commit time.

## What Changes

- **MODIFIED (`blockr.ui`)** `block_browser_ui()` stops baking
  board-seeded default block / link ids into the per-card form. The default
  id is resolved at commit (server-side) or generated client-side. For the
  *add* flow (`target = NULL`) the rendered markup becomes independent of
  board state, so it can be rendered once and reused across opens. The
  *append* / *prepend* flows still take `board` for the target context and
  the prepend port picker, and stay per-trigger.
- **MODIFIED (`blockr.ui`)** `block_browser_server(id, board = NULL, target
  = NULL)` accepts a `board` reactive and a `target`, owns id validation,
  and returns a ready-to-apply value instead of a spec: a `blocks` object
  for *add*, or `list(blocks, links)` for *append* / *prepend* with the
  link's input port already resolved from the live board using only
  `blockr.core` primitives (the resolution `link_menu_server()` already
  reproduces). This is parity with `link_menu_server()` /
  `stack_menu_server()`.
- **MODIFIED (`blockr.dock`)** The add block browser is mounted once in
  `board-ui.R` on its own `sidebar_ui(ns, ui = block_browser_ui(ns))` and
  opened client-side / open-only, like the settings sidebar. The add /
  append / prepend handlers become thin adapters that apply
  `committed()` via `update()`; `build_block_from_spec()`,
  `block_input_select()` and `new_link()` assembly leave the handlers.
- **NO new public API.** `block_browser_ui()` / `block_browser_server()`
  keep their names; their signatures and committed-value shape change (a
  breaking change for out-of-tree consumers, of which `blockr.dock` is the
  only known one, updated in lockstep).

## Capabilities

### Modified Capabilities

- `block-browser`: the UI markup for the *add* flow becomes independent of
  board state (pre-renderable once), and the server gains the `board` /
  `target` inputs, owns id validation, and returns ready-to-apply
  `blockr.core` `blocks` / `links` objects rather than a raw spec.

## Impact

- **Affected packages.** `blockr.ui` (the block-browser module) and
  `blockr.dock` (the add / append / prepend handlers and the board-ui
  sidebar mount). No `blockr.core` change.
- **Affected behaviour.** The committed value shape changes from a spec list
  to ready `blockr.core` objects. Breaking for `block_browser_server()`
  consumers; `blockr.dock` is updated in the same effort.
- **Affected performance.** The add browser stops re-rendering on every
  open (the dock#224 symptom). Append / prepend stay per-trigger but no
  longer re-instantiate blocks dock-side for port resolution.
- **Related / overlap.** `refactor-dock-action-handlers` factors a
  dock-side `resolve_input_port()` and a thin action scaffold. This change
  removes the block-flow construction that change was going to factor, so
  the two must be reconciled: this change lands first, and the scaffold
  refactor adapts to handlers that already receive ready objects. dock#224
  is the performance driver this change resolves.
