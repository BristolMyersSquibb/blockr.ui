## Context

The block browser is one Shiny module reused for three flows: *add* (no
target), *append* (link from a source block), *prepend* (link into a target
block). Today:

- `block_browser_ui(id, board, target)` renders one card per registered
  block and bakes a board-seeded unique default id into each card's
  (collapsed) form, so the markup depends on board state.
- `block_browser_server(id)` returns the raw commit spec `list(type, id,
  title, link_id, block_input, target_input)`.
- `blockr.dock`'s `add_block_action()` / `append_block_action()` /
  `prepend_block_action()` rebuild the UI on every open (`show_sidebar(id,
  ui = browser_ui())`), then on commit call `build_block_from_spec()`,
  resolve the port with `block_input_select()`, and assemble the link with
  `new_link()`.

The other two menus already moved past this. `link_menu_server(id, board,
anchor)` resolves the port from the live board with `blockr.core`
primitives and returns `as_links(...)`; `stack_menu_server(id, board,
target)` returns `as_stacks(...)`. The dock handlers for those flows are
thin: `update(list(links = list(add = committed())))`.

Two costs follow from the block browser lagging that pattern: the dock
re-renders the whole card list per open (dock#224), and the construction
logic is duplicated dock-side and shaped differently from the menus.

## Goals / Non-Goals

**Goals**

- The *add* browser is rendered once and reused across opens (no per-open
  `renderTags`).
- `block_browser_server()` returns ready-to-apply `blockr.core` objects, so
  the dock applies them verbatim like the link / stack menus.
- Port resolution and block construction live in `blockr.ui`, expressed
  with `blockr.core` primitives only (no `blockr.dock` dependency).

**Non-Goals**

- Pre-rendering the *append* / *prepend* browsers. Their content depends on
  the right-clicked trigger block, so they stay per-open.
- The dock-side scaffold DRY-up (`sidebar_menu_action()` etc.): that is
  `refactor-dock-action-handlers`. This change only reshapes what the
  handlers receive; reconciling the two is sequenced, not merged.
- Caching the registry-derived card metadata in `blockr.ui` (a separate,
  smaller optimization for the still-dynamic append / prepend flows).

## Decisions

### 1. Default ids leave the static markup

The only thing tying the *add* markup to board state is the seeded default
id baked into each card. Move it out: the card ships without a server-baked
id, and the unique id is resolved when the block is committed (server-side,
against the live board) or generated client-side as a placeholder the
server finalizes. With that removed, the *add* markup is a pure function of
the registry and can be rendered once.

`board` stays on `block_browser_ui()` for the *prepend* target-input picker
(which lists the target block's currently-free ports, genuinely board- and
trigger-dependent) and the *append* / *prepend* context subtitle. For the
*add* flow `board` is unused.

### 2. The server returns ready objects, mirroring the menus

`block_browser_server(id, board = NULL, target = NULL)` returns a reactive
firing once per add:

- *add*: a `blocks` object (`as_blocks(set_names(list(blk), id))`), the
  block created from the committed `type` and named from the committed
  `title`.
- *append* / *prepend*: `list(blocks = <blocks>, links = <links>)`, the link
  keyed by its id with the input port resolved.

This is the same move `link_menu_server()` made (raw spec to `as_links`).
The dock then applies `update(list(blocks = ..., links = ...))` directly.

### 3. Port resolution and construction use `blockr.core` only

`link_menu_server()` already reproduces `block_input_select(mode =
"inputs")` with `blockr.core` primitives (`resolve_free_input()`: explicit
pick, else first free named slot, else a fresh integer slot for variadic
targets) specifically to avoid a `blockr.dock` dependency. The block
browser reuses that resolution. Block construction (`create_block(type)` +
`block_name<-`) and the unique-id seeding also move into `blockr.ui` against
the live board.

### 4. The server owns validation

When a `board` reactive is supplied, the server validates the committed
block id (and link id for append / prepend) and notifies + does not fire on
a duplicate / empty id, exactly as `link_menu_server()` /
`stack_menu_server()` do. The dock's `valid_block_id()` / `valid_link_id()`
pre-flight then drops from the block handlers.

### 5. The dock pre-renders the add browser on a dedicated sidebar

A single shared `actions_sidebar` cannot pre-hold multiple bodies. The add
browser gets its own `sidebar_ui(ns, ui = block_browser_ui(ns))` mount in
`board-ui.R`, opened via `data-blockr-sidebar-target` / `show_sidebar(id)`
(no `ui`). append / prepend / link / stack stay on the shared dynamic
sidebar since they are trigger-specific (link / stack already patch in
place via `sendInputMessage`).

## Risks / Trade-offs

- **Breaking return shape.** `block_browser_server()` changes from a spec to
  ready objects. Mitigation: `blockr.dock` is the only known consumer and is
  updated in lockstep; NEWS in both packages.
- **Cross-package lockstep.** The `blockr.ui` and `blockr.dock` halves must
  land together (Remotes already points the dock at the dev `blockr.ui`).
- **Overlap with `refactor-dock-action-handlers`.** That change factors a
  dock-side `resolve_input_port()` the block handlers will no longer need.
  Mitigation: land this first; the scaffold refactor then targets handlers
  that already receive ready objects (smaller surface).
- **Default-id strategy.** Generating the placeholder client-side vs.
  resolving purely at commit is an implementation choice with a UX edge
  (what the user sees in the id field before committing). Resolved in Open
  Questions.

## Migration Plan

1. `blockr.ui`: add the board-aware `block_browser_server()` returning ready
   objects and the board-independent *add* markup; keep the input-binding
   protocol. Update `testServer` coverage to the new shapes.
2. `blockr.dock`: mount the add browser once in `board-ui.R`; rewrite add /
   append / prepend handlers as thin `update(committed())` adapters; drop
   `build_block_from_spec()` / `block_input_select()` / `new_link()` from
   them. Update the action `testServer` suites.
3. Reconcile `refactor-dock-action-handlers` against the slimmer handlers.
4. NEWS in both packages; `devtools::check()` + `lintr` clean on both.

## Open Questions

- **Default id placement.** Generate the placeholder id client-side (so the
  user sees a non-colliding default before committing) or leave the field
  blank and have the server assign at commit? Client-side keeps the current
  UX; commit-side is simpler. Leaning client-side for the *add* flow to
  preserve the "click a card, get a sensible unique id" behaviour.
- **`board` argument on `block_browser_ui()`.** Keep it (used by prepend) or
  split a separate `target`-only entry point so the *add* call site is
  obviously board-free? Leaning keep-it-optional, documented as add-unused.
- **Whether to also drop `block_browser_ui()`'s `board` for append.** Append
  uses `board` only for the (now-removed) seed ids and the subtitle; the
  subtitle needs the trigger, not the board. Possibly append becomes
  board-free too, leaving `board` exclusively a prepend concern.
