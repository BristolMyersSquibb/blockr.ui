## Why

The add-link flow (`add_link_action` in `blockr.dock`) is the last remaining
chooser in the sidebar that still uses the legacy selectize-based form.
After `add-block-browser` (#5) shipped the card-list pattern for the
add / append / prepend block flows and `add-stack-menu` (#7) brought the
add / edit stack flows in line, the link menu is the odd one out: same
panel, same sidebar primitive, completely different chooser.

Beyond the UI consistency win, the legacy flow also has a behavioural
gap: it only supports "link **from** the right-clicked block" - so
right-clicking a `head_block` that has free input ports surfaces the
unhelpful warning "No inputs are currently available." The user has to
remember to right-click the upstream block instead, even though
conceptually a link is a property of a *pair*, not of one end. This
change makes the new link menu **bidirectional**: it picks both the
outgoing and incoming candidates for the anchor block and renders the
ones that are valid.

Bringing the link panel in line also produces the third consumer of the
card-list visual primitive - the natural moment to revisit a small
`card_list` factor-out, flagged as a deferred follow-up in both prior
changes.

The link flow differs from the block-browser and stack-menu flows in
four ways that matter:

- Cards represent **board instances** (existing blocks on the board),
  like the stack menu - not registry **types** like the block browser.
- Selection is **single-shot click-to-add**, like the block browser -
  not the multi-select toggle of the stack menu. Each click creates one
  link.
- The menu is **bidirectional**: the anchor block (right-clicked) can
  be either the source or the target of the new link. The eligible
  pool is computed for both directions and rendered as up to two
  category-grouped sections ("OUTGOING - connect to" and "INCOMING -
  connect from"). Each card carries a `data-direction` attribute so
  the binding can construct the link in the correct orientation.
- The per-card form (revealed via the chevron) carries the **link id**
  and an optional **block input port** picker. Port picking targets the
  **target** end of the chosen direction; it is hidden when the
  target's arity is 1 (the only slot is forced) or NA (variadic - a
  fresh slot is generated server-side).

This change adds a `link_menu_ui()` / `link_menu_server()` module in
`blockr.ui` and adopts it in `blockr.dock::add_link_action`. The legacy
`link_sidebar_body()` is removed.

## What Changes

- **NEW** `blockr.ui::link_menu_ui(id, board, anchor)` - the
  card-list-with-search body. `anchor` is the block id the user
  right-clicked (required; supplied by the dock from `trigger()`). The
  function renders up to two category-grouped sections:
  - **OUTGOING** cards (anchor is source): every board block other
    than `anchor` that has at least one free named input port or is
    variadic. Hidden entirely if there are no candidates.
  - **INCOMING** cards (anchor is target): every other board block,
    **rendered only when** `anchor` itself has at least one free named
    input port or is variadic.
  An empty-state is rendered when neither direction has eligible
  candidates. Cards carry a `data-direction` attribute (`"outgoing"`
  or `"incoming"`) the binding reads on commit.
- **NEW** `blockr.ui::link_menu_server(id)` - module server returning
  a `reactive` that fires once per commit with the link spec:
  `list(source, target, link_id, block_input)`. `source` and `target`
  are derived from the clicked card's direction:
  - `direction = "outgoing"` -> `source = anchor`, `target = card.id`
  - `direction = "incoming"` -> `source = card.id`, `target = anchor`
  `block_input` is the chosen / forced port on the **target** end; it
  is `NULL` when the target has a single named input or is variadic.
- **NEW** A `Shiny.InputBinding` (`blockr.ui.linkMenu`) registered on
  the module's root element. `getValue` returns the spec (with a
  `nonce` so repeat commits re-fire); `subscribe` listens for the
  commit event; `receiveMessage` accepts a `{ type = "pool-update",
  eligible = list(outgoing = c(...), incoming = c(...)) }` payload so
  the consumer can push live pool updates to the open menu after each
  commit, without re-rendering.
- **NEW** Bundled JS (`inst/assets/js/blockr-link-menu.js`) for the
  click-to-commit + per-card expand state machine. The root element
  carries `data-anchor`; each card carries `data-direction`; on commit
  the binding assembles `source` / `target` accordingly.
- **REUSE** The `blockr-block-browser-card*` CSS classes from
  `add-block-browser` and the section-header pattern from
  `add-stack-menu` are reused as-is (no rename). The cards drop
  description + package badge and show an `id: <block_id>` subtitle.
- **MODIFIED (in `blockr.dock`)** `add_link_action` mounts the new
  module and observes its returned reactive; the per-field input
  observers (`create_link`, `add_link_input`, `add_link_id`,
  `add_link_confirm`) and `link_sidebar_body()` are removed. The
  sidebar title becomes a short direction-agnostic `Connect <anchor>`
  so the wording reads correctly whichever direction the user picks.
  After each commit the handler sends a `pool-update` message via
  `session$sendInputMessage` instead of re-rendering, so the menu
  stays open and the just-wired card disappears from the list live -
  letting the user create several links from the same anchor in one
  uninterrupted session.

## What's *not* in this change

- A bigger CSS rename from `blockr-block-browser-*` to `blockr-card-*`.
  With this PR the card-list pattern has three consumers (block-browser
  + stack-menu + link-menu), so a small factor-out into a
  `card_list_dep()` primitive that exports the shared CSS / JS / R
  builders becomes attractive. Tracked as a follow-up in `design.md`;
  not in this PR because it touches all three modules at once.
- A toggle / radio control above the cards to filter by direction. The
  two sections are clearly labelled; an explicit toggle adds chrome
  without changing what's selectable. Easy to add later if user
  testing shows confusion.
- Arbitrating between competing ports across instances. The current
  `block_input_select(..., mode = "inputs")` filter (free ports given
  current links) is preserved as-is; the menu only changes the chooser
  shape.
- A bulk "add many links at once" flow. The link menu stays
  single-shot - consistent with how `add_link_action`'s sidebar already
  rebuilds itself for a follow-on link after each commit.
- Changes to `new_link()` / `link` / `as_links` in `blockr.core`.

## Capabilities

### New Capabilities

- `link-menu`: the bidirectional single-select card-list link picker
  for the add-link flow (`link_menu_ui()`, `link_menu_server()`,
  `link_menu_dep()`), plus the JS binding that owns the
  click-to-commit + per-card expand state machine and the direction
  encoding on the commit spec.

### Modified Capabilities

None in `blockr.ui`. In `blockr.dock`, `add_link_action` is modified to
mount the new module and observe its committed-link reactive;
`link_sidebar_body()` is removed.

## Phases as separate PRs

Two PRs - one per repo, sequenced like `add-block-browser` and
`add-stack-menu`.

1. **PR 1 (`blockr.ui`):** `link_menu_ui()` + `link_menu_server()` +
   `link_menu_dep()`, bundled CSS / JS, unit tests + shinytest2 against
   a runnable example, vignette section, README screenshot, and the
   openspec change. Reuses `block_browser_dep()` for the shared card
   visuals.
2. **PR 2 (`blockr.dock`):** action-handler adoption. `add_link_action`
   mounts the module, observes the returned reactive, builds the link.
   `link_sidebar_body()` is removed. Tests updated. NEWS.

Builds on top of `add-block-browser` and `add-stack-menu`; both prior
PRs must be on `blockr.ui` `main` before this lands cleanly. The
`blockr.ui` `Remotes` pin in `blockr.dock` follows the same pattern -
pinned to the link-menu branch during PR 2 review, reverted once PR 1
lands.

## Impact

- **Affected packages.** `blockr.ui` gains the link-menu capability;
  `blockr.dock` rewrites one action handler and drops
  `link_sidebar_body()`.
- **Affected dependencies.** None new.
- **Affected APIs.** `link_sidebar_body()` is removed (no out-of-tree
  consumers known; called out in NEWS). The per-field link inputs
  (`create_link`, `add_link_input`, `add_link_id`, `add_link_confirm`)
  are replaced by `link_menu_server()`'s returned reactive.
- **Affected behaviour.** Right-clicking a block that has free input
  ports (but the legacy flow's source-only assumption rejected) now
  opens a working link menu with the available source candidates. The
  "No inputs are currently available" warning case shrinks to a real
  empty-state (no other blocks on the board, or every relevant slot is
  full in both directions).
- **Affected downstream.** Extensions inherit the new picker for free.
