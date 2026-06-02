## Context

`add-block-browser` (#5) and `add-stack-menu` (#7) replaced the legacy
selectize-based bodies for the add / append / prepend block flows and
the add / edit stack flows respectively. The add-link flow
(`add_link_action`) is the last remaining chooser still on the legacy
form-shaped pattern, with `link_sidebar_body()` rendering a selectize
of target blocks + an Advanced toggle for the link id and port picker.

In addition to its visual mismatch, the legacy flow is also
**directional**: it only supports "link from the right-clicked block",
so right-clicking a downstream block that has free inputs surfaces the
warning "No inputs are currently available." even though the user
might reasonably want to connect *to* it. This change makes the new
link menu bidirectional: the right-clicked block becomes the *anchor*,
and the menu shows up to two category-grouped sections for the
candidates in each direction.

This change brings the link panel in line by adding a sibling module
`link_menu_ui()` / `link_menu_server()` in `blockr.ui`. It sits between
the prior two:

| Concern         | `block_browser`                           | `stack_menu`                                          | `link_menu`                                                   |
| --------------- | ----------------------------------------- | ----------------------------------------------------- | ------------------------------------------------------------- |
| Card source     | Registry types (`available_blocks()`)     | Board instances (free pool + target stack's members)  | Board instances, split into OUTGOING / INCOMING vs the anchor |
| Selection model | Single-shot click-to-add                  | Multi-select toggle, published on confirm             | Single-shot click-to-add (one click = one link)               |
| Per-card form   | id / title / link / port (mode-dependent) | None (stack-level fields live at the panel level)     | `link_id` (always) + `block_input` (when target arity > 1)    |
| Confirm action  | Add one block (+ optional link)           | Create or update a stack                              | Add one link in the card's direction                          |

## Goals

- One new public module - `blockr.ui::link_menu_ui()` /
  `link_menu_server()` - that the add-link action handler mounts in
  place of `link_sidebar_body()`.
- **Bidirectional cards.** The anchor block is the right-clicked
  block. The menu renders up to two sections:
  - **OUTGOING** ("Connect to..."): blocks the anchor can link TO.
    Eligibility = every block other than the anchor that has at least
    one free named input port or is variadic.
  - **INCOMING** ("Connect from..."): blocks that can link INTO the
    anchor. Eligibility = every block other than the anchor, gated on
    the anchor itself having at least one free named input port or
    being variadic.
  Both sections are also internally grouped by category (mirroring
  the block browser and stack menu) for at-a-glance scanning.
- Same card chrome as the stack menu (icon + name + `id: <block_id>`
  subtitle, no description, no package badge).
- Single-shot click-to-add. Click a card body to commit immediately
  with default link id + default port; click the chevron to open the
  per-card form, edit the link id / port, then click the in-card
  "Add link" button.
- Empty-state inside the panel when no eligible target exists in
  either direction.
- A single `Shiny.InputBinding` (`blockr.ui.linkMenu`); no
  `Shiny.setInputValue` ad-hoc calls; commit value carries an internal
  nonce so repeat commits re-fire.

## Non-goals

- The card-list primitive factor-out. With three consumers now sharing
  the same card visual chrome, factoring `blockr-card-list.*` into a
  `card_list_dep()` is the natural next step - tracked as a deferred
  follow-up below.
- An explicit direction toggle / radio. The two sections are clearly
  labelled with section headers; an additional toggle adds chrome
  without changing what's selectable.
- A "Block input" picker for arity-1 targets. Hidden, like the
  append-block case in `block_browser` (single slot is forced; the
  dock falls back to that slot at validation time).
- A port picker for variadic targets. Variadic blocks accept arbitrary
  fresh slots; no choice to expose. The dock generates the slot id.
- Bulk link creation. The sidebar already rebuilds itself for a
  follow-on link after each commit; that pattern stays.
- Changes to the link data model in `blockr.core`.

## API

```r
link_menu_ui(id, board, anchor)
link_menu_server(id)
link_menu_dep()

# Helper for live multi-link updates (used by consumers after each
# commit to push a `pool-update` receiveMessage).
link_eligible_pools(board, anchor)
```

- `id` is the module id (the `*_ui()` call site passes
  `NS(id)("...")` from the parent).
- `board` is the current board (read-only for rendering).
- `anchor` is the right-clicked block id (a non-empty character
  scalar). It must be a member of `board_block_ids(board)`; otherwise
  a clean error is raised. The menu computes the OUTGOING and INCOMING
  pools against `anchor`.
- `link_eligible_pools(board, anchor)` returns a list
  `list(outgoing = <chr>, incoming = <chr>)` with the same
  eligibility rules `link_menu_ui()` uses for its initial render. The
  same `anchor` validation applies. Exported so the dock (or any
  consumer wiring multi-link sessions) recomputes pools against the
  post-commit board without re-implementing the eligibility logic.

The root element's `id` is `NS(id)("commit")`, the Shiny input the
`blockr.ui.linkMenu` binding reports against. The root also carries
`data-anchor` with the anchor block id so the JS can echo it back on
commit.
`link_menu_server(id)` strips the internal `nonce` and returns a
`reactive` whose value is the spec
`list(source, target, link_id, block_input)`.

## Eligible-pool semantics

Two helpers compute the per-direction pools:

`link_outgoing_targets(board, anchor)` = every board block id other
than `anchor` that has at least one free named input port (per
`blockr.core::block_input_select(blk, id, links, mode = "inputs")`)
or has variadic arity (`is.na(blockr.core::block_arity(blk))`).

`link_incoming_sources(board, anchor)` = the same eligibility filter,
but gated on `anchor` first. If `anchor` has no free input port and is
not variadic, the function returns `character()` (no INCOMING is
possible). Otherwise it returns every board block id other than
`anchor`.

The OUTGOING / INCOMING sections render only when their pool is
non-empty. The empty-state replaces both sections when both pools are
empty.

## Commit shape

Each card carries `data-direction="outgoing"` or
`data-direction="incoming"`. On commit the JS binding reads the
card's direction, the root's `data-anchor`, and the card's
`data-block-type` to construct:

```text
direction = "outgoing": source = anchor,    target = card.id
direction = "incoming": source = card.id,   target = anchor
```

The published value on `input$commit` is:

```text
list(source, target, link_id, block_input, nonce)
```

The server strips the internal `nonce` and returns the rest:

```r
eventReactive(input$commit, {
  spec <- input$commit
  spec[["nonce"]] <- NULL
  spec
}, ignoreNULL = TRUE)
```

- `link_id` is the value of the per-card link-id `<input>` at commit
  time (always present; pre-filled with `rand_names(board_link_ids)`).
- `block_input` refers to the **target** end's input port: when the
  card's direction is OUTGOING, the target is the card and the picker
  is rendered when the card's arity > 1; when INCOMING, the target is
  the anchor and the picker is rendered when the anchor's arity > 1.
  Variadic and arity-1 targets render only the Link ID field, and the
  commit emits `block_input = NULL` (the dock falls back to the
  target's first free slot, or generates a fresh slot for variadic).

## Card shape

Same as the stack-menu cards (board instances, trimmed chrome):

- Icon (category-tinted via `data-category`)
- Block name (`block_name(blk)`)
- `id: <block_id>` subtitle
- No description, no package badge
- Chevron toggling the advanced form on the card

The advanced form (inside the card, `block-browser-card-advanced`):

| Field         | Shown when                                       | Default                                |
| ------------- | ------------------------------------------------ | -------------------------------------- |
| Link ID       | Always                                           | `rand_names(board_link_ids(board))`    |
| Block input   | Target end has finite arity > 1 free slots       | The first free slot                    |

"Target end" depends on the card's direction (see Commit shape above).
Variadic and arity-1 target ends render only the Link ID field. The
dock handler uses `block_input_select(...)` with the target id to
resolve the chosen / forced slot at link-build time, same as today.

## Server flow

`add_link_action` becomes:

```r
add_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      committed <- blockr.ui::link_menu_server("menu")

      menu_ui <- function() {
        blockr.ui::link_menu_ui(
          session$ns("menu"), board$board, anchor = trigger()
        )
      }

      # Direction-agnostic title; the menu body labels which section
      # the user is picking from.
      sidebar_title <- function() paste0("Connect ", trigger())

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        spec <- committed()

        if (!valid_link_id(spec$link_id, board$board, session)) return()

        target_blk <- board_blocks(board$board)[[spec$target]]
        chosen_input <- spec$block_input
        if (is.null(chosen_input) || !nzchar(chosen_input)) {
          chosen_input <- block_input_select(
            target_blk, spec$target,
            links = board_links(board$board),
            mode = "inputs"
          )[1L]
        }

        new_lnk <- new_link(
          from = spec$source, to = spec$target, input = chosen_input
        )

        update(list(
          links = list(add = as_links(set_names(list(new_lnk), spec$link_id)))
        ))

        # Multi-link session: push a live pool update to the open menu
        # instead of re-rendering. See "Multi-link sessions" below.
        session$onFlushed(once = TRUE, function() {
          isolate({
            pools <- blockr.ui::link_eligible_pools(board$board, trigger())
            if (length(pools$outgoing) == 0L &&
                length(pools$incoming) == 0L) {
              blockr.ui::hide_sidebar(sidebar_id)
            } else {
              session$sendInputMessage(
                session$ns("menu-commit"),
                list(
                  type = "pool-update",
                  eligible = pools,
                  link_id_seed = rand_names(board_link_ids(board$board))
                )
              )
            }
          })
        })
      })

      NULL
    },
    id = "add_link_action"
  )
}
```

The sidebar title carries the anchor block identifier so the user
always sees which block they anchored from; direction-agnostic wording
(`Connect <anchor>`) reads correctly for both OUTGOING and INCOMING
picks.

## Section headers

The cards section is preceded by an uppercase section header per
direction. Examples:

- "CONNECT TO" - cards in this section are OUTGOING from the anchor.
- "CONNECT FROM" - cards in this section are INCOMING into the anchor.

Header treatment matches the stack menu's section headers (small
uppercase / tracked / muted colour, sized to out-rank the per-category
sub-headers inside each section).

When only one direction has eligible candidates, only that section
header is rendered.

## Empty-state

When both pools are empty, the panel still renders - search box and a
centred empty-state inside the cards container ("This block can't be
linked: no other blocks have free inputs, and the anchor itself has no
free inputs either."). The current dock-side early-bail ("No inputs
are currently available") is replaced with this in-panel state. One
extra click closes the sidebar; in exchange the menu's eligibility
logic stays in `blockr.ui` and the action handler simplifies.

## Multi-link sessions (live pool updates)

The menu is **single-shot per click**, but designed for a **multi-link
session**: the sidebar stays open after a commit and the user can keep
clicking cards to add more links from the same anchor. The eligible
pool changes after each commit (the just-wired card may exhaust its
last free input, or a variadic target may stay), so the menu has to
reflect the new state without losing focus or scroll position.

Implementing this with a full re-render after every commit (the
straightforward `keep_or_hide_sidebar(...)` pattern the legacy add-link
flow uses today) works but is heavy: the panel rebuilds, the sidebar
transitions, and the user's place in the list is lost. The new menu
uses an R -> JS push instead, via the binding's `receiveMessage` hook:

```text
session$sendInputMessage(
  ns("commit"),
  list(
    type     = "pool-update",
    eligible = list(
      outgoing = <chr ids>,   # post-commit OUTGOING pool
      incoming = <chr ids>    # post-commit INCOMING pool (empty if N/A)
    ),
    link_id_seed = <chr>      # fresh rand_names() value for new cards
  )
)
```

The binding's `receiveMessage` handler:

- For every card in the panel, toggles `.hidden` based on whether the
  card's `data-block-type` is still in the eligible pool for its
  `data-direction`.
- Recomputes the panel's `is-empty` state so the empty-state shows
  when the pool drains to zero.
- Optionally re-seeds the per-card Link ID inputs from `link_id_seed`
  so the user-visible defaults don't drift toward collision with the
  links just added (one fresh `rand_names()` value, the JS appends a
  monotonic suffix per card so each card still proposes a unique id).
- Leaves card expansion state, scroll position, and the user's typed
  search query untouched.

The dock-side handler becomes:

```r
observeEvent(committed(), {
  spec <- committed()
  # ... validate + build + update() ...

  session$onFlushed(once = TRUE, function() {
    isolate({
      pools <- link_eligible_pools(board$board, trigger())
      if (length(pools$outgoing) == 0L && length(pools$incoming) == 0L) {
        blockr.ui::hide_sidebar(sidebar_id)
      } else {
        session$sendInputMessage(
          session$ns("menu-commit"),
          list(
            type = "pool-update",
            eligible = pools,
            link_id_seed = rand_names(board_link_ids(board$board))
          )
        )
      }
    })
  })
})
```

`hide_sidebar` still fires when the pool drains to zero (so the user
isn't left staring at an empty-state after the last link they could
make). Otherwise the panel stays put and the live update absorbs the
state change.

Trade-off: this duplicates the eligibility computation between the
client (initial render via `link_menu_ui()`) and the server (pool
recomputation after each commit). Acceptable for now because both
share the same `link_eligible_*` helpers in `R/link-menu.R` - one
source of truth, two call sites.

## Implementation building blocks

- `R/link-menu.R` (new): module + helpers, modelled on
  `R/stack-menu.R` and `R/block-browser.R`. The two eligibility
  helpers (`link_outgoing_targets`, `link_incoming_sources`) sit as
  small private functions.
- `inst/assets/css/blockr-link-menu.css` (new): small additions on top
  of `blockr-block-browser.css`. Section-header style reuses the
  stack-menu's `.blockr-stack-menu-section-header` (or a generalised
  `.blockr-card-section-header` once the card-list factor-out lands).
- `inst/assets/js/blockr-link-menu.js` (new): `Shiny.InputBinding`
  `blockr.ui.linkMenu`, click-to-commit + per-card expand + in-card
  add button, direction encoding read from `data-direction`. Reuses
  `window.BlockrUI.cardSearch` for the search filter.
- Tests: `tests/testthat/test-link-menu.R` (unit) and
  `tests/testthat/test-link-menu-shinytest2.R` (e2e against
  `inst/examples/link-menu/app.R`).
- Vignette: `vignettes/link-menu.Rmd` (small).
- README: a third figure under "Example", captured via shinytest2.

## Risks / Trade-offs

- **Two-section visual density.** When both OUTGOING and INCOMING
  pools are populated, the panel renders two section headers and two
  card lists. For small boards (~5 blocks) this is fine; for larger
  boards each section can still scroll independently if needed. The
  search filter hides matches across both sections at once.
- **Three near-duplicate JS bindings.** Block-browser, stack-menu, and
  link-menu each register their own `Shiny.InputBinding`. The shared
  search helper already lives on `window.BlockrUI.cardSearch`; the
  rest of the binding structure could plausibly be factored too, but
  the per-module commit shape doesn't generalise cleanly. The
  card-list primitive follow-up below is the right time to revisit.
- **In-panel empty-state vs. early bail.** The legacy flow silently
  refuses to open the sidebar when no targets are eligible; the new
  behaviour opens it and shows a message. Slightly more friction (one
  close click) in exchange for keeping the empty-state logic inside
  the module.

## Migration / Compatibility

- `link_sidebar_body()` is removed in `blockr.dock` PR 2. No in-tree
  callers remain after the action-handler swap; out-of-tree consumers
  (if any) migrate to `blockr.ui::link_menu_ui()` - called out in
  NEWS.
- Per-field link inputs (`create_link`, `add_link_input`,
  `add_link_id`, `add_link_confirm`) are removed in favour of
  `link_menu_server()`'s returned reactive. Out-of-tree code observing
  those inputs migrates to the reactive. Called out in NEWS.
- **Behaviour change.** Right-clicking a downstream block that has
  free inputs now opens a working menu (the INCOMING section) instead
  of warning "No inputs are currently available." This is a strict
  improvement, but called out in NEWS so the change is discoverable.
- No change to the link data model in `blockr.core`.

## Deferred / follow-up

- **Card-list primitive.** With three consumers, factor the shared
  card chrome (CSS classes + search filter JS + R section-header
  builder) into a `card_list_dep()` + a small set of public helpers
  in `blockr.ui`. Block-browser, stack-menu, and link-menu would
  consume the primitive. Touches all three modules at once - separate
  PR.
- **Live `pool-update` retrofit for block-browser and stack-menu.**
  The `receiveMessage` payload shape (typed message + per-direction
  eligible pools + optional id seed) generalises to the sibling
  modules. Highest payoff: block-browser's add / append / prepend
  flow benefits from live id / link-id re-seeding (avoids a full
  re-render of 20-30 cards on each commit), and prepend mode can
  drain its target's free ports without rebuilding. Stack-menu
  benefits less (one stack per panel open, multi-commit rare) - low
  priority. Retrofit lands in a follow-up change once the link-menu
  version has been exercised in the wild and the protocol has
  stabilised.
- **Reorderable / bulk link creation.** Not in scope. The
  follow-on-link pattern (live pool-update after commit) already
  handles the "add several links from the same anchor" workflow
  comfortably.
