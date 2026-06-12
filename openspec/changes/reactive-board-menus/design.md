## Context

The `blockr.ui` instance-backed menus (`stack_menu_*`, `link_menu_*`) render
cards for board instances. Today their server takes only `id`; the board is
passed once to `*_menu_ui(id, board, ...)` at open time and never revisited.
A pinned sidebar therefore shows a stale snapshot when the board mutates
underneath it (block/link removed elsewhere, undo, another action).

A first attempt put the fix in `blockr.dock`: each action observed
`board$board` and reconciled the open menu, needing (1) ownership
bookkeeping so the refresh wouldn't clobber a sibling action sharing the
single `actions_sidebar` slot, (2) `rendered_ids` to detect what was already
in the DOM, and (3) a grow-vs-shrink branch (re-render to add a card,
`pool-update` to remove one). Three mechanisms in the consumer layer, due to
be duplicated into the stack action. The team flagged it as a smell.

The diagnosis: a board-dependent view was being kept fresh *imperatively by
the consumer* instead of *reactively by the component*. The menu already
owns the eligibility logic (`link_eligible_pools`, the stack's member
computation) and the client update protocol (`pool-update`); it should own
freshness too.

## Goals / Non-Goals

**Goals:**

- The instance menus stay consistent with the board while open, with **no
  re-render**: scroll position, card-expansion, the search query, and
  in-progress per-card / panel-level inputs are preserved.
- All board-sync logic lives in `blockr.ui`, inside the menu modules.
- `blockr.dock` drops every line of board-sync orchestration and the
  sidebar-ownership apparatus; it only passes a board reactive and keeps the
  show/pin/hide lifecycle.
- The committed-spec shapes, eligibility rules, and empty-state semantics
  are unchanged. The change is additive at the API level (`board = NULL`
  keeps today's behaviour).

**Non-Goals:**

- Re-rendering as the refresh mechanism (rejected: loses user state).
- Auto-closing a pinned sidebar when the pool drains (a lifecycle/UX
  decision left to the consumer; the menu shows its empty-state in place).
- The `dock-action-handlers` scaffold refactor and the card-list primitive
  factor-out (separate changes).
- Changing the block browser (board-independent; stays a snapshot).
- Changes to `blockr.core`.

## Decisions

### 1. The menu is a reactive component; the board is a reactive input

`*_menu_server()` gains a `board` argument that is a **reactive** (a getter,
not a value):

```r
link_menu_server  <- function(id, board = NULL) { ... }
stack_menu_server <- function(id, board = NULL) { ... }
```

Inside the module:

```r
if (!is.null(board)) {
  observeEvent(board(), {
    session$sendInputMessage(ns("commit"), menu_sync_payload(board(), ...))
  }, ignoreInit = TRUE)
}
```

The dock passes `reactive(board$board)`. `board = NULL` preserves the
current static-snapshot behaviour (and keeps the existing tests valid).

**Why a reactive, not a value?** The whole point is reacting to change; a
value can't. **Why in the server, not `*_ui()`?** `*_ui()` builds static
markup; the live behaviour belongs to the running module. **Alternative
considered** — a `uiOutput` + `renderUI` over the board reactive (full
reactive re-render): simplest, but destroys/recreates the inputs and cards
on every board change, losing exactly the user state we want to keep. The
diff is chosen specifically to preserve that state.

### 2. Self-scoped diff — ownership tracking disappears

The module addresses its sync message to its **own** binding root
(`ns("commit")`). If another action currently owns the shared sidebar slot,
this module's panel is not in the DOM, so the message simply finds no target
and no-ops. A self-scoped diff therefore *cannot* clobber a sibling's form -
which is the only reason the prototype needed `claim_sidebar` /
`sidebar_owner`. That apparatus (and `sidebar_owner_store` in
`R/action-utils.R`) is removed.

### 3. `menu:sync` generalises `pool-update` to add cards, not just toggle

The prototype's grow-vs-shrink branch existed because `pool-update` could
only hide/show cards **already in the DOM** - a newly-eligible *new* block
had no card, forcing a server re-render. We remove that branch by letting
the diff **insert** cards.

The module computes its full desired card set against the current board and
emits one message:

```text
session$sendInputMessage(ns("commit"), list(
  type        = "menu:sync",
  cards       = <list of { id, direction?, html } for every card that
                 SHOULD exist now, html only required for new ones>,
  eligible    = <per-direction id vectors, or a flat set for the stack>,
  free_inputs = <per-target free input ports>,   # link menu
  selected    = <currently-selected member ids>, # stack menu (reconciled)
  id_seed     = <fresh rand_names() value>
))
```

The client reconciliation (`receiveMessage`, shared on `window.BlockrUI`):

1. **Remove** card nodes whose id is no longer in `cards`.
2. **Insert** card nodes present in `cards` but absent from the DOM, using
   the carried `html` (placed into the correct category/direction section,
   creating the section if needed).
3. **Retune** surviving cards: toggle the eligibility/`.hidden` class,
   rebuild the input-port `<select>` from `free_inputs` (preserving a still-
   valid selection), reconcile the stack's selected state.
4. Recompute the panel's `is-empty` state.
5. **Never** touch scroll, card-expansion, the search input, or panel-level
   inputs (stack name / colour / id, per-card link-id the user has edited).

Carrying `html` for new cards keeps card markup authored in **one place**
(the R card builder), reused for both initial render and live insertion -
no parallel JS card template.

### 4. Post-commit refresh folds into the same observer

A commit calls the dock's `update()`, which mutates the board on the next
flush. The module's `observeEvent(board())` then fires and emits the sync -
so the **same** mechanism that handles external board changes also handles
the menu's own commits. The dock's deferred post-commit
`pool-update` / `keep_or_hide_sidebar` rebuilds are deleted; the multi-link
session "just-wired card drops" behaviour is preserved by the board
observer, now driven from `blockr.ui`.

### 5. Eligibility recompute helpers stay in `blockr.ui`

`link_eligible_pools(board, anchor)` already exists and is reused by the
link module's observer. The stack module needs an analogous internal
computation (candidate member blocks for the stack + reconciliation of the
selected set when a member block is removed); it lives next to the stack
module, mirroring `link_eligible_pools`. The anchor (link) / target stack id
(stack) the menu was opened with are captured at mount and reused on each
recompute.

### 6. The menu owns validation (PR #172 review)

PR #172's review asked whether the validators could live in `blockr.ui` and
the server take `board` + `session`. Since the server is already gaining the
`board` reactive for sync, adding `session` and folding validation in is the
same evolution, not a new one:

```r
stack_menu_server(id, board = NULL, session = getDefaultReactiveDomain())
link_menu_server (id, board = NULL, session = getDefaultReactiveDomain())
```

The committed reactive validates before it fires: id-uniqueness against
`board()`, name presence, and (stack) hex-colour format, surfacing any
failure via `notify()` from inside the module. A rejected commit does not
fire the reactive at all, so the consumer never sees an invalid payload and
needs no validators of its own. The dock-side `valid_stack_id` /
`valid_stack_name` / `valid_stack_color` and the id-collision link validator
are deleted.

**Layering caveat.** `blockr.ui` depends on `blockr.core`, so it can use
`board_stack_ids()`, `board_link_ids()`, `is_hex_color()` (or a local
equivalent) - everything validation needs. It must NOT reach into
`blockr.dock`.

### 7. Return shape: validated core object, dock does the `dock_stack` wrap

PR #172's review also asked whether the server could return a `stacks`
object of `dock_stack`s directly. It can't *literally* - `dock_stack` is a
`blockr.dock` class and `blockr.ui` must not depend on `blockr.dock`. Two
realizable options:

- **(chosen)** The stack menu returns a validated `blockr.core` `stack`
  (name + colour + member ids) or a validated spec; the dock's handler does
  the one-line `as_dock_stack()` / `new_dock_stack()` upgrade and calls
  `update()`. Keeps the dependency arrow intact; the dock shrinks to a thin
  adapter.
- **(alternative, open question)** `stack_menu_server(..., ctor =
  new_dock_stack)` - the consumer injects the constructor as data, so the
  menu returns ready `dock_stack` objects and the dock handler is a bare
  `update()`. Fully realizes the reviewer's suggestion without a static
  dependency, at the cost of an extra server argument.

The link menu already returns a spec the dock turns into a `blockr.core`
`link` via `new_link()` (a core class), so the link side can return the
`link` / `links` object directly with no layering issue.

Either way, validation (Decision 6) moves regardless of which return shape
wins; the return shape only changes how thin the dock adapter is.

## Risks / Trade-offs

- **Stale anchor / target after the anchored entity is deleted** → If the
  link menu's `anchor` block (or the stack being edited) is itself removed
  while pinned, there is nothing to anchor on. The module emits an
  empty-state sync (all cards removed); the consumer's lifecycle may choose
  to close the sidebar, but the menu never errors. Specified explicitly.
- **`html`-carrying messages grow with board size** → Only *new* cards
  carry markup; surviving cards are referenced by id. For boards in the
  expected range (tens of blocks) this is negligible; a very large board
  adding many blocks at once sends proportional markup, which is still far
  cheaper than a full re-render.
- **Two menus, one protocol** → Generalising `pool-update` to `menu:sync`
  and giving the stack menu the same reconciliation is more `blockr.ui` code
  than a dock-side patch. Accepted: it is written twice in the right layer
  instead of orchestrated per-action in the wrong one, and it sets up the
  shared client helper the card-list primitive follow-up will absorb.
- **Backward compatibility** → `board = NULL` default means existing callers
  and the current `*_menu` test suites keep working unchanged; the new
  behaviour is opt-in by passing a reactive.

## Migration Plan

1. **`blockr.ui`:** add `board = NULL` + `session` to both servers; add the
   internal `observe(board())` → `menu:sync`; move validation into the
   committed reactive (Decision 6) and enrich the return to a validated
   result (Decision 7); generalise the link JS to insert cards and factor
   the reconciliation onto `window.BlockrUI`; add the stack recompute helper
   and the stack reconciliation; tests (unit + shinytest2 driving
   `session$setInputs`/`sendInputMessage` across a board mutation, plus
   validation-rejection tests).
2. **`blockr.dock`:** pass `reactive(board$board)` (and the session) into the
   three menu servers; shrink the commit handlers to the thin
   wrap-and-`update()` adapter (Decision 7); delete the board-sync observers,
   `rendered_ids`, `push_pool_update`, deferred rebuilds, the dock-side
   validators, and `claim_sidebar` / `sidebar_owner` / `sidebar_owner_store`;
   update tests.
3. Pin the `blockr.ui` `Remotes` in `blockr.dock` to the feature branch
   during review, revert on merge (same pattern as the prior changes).

Two PRs, `blockr.ui` first. Rollback is `git revert`; nothing persists.

## Open Questions

- **Should the consumer auto-close a pinned menu when it drains to empty?**
  The menu shows its empty-state; whether the dock then closes the sidebar
  is a lifecycle choice. Lean: leave it open (honour the pin), revisit after
  use. Out of scope here.
- **Does the stack menu need `free_inputs` semantics at all?** Stacks select
  members; there are no input ports. Its sync payload likely needs only
  `cards` / `selected` / `id_seed`. Confirm during implementation that the
  shared reconciliation degrades cleanly when port fields are absent.
- **Order vs `refactor-dock-action-handlers`.** This change should land
  first so the refactor factors already-simplified handlers; if the refactor
  lands first, its link `after_commit` hook becomes dead code this change
  then removes.
