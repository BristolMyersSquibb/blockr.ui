## Context

The six sidebar-menu action handlers in `blockr.dock` were ported onto the
card-list pattern one change at a time (`add-block-browser`,
`add-stack-menu`, `add-link-menu`). Each port copied the previous handler's
shape, so they now share a structure that is identical up to a handful of
variation points, but no shared abstraction expresses that. The
duplication is both structural (the action server scaffold) and at the leaf
(id validators, port resolution, wrap-and-name idioms), and the most recent
port left `valid_link_id()` defined in two files.

The handlers all conform to the `new_action()` contract from
`R/action-class.R`: an action is `new_action(function(input, output,
session) { ... ; NULL }, id = "...")`, mounted by `register_action()` via
`moduleServer(id, action(...))`. Each receives `trigger` (a reactive id),
`board` (a `reactiveValues` with `$board` and `$board_id`), `update` (a
`reactiveVal` setter), and `...`.

### The shared scaffold (today, in every menu handler)

```r
X_action <- function(trigger, board, update, ...) {
  new_action(function(input, output, session) {
    sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
    committed  <- blockr.ui::<MODULE>_server("<modid>")
    menu_ui    <- function() blockr.ui::<MODULE>_ui(session$ns("<modid>"),
                                                    board$board, <extra>)
    observeEvent(trigger(), blockr.ui::show_sidebar(sidebar_id,
                                                    title = <TITLE>,
                                                    ui = menu_ui()))
    observeEvent(committed(), {
      spec <- committed()
      <validate; return() on failure>
      <build domain object(s)>
      update(list(...))
      <post-commit rebuild>
    })
    NULL
  }, id = "X_action")
}
```

### Variation points

| Axis | add_block | append_block | prepend_block | add_link | add_stack | edit_stack |
| --- | --- | --- | --- | --- | --- | --- |
| module | block_browser | block_browser | block_browser | link_menu | stack_menu | stack_menu |
| modid | `"browser"` | `"browser"` | `"browser"` | `"menu"` | `"menu"` | `"menu"` |
| `*_ui` extra | (none) | `append_to(trigger())` | `prepend_to(trigger())` | `anchor = trigger()` | (none) | `target = trigger()` |
| title | `"Add new block"` | `"Append new block"` | `"Prepend new block"` | `paste0("Connect ", trigger())` | `"Create new stack"` | `paste0("Edit stack ", trigger())` |
| commit body | block | block + link (append) | block + link (prepend) | link | stack create | stack update |
| rebuild | sync keep_or_hide | sync | sync | flush → hide / pool-update | flush → keep_or_hide | flush → keep_or_hide |

### Leaf duplication

- **Id validators** (`valid_block_id`, `valid_link_id` ×2, `valid_stack_id`):
  all `if (is.null(x) || !nzchar(x) || x %in% existing) { notify(...); FALSE } else TRUE`.
- **Port resolution** (append, prepend, add_link): `port <- spec$<field>;
  if (is.null(port) || !nzchar(port)) port <- block_input_select(blk, id,
  links, mode = "inputs")[1L]`.
- **Wrap-and-name**: `as_links(set_names(list(lnk), id))` ×3;
  `as_blocks(set_names(list(blk), id))` ×3.
- **`remove_*_action`** ×3: identical but for the `update()` key.

## Goals / Non-Goals

**Goals:**

- One internal factory, `sidebar_menu_action()`, that owns the scaffold;
  every menu handler becomes a thin call supplying only its variation
  points.
- One `remove_action(id, kind)` factory collapsing the three rm handlers.
- One `require_valid_id()` collapsing the three id validators (and removing
  the duplicate `valid_link_id`).
- One `resolve_input_port()` for the port-resolution block.
- Zero behaviour change: identical committed-spec handling, `update()`
  payloads, titles, validation messages, and rebuild behaviour. The
  existing `testServer` suites pass unchanged.

**Non-Goals:**

- Exporting any of the new helpers (deferred; see Open Questions).
- Changing the `blockr.ui` module contracts (committed-spec shapes,
  pool-update protocol) or `blockr.core`.
- The `blockr.ui`-side card-list primitive factor-out (separate change,
  different package).
- Touching `remove_block_action`'s / the non-menu actions' externally
  observable behaviour.
- Generalising across the `new_action()` contract itself. The factory
  produces `new_action()` values, it does not replace the action class.

## Decisions

### 1. A higher-order factory, not an S3 class or config object

`sidebar_menu_action()` is a plain function that returns an action
constructor of the existing `function(trigger, board, update, ...)` shape,
so `board_actions.dock_board()` keeps listing the same constructor symbols
and nothing downstream changes.

```r
sidebar_menu_action <- function(id,
                                server,        # function(module_id) -> committed reactive
                                ui,            # function(ns_id, board, trigger) -> tag
                                title,         # character scalar OR function(trigger) -> chr
                                on_commit,     # function(spec, board, update, session, trigger) -> logical
                                after_commit = NULL,  # function(session, sidebar_id, board, trigger, ui_fn)
                                module_id = "menu") {
  force(id); force(server); force(ui); force(title)
  force(on_commit); force(after_commit); force(module_id)

  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      committed  <- server(module_id)
      menu_ui    <- function() ui(session$ns(module_id), board$board, trigger())
      title_of   <- function() if (is.function(title)) title(trigger()) else title
      rebuild    <- after_commit %||% default_after_commit

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(sidebar_id, title = title_of(), ui = menu_ui())
      })

      observeEvent(committed(), {
        handled <- on_commit(committed(), board$board, update, session, trigger())
        if (isTRUE(handled)) {
          rebuild(session, sidebar_id, board, trigger, menu_ui, title_of)
        }
      })

      NULL
    },
    id = id
  )
}
```

**Why a factory over an S3 action subclass?** The variation is data
(strings, closures), not dispatch on object type. A factory keeps the
`new_action()` contract intact and reads as "fill in the blanks", matching
the team's preference for promoting concrete code to generic only once the
shape is proven, which it now is across six call sites. **Alternative
considered:** a `new_menu_action` S3 subclass of `action`; rejected because
nothing dispatches on it and it would complicate `register_action()`.

**Why pass `server`/`ui` as functions rather than the module name?** The
three modules don't share a uniform `ui(ns, board, ...)` signature (extra
args differ), and the `server`/`ui` pair must come from `blockr.ui` by
name. Passing thunks keeps the factory ignorant of which module it drives
and avoids a brittle string→function lookup. Each handler writes a
one-line `ui = function(ns, board, trg) blockr.ui::append_to(...)`.

### 2. `on_commit` returns a flag; the factory owns the rebuild dispatch

`on_commit` does the domain work (validate → build → `update()`) and
returns `TRUE` when an `update()` happened, `FALSE` (or invisibly) when a
validator short-circuited. The factory only rebuilds when `TRUE`. This
keeps "should the sidebar rebuild?" out of every handler body and
centralises the three rebuild strategies behind `after_commit`.

### 3. Three rebuild strategies, expressed as `after_commit` hooks

- **`default_after_commit`** (deferred keep-or-hide): the stack flows'
  current behaviour, which calls `session$onFlushed(once = TRUE, ...)` then
  `keep_or_hide_sidebar()`, deferred so the rebuilt UI reads the
  post-`update()` board.
- **`sync_after_commit`** (block flows): synchronous
  `keep_or_hide_sidebar()`, with no flush deferral (the block browser rebuilds
  from the registry, not the just-mutated board, so the pre-flush snapshot
  is fine).
- **link flow** supplies its own closure: deferred, recompute
  `link_eligible_pools()`, then `hide_sidebar()` when both pools drain else
  `sendInputMessage(ns("menu-commit"), pool-update)`.

Encoding the strategy as a hook (rather than an enum) means the link flow's
bespoke push isn't special-cased inside the factory.

**Note on the block flows' current sync rebuild:** it is preserved as-is to
guarantee zero behaviour change. Whether the block flows *should* defer
like the stack flows is a latent question, but answering it would change
behaviour and is therefore out of scope.

### 4. Shared leaf helpers

```r
require_valid_id <- function(id, existing, what, session) {
  if (is.null(id) || !nzchar(id) || id %in% existing) {
    notify(paste0("Please choose a valid ", what, " ID."),
           type = "warning", session = session)
    return(FALSE)
  }
  TRUE
}

resolve_input_port <- function(block, block_id, links, explicit = NULL) {
  if (!is.null(explicit) && nzchar(explicit)) return(explicit)
  block_input_select(block, block_id, links, mode = "inputs")[1L]
}
```

`require_valid_id(spec$id, board_block_ids(board), "block", session)` etc.
The existing messages are preserved verbatim (`"Please choose a valid block
ID."` / `"... link ID."` / `"... stack ID."`). `valid_stack_name` and
`valid_stack_color` stay as-is. Their checks (presence, hex format) don't
fit the id-collision shape and there are only two of them; folding them
would be over-abstraction.

### 5. File layout

A new `R/action-menu.R` holds `sidebar_menu_action()`, the `after_commit`
helpers, `remove_action()`, `require_valid_id()`, and `resolve_input_port()`.
`R/action-block.R`, `R/action-link.R`, `R/action-stack.R` keep only the
thin handler definitions and their genuinely domain-specific helpers
(`build_block_from_spec`, `valid_stack_name`, `valid_stack_color`). Add
`'action-menu.R'` to the `Collate` field before the handler files.

## Risks / Trade-offs

- **Over-abstraction risk** → The factory is justified by six concrete call
  sites that already converged independently; this is consolidation of
  proven shape, not speculative generalisation. The leaf helpers each
  replace 3+ copies. `valid_stack_name`/`valid_stack_color` are
  deliberately left un-factored.
- **Hidden behaviour drift during the rewrite** → The three `testServer`
  suites assert the externally observable contract (committed inputs →
  `update()` payloads, validation short-circuits, rebuild calls via mocked
  sidebar bindings). They must pass byte-for-byte unchanged; any required
  test edit is a signal that behaviour drifted and is a refactor bug, not
  an expected churn.
- **`after_commit` signature creep** → The link flow needs `board`,
  `trigger`, `sidebar_id`, `session`, and the `ui` thunk; the default needs
  the same minus the pool logic. Passing the full set to every hook keeps
  one signature. Trade-off: the simple hooks ignore args they don't use -
  acceptable for an internal helper.
- **`force()` discipline** → The factory captures its arguments in
  closures; all must be `force()`d (see `function-factories` guidance) to
  avoid lazy-eval surprises when the same factory is called six times.

## Migration Plan

Pure in-tree refactor, no deploy concerns. Steps:

1. Add `R/action-menu.R` with the factory + helpers; wire `Collate`.
2. Rewrite the six menu handlers + three remove handlers as thin wrappers,
   one file at a time, running that file's `testServer` suite after each.
3. Delete the now-dead private helpers (the duplicate `valid_link_id`, the
   three id validators, the inlined port-resolution blocks).
4. Full `devtools::test()`, then `devtools::check()` and `lintr` under the
   CI config.

Rollback is `git revert` of the single PR; nothing persists outside the
source tree.

## Open Questions

- **Export `sidebar_menu_action()` for extension authors?** Extensions can
  register their own actions via `board_actions.dock_extension()`, so a
  public factory could spare them the scaffold. Deferred: keep it internal
  until a concrete out-of-tree consumer asks, per the team's
  promote-concrete-to-generic-later stance. If exported later, it needs
  `@param`/`@return` docs, a `@keywords internal` vs. full-public decision,
  and a `testServer` example, none of which this refactor blocks.
- **Should the block flows defer their rebuild like the stack flows?**
  Out of scope here (would be a behaviour change); worth a follow-up look.
