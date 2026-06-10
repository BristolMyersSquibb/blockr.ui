# Link menu

A bidirectional card-list link picker. The link menu lives inside the
sidebar primitive and replaces the legacy selectize-style chooser used
by `blockr.dock`'s add-link flow. Each card represents a board
**instance** that the new link can attach to from either direction
relative to a fixed `anchor` block (the right-clicked block in the dock
flow).

## Usage

``` r
link_menu_ui(id, board, anchor)

link_menu_server(id, board = NULL, anchor = NULL)

link_menu_dep()

link_eligible_pools(board, anchor)
```

## Arguments

- id:

  Module id. As usual for Shiny modules, pass `NS(id)("...")` from the
  parent at the `*_ui()` call site and the bare id to `*_server()`.

- board:

  The current board state. In `link_menu_ui()` a plain board value; in
  `link_menu_server()` it may be a **reactive** returning the board (to
  opt into live sync + validation) or `NULL` (default; static snapshot).

- anchor:

  Block id the new link is anchored on (the right-clicked block in the
  dock flow). A non-empty character scalar; must be a member of
  [`blockr.core::board_block_ids()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.html).
  `link_menu_server()` also accepts a reactive (needed to recompute the
  eligible pools on each board change).

## Value

- `link_menu_ui()` returns an
  [htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
  with `link_menu_dep()` attached.

- `link_menu_server()` returns a
  [shiny::reactive](https://rdrr.io/pkg/shiny/man/reactive.html) of the
  committed
  [blockr.core::links](https://bristolmyerssquibb.github.io/blockr.core/reference/new_link.html)
  object (one id-keyed link), firing once per commit.

- `link_menu_dep()` returns an
  [htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html).

- `link_eligible_pools()` returns
  `list(outgoing = <chr>, incoming = <chr>)` using the same eligibility
  rules `link_menu_ui()` uses for its initial render.

## Details

The link menu is a Shiny module. `link_menu_ui(id, board, anchor)`
renders the panel; `link_menu_server(id, board, anchor)` returns a
reactive that fires once per commit with the link spec:
`list(source, target, link_id, block_input)`. When `board` is passed to
the server as a **reactive** (with the `anchor`), the module owns
link-id validation (via
[`blockr.core::notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.html))
and keeps an open menu in sync with the board: on every board change it
pushes a `menu:sync` diff that adds newly-eligible cards, removes
vanished ones, and refreshes ports - no re-render, so scroll,
card-expansion, and in-progress inputs are preserved. `board = NULL`
(default) keeps the static snapshot behaviour. The JS binding composes
`source` / `target` from the clicked card's `data-direction` and the
panel's `data-anchor`, so the consumer never has to know the orientation
up front:

- `data-direction = "outgoing"` -\> `source = anchor`, `target = card`

- `data-direction = "incoming"` -\> `source = card`, `target = anchor`

The panel renders up to two category-grouped sections, labelled from the
anchor's port-role perspective:

- **OUTPUT TO** (anchor -\> card): blocks the anchor can link to.
  Eligible = every other board block that has at least one free named
  input port or is variadic, excluding any that already reach the anchor
  (would close a cycle).

- **INPUT FROM** (card -\> anchor): blocks that can link into the
  anchor. Only rendered when the anchor itself has at least one free
  named input port or is variadic. Eligible = every other board block,
  excluding any that the anchor already reaches.

When both pools are empty the panel renders an in-place empty-state
instead of returning `NULL`.

Internally the panel publishes its committed link spec through the
`blockr.ui.linkMenu` `Shiny.InputBinding` (the root element's `id` is
`NS(id)("commit")`). The binding's `receiveMessage` accepts a
`list(type = "pool-update", eligible = list(outgoing, incoming), link_id_seed)`
payload so consumers can keep the menu open across multiple commits and
push live eligibility updates without re-rendering - the multi-link
session pattern. Use `link_eligible_pools()` to compute the post-commit
pool on the consumer side and send it via `session$sendInputMessage()`
(see [`shiny::session`](https://rdrr.io/pkg/shiny/man/session.html)).

## Examples

``` r
if (interactive()) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      sidebar_ui("panel", side = "right"),
      shiny::actionButton("open", "Open link menu")
    ),
    server = function(input, output, session) {
      board <- blockr.core::new_board(
        blockr.core::as_blocks(list(
          a = blockr.core::new_dataset_block(),
          b = blockr.core::new_head_block()
        ))
      )
      committed <- link_menu_server("menu")
      shiny::observeEvent(input$open, {
        show_sidebar(
          "panel", title = "Connect a",
          ui = link_menu_ui(session$ns("menu"), board, anchor = "a")
        )
      })
      shiny::observeEvent(committed(), print(committed()))
    }
  )
}
```
