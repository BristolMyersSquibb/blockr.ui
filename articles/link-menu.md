# The link menu

[`blockr.ui::link_menu_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/link-menu.md)
is a **bidirectional** card-list link picker. It lives inside the
[sidebar
primitive](https://bristolmyerssquibb.github.io/blockr.ui/articles/sidebar.md)
and replaces the legacy selectize-style chooser used by `blockr.dock`’s
add-link flow. Each card represents a board *instance* that the new link
can attach to, and the menu computes eligibility against a fixed
*anchor* block (the right-clicked block in the dock flow).

## Two directions, one anchor

`link_menu_ui(id, board, anchor)` renders up to two category-grouped
sections relative to `anchor`. When both render, INPUT FROM appears
above OUTPUT TO so the order matches the natural left-to-right data flow
(sources feed the anchor; the anchor feeds targets):

- **INPUT FROM** (INCOMING, card -\> anchor): blocks that can link
  *into* the anchor. Only rendered when the anchor itself has at least
  one free named input port or is variadic. Eligibility = every other
  board block, excluding any that the anchor already reaches (would
  close a cycle).
- **OUTPUT TO** (OUTGOING, anchor -\> card): blocks the anchor can link
  *to*. Eligibility = every other board block with at least one free
  named input port or variadic arity, excluding any that already reach
  the anchor (would close a cycle).

If both pools are empty the panel renders an in-place empty-state
instead of returning `NULL`. This means right-clicking a downstream
block with free inputs no longer surfaces the legacy “No inputs are
currently available” warning - the menu opens with its INCOMING section
populated.

## Commit contract

The JS binding publishes
`list(source, target, link_id, block_input, nonce)` on `input$commit`.
`source` / `target` are derived from the clicked card’s `data-direction`
and the panel’s `data-anchor`:

- `data-direction = "outgoing"` -\> `source = anchor`, `target = card`
- `data-direction = "incoming"` -\> `source = card`, `target = anchor`

`link_menu_server(id)` strips the internal `nonce` and returns a
`reactive` whose value is the spec. The consumer doesn’t have to know
the orientation up front - the spec carries both ends explicitly.

## Multi-link sessions

Single click commits one link, but the menu is designed for repeated
adds: the consumer pushes a typed `pool-update` payload to the open
panel via `session$sendInputMessage()` after each commit, and the
binding’s `receiveMessage` handler hides the just-wired card
client-side. Card-expansion state, scroll position, and the search
filter value are preserved. If both pools drain to zero, the consumer
calls
[`hide_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
and the panel closes itself.

`link_eligible_pools(board, anchor)` is exported so the consumer
recomputes the post-commit pool against the same eligibility logic the
menu uses for its initial render - one source of truth shared across
both call sites.

## Minimal example

``` r

library(shiny)
library(blockr.ui)
library(blockr.core)

board <- new_board(
  blocks = as_blocks(list(
    a = new_dataset_block(),
    h = new_head_block(),
    m = new_merge_block()
  ))
)

ui <- fluidPage(
  sidebar_ui("panel", side = "right", width = "420px"),
  actionButton("open", "Connect a"),
  verbatimTextOutput("spec")
)

server <- function(input, output, session) {
  committed <- link_menu_server("menu")

  observeEvent(input$open, {
    show_sidebar(
      "panel",
      title = "Connect a",
      ui = link_menu_ui(session$ns("menu"), board, anchor = "a")
    )
  })

  output$spec <- renderPrint(committed())
}

shinyApp(ui, server)
```

A runnable copy is shipped at:

``` r

system.file("examples", "link-menu", "app.R", package = "blockr.ui")
```

The bundled example exercises all four anchor cases (source-only,
arity-1 target, arity-N target, variadic) and demonstrates the live
`pool-update` flow.
