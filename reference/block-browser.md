# Block browser

A card-list block picker designed to live inside the sidebar primitive.
Replaces the legacy single-select form used for the add / append /
prepend block flows in `blockr.dock`. Each card represents a registered
block (one row of
[`blockr.core::available_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.html)),
grouped by category and filterable with the search bar.

## Usage

``` r
block_browser_ui(id, board, target = NULL)

append_to(block_id)

prepend_to(block_id)

block_browser_server(id)

block_browser_dep()
```

## Arguments

- id:

  Module id. As usual for Shiny modules, pass `NS(id)("...")` from the
  parent at the `*_ui()` call site and the bare id to `*_server()`.

- board:

  The current board state. Used to resolve the target block's input
  arity (for prepend) and to seed unique default ids avoiding the
  board's existing block / link ids.

- target:

  Where the new block attaches. `NULL` (default) is a plain add;
  `append_to()` appends from a source block; `prepend_to()` prepends
  into a target block.

- block_id:

  Block id the new block attaches to: a source block for `append_to()`,
  a target block for `prepend_to()`.

## Value

- `block_browser_ui()` returns an
  [htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
  with `block_browser_dep()` attached.

- `block_browser_server()` returns a
  [shiny::reactive](https://rdrr.io/pkg/shiny/man/reactive.html) of the
  committed block spec (a list), firing once per add.

- `append_to()` / `prepend_to()` return a `bb_target` descriptor.

- `block_browser_dep()` returns an
  [htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html).

## Details

Adding a block is single-shot and works two ways:

- **Click the card body** to add the block immediately with sensible
  defaults (a freshly-generated unique id, the block's default title,
  and - for append / prepend - a default link and port).

- **Open the card** via its chevron to tweak the id, title, link id and
  input port first, then click the in-card add button.

The block browser is a Shiny module. `block_browser_ui(id, ...)` renders
the panel; `block_browser_server(id)` returns a reactive that fires once
per add with the block to create:
`list(type, id, title, link_id, block_input, target_input)`. Fields not
applicable to the current flow are `NULL`. Because the suggested ids are
generated avoiding the board's existing block / link ids, clicking the
same card repeatedly (with a pinned sidebar) yields distinct blocks.

The flow is selected by the `target` argument: `NULL` (default) adds a
standalone block, `append_to()` appends from a source block, and
`prepend_to()` prepends into a target block. Because the append /
prepend constructors always carry the block they attach to, the flow and
its trigger can never disagree.

Internally the panel publishes its value through the
`blockr.ui.blockBrowser` `Shiny.InputBinding` (the root element's `id`
is `NS(id)("commit")`); the value carries an internal `nonce` so repeat
adds re-fire as events. `block_browser_server()` strips the nonce and
hands back the clean spec, so consumers never see it.

## Examples

``` r
if (interactive()) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      sidebar_ui("panel", side = "right"),
      shiny::actionButton("open", "Open block browser")
    ),
    server = function(input, output, session) {
      added <- block_browser_server("browser")
      shiny::observeEvent(input$open, {
        show_sidebar(
          "panel",
          title = "Add new block",
          ui = block_browser_ui(session$ns("browser"), NULL)
        )
      })
      shiny::observeEvent(added(), print(added()))
    }
  )
}
```
