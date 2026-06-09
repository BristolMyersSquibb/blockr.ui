# Stack menu

A multi-select card-list block picker for stacks. The stack menu lives
inside the sidebar primitive and replaces the legacy selectize-style
chooser used by `blockr.dock`'s add / edit stack flows. Each card
represents a board **instance** (a block already on the board); clicking
a card toggles its membership in the new stack. A small panel-level form
below the cards carries the stack-level parameters (name, color, id).

## Usage

``` r
stack_menu_ui(id, board, target = NULL)

stack_menu_server(id, board = NULL, target = NULL)

stack_menu_dep()
```

## Arguments

- id:

  Module id. As usual for Shiny modules, pass `NS(id)("...")` from the
  parent at the `*_ui()` call site and the bare id to `*_server()`.

- board:

  The current board state. Used to resolve the eligible pool of board
  blocks and (in edit mode) to look the target stack up. In
  `stack_menu_ui()` this is a plain board value; in
  `stack_menu_server()` it may be a **reactive** returning the board (to
  opt into live sync + validation) or `NULL` (default; static snapshot,
  no validation).

- target:

  Stack to edit. `NULL` (default) selects the *create* flow; a non-empty
  character scalar selects the *edit* flow and names the stack being
  edited. `stack_menu_server()` also accepts a reactive. The parameter
  name mirrors
  [`block_browser_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/block-browser.md).

## Value

- `stack_menu_ui()` returns an
  [htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
  with `stack_menu_dep()` attached.

- `stack_menu_server()` returns a
  [shiny::reactive](https://rdrr.io/pkg/shiny/man/reactive.html) of the
  committed
  [blockr.core::stacks](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.html)
  object (one id-keyed `dock_stack`), firing once per confirm. Requires
  the suggested `blockr.dock` package.

- `stack_menu_dep()` returns an
  [htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html).

## Details

The stack menu is a Shiny module. `stack_menu_ui(id, board, target)`
renders the panel; `stack_menu_server(id, board, target)` returns a
reactive that fires once per confirm with a
[blockr.core::stacks](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.html)
object whose single member is a `dock_stack` built via
[`blockr.dock::new_dock_stack()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.html),
carrying the chosen name and colour and keyed by its id - the typed id
in create mode, the edited stack's id (`target`) in edit mode. The
consumer adds it to the board as-is, with no reshaping. Colour is a
first-class `dock_stack` attribute, so the menu requires the suggested
`blockr.dock` package.

The server owns **validation** of the committed spec: it checks the
stack id (create mode), name, and colour against the current board and
surfaces failures via
[`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html),
so the committed reactive only ever fires with a valid spec and the
consumer needs no validators of its own. When `board` is passed to
`stack_menu_server()` as a **reactive**, the module also keeps an open
menu in sync with the board: on every board change it pushes a
`menu:sync` diff to its own binding that inserts newly-eligible cards,
removes vanished ones, and leaves scroll / selection / in-progress
inputs untouched - no re-render. Pass `board = NULL` (the default) for
the static snapshot behaviour.

The flow is selected by `target`: `NULL` (default) is the *create* flow;
a character scalar names the stack being edited. In edit mode the
function pre-selects the stack's current blocks, augments the eligible
pool with them (so the user can deselect them), and pre-fills the name /
color fields. An unknown id raises a clean error.

Internally the panel publishes the selection set through the
`blockr.ui.stackMenu` `Shiny.InputBinding` (the root element's `id` is
`NS(id)("commit")`); the published value carries an internal `nonce` so
repeat confirms re-fire as events. The panel-level name / color / id
fields are normal Shiny inputs the server reads directly;
`stack_menu_server()` strips the nonce and composes the full spec.

## Examples

``` r
if (interactive()) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      sidebar_ui("panel", side = "right"),
      shiny::actionButton("open", "Open stack menu")
    ),
    server = function(input, output, session) {
      committed <- stack_menu_server("menu")
      shiny::observeEvent(input$open, {
        show_sidebar(
          "panel",
          title = "New stack",
          ui = stack_menu_ui(session$ns("menu"), NULL)
        )
      })
      shiny::observeEvent(committed(), print(committed()))
    }
  )
}
```
