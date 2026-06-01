# Driving the sidebar primitive

`blockr.ui` ships a Bootstrap-free slide-in panel that mirrors
[`shiny::showModal()`](https://rdrr.io/pkg/shiny/man/showModal.html) /
[`shiny::removeModal()`](https://rdrr.io/pkg/shiny/man/showModal.html)
in spirit. Mount the panel once per page with
[`sidebar_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md),
then drive its content from any
[`moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html) body
using
[`show_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
and
[`hide_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md).

## Minimal example

``` r

library(shiny)
library(blockr.ui)

ui <- fluidPage(
  actionButton("open", "Open sidebar"),
  sidebar_ui("main_sidebar")
)

server <- function(input, output, session) {
  observeEvent(input$open, {
    show_sidebar(
      "main_sidebar",
      title = "Add a new block",
      ui = tagList(
        textInput("block_name", "Name"),
        selectInput("block_kind", "Type", c("data", "transform", "plot")),
        actionButton("confirm", "Add", class = "btn-primary")
      )
    )
  })

  observeEvent(input$confirm, {
    # validate, mutate state, ...
    hide_sidebar("main_sidebar")
  })
}

shinyApp(ui, server)
```

The inputs inside the body (`input$block_name`, `input$block_kind`,
`input$confirm`) live in the calling session - exactly as they would
inside a
[`modalDialog()`](https://rdrr.io/pkg/shiny/man/modalDialog.html). No
nested
[`moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html) is
required.

## Observing user-driven dismissals

Each
[`sidebar_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
call also registers a Shiny input under the same id, exposing two
logical fields:

- `input[[id]]$open` - `TRUE` while the panel is open, `FALSE` once the
  user closes it (Esc, X button) or the server calls
  [`hide_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md).
- `input[[id]]$pinned` - `TRUE` while the user has the panel pinned.

``` r

observe({
  state <- input$main_sidebar
  req(state)
  cat("open:", state$open, " pinned:", state$pinned, "\n")
})
```

## Static body + client-side trigger

For a panel whose body is known at UI-build time (a settings form, a
help panel, an “About” sheet), you don’t need a server round-trip to
open it. Pre-render the body at
[`sidebar_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
time and put `data-blockr-sidebar-target = "<id>"` on any element that
should toggle it.

``` r

ui <- fluidPage(
  actionButton(
    "settings_btn", icon("gear"),
    `data-blockr-sidebar-target` = "settings_sidebar"
  ),
  sidebar_ui(
    "settings_sidebar",
    side = "left",
    title = "Board options",
    ui = tagList(
      checkboxInput("opt_a", "Option A", value = TRUE),
      sliderInput("opt_b", "Option B", min = 0, max = 100, value = 50)
    )
  )
)
```

Clicking the gear toggles the panel open / closed entirely on the
client. The inputs inside (`input$opt_a`, `input$opt_b`) are observable
from the server exactly as if they sat in the page directly. A later
`show_sidebar("settings_sidebar")` (no `ui`) still opens the panel from
the server when needed; `show_sidebar("settings_sidebar", ui = <tags>)`
replaces the pre-rendered body if you want to swap it dynamically.

## One id per concern

[`show_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
replaces the panel’s body and title in place, and pin state is bound to
the panel element. Re-showing the same panel with fresh content from the
same caller (the “chain after confirm” flow, which
[`keep_or_hide_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
builds on) is the supported pattern.

Two unrelated concerns - say a “Settings” panel and an “Add block”
form - should mount two
[`sidebar_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
panels with distinct ids. Sharing one id between unrelated callers means
the second
[`show_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
will silently overwrite the first, including any pinned content the user
expected to stay put.

## Auto-open on an empty board

A common renderer pattern is to open the sidebar with a “first block”
hint when the session starts on an empty board, while still respecting a
later user dismissal. The recipe is built on top of
[`show_sidebar()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md)
and `input[[id]]$open` - no special opt-in is needed on
[`sidebar_ui()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/sidebar.md).

``` r

empty_board_renderer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    auto_opened <- reactiveVal(FALSE)

    observe({
      if (auto_opened()) return()
      if (length(blockr.core::board_blocks(board)) > 0L) return()

      show_sidebar(
        session$ns("main_sidebar"),
        title = "Add your first block",
        ui = tagList(
          p("Pick a block to get started."),
          actionButton(session$ns("add_dataset"), "Dataset block"),
          actionButton(session$ns("add_import"), "Import data")
        )
      )
      auto_opened(TRUE)
    })

    # Once the user dismisses the hint, input[[id]]$open flips to FALSE
    # and the auto_opened flag prevents this observer from re-opening.
  })
}
```

Push-mode panels reflow the page content via padding on `<html>`, driven
by `--blockr-sidebar-width-left` and `--blockr-sidebar-width-right`
custom properties (both nonzero when both sides are open). The legacy
`--blockr-sidebar-width` is also set to the open side’s width when a
single panel is open, so existing consumers of the form
`margin-right: var(--blockr-sidebar-width, 0px)` keep working.
