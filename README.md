
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.ui

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![status](https://github.com/BristolMyersSquibb/blockr.ui/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.ui/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.ui/graph/badge.svg)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.ui)
[![CRAN
status](https://www.r-pkg.org/badges/version/blockr.ui)](https://CRAN.R-project.org/package=blockr.ui)
<!-- badges: end -->

User-interface primitives shared across the
[blockr](https://blockr.site/) ecosystem. This release ships a
Bootstrap-free slide-in sidebar panel that mirrors `shiny::showModal()`
/ `shiny::removeModal()` in spirit and is intended to replace the modal
dialogs used by `blockr.dock` for board mutations. Designed to sit
between `blockr.core` and renderers such as `blockr.dock`, so extensions
and embedded apps can adopt the same panel without pulling in
`dockViewR`.

## Installation

You can install the development version of blockr.ui from
[GitHub](https://github.com/BristolMyersSquibb/blockr.ui) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.ui")
```

## Example

The four-function API mirrors `shiny::showModal()` /
`shiny::removeModal()`: mount the panel once with `sidebar_ui()`, then
drive its content with `show_sidebar()` / `hide_sidebar()` from any
`moduleServer()` body — exactly like a modal.

``` r
library(shiny)
library(blockr.ui)

ui <- fluidPage(
  titlePanel("blockr.ui sidebar primitive"),
  actionButton("open", "Open sidebar", class = "btn-primary"),
  verbatimTextOutput("state"),
  sidebar_ui("main_sidebar")
)

server <- function(input, output, session) {
  observeEvent(input$open, {
    show_sidebar(
      "main_sidebar",
      title = "Add a new block",
      ui = tagList(
        textInput("block_name", "Name"),
        selectInput(
          "block_kind",
          "Type",
          c("data", "transform", "plot")
        ),
        actionButton("confirm", "Add", class = "btn-primary")
      )
    )
  })

  observeEvent(input$confirm, {
    hide_sidebar("main_sidebar")
  })

  output$state <- renderPrint({
    state <- input$main_sidebar
    if (is.null(state)) return("sidebar: not yet rendered")
    sprintf(
      "sidebar: open = %s, pinned = %s",
      state$open, state$pinned
    )
  })
}

shinyApp(ui, server)
```

Inputs inside the panel body (`input$block_name`, `input$confirm`, …)
live in the calling session — no nested `moduleServer()` is required.
The panel itself is also a Shiny input, so `input[["main_sidebar"]]`
exposes `list(open, pinned)` for observing user-driven dismissals (Esc,
X button, outside click).

## What’s in the panel

- **Pin toggle** — when pinned, the panel stays open while you click
  elsewhere on the page. The pinned width is exposed as
  `--blockr-sidebar-width` on `<body>` so apps can reflow the main
  canvas (`margin-right: var(--blockr-sidebar-width, 0px)`).
- **Esc / outside-click / X-button** close the panel when not pinned.
- **Focus trap** — Tab and Shift+Tab cycle within the panel while it is
  open; on close, focus returns to the element that opened it.
- **Bootstrap-free** — no `.offcanvas` / `.modal` / `data-bs-*`. The
  panel is a plain `<div>` + scoped CSS + a single `Shiny.InputBinding`.

## Roadmap

`blockr.ui` is the home for cross-cutting UI extracted from
`blockr.dock` over time. The current scope is intentionally tiny — one
primitive, four exports — so each follow-up extraction (design tokens,
block card, app shell, selectize widgets, block browser) can land as its
own pull request without churning the API.

## Code of Conduct

Please note that the blockr.ui project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
