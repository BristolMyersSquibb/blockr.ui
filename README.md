
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
[blockr](https://blockr.site/) ecosystem. Designed to sit between
`blockr.core` and renderers such as `blockr.dock`.

## Installation

You can install the development version of blockr.ui from
[GitHub](https://github.com/BristolMyersSquibb/blockr.ui) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.ui")
```

## Example

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

<figure>
<img src="man/figures/sidebar-minimal.png"
alt="blockr.ui minimal sidebar example" />
<figcaption aria-hidden="true">blockr.ui minimal sidebar
example</figcaption>
</figure>

## Code of Conduct

Please note that the blockr.ui project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
