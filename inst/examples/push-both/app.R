library(shiny)
library(blockr.ui)

ui <- fluidPage(
  titlePanel("blockr.ui sidebar -- push mode, both sides"),
  p(
    "Opening both sidebars constrains the page content between them",
    " rather than hiding it under whichever opens second. Each side",
    " contributes its own width via",
    " ", code("--blockr-sidebar-width-left"), " /",
    " ", code("--blockr-sidebar-width-right"), " on ", code("<html>"), "."
  ),
  actionButton("open_left", "Open left", class = "btn-primary"),
  actionButton("open_right", "Open right", class = "btn-primary"),
  sidebar_ui("left_sidebar", side = "left", mode = "push"),
  sidebar_ui("right_sidebar", side = "right", mode = "push")
)

server <- function(input, output, session) {
  observeEvent(input$open_left, {
    show_sidebar(
      "left_sidebar",
      title = "Left panel",
      ui = tagList(
        p("Left side; page is pushed right."),
        actionButton("close_left", "Close")
      )
    )
  })
  observeEvent(input$open_right, {
    show_sidebar(
      "right_sidebar",
      title = "Right panel",
      ui = tagList(
        p("Right side; page is pushed left."),
        actionButton("close_right", "Close")
      )
    )
  })

  observeEvent(input$close_left, hide_sidebar("left_sidebar"))
  observeEvent(input$close_right, hide_sidebar("right_sidebar"))
}

shinyApp(ui, server)
