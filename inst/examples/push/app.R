library(shiny)
library(blockr.ui)

ui <- fluidPage(
  titlePanel("blockr.ui sidebar -- push mode"),
  p(
    "When the panel opens it pushes the page content aside via the bundled",
    " ", code("body.blockr-body-pushed-right"), " class plus",
    " ", code("--blockr-sidebar-width"), " on ", code("<body>"),
    " (vs the default overlay mode where the panel hovers above the page)."
  ),
  actionButton("open", "Open sidebar", class = "btn-primary"),
  sidebar_ui("main_sidebar", side = "right", mode = "push")
)

server <- function(input, output, session) {
  observeEvent(input$open, {
    show_sidebar(
      "main_sidebar",
      title = "Push-mode panel",
      ui = tagList(
        p("Notice the page content shifted left to make room."),
        textInput("name", "Name"),
        actionButton("ok", "OK", class = "btn-primary")
      )
    )
  })

  observeEvent(input$ok, hide_sidebar("main_sidebar"))
}

shinyApp(ui, server)
