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
