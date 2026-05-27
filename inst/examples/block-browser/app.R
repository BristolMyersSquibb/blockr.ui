library(shiny)
library(blockr.ui)
library(blockr.core)

# A minimal board with a couple of blocks so we have a real `trigger_id`
# to point append / prepend at. `merge_block` has arity 2 so prepend
# shows the target-input picker; `head_block` has arity 1 so prepend
# hides it. Adjust to taste when experimenting.
board <- new_board(
  blocks = list(
    src    = new_dataset_block(),
    head1  = new_head_block(),
    merge1 = new_merge_block()
  )
)

ui <- fluidPage(
  titlePanel("blockr.ui block browser"),
  tags$p(
    "Click a card to add it immediately with defaults, or open a card",
    " (chevron) to tweak its id / title / link first."
  ),
  div(
    style = "display: flex; gap: 8px; margin-bottom: 8px;",
    actionButton("open_add", "Add (no trigger)", class = "btn-primary"),
    actionButton("open_append", "Append from 'src'"),
    actionButton("open_prepend_h", "Prepend to 'head1' (arity 1)"),
    actionButton("open_prepend_m", "Prepend to 'merge1' (arity 2)")
  ),
  tags$h4("Last commit spec"),
  verbatimTextOutput("commit"),
  # Hidden outputs read by the shinytest2 suite.
  tags$p(textOutput("commit_type", inline = TRUE)),
  tags$p(textOutput("commit_id", inline = TRUE)),
  sidebar_ui("panel", side = "right", width = "420px")
)

server <- function(input, output, session) {
  last_commit <- reactiveVal(NULL)

  # One block browser module instance, reused across the four triggers.
  added <- blockr.ui::block_browser_server("browser")

  open_browser <- function(mode, trigger_id = NULL) {
    show_sidebar(
      "panel",
      title = switch(mode,
        add = "Add new block",
        append = "Append block",
        prepend = "Prepend block"
      ),
      ui = blockr.ui::block_browser_ui(
        session$ns("browser"), board, mode = mode, trigger_id = trigger_id
      )
    )
  }

  observeEvent(input$open_add, open_browser("add"))
  observeEvent(input$open_append, open_browser("append", "src"))
  observeEvent(input$open_prepend_h, open_browser("prepend", "head1"))
  observeEvent(input$open_prepend_m, open_browser("prepend", "merge1"))

  observeEvent(added(), last_commit(added()))

  output$commit <- renderPrint({
    spec <- last_commit()
    if (is.null(spec)) cat("(no commit yet)") else str(spec)
  })

  output$commit_type <- renderText({
    spec <- last_commit()
    if (is.null(spec)) "" else spec$type
  })
  output$commit_id <- renderText({
    spec <- last_commit()
    if (is.null(spec) || is.null(spec$id)) "" else spec$id
  })
}

shinyApp(ui, server)
