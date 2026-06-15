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
  # The add browser's card list never varies, so its body is pre-rendered
  # ONCE here. Opening it (below) just toggles the panel - no re-render,
  # no server round-trip for the markup. This is the pattern the dock
  # uses to avoid rebuilding the catalogue on every open.
  sidebar_ui(
    "add_panel",
    side = "right", width = "420px",
    title = "Add new block",
    ui = block_browser_ui("add_browser", board)
  ),
  # Append / prepend depend on the right-clicked block, so that browser is
  # target-specific and rendered per open (the dynamic show_sidebar path).
  sidebar_ui("edit_panel", side = "right", width = "420px")
)

server <- function(input, output, session) {
  last_commit <- reactiveVal(NULL)
  current_target <- reactiveVal(NULL)

  # Two browser instances, both fed the board so they return ready-to-
  # apply values: a `blocks` object for add, or `list(blocks, links)` for
  # append / prepend with the port resolved.
  added_add <- block_browser_server("add_browser", board = reactive(board))
  added_edit <- block_browser_server(
    "edit_browser",
    board = reactive(board),
    target = reactive(current_target())
  )

  # Add: open the pre-rendered panel (no `ui` -> no re-render).
  observeEvent(input$open_add, show_sidebar("add_panel"))

  # Append / prepend: render the target-specific browser on open.
  open_edit <- function(title, target) {
    current_target(target)
    show_sidebar(
      "edit_panel",
      title = title,
      ui = block_browser_ui("edit_browser", board, target)
    )
  }
  observeEvent(input$open_append,
               open_edit("Append block", append_to("src")))
  observeEvent(input$open_prepend_h,
               open_edit("Prepend block", prepend_to("head1")))
  observeEvent(input$open_prepend_m,
               open_edit("Prepend block", prepend_to("merge1")))

  observeEvent(added_add(), last_commit(added_add()))
  observeEvent(added_edit(), last_commit(added_edit()))

  # `last_commit()` is either a `blocks` object (add) or `list(blocks,
  # links)` (append / prepend); pull the committed block out of either.
  commit_blocks <- reactive({
    res <- last_commit()
    if (is.null(res)) {
      NULL
    } else if (blockr.core::is_blocks(res)) {
      res
    } else {
      res$blocks
    }
  })

  output$commit <- renderPrint({
    res <- last_commit()
    if (is.null(res)) cat("(no commit yet)") else str(res)
  })

  output$commit_type <- renderText({
    blks <- commit_blocks()
    if (is.null(blks)) "" else class(blks[[1L]])[1L]
  })
  output$commit_id <- renderText({
    blks <- commit_blocks()
    if (is.null(blks)) "" else names(blks)[1L]
  })
}

shinyApp(ui, server)
