library(shiny)
library(blockr.ui)
library(blockr.core)

# A small board mixing arity types so all three layout cases get
# exercised: a source-only block (dataset), a target-only block
# (head, arity 1), an arity > 1 block (merge), and a variadic block
# (rbind).
make_board <- function() {
  new_board(
    blocks = as_blocks(list(
      a = new_dataset_block(),
      h = new_head_block(),
      m = new_merge_block(),
      r = new_rbind_block()
    ))
  )
}

ui <- fluidPage(
  titlePanel("blockr.ui link menu"),
  tags$p(
    "Click a card to add a link in that direction immediately, or ",
    "open a card (chevron) to tweak its link ID / input port first."
  ),
  div(
    style = "display: flex; gap: 8px; margin-bottom: 8px;",
    actionButton("open_a", "Connect a (dataset)", class = "btn-primary"),
    actionButton("open_h", "Connect h (head, arity 1)"),
    actionButton("open_m", "Connect m (merge, arity 2)"),
    actionButton("open_r", "Connect r (variadic)")
  ),
  tags$h4("Last committed spec"),
  verbatimTextOutput("commit"),
  # Hidden text outputs the shinytest2 suite asserts against.
  tags$p(textOutput("commit_source", inline = TRUE)),
  tags$p(textOutput("commit_target", inline = TRUE)),
  tags$p(textOutput("commit_link_id", inline = TRUE)),
  tags$p(textOutput("commit_block_input", inline = TRUE)),
  tags$p(textOutput("commit_count", inline = TRUE)),
  sidebar_ui("panel", side = "right", width = "420px")
)

server <- function(input, output, session) {
  board_rv <- reactiveVal(make_board())
  last_commit <- reactiveVal(NULL)
  count <- reactiveVal(0L)
  current_anchor <- reactiveVal(NULL)

  # Pass the board + anchor as reactives (the same contract the dock's
  # add_link_action uses): the menu then validates the committed link id,
  # resolves the target input slot, and keeps the open panel in sync with
  # the board on every change. `committed()` is therefore a ready-to-apply
  # `blockr.core` links object (one id-keyed link, port already resolved),
  # so this server is a thin adapter - no manual port resolution, no
  # manual pool-update.
  committed <- blockr.ui::link_menu_server(
    "menu",
    board = reactive(board_rv()),
    anchor = reactive(current_anchor())
  )

  open_menu <- function(anchor) {
    current_anchor(anchor)
    show_sidebar(
      "panel",
      title = paste0("Connect ", anchor),
      ui = blockr.ui::link_menu_ui(
        session$ns("menu"), board_rv(), anchor = anchor
      )
    )
  }

  observeEvent(input$open_a, open_menu("a"))
  observeEvent(input$open_h, open_menu("h"))
  observeEvent(input$open_m, open_menu("m"))
  observeEvent(input$open_r, open_menu("r"))

  observeEvent(committed(), {
    links <- committed()
    last_commit(links)
    count(count() + 1L)

    # Add the ready links object to the board. The menu's own board
    # observer then drops the just-wired card in place (no re-render),
    # so the panel can stay open for the next link.
    brd <- board_rv()
    board_links(brd) <- c(board_links(brd), links)
    board_rv(brd)
  })

  # `committed()` is a `links` object; read its single row's parts for
  # the assertion outputs (id / from / to / input).
  commit_row <- reactive({
    lks <- last_commit()
    if (is.null(lks) || length(lks) == 0L) NULL else as.data.frame(lks)
  })

  output$commit <- renderPrint(last_commit())
  output$commit_source <- renderText(commit_row()$from %||% "")
  output$commit_target <- renderText(commit_row()$to %||% "")
  output$commit_link_id <- renderText(commit_row()$id %||% "")
  output$commit_block_input <- renderText(commit_row()$input %||% "")
  output$commit_count <- renderText(as.character(count()))
}

shinyApp(ui, server)
