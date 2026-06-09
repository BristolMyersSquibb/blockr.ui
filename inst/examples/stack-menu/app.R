library(shiny)
library(blockr.ui)
library(blockr.core)

# A small board: three un-stacked blocks plus one stack ("s1") with two
# members, so both the create flow (eligible = un-stacked) and the edit
# flow (s1 augmented with its members) have something to render.
board <- new_board(
  blocks = list(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_subset_block(),
    d = new_scatter_block(),
    e = new_dataset_block()
  ),
  stacks = list(s1 = new_stack(c("d", "e")))
)

ui <- fluidPage(
  titlePanel("blockr.ui stack menu"),
  tags$p(
    "Click cards to toggle membership, edit name / color / id, then ",
    "Create or Update."
  ),
  div(
    style = "display: flex; gap: 8px; margin-bottom: 8px;",
    actionButton("open_create", "Create stack", class = "btn-primary"),
    actionButton("open_edit", "Edit stack s1")
  ),
  tags$h4("Last committed spec"),
  verbatimTextOutput("commit"),
  # Hidden text outputs the shinytest2 suite asserts against.
  tags$p(textOutput("commit_blocks", inline = TRUE)),
  tags$p(textOutput("commit_name", inline = TRUE)),
  tags$p(textOutput("commit_color", inline = TRUE)),
  tags$p(textOutput("commit_id", inline = TRUE)),
  tags$p(textOutput("commit_count", inline = TRUE)),
  sidebar_ui("panel", side = "right", width = "420px")
)

server <- function(input, output, session) {
  last_commit <- reactiveVal(NULL)
  count <- reactiveVal(0L)

  # Track which stack (if any) the menu is currently editing, so the
  # server keys its committed stacks object by the right id (the real
  # dock passes `target = reactive(trigger())` the same way).
  current_target <- reactiveVal(NULL)

  committed <- blockr.ui::stack_menu_server(
    "menu", target = reactive(current_target())
  )

  open_menu <- function(title, target = NULL) {
    current_target(target)
    show_sidebar(
      "panel",
      title = title,
      ui = blockr.ui::stack_menu_ui(session$ns("menu"), board, target)
    )
  }

  observeEvent(input$open_create, open_menu("Create stack"))
  observeEvent(input$open_edit, open_menu("Edit stack s1", target = "s1"))

  observeEvent(committed(), {
    spec <- committed()
    last_commit(spec)
    count(count() + 1L)
  })

  # `committed()` is a blockr.core `stacks` object (one id-keyed stack,
  # colour carried as an attribute), so read its parts via the accessors.
  commit_stack <- reactive({
    sks <- last_commit()
    if (is.null(sks) || length(sks) == 0L) NULL else sks[[1L]]
  })

  output$commit <- renderPrint(last_commit())
  output$commit_blocks <- renderText({
    stk <- commit_stack()
    if (is.null(stk)) {
      ""
    } else {
      paste(blockr.core::stack_blocks(stk), collapse = ",")
    }
  })
  output$commit_name <- renderText({
    stk <- commit_stack()
    if (is.null(stk)) "" else blockr.core::stack_name(stk)
  })
  output$commit_color <- renderText({
    stk <- commit_stack()
    if (is.null(stk)) "" else attr(stk, "color") %||% ""
  })
  output$commit_id <- renderText({
    sks <- last_commit()
    if (is.null(sks) || length(sks) == 0L) "" else names(sks)[[1L]]
  })
  output$commit_count <- renderText(as.character(count()))
}

shinyApp(ui, server)
