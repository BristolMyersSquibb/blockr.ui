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

  committed <- blockr.ui::link_menu_server("menu")

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

  # Resolve which input slot the new link wires to. `spec$block_input`
  # is set when the user picked one from the per-card form (target had
  # arity > 1 free at render time). When NULL we fall back to:
  #
  # * variadic target: a fresh integer slot ("1", "2", ...), same
  #   logic `blockr.dock::block_input_select()` applies on the dock
  #   side.
  # * non-variadic target: the first *free* named input port (filter
  #   `block_inputs()` by already-wired ports). Never just
  #   `block_inputs(blk)[[1L]]` because the first-registered port may
  #   already be consumed; using it server-side trips
  #   `validate_links: Block <id> has multiple identical inputs`.
  resolve_input <- function(brd, target, requested) {
    if (!is.null(requested) && nzchar(requested)) return(requested)
    blk <- board_blocks(brd)[[target]]
    links_df <- as.data.frame(board_links(brd))
    used <- as.character(links_df$input[links_df$to == target])
    if (is.na(block_arity(blk))) {
      nums <- suppressWarnings(as.integer(used))
      nums <- nums[!is.na(nums)]
      if (length(nums) == 0L) return("1")
      missing_slots <- setdiff(seq_len(max(nums)), nums)
      if (length(missing_slots)) {
        as.character(min(missing_slots))
      } else {
        as.character(max(nums) + 1L)
      }
    } else {
      free <- setdiff(block_inputs(blk), used)
      if (length(free) == 0L) {
        stop("Target ", target, " has no free named input ports.")
      }
      free[[1L]]
    }
  }

  observeEvent(committed(), {
    spec <- committed()
    last_commit(spec)
    count(count() + 1L)

    # Apply the link to the board so the next pool-update push reflects
    # the new state.
    brd <- board_rv()
    new_lnk <- new_link(
      from = spec$source, to = spec$target,
      input = resolve_input(brd, spec$target, spec$block_input)
    )
    new_lnks <- as_links(set_names(list(new_lnk), spec$link_id))
    blockr.core::board_links(brd) <- c(
      board_links(brd), new_lnks
    )
    board_rv(brd)

    # Push live pool-update to the still-open menu. The `eligible`
    # map drives card visibility; `free_inputs` drives the per-card
    # block-input <select> options (refreshed so a port the user
    # just consumed isn't offered again on the next commit).
    session$onFlushed(once = TRUE, function() {
      isolate({
        anchor <- current_anchor()
        if (is.null(anchor)) return()
        pools <- blockr.ui::link_eligible_pools(board_rv(), anchor)
        if (length(pools$outgoing) == 0L &&
              length(pools$incoming) == 0L) {
          hide_sidebar("panel")
        } else {
          session$sendInputMessage(
            session$ns("menu-commit"),
            list(
              type = "pool-update",
              eligible = list(
                outgoing = pools$outgoing,
                incoming = pools$incoming
              ),
              free_inputs = pools$free_inputs,
              link_id_seed = rand_names(board_link_ids(board_rv()))
            )
          )
        }
      })
    })
  })

  output$commit <- renderPrint(last_commit())
  output$commit_source <- renderText(last_commit()$source %||% "")
  output$commit_target <- renderText(last_commit()$target %||% "")
  output$commit_link_id <- renderText(last_commit()$link_id %||% "")
  output$commit_block_input <- renderText(last_commit()$block_input %||% "")
  output$commit_count <- renderText(as.character(count()))
}

shinyApp(ui, server)
