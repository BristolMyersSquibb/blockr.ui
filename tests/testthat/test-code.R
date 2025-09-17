test_that("generate_code_ui works", {
  ui <- generate_code_ui("mod", new_dag_board())
  expect_null(ui)
})

test_session <- shiny::MockShinySession$new()
test_blk <- new_dataset_block(dataset = "BOD")
attr(test_blk, "uid") <- "test"

mock_add_block <- function(blk, rv, parent, session) {
  board_blocks(rv$board) <- c(board_blocks(rv$board), as_blocks(blk))
  blk_id <- last(board_block_ids(rv$board))
  rv$blocks[[blk_id]] <- list(
    block = blk,
    # Needed for code generation
    server = block_server(blk_id, blk)
  )
  rv$inputs[[blk_id]] <- if (!length(block_inputs(blk))) {
    list()
  } else {
    set_names(
      list(reactiveVal()),
      block_inputs(blk)
    )
  }
  session$flushReact()
}

testServer(
  generate_code_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_dag_board(),
      inputs = list()
    ),
    parent = reactiveValues(
      display_code = FALSE
    )
  ),
  {
    mock_add_block(test_blk, board, parent, session)
    expect_true(nchar(board_code()) > 0)
    output$code_out
    parent$display_code <- TRUE
    session$flushReact()
    expect_false(parent$display_code)
  }
)
