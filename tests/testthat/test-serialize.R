library(blockr.core)
library(blockr.dplyr)

mock_add_block <- function(blk, rv, parent, session) {
  board_blocks(rv$board) <- c(board_blocks(rv$board), as_blocks(blk))
  attr(blk, "uid") <- tail(board_block_ids(rv$board), n = 1)
  rv$blocks[[attr(blk, "uid")]] <- list(
    block = blk,
    # Need server part for serialisation
    server = block_server(attr(blk, "uid"), blk)
  )
  create_node(blk, parent, rv, FALSE, session)
}

temp <- withr::local_tempdir()

testServer(
  ser_deser_server,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_dag_board(),
      board_id = "board"
    ),
    parent = reactiveValues(
      network = list(),
      refreshed = NULL,
      selected_block = NULL,
      save_board = FALSE
    )
  ),
  {
    session$userData[["snapshot"]] <- reactiveVal(
      list(location = temp, auto = FALSE)
    )

    expect_null(vals$current_backup)
    expect_length(vals$backup_list, 0L)

    # Add new block
    mock_add_block(
      new_dataset_block(dataset = "BOD"),
      board,
      parent,
      session
    )

    parent$selected_block <- board_block_ids(board$board)
    parent$save_board <- TRUE
    session$flushReact()
    parent$save_board <- FALSE
    session$elapse(500)
    Sys.sleep(0.5)

    # We now have 1 snapshot
    expect_identical(vals$current_backup, 1L)
    # Add another block
    mock_add_block(
      new_dataset_block(dataset = "CO2"),
      board,
      parent,
      session
    )

    parent$selected_block <- board_block_ids(board$board)[2]
    parent$save_board <- TRUE
    session$flushReact()
    parent$save_board <- FALSE
    session$elapse(500)
    Sys.sleep(0.5)

    # We should have 2 snaps
    expect_identical(vals$current_backup, 2L)

    # Restore previous snapshot
    session$setInputs(undo = 0)
    session$flushReact()
    expect_identical(vals$current_backup, 1L)
    expect_identical(
      parent$selected_block,
      board_block_ids(board$board)[1]
    )

    # Restore latest
    session$setInputs(redo = 0)
    session$flushReact()
    expect_identical(vals$current_backup, 2L)
    expect_identical(
      parent$selected_block,
      board_block_ids(board$board)[2]
    )

    # Manual restore
    session$setInputs(
      restore = list(datapath = parent$backup_list[[1]])
    )

    expect_identical(
      parent$selected_block,
      board_block_ids(board$board)[1]
    )

    # Manual serialize
    output$serialize
  }
)

test_that("ser_deser_ui works", {
  ui <- ser_deser_ui("mod", new_dag_board())
  expect_length(ui, 2)
  expect_named(ui, c("buttons", "restore"))
})
