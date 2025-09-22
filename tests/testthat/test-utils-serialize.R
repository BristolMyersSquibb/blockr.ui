test_that("ser/deser roundtrip", {

  board_initial <- new_dag_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight"),
      c = new_merge_block("Time")
    ),
    links = c(
      ac = new_link("a", "c", "x"),
      bc = new_link("b", "c", "y")
    ),
    stacks = list(ac = c("a", "c"))
  )

  json <- character()

  testServer(
    main_server,
    args = list(board = board_initial, board_id = "board"),
    {
      session$flushReact()

      # mock dag panel to as it is selected on restore in an oberver
      # avoid a warning in the test below
      test_dock <- list()
      test_dock[["panels"]][["dag"]] <- list(
        id = "dag",
        params = list(
          content = list(html = "blabla")
        )
      )
      res$parent$app_layout <- test_dock
      board <- res$board
      json <<- serialize_board(board$board, board$blocks, res$parent,
                               session = session)
    }
  )

  testServer(
    main_server,
    args = list(board = new_dag_board(), board_id = "board"),
    {
      session$flushReact()

      expect_length(res$board$blocks, 0L)

      board_srv <- session$makeScope("board")
      ser_srv <- board_srv$makeScope("preserve_board")

      ser_srv$setInputs(restore = list(datapath = json))
      expect_identical(
        names(res$board$blocks),
        board_block_ids(board_initial)
      )
    }
  )
})
