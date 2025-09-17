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
    args = list(board = board_initial),
    {
      session$flushReact()
      json <<- board_to_json(res$board, res$parent, session)
    }
  )

  testServer(
    main_server,
    args = list(board = new_dag_board()),
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
