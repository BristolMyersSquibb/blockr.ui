test_that("main_server works", {

  empty_board <- new_dag_board()

  expect_error(main_server("id", NULL, NULL, NULL))
  expect_error(main_server("id", empty_board, NULL, list()))

  ex_board <- new_dag_board(
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

  testServer(
    main_server,
    args = list(board = board),
    {
      expect_false(app_state$cold_start)
      expect_null(app_state$refreshed)
      expect_length(app_state$network, 0)
      expect_false(app_state$append_block)
      expect_null(app_state$added_block)
      expect_null(app_state$removed_block)
      expect_null(app_state$selected_block)
      expect_null(app_state$cancelled_edge)
      expect_null(app_state$added_edge)
      expect_null(app_state$removed_edge)
      expect_null(app_state$added_stack)
      expect_null(app_state$stack_added_block)
      expect_null(app_state$stack_removed_block)
      expect_null(app_state$removed_stack)
      expect_null(app_state$stacks)
      expect_false(app_state$open_scoutbar)
      expect_null(app_state$scoutbar$trigger)
      expect_null(app_state$scoutbar$action)
      expect_null(app_state$scoutbar$value)
      expect_false(app_state$scoutbar$is_open)
      expect_false(app_state$save_board)
      expect_length(app_state$backup_list, 0)
      expect_false(app_state$display_code)
      expect_null(app_state$added_to_dashboard)
      expect_null(app_state$removed_from_dashboard)
    }
  )
})
