dash_mod <- new_dashboard_module()

test_that("board_module utils work", {
  res <- board_module_server(dash_mod)
  expect_type(res, "closure")

  res <- board_module_ui(dash_mod)
  expect_type(res, "closure")

  res <- board_module_id(dash_mod)
  expect_identical(res, "dashboard")

  res <- board_module_title(dash_mod)
  expect_identical(res, "Dashboard")

  res <- board_module_position(dash_mod)
  expect_null(res)

  res <- board_module_positions(list(dash_mod))[[1]]
  expect_type(res, "list")
  expect_identical(res$referencePanel, "dag")
  expect_identical(res$direction, "right")

  res <- call_board_module_ui(
    dash_mod,
    id = "test",
    board = new_dag_board()
  )
  expect_s3_class(res, "shiny.tag")
})

test_that("board_modules works", {
  res <- new_context_menu_entry(
    "test",
    js = "1+1"
  )
  expect_s3_class(res, "context_menu_entry")
  expect_type(res, "list")
})

test_that("build_context_menu works", {

  empty_board <- new_dag_board()

  res <- build_context_menu(
    context_menu_items(empty_board),
    board = list(),
    parent = list(),
    target = list(type = "node", id = "test-node")
  )

  expect_type(res, "list")
  expect_named(res[[1]], c("name", "value"))
})
