test_that("process_app_state works", {
  expect_error(process_app_state(""))
  state <- list(
    network = list(
      nodes = list(list(id = 1, x = 1, y = 2)),
      combos = list(list(x = 1, y = 2))
    ),
    backup_list = list(letters),
    app_layout = list(
      panels = list(
        list(id = "a", params = list(x = 1)),
        list(id = "b", params = list(y = 2))
      )
    ),
    module_state = list(module_1 = "a", module_2 = "b")
  )
  res <- process_app_state(state)
  expect_null(res$network$nodes[[1]]$x)
  expect_null(res$network$nodes[[1]]$y)
  expect_null(res$network$combos[[1]]$x)
  expect_null(res$network$combos[[1]]$y)
  expect_null(res$backup_list)
  expect_null(res$module_state)
  expect_null(res$app_layout$panels[[1]]$params)
  expect_null(res$app_layout$panels[[2]]$params)
})

test_that("firstup works", {
  expect_identical(firstup("hello"), "Hello")
  expect_identical(firstup("Hello"), "Hello")
  expect_identical(firstup("HELLO"), "HELLO")
  expect_identical(firstup("hElLo"), "HElLo")
  expect_identical(firstup(""), "")
  expect_error(firstup(123))
})

test_that("reval_if works", {
  expect_identical(reval_if("not a function"), "not a function")
  expect_identical(reval_if(function() 1 + 1), 2)
  expect_equal(reval_if(NULL), NULL)
})

test_that("%OR% works", {
  res <- 1 %OR% NULL
  expect_identical(res, 1)
  res <- NULL %OR% 1
  expect_identical(res, 1)
  res <- NULL %OR% NULL
  expect_null(res)
})

test_that("v_rule works", {
  expect_s3_class(v_rule(), "shiny.tag")
})
