test_that("theme_variables() gives the full names of the theme variables", {
  vars <- theme_variables()

  expect_type(vars, "character")
  expect_true(all(startsWith(vars, "--bk-")))
  expect_identical(anyDuplicated(vars), 0L)
  expect_true(
    all(c("--bk-color-primary", "--bk-font-sans", "--bk-spacing") %in% vars)
  )
})

test_that("the stylesheet that blockr.ui ships sets each theme variable", {
  css <- readLines(
    system.file("js", "blockr-ui.css", package = "blockr.ui", mustWork = TRUE)
  )
  set <- regmatches(css, regexpr("--bk-[[:alnum:]-]+(?=:)", css, perl = TRUE))

  expect_true(all(theme_variables() %in% set))
})
