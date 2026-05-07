test_that("package namespace can be loaded", {
  expect_true(is.environment(asNamespace("blockr.ui")))
})
