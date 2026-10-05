# The script itself is tested in tests/js/shiny-input-batch.test.js.
test_that("shiny_input_batch_dep ships the script it documents", {

  dep <- shiny_input_batch_dep()

  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$script, "js/shiny-input-batch.js")
  expect_true(
    file.exists(
      system.file("assets", dep$script, package = "blockr.ui")
    )
  )
})
