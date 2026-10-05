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

# The script works around rstudio/shiny#4436 through the one call the batcher
# sends with. This fails once the installed Shiny changes either; the cleanup
# is tracked in #88.
test_that("Shiny's batcher still never records a queued send", {

  js <- system.file("www", "shared", "shiny.js", package = "shiny")
  skip_if(js == "", "Shiny's JavaScript not installed")

  src <- paste(readLines(js, warn = FALSE), collapse = "\n")

  expect_match(src, "this.shinyapp.sendInput(", fixed = TRUE)
  expect_match(src, "sendIsEnqueued", fixed = TRUE)
  expect_no_match(src, "sendIsEnqueued\\s*=\\s*(true|!0)", perl = TRUE)
})
