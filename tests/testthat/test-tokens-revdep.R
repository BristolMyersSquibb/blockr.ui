# The shared revdep workflow runs this file in each downstream's leg, with
# that downstream installed and named in REVDEP_PKG. Unset anywhere else,
# so these skip in this package's own CI and in a working copy.

consumer_apps <- c(blockr.dock = "empty")

test_that("a consumer's fallback literals agree with the tokens", {

  expect_tokens_agree(revdep_consumer())
})

test_that("a consumer's referenced tokens resolve in its own app", {

  consumer <- revdep_consumer()
  example <- consumer_apps[consumer]

  if (is.na(example)) {
    stop(
      "No example app recorded for '", consumer, "'. Add one to ",
      "consumer_apps so reachability is checked rather than skipped."
    )
  }

  skip_if_not_installed("shinytest2")

  app <- app_driver(
    system.file("examples", example, "app.R", package = consumer),
    name = paste0("revdep-", consumer),
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  expect_tokens_reachable(app, consumer)
})
