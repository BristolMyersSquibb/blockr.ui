test_that("controls_dep ships the controls after the tokens", {

  deps <- htmltools::findDependencies(controls_dep())
  names <- vapply(deps, `[[`, character(1L), "name")

  # blockr-ui.js defines the namespace Select builds on, and the stylesheets
  # read the tokens, so the order is part of the contract.
  expect_identical(
    names,
    c("blockr-theme", "blockr-ui-js", "blockr-blocks-css",
      "blockr-settings-band", "blockr-select-js", "blockr-select-css")
  )

  assets <- system.file("assets", package = "blockr.ui")
  files <- unlist(lapply(deps, function(d) c(d$script, d$stylesheet)))

  expect_true(all(file.exists(file.path(assets, files))))
})

test_that("the controls' stylesheets read only tokens this package defines", {

  sites <- token_references("blockr.ui")
  controls <- sites[
    basename(sites$file) %in%
      c("blockr-blocks.css", "blockr-select.css", "blockr-settings-band.css"),
  ]

  expect_gt(nrow(controls), 0L)
  expect_identical(unique(controls$token[is.na(controls$value)]), character())
})
