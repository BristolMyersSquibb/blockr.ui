test_that("controls_dep ships the controls after the tokens", {

  deps <- htmltools::findDependencies(controls_dep())
  names <- blockr.core::chr_xtr(deps, "name")

  # blockr-ui.js defines the namespace Select builds on, and the stylesheets
  # read the tokens, so the order is part of the contract.
  expect_identical(
    names,
    c("blockr-theme", "blockr-ui-js", "blockr-blocks-css",
      "blockr-settings-band", "blockr-select-js", "blockr-select-css")
  )

  assets <- system.file("assets", package = "blockr.ui")
  files <- blockr.core::unlst(
    c(
      blockr.core::lst_xtr(deps, "script"),
      blockr.core::lst_xtr(deps, "stylesheet")
    )
  )

  expect_true(all(file.exists(file.path(assets, files))))
})

test_that("the controls read only meaning tokens this package defines", {

  sites <- token_references("blockr.ui")
  controls <- sites[
    basename(sites$file) %in%
      c("blockr-blocks.css", "blockr-select.css", "blockr-settings-band.css"),
  ]
  stray <- grepl(palette_token, controls$token) |
    controls$token %in% names(legacy_tokens())

  expect_gt(nrow(controls), 0L)
  expect_identical(unique(controls$token[is.na(controls$value)]), character())
  expect_identical(unique(controls$token[stray]), character())
})
