test_that("no selector is styled in two stylesheets", {

  # A rule kept in two files drifts: one copy gets the fix and the other
  # keeps winning wherever it loads later. This branch merged the menu's
  # two copies by hand; the test keeps them merged.
  files <- list.files(
    system.file("assets", "css", package = "blockr.ui"),
    pattern = "\\.css$",
    full.names = TRUE
  )
  files <- files[!grepl("tokens", basename(files))]

  selectors <- lapply(files, css_selectors)
  owners <- split(
    rep(basename(files), lengths(selectors)),
    blockr.core::unlst(selectors)
  )
  shared <- owners[lengths(lapply(owners, unique)) > 1L]

  expect_gt(length(owners), 0L)
  expect_identical(
    blockr.core::chr_ply(
      names(shared),
      function(s) paste0(s, ": ", paste(unique(shared[[s]]), collapse = ", "))
    ),
    character()
  )
})

test_that("the design system page draws from the shipped tokens", {

  page <- test_path("..", "..", "vignettes", "articles", "design-system")

  skip_if_not(dir.exists(page), "the article's sources are not at hand")

  for (sheet in c("blockr-tokens.css", "blockr-tokens-dark.css")) {
    expect_identical(
      readLines(file.path(page, sheet), warn = FALSE),
      readLines(
        system.file("assets", "css", sheet, package = "blockr.ui"),
        warn = FALSE
      ),
      label = sheet
    )
  }
})
