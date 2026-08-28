css_source <- function(file) {
  paste(
    readLines(
      system.file("assets", "css", file, package = "blockr.ui"),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

css_matches <- function(css, pattern) {
  unique(regmatches(css, gregexpr(pattern, css))[[1]])
}

css_hidden_selectors <- function(css) {
  rules <- css_matches(
    gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE),
    "[^{}]*\\{[^{}]*display:\\s*none[^{}]*\\}"
  )
  trimws(unlist(strsplit(sub("\\{.*", "", rules), ",")))
}

test_that("theme_dep ships the token and theme stylesheets", {

  dep <- theme_dep()

  expect_s3_class(dep, "html_dependency")
  expect_identical(
    dep$stylesheet,
    c("css/blockr-tokens.css", "css/blockr-theme.css")
  )

  assets <- system.file("assets", package = "blockr.ui")

  expect_true(all(file.exists(file.path(assets, dep$stylesheet))))
})

test_that("the shared stylesheet reads only names this package claims", {

  sites <- token_references("blockr.ui")
  shared <- sites[
    basename(sites$file) %in% c("blockr-tokens.css", "blockr-theme.css"),
  ]

  expect_gt(nrow(shared), 0L)
  expect_identical(unique(shared$token[is.na(shared$value)]), character())
})

test_that("every token read without a fallback is defined", {

  sites <- token_references("blockr.ui")
  bare <- sites[is.na(sites$fallback), ]

  expect_gt(nrow(bare), 0L)
  expect_identical(unique(bare$token[is.na(bare$value)]), character())
})

test_that("the theme layer hides only chrome the host cannot reach", {

  expect_identical(
    css_hidden_selectors(css_source("blockr-theme.css")),
    ".popover .btn-close"
  )
})
