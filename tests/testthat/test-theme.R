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

test_that("theme_dep ships the tokens, then the theme layer", {

  deps <- htmltools::findDependencies(theme_dep())
  sheets <- blockr.core::lst_xtr(deps, "stylesheet")

  expect_identical(
    blockr.core::chr_xtr(deps, "name"),
    c("blockr-tokens", "blockr-theme")
  )
  expect_identical(
    sheets,
    list(
      c("css/blockr-tokens.css", "css/blockr-tokens-dark.css"),
      "css/blockr-theme.css"
    )
  )

  assets <- system.file("assets", package = "blockr.ui")

  expect_true(all(file.exists(file.path(assets, blockr.core::unlst(sheets)))))
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

test_that("the theme draws Bootstrap's checkboxes as the blockr checkbox", {

  css <- css_source("blockr-theme.css")

  # Shiny puts the input inside its label, bslib next to it; both boxes are
  # 16px at radius-sm, filled with the accent when checked.
  for (sel in c(
    ':root .checkbox > label > input[type="checkbox"]',
    ':root .form-check > input.form-check-input[type="checkbox"]'
  )) {
    expect_match(css, sel, fixed = TRUE, info = sel)
  }

  box <- css_matches(
    css,
    ':root \\.checkbox > label > input\\[type="checkbox"\\],[^{]*\\{[^}]*\\}'
  )
  expect_length(box, 1L)
  expect_match(box, "appearance: none", fixed = TRUE)
  expect_match(box, "width: 16px", fixed = TRUE)
  expect_match(box, "border-radius: var(--blockr-radius-sm)", fixed = TRUE)
  expect_match(css, "background-color: var(--blockr-color-bg-accent)",
               fixed = TRUE)

  # Radios are not checkboxes and keep their own look.
  expect_no_match(css, 'input[type="radio"]', fixed = TRUE)
})
