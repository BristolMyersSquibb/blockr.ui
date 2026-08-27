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

css_defines <- function(css) {
  sub("\\s*:$", "", css_matches(css, "--blockr-[a-z0-9-]+\\s*:"))
}

css_references <- function(css) {
  sub("^var\\(\\s*", "", css_matches(css, "var\\(\\s*--blockr-[a-z0-9-]+"))
}

css_bare_references <- function(css) {
  gsub(
    "^var\\(\\s*|\\s*\\)$", "",
    css_matches(css, "var\\(\\s*--blockr-[a-z0-9-]+\\s*\\)")
  )
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

test_that("the token block defines every name the theme layer reads", {

  defined <- css_defines(css_source("blockr-tokens.css"))
  referenced <- css_references(css_source("blockr-theme.css"))

  expect_identical(setdiff(referenced, defined), character())
})

test_that("the token block's own derivations resolve within it", {

  tokens <- css_source("blockr-tokens.css")

  expect_identical(
    setdiff(css_references(tokens), css_defines(tokens)),
    character()
  )
})

test_that("every token read without a fallback is defined", {

  assets <- system.file("assets", "css", package = "blockr.ui")
  shipped <- list.files(assets, pattern = "\\.css$")

  defined <- css_defines(css_source("blockr-tokens.css"))
  bare <- css_bare_references(
    paste(vapply(shipped, css_source, character(1)), collapse = "\n")
  )

  expect_gt(length(shipped), 0L)
  expect_identical(setdiff(bare, defined), character())
})
