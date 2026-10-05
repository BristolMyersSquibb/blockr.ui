test_that("small_icon() draws an icon of the set, by name", {

  gear <- small_icon("gear")

  expect_s3_class(gear, "html")
  expect_identical(as.character(gear), icon_set()[["gear"]])

  expect_error(small_icon("cog"), '"check", "chevron", "code"', fixed = TRUE)
  expect_error(small_icon(c("x", "plus")))
})

test_that("every icon is one svg in the text colour, hidden from readers", {

  icons <- icon_set()

  expect_match(icons, '^<svg [^>]*aria-hidden="true">')
  expect_match(icons, "</svg>$")
  expect_no_match(substring(icons, 2L), "<svg", fixed = TRUE)
  expect_match(icons, "currentColor", fixed = TRUE)
})

test_that("an icon is its file, without the note and the space between tags", {

  file <- withr::local_tempfile(
    lines = c(
      "<!-- A note on the icon,",
      "     over two lines. -->",
      '<svg viewBox="0 0 8 8" aria-hidden="true">',
      '  <circle cx="4" cy="4" r="1"></circle>',
      "</svg>"
    ),
    fileext = ".svg"
  )

  expect_identical(
    read_icon(file),
    paste0(
      '<svg viewBox="0 0 8 8" aria-hidden="true">',
      '<circle cx="4" cy="4" r="1"></circle></svg>'
    )
  )
})

test_that("the icons go into the page wherever blockr-ui.js goes, ahead", {

  names_of <- function(x) {
    blockr.core::chr_xtr(htmltools::findDependencies(x), "name")
  }

  for (x in list(controls_dep(), build_html_table(data.frame(a = 1), 1L))) {
    deps <- names_of(x)
    expect_identical(
      match("blockr-icons", deps) + 1L,
      match("blockr-ui-js", deps)
    )
  }

  expect_identical(
    icons_dep()$head,
    paste0("<script>", icons_js(), "</script>")
  )
})

test_that("the scripts draw no icon of their own", {

  # A script draws its icons from Blockr.icons. One written into the script
  # is a copy small_icon() cannot read, and other packages copy it from
  # there.
  scripts <- list.files(
    system.file("assets", "js", package = "blockr.ui"),
    pattern = "\\.js$",
    full.names = TRUE
  )
  src <- blockr.core::unlst(lapply(scripts, readLines, warn = FALSE))

  expect_identical(grep("<svg", src, fixed = TRUE, value = TRUE), character())
})

test_that("the page's script sets every icon by name, and cannot end early", {

  # Each name is quoted, as a file's name need not be a JavaScript
  # identifier, and no "</" in an icon can close the script it sits in.
  js <- icons_js()

  for (name in names(icon_set())) {
    expect_match(js, paste0('\n  "', name, '": "<svg '), fixed = TRUE)
  }
  expect_no_match(js, "</", fixed = TRUE)
})
