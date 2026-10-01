test_that("small_icon() draws an icon of the list, by name", {

  gear <- small_icon("gear")

  expect_s3_class(gear, "html")
  expect_identical(as.character(gear), icon_set[["gear"]])

  expect_error(small_icon("cog"), '"chevron", "remove", "x"', fixed = TRUE)
  expect_error(small_icon(c("x", "plus")))
})

test_that("every icon is one svg in the text colour, hidden from readers", {

  expect_match(icon_set, '^<svg [^>]*aria-hidden="true">')
  expect_match(icon_set, "</svg>$")
  expect_no_match(substring(icon_set, 2L), "<svg", fixed = TRUE)
  expect_match(icon_set, "currentColor", fixed = TRUE)
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

test_that("the JavaScript tests run the icons controls_dep() writes", {

  # The tests in tests/js load Blockr.icons from this snapshot, so they run
  # the script an app gets. After changing an icon, accept the new snapshot
  # with testthat::snapshot_accept("icons/").
  script <- withr::local_tempfile(fileext = ".js")
  writeLines(icons_js(), script)

  expect_snapshot_file(script, "icons.js", compare = compare_file_text)
})
