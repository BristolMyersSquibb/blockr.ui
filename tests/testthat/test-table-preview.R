# build_html_table(): pure presentation. DOM structure, labels, type tags,
# NA / negative styling, input-id wiring, edge cases.

render_chr <- function(x) as.character(htmltools::renderTags(x)$html)

test_that("a normal frame renders the full table structure", {
  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  html <- render_chr(build_html_table(df, 5L))
  expect_match(html, "blockr-table-container")
  expect_match(html, "blockr-table-wrapper")
  expect_match(html, "data-column=\"a\"")
  expect_match(html, "data-column=\"b\"")
  expect_match(html, "1–5 of 5")
})

test_that("type tags are rendered per column", {
  df <- data.frame(
    i = 1L, d = 1.5, c = "x", l = TRUE,
    dt = as.Date("2026-01-01"), f = factor("a"),
    stringsAsFactors = FALSE
  )
  html <- render_chr(build_html_table(df, 1L))
  for (tag in c("&lt;int&gt;", "&lt;dbl&gt;", "&lt;chr&gt;", "&lt;lgl&gt;",
                "&lt;date&gt;", "&lt;fct&gt;")) {
    expect_match(html, tag, fixed = TRUE)
  }
})

test_that("column labels render, truncate and carry a title", {
  df <- data.frame(x = 1, y = 2)
  attr(df$x, "label") <- "Short label"
  attr(df$y, "label") <- strrep("Long label ", 5)
  html <- render_chr(build_html_table(df, 1L))
  expect_match(html, "Short label")
  expect_match(html, "blockr-col-label")
  expect_match(html, "title=")
  expect_match(html, "…")
})

test_that("NA and negative values get their styling hooks", {
  df <- data.frame(v = c(NA, -3.2))
  html <- render_chr(build_html_table(df, 2L))
  expect_match(html, "blockr-na")
  expect_match(html, "blockr-negative")
  expect_match(html, "blockr-td-numeric")
})

test_that("zero-column and zero-row frames render gracefully", {
  empty <- render_chr(build_html_table(data.frame(), 0L))
  expect_match(empty, "Empty data frame \\(0 columns\\)")
  norows <- render_chr(build_html_table(data.frame(x = numeric(0)), 0L))
  expect_match(norows, "No rows")
})

test_that("list columns render", {
  df <- data.frame(id = 1:2)
  df$payload <- list(1:3, letters)
  html <- render_chr(build_html_table(df, 2L))
  expect_match(html, "&lt;list&gt;", fixed = TRUE)
})

test_that("explicit input ids land in the container data attributes", {
  df <- data.frame(x = 1)
  html <- render_chr(build_html_table(
    df, 1L,
    sort_input = "mod-out1_table_sort", page_input = "mod-out1_table_page"
  ))
  expect_match(html, "data-sort-input=\"mod-out1_table_sort\"")
  expect_match(html, "data-page-input=\"mod-out1_table_page\"")
})

test_that("legacy default ids are kept when no explicit ids are given", {
  df <- data.frame(x = 1)
  html <- render_chr(build_html_table(df, 1L, ns = shiny::NS("mod")))
  expect_match(html, "data-sort-input=\"mod-blockr_table_sort\"")
})

test_that("sort state is reflected in header classes", {
  df <- data.frame(x = 1)
  html <- render_chr(build_html_table(
    df, 1L, sort_state = list(col = "x", dir = "desc")
  ))
  expect_match(html, "blockr-sort-desc")
  expect_match(html, "blockr-sort-icon-desc")
})

test_that("pagination buttons disable at the bounds", {
  df <- data.frame(x = 1:5)
  first <- render_chr(build_html_table(df[1:5, , drop = FALSE], 12L, page = 1L))
  expect_match(first, "data-direction=\"prev\"[^>]*")
  expect_match(first, "blockr-nav-btn disabled")
  last <- render_chr(build_html_table(df[1:2, , drop = FALSE], 12L, page = 3L))
  expect_match(last, "data-max-page=\"3\"")
})

test_that("the table label shows in the footer", {
  df <- data.frame(x = 1)
  html <- render_chr(
    build_html_table(df, 1L, table_label = "ADSL: Subject-Level")
  )
  expect_match(html, "blockr-table-label")
  expect_match(html, "ADSL: Subject-Level")
})

test_that("the html dependency is attached (no inline style/script)", {
  df <- data.frame(x = 1)
  tag <- build_html_table(df, 1L)
  deps <- htmltools::findDependencies(tag)
  expect_true("blockr-table-preview" %in% vapply(deps, `[[`, "", "name"))
  html <- as.character(htmltools::as.tags(tag))
  expect_no_match(html, "<style>")
  expect_no_match(html, "<script>")
})

# Server-computed column widths: fixed layout from the first render, widths
# memoized per result so sort/page renders never reflow the columns.

test_that("columns carry fixed-layout widths from the first render", {
  df <- data.frame(x = 1:5, long = strrep("a", 40))
  html <- render_chr(build_html_table(df, 5L))
  expect_match(html, "table-layout: fixed")
  expect_match(html, "width: [0-9]+px")
})

test_that("widths grow with content and clamp at the bounds", {
  wide <- column_widths_px("x", "", "<chr>", list(strrep("a", 100)), list(FALSE))
  narrow <- column_widths_px("x", "", "<chr>", list("a"), list(FALSE))
  expect_equal(wide, 320)    # upper clamp
  expect_gte(narrow, 60)     # lower clamp
  expect_lt(narrow, wide)
})

test_that("header parts set the floor when cells are short", {
  by_name <- column_widths_px(
    "a_rather_long_column_name", "", "<dbl>", list("1"), list(FALSE)
  )
  expect_gte(by_name, nchar("a_rather_long_column_name") * 8)
  all_na <- column_widths_px("x", "", "<lgl>", list(NA_character_), list(TRUE))
  expect_gte(all_na, 60)
})

test_that("a cache pins widths across pages of the same columns", {
  cache <- new.env(parent = emptyenv())
  page1 <- data.frame(x = strrep("a", 30))
  page2 <- data.frame(x = "b")
  html1 <- render_chr(build_html_table(page1, 2L, page = 1L, cache = cache))
  html2 <- render_chr(build_html_table(page2, 2L, page = 2L, cache = cache))
  w1 <- regmatches(html1, regexpr("width: [0-9]+px", html1))
  w2 <- regmatches(html2, regexpr("width: [0-9]+px", html2))
  expect_identical(w1, w2)
  # without a cache the short page computes a narrower column
  html3 <- render_chr(build_html_table(page2, 2L, page = 2L))
  w3 <- regmatches(html3, regexpr("width: [0-9]+px", html3))
  expect_false(identical(w1, w3))
})

test_that("the width cache resets when the columns change", {
  cache <- new.env(parent = emptyenv())
  render_chr(build_html_table(data.frame(x = strrep("a", 30)), 1L, cache = cache))
  html <- render_chr(build_html_table(data.frame(y = "b"), 1L, cache = cache))
  expect_match(html, "data-column=\"y\"")
  w <- as.numeric(sub("width: ([0-9]+)px.*", "\\1",
                      regmatches(html, regexpr("width: [0-9]+px", html))))
  expect_lt(w, 100)
})

test_that("NA cells render without pretty-printing whitespace", {
  # htmltools indents nested tags; under the cell's white-space rule those
  # literal newlines rendered NA cells three lines tall (rows ballooned on
  # any row containing an NA).
  df <- data.frame(x = c(NA_integer_, 1L))
  html <- render_chr(build_html_table(df, 2L))
  expect_match(html, "<td[^>]*><span class=\"blockr-na\">NA</span></td>")
  expect_no_match(html, ">\\s+<span class=\"blockr-na\"")
})
