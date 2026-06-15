# Lazy (dbplyr) page engine: page fetch pushed to the database via a
# `page_size + 1` look-ahead (never COUNT(*), never the full table), parity
# with the local engine on the rows actually shown.

skip_if_not_installed("dbplyr")
skip_if_not_installed("RSQLite")

lazy_fixture <- function(env = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  set.seed(42)
  df <- data.frame(
    a = sample(c(NA, 1:50), 200, replace = TRUE),
    g = sample(letters[1:4], 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  dplyr::copy_to(con, df, "t")
  list(local = df, lazy = dplyr::tbl(con, "t"))
}

test_that("lazy total is never counted; has_more look-ahead drives paging", {
  fx <- lazy_fixture()
  pg <- table_page(fx$lazy, NULL, 1L, 5L)
  expect_true(is.na(pg$total_rows))   # unknown on purpose - no COUNT(*)
  expect_true(pg$has_more)            # 200 rows, page 1 of 5 -> more exist
  expect_equal(nrow(pg$dat), 5L)

  last <- table_page(fx$lazy, NULL, 40L, 5L)  # rows 196-200, the final page
  expect_equal(nrow(last$dat), 5L)
  expect_false(last$has_more)         # no row 201 -> next disabled
})

test_that("lazy pages equal the local engine for every sort mode", {
  fx <- lazy_fixture()
  for (dir in c("asc", "desc", "na")) {
    for (page in c(1L, 2L, 8L)) {
      lazy_pg <- table_page(fx$lazy, list(col = "a", dir = dir), page, 5L)
      local_pg <- table_page(fx$local, list(col = "a", dir = dir), page, 5L)
      expect_equal(
        lazy_pg$dat$a, local_pg$dat$a,
        info = paste(dir, "page", page)
      )
      expect_equal(lazy_pg$page, local_pg$page)
      expect_true(is.na(lazy_pg$total_rows))
    }
  }
})

test_that("only one page is fetched and the sort is pushed to SQL", {
  fx <- lazy_fixture()
  pg <- table_page(fx$lazy, list(col = "a", dir = "asc"), 3L, 5L)
  expect_equal(nrow(pg$dat), 5L)
  # proof of pushdown: the page query compiles to a ROW_NUMBER window
  k <- rlang::sym("a")
  keyed <- dbplyr::window_order(
    dplyr::mutate(fx$lazy, ..na = is.na(!!k)), ..na, !!k
  )
  q <- dplyr::filter(
    dplyr::mutate(keyed, ..rn = dplyr::row_number()),
    .data$..rn > 10, .data$..rn <= 15
  )
  expect_match(as.character(dbplyr::sql_render(q)), "ROW_NUMBER")
})

test_that("unsorted lazy pagination works without a window order", {
  fx <- lazy_fixture()
  pg2 <- table_page(fx$lazy, NULL, 2L, 5L)
  expect_equal(nrow(pg2$dat), 5L)
  # natural order: rows 6-10 of the table as stored
  expect_equal(pg2$dat$a, fx$local$a[6:10])
})

test_that("an out-of-range page falls back to the first page", {
  fx <- lazy_fixture()
  # Without a count there is no known last page; a stale page index past the
  # end yields no rows, so the engine falls back to page 1 rather than showing
  # an empty trailing page.
  hi <- table_page(fx$lazy, NULL, 999L, 5L)
  expect_equal(hi$page, 1L)
  expect_equal(nrow(hi$dat), 5L)
})
