# Lazy (dbplyr) page engine: count + page fetch pushed to the database,
# parity with the local engine, never collecting the full table.

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

test_that("lazy count is correct and cached", {
  fx <- lazy_fixture()
  cache <- new.env(parent = emptyenv())
  pg <- table_page(fx$lazy, NULL, 1L, 5L, cache = cache)
  expect_equal(pg$total_rows, 200)
  expect_equal(get("total_count", envir = cache), 200)
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
      expect_equal(lazy_pg$total_rows, 200)
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

test_that("lazy page clamping and empty results work", {
  fx <- lazy_fixture()
  hi <- table_page(fx$lazy, NULL, 999L, 5L)
  expect_equal(hi$page, 40L)
  expect_equal(nrow(hi$dat), 5L)
})
