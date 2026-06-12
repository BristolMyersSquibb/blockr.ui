# Local page engine: parity with the legacy whole-frame sort, label
# preservation, caching, page clamping.

sortable_df <- function() {
  df <- data.frame(
    num = c(3.5, -1, NA, 2, NA, 10, 0),
    int = c(2L, 7L, 1L, NA, 3L, 3L, 5L),
    chr = c("b", "a", NA, "C", "aa", "B", "z"),
    fct = factor(c("lo", "hi", "mid", NA, "lo", "hi", "mid"),
                 levels = c("lo", "mid", "hi")),
    stringsAsFactors = FALSE
  )
  attr(df$num, "label") <- "A numeric label"
  df
}

test_that("table_page sort order matches apply_table_sort for every mode", {
  df <- sortable_df()
  n <- nrow(df)
  for (col in names(df)) {
    for (dir in c("asc", "desc", "na")) {
      legacy <- apply_table_sort(df, col, dir)
      pg <- table_page(df, list(col = col, dir = dir), page = 1L, page_size = n)
      expect_identical(
        pg$dat[[col]], legacy[[col]],
        info = paste(col, dir)
      )
    }
  }
})

test_that("ties keep original row order (stable sort, like arrange)", {
  df <- data.frame(k = c(1, 2, 1, 2, 1), id = 1:5)
  legacy <- apply_table_sort(df, "k", "asc")
  pg <- table_page(df, list(col = "k", dir = "asc"), page = 1L, page_size = 5L)
  expect_identical(pg$dat$id, legacy$id)
})

test_that("column labels survive sorting and paging", {
  df <- sortable_df()
  pg <- table_page(
    df, list(col = "num", dir = "asc"), page = 2L, page_size = 3L
  )
  expect_identical(attr(pg$dat$num, "label"), "A numeric label")
})

test_that("pagination slices the sorted frame correctly", {
  df <- data.frame(x = c(5, 1, 4, 2, 3))
  pg1 <- table_page(df, list(col = "x", dir = "asc"), page = 1L, page_size = 2L)
  pg2 <- table_page(df, list(col = "x", dir = "asc"), page = 2L, page_size = 2L)
  pg3 <- table_page(df, list(col = "x", dir = "asc"), page = 3L, page_size = 2L)
  expect_equal(pg1$dat$x, c(1, 2))
  expect_equal(pg2$dat$x, c(3, 4))
  expect_equal(pg3$dat$x, 5)
  expect_equal(pg1$total_rows, 5L)
})

test_that("page is clamped to the valid range", {
  df <- data.frame(x = 1:6)
  too_high <- table_page(df, NULL, page = 99L, page_size = 5L)
  expect_equal(too_high$page, 2L)
  expect_equal(too_high$dat$x, 6L)
  too_low <- table_page(df, NULL, page = 0L, page_size = 5L)
  expect_equal(too_low$page, 1L)
})

test_that("unsorted and unknown-column requests fall back to natural order", {
  df <- data.frame(x = c(2, 1, 3))
  expect_equal(table_page(df, NULL, 1L, 5L)$dat$x, c(2, 1, 3))
  expect_equal(
    table_page(df, list(col = "nope", dir = "asc"), 1L, 5L)$dat$x,
    c(2, 1, 3)
  )
})

test_that("empty frames work", {
  pg0 <- table_page(data.frame(), NULL, 1L, 5L)
  expect_equal(pg0$total_rows, 0L)
  expect_equal(ncol(pg0$dat), 0L)
  df <- data.frame(x = numeric(0))
  pg <- table_page(df, list(col = "x", dir = "asc"), 1L, 5L)
  expect_equal(nrow(pg$dat), 0L)
  expect_equal(names(pg$dat), "x")
})

test_that("the sort index is cached per (col, dir)", {
  df <- data.frame(x = sample(1e4))
  cache <- new.env(parent = emptyenv())
  table_page(df, list(col = "x", dir = "asc"), 1L, 5L, cache = cache)
  key <- ls(cache, all.names = TRUE)
  expect_length(key, 1L)
  idx_first <- get(key, envir = cache)
  table_page(df, list(col = "x", dir = "asc"), 2L, 5L, cache = cache)
  expect_identical(get(key, envir = cache), idx_first)
  table_page(df, list(col = "x", dir = "desc"), 1L, 5L, cache = cache)
  expect_length(ls(cache, all.names = TRUE), 2L)
})
