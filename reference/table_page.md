# Fetch one page of a tabular result

Resolves sorting and pagination for the table preview and returns a
single materialized page. In-memory data frames are sorted via a cached
order index on the sort column only (O(page) on page navigation); lazy
dbplyr tables are counted and paged in the database via a `ROW_NUMBER()`
window query, so the full table is never collected.

## Usage

``` r
table_page(
  result,
  sort_state = NULL,
  page = 1L,
  page_size = 5L,
  cache = new.env(parent = emptyenv())
)
```

## Arguments

- result:

  A data frame or lazy table (`tbl_lazy`).

- sort_state:

  List with elements `col` and `dir` (`"asc"`, `"desc"`, `"na"` or
  `"none"`) or `NULL` for unsorted.

- page:

  Requested page (1-based; clamped to the valid range).

- page_size:

  Rows per page.

- cache:

  Environment used to memoize the sort index (local) and the row count
  (lazy) across interactions on the same `result`. Callers should keep
  one cache per result; the default creates a fresh one.

## Value

A list with `dat` (materialized page as a data frame), `total_rows` and
`page` (after clamping).

## Details

NA placement matches local
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
semantics on both backends (`asc` / `desc` put NA last, `na` puts NA
first): SQL `ORDER BY` would put NULLs first, so the remote sort keys
are normalized with an explicit
[`is.na()`](https://rdrr.io/r/base/NA.html) key.
