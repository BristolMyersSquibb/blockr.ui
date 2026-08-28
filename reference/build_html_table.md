# Build the HTML table preview for one page

Pure presentation: renders an already-materialized page of data as the
blockr table preview (header with column names, labels and type tags,
pillar-formatted cells, NA / negative styling, footer with row range and
prev / next pagination). Data access (sorting, pagination, lazy tables)
is handled by
[`table_page()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/table_page.md).

## Usage

``` r
build_html_table(
  dat,
  total_rows,
  sort_state = NULL,
  ns = NULL,
  page = 1L,
  page_size = 5L,
  table_label = NULL,
  sort_input = NULL,
  page_input = NULL,
  has_more = NULL,
  cache = NULL
)
```

## Arguments

- dat:

  Materialized data frame holding only the rows of the current page.

- total_rows:

  Total number of rows in the full result (drives the row-range text and
  pagination).

- sort_state:

  List with elements `col` and `dir` (`"asc"`, `"desc"`, `"na"` or
  `"none"`) or `NULL`.

- ns:

  Optional namespace function used for the legacy default input ids when
  `sort_input` / `page_input` are not supplied.

- page:

  Current page (1-based).

- page_size:

  Rows per page.

- table_label:

  Optional table-level label shown in the footer.

- sort_input, page_input:

  Full (already namespaced) ids of the Shiny inputs receiving sort and
  page events. Supply per-instance ids when a session shows more than
  one preview; when `NULL` the legacy shared names `blockr_table_sort` /
  `blockr_table_page` are used.

- has_more:

  Look-ahead flag used when `total_rows` is `NA` (remote / lazy tables
  that are never counted): `TRUE` signals that rows exist beyond the
  current page, which keeps the next button enabled and shows the "of
  many" row-range text instead of a known total.

- cache:

  Optional environment memoizing the column widths across renders of the
  same result (keyed on the column names). Pass the same per-result
  cache used for
  [`table_page()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/table_page.md)
  so sorting and paging emit identical widths and the columns never
  reflow. When `NULL`, widths are recomputed from the current page on
  every render.

## Value

A [`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)
with the table preview, carrying the
[`table_preview_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/table_preview_dep.md)
html dependency.
