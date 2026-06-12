# Data access for the table preview: one entry point over in-memory frames
# and lazy (dbplyr) tables.
#
# dplyr is load-bearing here, not convenience - do NOT "simplify" to base R:
# * the same verbs dispatch on lazy/dbplyr tables, so sorting and pagination
#   push to the database and the full table is never collected;
# * dplyr::slice() preserves per-column attr(col, "label") (ADaM labels)
#   where base `[` row-subsetting drops them.

#' Fetch one page of a tabular result
#'
#' Resolves sorting and pagination for the table preview and returns a single
#' materialized page. In-memory data frames are sorted via a cached order
#' index on the sort column only (O(page) on page navigation); lazy dbplyr
#' tables are counted and paged in the database via a `ROW_NUMBER()` window
#' query, so the full table is never collected.
#'
#' NA placement matches local [dplyr::arrange()] semantics on both backends
#' (`asc` / `desc` put NA last, `na` puts NA first): SQL `ORDER BY` would put
#' NULLs first, so the remote sort keys are normalized with an explicit
#' `is.na()` key.
#'
#' @param result A data frame or lazy table (`tbl_lazy`).
#' @param sort_state List with elements `col` and `dir` (`"asc"`, `"desc"`,
#'   `"na"` or `"none"`) or `NULL` for unsorted.
#' @param page Requested page (1-based; clamped to the valid range).
#' @param page_size Rows per page.
#' @param cache Environment used to memoize the sort index (local) and the
#'   row count (lazy) across interactions on the same `result`. Callers
#'   should keep one cache per result; the default creates a fresh one.
#'
#' @return A list with `dat` (materialized page as a data frame),
#'   `total_rows` and `page` (after clamping).
#' @export
table_page <- function(result, sort_state = NULL, page = 1L, page_size = 5L,
                       cache = new.env(parent = emptyenv())) {
  col <- sort_state$col
  dir <- sort_state$dir
  if (is.null(dir)) dir <- "none"
  if (is.null(col) || !col %in% colnames(result)) dir <- "none"

  if (inherits(result, "tbl_lazy")) {
    table_page_lazy(result, col, dir, page, page_size, cache)
  } else {
    table_page_local(result, col, dir, page, page_size, cache)
  }
}

table_page_local <- function(result, col, dir, page, page_size, cache) {
  total <- nrow(result)
  max_page <- max(1L, ceiling(total / page_size))
  page <- min(max(1L, as.integer(page)), max_page)
  lo <- (page - 1L) * page_size
  hi <- min(page * page_size, total)
  rows <- if (total > 0L && hi > lo) seq.int(lo + 1L, hi) else integer(0L)

  if (dir != "none") {
    key <- paste0(col, "\r", dir)
    idx <- get0(key, envir = cache, inherits = FALSE)
    if (is.null(idx)) {
      # Order index on the sort column only - no whole-frame copy. Stable
      # radix sort in the C locale, matching dplyr >= 1.1 arrange() defaults;
      # na.last placement reproduces arrange() / arrange(desc()) semantics.
      x <- result[[col]]
      idx <- switch(
        dir,
        asc = order(x, na.last = TRUE, method = "radix"),
        desc = order(x, decreasing = TRUE, na.last = TRUE, method = "radix"),
        na = order(x, na.last = FALSE, method = "radix"),
        seq_len(total)
      )
      assign(key, idx, envir = cache)
    }
    rows <- idx[rows]
  }

  list(
    dat = as.data.frame(dplyr::slice(result, rows)),
    total_rows = total,
    page = page
  )
}

table_page_lazy <- function(result, col, dir, page, page_size, cache) {
  if (!requireNamespace("dbplyr", quietly = TRUE)) {
    stop("Previewing lazy tables requires the 'dbplyr' package.")
  }

  total <- get0("..count", envir = cache, inherits = FALSE)
  if (is.null(total)) {
    # One aggregate query; numeric (not integer) so counts beyond
    # .Machine$integer.max survive.
    total <- as.numeric(dplyr::pull(dplyr::count(dplyr::ungroup(result))))
    assign("..count", total, envir = cache)
  }

  max_page <- max(1L, ceiling(total / page_size))
  page <- min(max(1L, as.integer(page)), max_page)
  lo <- (page - 1L) * page_size
  hi <- min(page * page_size, total)

  dat <- if (total <= 0 || hi <= lo) {
    dplyr::collect(utils::head(result, 0L))
  } else if (dir == "none") {
    # No sort, hence no window order: native LIMIT, keep the last page_size
    # rows. Collects at most page * page_size rows (prev/next navigation is
    # shallow).
    utils::tail(dplyr::collect(utils::head(result, hi)), hi - lo)
  } else {
    # NB: bare desc() / row_number() / is.na() on purpose - dbplyr translates
    # calls by name and cannot translate namespaced `dplyr::` ASTs.
    k <- rlang::sym(col)
    keyed <- dplyr::mutate(result, ..na = is.na(!!k))
    keyed <- switch(
      dir,
      asc = dbplyr::window_order(keyed, ..na, !!k),
      desc = dbplyr::window_order(keyed, ..na, desc(!!k)),
      na = dbplyr::window_order(keyed, desc(..na), !!k)
    )
    page_q <- dplyr::select(
      dplyr::arrange(
        dplyr::filter(
          dplyr::mutate(keyed, ..rn = row_number()),
          .data$..rn > lo, .data$..rn <= hi
        ),
        .data$..rn
      ),
      -"..na", -"..rn"
    )
    dplyr::collect(page_q)
  }

  list(
    dat = as.data.frame(dat),
    total_rows = total,
    page = page
  )
}

#' Sort a data frame for the table preview
#'
#' Legacy whole-frame sort kept for direct callers (blockr.dm) during the
#' transition to [table_page()], which sorts via a cached order index
#' instead. Same semantics: `asc` / `desc` put NA last, `na` puts NA first.
#'
#' @param data A data frame.
#' @param sort_col Column name to sort by.
#' @param sort_dir `"asc"`, `"desc"`, `"na"` or `"none"`.
#'
#' @return The sorted data frame.
#' @export
apply_table_sort <- function(data, sort_col, sort_dir) {
  if (is.null(sort_col) || is.null(sort_dir) || sort_dir == "none") {
    return(data)
  }
  if (!sort_col %in% names(data)) {
    return(data)
  }
  if (sort_dir == "desc") {
    dplyr::arrange(data, dplyr::desc(.data[[sort_col]]))
  } else if (sort_dir == "na") {
    dplyr::arrange(data, !is.na(.data[[sort_col]]), .data[[sort_col]])
  } else {
    dplyr::arrange(data, .data[[sort_col]])
  }
}
