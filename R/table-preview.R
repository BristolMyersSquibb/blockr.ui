# Server-side paginated HTML table preview - canonical home.
#
# Moved here from blockr.extra/blockr.dm (was duplicated; see
# _blockr.design/open/html-table-preview/ for the spec). build_html_table()
# is the pure presentation layer: it receives an already-materialized page
# and never touches data access (see table-preview-engine.R for that).

#' @keywords internal
col_type_label <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    "<dttm>"
  } else if (inherits(x, "Date")) {
    "<date>"
  } else if (is.factor(x)) {
    "<fct>"
  } else if (is.logical(x)) {
    "<lgl>"
  } else if (is.integer(x)) {
    "<int>"
  } else if (is.numeric(x)) {
    "<dbl>"
  } else if (is.character(x)) {
    "<chr>"
  } else if (is.list(x)) {
    "<list>"
  } else {
    paste0("<", class(x)[1], ">")
  }
}

# pillar (not format()) on purpose: column-aware alignment, tabular nums and
# sensible truncation; comes for free as a direct Import of dplyr.
#' @keywords internal
format_column_inner <- function(x, max_chars = 50) {
  if (is.character(x)) {
    x
  } else {
    shaft <- pillar::pillar_shaft(x)
    trimws(format(shaft, width = max_chars))
  }
}

#' Build the HTML table preview for one page
#'
#' Pure presentation: renders an already-materialized page of data as the
#' blockr table preview (header with column names, labels and type tags,
#' pillar-formatted cells, NA / negative styling, footer with row range and
#' prev / next pagination). Data access (sorting, pagination, lazy tables)
#' is handled by [table_page()].
#'
#' @param dat Materialized data frame holding only the rows of the current
#'   page.
#' @param total_rows Total number of rows in the full result (drives the
#'   row-range text and pagination).
#' @param sort_state List with elements `col` and `dir` (`"asc"`, `"desc"`,
#'   `"na"` or `"none"`) or `NULL`.
#' @param ns Optional namespace function used for the legacy default input
#'   ids when `sort_input` / `page_input` are not supplied.
#' @param page Current page (1-based).
#' @param page_size Rows per page.
#' @param table_label Optional table-level label shown in the footer.
#' @param sort_input,page_input Full (already namespaced) ids of the Shiny
#'   inputs receiving sort and page events. Supply per-instance ids when a
#'   session shows more than one preview; when `NULL` the legacy shared
#'   names `blockr_table_sort` / `blockr_table_page` are used.
#'
#' @return A [shiny::tagList()] with the table preview, carrying the
#'   [table_preview_dep()] html dependency.
#' @export
build_html_table <- function(dat, total_rows, sort_state = NULL, ns = NULL,
                             page = 1L, page_size = 5L, table_label = NULL,
                             sort_input = NULL, page_input = NULL) {
  n_showing <- nrow(dat)
  n_cols <- ncol(dat)

  sort_col <- sort_state$col
  sort_dir <- sort_state$dir
  # Normalise once so the per-column header logic below can rely on a clean
  # scalar (a NA/NULL `sort_dir` would make `sort_dir != "none"` return NA and
  # crash the `&&` with "missing value where TRUE/FALSE needed").
  if (length(sort_dir) != 1L || is.na(sort_dir)) sort_dir <- "none"
  if (length(sort_col) != 1L || is.na(sort_col)) sort_col <- NULL

  sort_input_id <- if (!is.null(sort_input)) {
    sort_input
  } else if (!is.null(ns)) {
    ns("blockr_table_sort")
  } else {
    "blockr_table_sort"
  }
  page_input_id <- if (!is.null(page_input)) {
    page_input
  } else if (!is.null(ns)) {
    ns("blockr_table_page")
  } else {
    "blockr_table_page"
  }

  # Handle empty data frame
  if (n_cols == 0) {
    return(
      htmltools::attachDependencies(
        shiny::tagList(
          shiny::tags$div(
            class = "blockr-table-container",
            `data-sort-input` = sort_input_id,
            `data-page-input` = page_input_id,
            shiny::tags$div(
              class = "blockr-table-footer",
              shiny::tags$span(
                class = "blockr-table-range",
                "Empty data frame (0 columns)"
              )
            )
          )
        ),
        table_preview_dep()
      )
    )
  }

  col_names <- names(dat)

  # Extract column labels (e.g. from ADaM datasets)
  col_labels <- vapply(dat, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) "" else lbl
  }, character(1))
  has_labels <- any(nzchar(col_labels))

  # Pre-compute column metadata
  col_is_numeric <- vapply(dat, is.numeric, logical(1))
  col_types <- vapply(dat, col_type_label, character(1))

  # Pre-format all columns
  old_opts <- options(cli.num_colors = 1)
  on.exit(options(old_opts), add = TRUE)

  formatted <- lapply(dat, format_column_inner)

  # Pre-compute NA and negative masks
  col_na <- lapply(dat, is.na)
  col_neg <- Map(function(vec, is_num) {
    if (is_num) !is.na(vec) & vec < 0 else rep(FALSE, length(vec))
  }, dat, col_is_numeric)

  # Build header row
  header_cells <- vector("list", n_cols + 1L)
  header_cells[[1L]] <- shiny::tags$th(class = "blockr-row-number", "")
  for (j in seq_along(col_names)) {
    col_name <- col_names[j]

    # Determine sort class for this column
    header_class <- "blockr-sortable"
    sort_icon_class <- "blockr-sort-icon"
    if (!is.null(sort_col) && sort_col == col_name && sort_dir != "none") {
      sort_class_suffix <- switch(
        sort_dir,
        asc = " blockr-sort-asc",
        desc = " blockr-sort-desc",
        na = " blockr-sort-na",
        ""
      )
      header_class <- paste0(header_class, sort_class_suffix)
      icon_class_suffix <- switch(
        sort_dir,
        asc = " blockr-sort-icon-asc",
        desc = " blockr-sort-icon-desc",
        na = " blockr-sort-icon-na",
        ""
      )
      sort_icon_class <- paste0(sort_icon_class, icon_class_suffix)
    }

    label_tag <- if (has_labels && nzchar(col_labels[j])) {
      is_truncated <- nchar(col_labels[j]) > 20
      display_text <- if (is_truncated) {
        paste0(substr(col_labels[j], 1, 18), "\u2026")
      } else {
        col_labels[j]
      }
      label_args <- list(
        class = "blockr-col-label",
        display_text
      )
      if (is_truncated) {
        label_args[["title"]] <- col_labels[j]
      }
      do.call(shiny::tags$span, label_args)
    }

    name_width <- nchar(col_name) * 8 + 32
    label_width <- if (has_labels && nzchar(col_labels[j])) {
      min(nchar(col_labels[j]), 20) * 7 + 32
    } else {
      0
    }
    min_width <- min(max(name_width, label_width, 60), 250)
    th_style <- sprintf("min-width: %dpx;", min_width)

    header_cells[[j + 1L]] <- shiny::tags$th(
      class = header_class,
      style = th_style,
      `data-column` = col_name,
      shiny::tags$span(class = "blockr-col-name", col_name),
      label_tag,
      shiny::tags$span(
        class = "blockr-type-row",
        shiny::tags$span(class = "blockr-type-label", col_types[j]),
        shiny::tags$span(class = sort_icon_class)
      )
    )
  }

  # Build body rows
  body_rows <- vector("list", n_showing)
  start_row_num <- (page - 1L) * page_size

  for (i in seq_len(n_showing)) {
    row_cells <- vector("list", n_cols + 1L)
    row_cells[[1L]] <- shiny::tags$td(
      class = "blockr-row-number",
      start_row_num + i
    )

    for (j in seq_along(col_names)) {
      is_na <- col_na[[j]][i]
      is_neg <- col_neg[[j]][i]

      cell_class <- if (col_is_numeric[j]) {
        if (is_neg) "blockr-td-numeric blockr-negative" else "blockr-td-numeric"
      } else {
        NULL
      }

      content <- if (is_na) {
        shiny::tags$span(class = "blockr-na", "NA")
      } else {
        formatted[[j]][i]
      }

      cell_title <- if (!is_na) formatted[[j]][i] else NULL
      row_cells[[j + 1L]] <- shiny::tags$td(
        class = cell_class,
        title = cell_title,
        content
      )
    }

    body_rows[[i]] <- do.call(shiny::tags$tr, row_cells)
  }

  # Build pagination info
  max_page <- max(1L, ceiling(total_rows / page_size))
  start_row <- (page - 1L) * page_size + 1L
  end_row <- min(page * page_size, total_rows)

  range_text <- if (total_rows == 0) {
    "No rows"
  } else {
    sprintf("%d\u2013%d of %d", start_row, end_row, total_rows)
  }

  # Build optional table label span (displayed in footer next to row range)
  # Coerce to a single string first: a non-scalar label (e.g. a stray
  # length-2 attr) would make the nzchar() below trip the
  # "'length = 2' in coercion to 'logical(1)'" error.
  if (length(table_label) > 1L) table_label <- table_label[[1L]]
  table_label_tag <- NULL
  if (!is.null(table_label) && is.character(table_label) &&
      nzchar(table_label)) {
    is_truncated <- nchar(table_label) > 60
    display_text <- if (is_truncated) {
      paste0(substr(table_label, 1, 58), "\u2026")
    } else {
      table_label
    }
    label_args <- list(
      class = "blockr-table-label",
      shiny::HTML("&middot;&nbsp;"),
      display_text
    )
    if (is_truncated) {
      label_args[["title"]] <- table_label
    }
    table_label_tag <- do.call(shiny::tags$span, label_args)
  }

  footer <- shiny::tags$div(
    class = "blockr-table-footer",
    shiny::tags$div(
      class = "blockr-table-footer-info",
      shiny::tags$span(class = "blockr-table-range", range_text),
      table_label_tag
    ),
    shiny::tags$div(
      class = "blockr-table-nav",
      shiny::tags$button(
        class = paste0("blockr-nav-btn", if (page == 1L) " disabled"),
        disabled = if (page == 1L) "disabled" else NULL,
        `data-direction` = "prev",
        shiny::HTML("&#x2039;")
      ),
      shiny::tags$button(
        class = paste0("blockr-nav-btn", if (page >= max_page) " disabled"),
        disabled = if (page >= max_page) "disabled" else NULL,
        `data-direction` = "next",
        shiny::HTML("&#x203A;")
      )
    )
  )

  htmltools::attachDependencies(
    shiny::tagList(
      shiny::tags$div(
        class = "blockr-table-container",
        `data-sort-input` = sort_input_id,
        `data-page-input` = page_input_id,
        `data-current-page` = page,
        `data-max-page` = max_page,
        shiny::tags$div(
          class = "blockr-table-wrapper",
          shiny::tags$table(
            class = "blockr-table",
            shiny::tags$thead(
              do.call(shiny::tags$tr, header_cells)
            ),
            do.call(shiny::tags$tbody, body_rows)
          )
        ),
        footer
      )
    ),
    table_preview_dep()
  )
}

#' Table-preview assets
#'
#' `table_preview_dep()` returns the [htmltools::htmlDependency] carrying the
#' table-preview CSS and JS; [build_html_table()] attaches it automatically.
#' `table_preview_css()` returns the base CSS as an inline
#' `shiny::tags$style` for hosts that layer delta CSS on top of it (e.g.
#' blockr.viz's html table block); it reads the same file the dependency
#' serves, so there is a single source.
#'
#' @return An [htmltools::htmlDependency] or a `shiny::tags$style` element.
#' @export
table_preview_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-table-preview",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = "css/blockr-table-preview.css",
    script = "js/blockr-table-preview.js",
    all_files = FALSE
  )
}

#' @rdname table_preview_dep
#' @export
table_preview_css <- function() {
  css <- paste(
    readLines(
      system.file("assets/css/blockr-table-preview.css", package = "blockr.ui"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  shiny::tags$style(shiny::HTML(css))
}
