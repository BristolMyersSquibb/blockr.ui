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

#' Estimate fixed-layout column widths
#'
#' Per-column width in px, from the header parts and the formatted cell
#' strings, for tables rendered with `table-layout: fixed` (the blockr
#' table preview and blockr.viz's html tables). Computing widths
#' server-side keeps the layout independent of measuring the DOM - the
#' measure-and-lock approach reads 0 whenever a render lands while a dock
#' panel is hidden or mid-relayout, crushing the columns.
#'
#' Deliberately px, not font-relative units: `ch` tracks the width of the
#' digit 0, which in some font stacks runs 40% narrower than average
#' text, so ch-sized columns clip hard exactly where fonts differ.
#' 8px/char at the 14px table font (the constant the header min-width
#' heuristic used for years) over-estimates almost every UI font, so the
#' estimate degrades to slightly roomy columns - or, at worst, mild
#' ellipsis with the title tooltip - never to crushed ones.
#'
#' @param col_names Character vector of column header texts (plain text,
#'   strip any HTML before calling).
#' @param col_labels Optional secondary header line per column (the
#'   preview's ADaM labels); `""` for none.
#' @param col_types Optional type-tag line per column (`"<dbl>"` etc.);
#'   `""` for none.
#' @param formatted List of character vectors: the display strings of
#'   each column's cells (only the rows being rendered).
#' @param col_na List of logical vectors flagging cells rendered as the
#'   `NA` marker instead of their display string.
#' @param wrap_names Measure each header by its longest *word* rather than
#'   its full length. A header is free to wrap (nothing pins it to one
#'   line), it simply never has to when the estimate buys it a full line -
#'   so a long title sets the column width outright and the table sprawls.
#'   Measuring the longest word instead gives the narrowest column that
#'   still never breaks a word, and the header wraps into it by itself.
#'   `FALSE` (the default) keeps the historical one-line estimate.
#'
#' @return Integer vector of pixel widths, one per column, including the
#'   two 16px cell paddings, clamped to \[60, 320\].
#' @export
column_widths_px <- function(col_names,
                             col_labels = character(length(col_names)),
                             col_types = character(length(col_names)),
                             formatted = rep(list(character(0)),
                                             length(col_names)),
                             col_na = lapply(formatted, is.na),
                             wrap_names = FALSE) {
  n <- length(col_names)
  content_chars <- vapply(seq_len(n), function(j) {
    vals <- formatted[[j]][!col_na[[j]]]
    w <- max(nchar(vals, type = "width"), 0L, na.rm = TRUE)
    max(w, if (any(col_na[[j]])) 2L else 0L)
  }, numeric(1))
  # A wrapping header only needs room for its longest word; without wrap it
  # must fit end to end on one line.
  name_chars <- if (isTRUE(wrap_names)) {
    vapply(strsplit(col_names, "[[:space:]]+"), function(w) {
      if (!length(w)) return(0)
      max(nchar(w, type = "width"))
    }, numeric(1))
  } else {
    nchar(col_names, type = "width")
  }
  name_px <- name_chars * 8
  # Labels render at 11px under a 120px CSS cap, truncated to 18 chars +
  # ellipsis; type tags at 11px next to the sort-icon slot.
  label_px <- pmin(nchar(col_labels, type = "width"), 19L) * 6
  type_px <- nchar(col_types, type = "width") * 6 + 16
  content_px <- content_chars * 8
  # 32 = the two 16px cell paddings; clamp to the old 60px floor and a cap
  # a bit above the old 250px header bound.
  round(pmin(pmax(pmax(name_px, label_px, type_px, content_px) + 32, 60), 320))
}

# pillar (not format()) on purpose: column-aware alignment, tabular nums and
# sensible truncation; comes for free as a direct Import of dplyr.
# Factors bypass pillar like character does: pillar's factor shaft QUOTES
# the levels ("NORMAL", and an empty level as ""), which read as if the
# data itself contained quote characters.
#' @keywords internal
format_column_inner <- function(x, max_chars = 50) {
  if (is.character(x)) {
    x
  } else if (is.factor(x)) {
    as.character(x)
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
#' @param has_more Look-ahead flag used when `total_rows` is `NA` (remote /
#'   lazy tables that are never counted): `TRUE` signals that rows exist
#'   beyond the current page, which keeps the next button enabled and shows
#'   the "of many" row-range text instead of a known total.
#' @param cache Optional environment memoizing the column widths across
#'   renders of the same result (keyed on the column names). Pass the same
#'   per-result cache used for [table_page()] so sorting and paging emit
#'   identical widths and the columns never reflow. When `NULL`, widths are
#'   recomputed from the current page on every render.
#'
#' @return A [shiny::tagList()] with the table preview, carrying the
#'   [table_preview_dep()] html dependency.
#' @export
build_html_table <- function(dat, total_rows, sort_state = NULL, ns = NULL,
                             page = 1L, page_size = 5L, table_label = NULL,
                             sort_input = NULL, page_input = NULL,
                             has_more = NULL, cache = NULL) {
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

  # Column widths are computed server-side from the formatted strings and
  # emitted as `table-layout: fixed` widths from the first render, so the
  # layout never depends on measuring the DOM (a measurement taken while a
  # dock panel is hidden or mid-relayout reads 0 and used to crush the
  # columns for the whole session). With the per-result `cache`, sorting
  # and paging emit identical widths, so the columns never reflow - by
  # construction, not via client-side locking.
  col_widths_px <- NULL
  if (!is.null(cache)) {
    stored <- get0(".col_widths", envir = cache, inherits = FALSE)
    if (!is.null(stored) && identical(stored$cols, col_names)) {
      col_widths_px <- stored$widths
    }
  }
  if (is.null(col_widths_px)) {
    col_widths_px <- column_widths_px(col_names, col_labels, col_types,
                                      formatted, col_na)
    if (!is.null(cache)) {
      assign(
        ".col_widths",
        list(cols = col_names, widths = col_widths_px),
        envir = cache
      )
    }
  }

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

    th_style <- sprintf("width: %dpx;", col_widths_px[j])

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
        # .noWS: htmltools pretty-prints nested tags with newline + indent,
        # and those literal characters render under the cell's white-space
        # rule - every NA cell became three lines tall, ballooning its
        # whole row.
        shiny::tags$span(class = "blockr-na", "NA", .noWS = "outside")
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

  # Build pagination info. `total_rows` is NA for remote/lazy tables (counting
  # them is deliberately avoided); then the range omits the total and the next
  # button is driven by a look-ahead (`has_more`) instead of a known max page.
  unknown_total <- length(total_rows) != 1L || is.na(total_rows)
  start_row <- (page - 1L) * page_size + 1L

  if (unknown_total) {
    n_shown <- nrow(dat)
    end_row <- start_row + n_shown - 1L
    next_disabled <- !isTRUE(has_more)
    # data-max-page only needs to let the JS step to the next page when there
    # is one; the server re-evaluates `has_more` on arrival.
    max_page <- if (isTRUE(has_more)) page + 1L else page
    range_text <- if (n_shown == 0L) {
      "No rows"
    } else if (isTRUE(has_more)) {
      # total unknown (never counted) but we know more rows exist
      sprintf("%d\u2013%d of many", start_row, end_row)
    } else {
      # last page: no further rows, so end_row IS the true total - shown for
      # free, without ever running a COUNT(*).
      sprintf("%d\u2013%d of %d", start_row, end_row, end_row)
    }
  } else {
    max_page <- max(1L, ceiling(total_rows / page_size))
    end_row <- min(page * page_size, total_rows)
    next_disabled <- page >= max_page
    range_text <- if (total_rows == 0) {
      "No rows"
    } else {
      sprintf("%d\u2013%d of %d", start_row, end_row, total_rows)
    }
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
        class = paste0("blockr-nav-btn", if (next_disabled) " disabled"),
        disabled = if (next_disabled) "disabled" else NULL,
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
            # Fixed layout from the FIRST paint: the ths carry explicit
            # widths, so content changes (sort, page) cannot reflow the
            # columns. Width stays 100% via the class: per CSS, a fixed
            # table's used width is max(100%, sum of columns), so narrow
            # tables still fill the panel and wide ones overflow-scroll.
            style = "table-layout: fixed;",
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
