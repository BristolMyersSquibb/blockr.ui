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
#' ellipsis with the tooltip - never to crushed ones.
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
        table_deps()
      )
    )
  }

  col_names <- names(dat)

  # Column labels (e.g. from ADaM datasets). `exact = TRUE`: without it a
  # column carrying haven value labels but no variable label returns those
  # (`attr(x, "label")` partial-matches `labels`) and vapply() errors. A label
  # equal to the column name adds nothing and is dropped.
  col_labels <- vapply(seq_along(dat), function(j) {
    lbl <- attr(dat[[j]], "label", exact = TRUE)
    ok <- is.character(lbl) && length(lbl) == 1L && !is.na(lbl) &&
      nzchar(lbl) && !identical(lbl, names(dat)[j])
    if (ok) lbl else ""
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

  # What each sort direction puts on its header: the order screen readers
  # get (aria-sort) and the tooltip that says in words what the cue shows,
  # since the dot for missing values first does not say "missing" by itself.
  # The classes are named after the direction.
  sort_states <- list(
    asc = list(
      aria = "ascending",
      tip = "Sorted ascending, missing values last"
    ),
    desc = list(
      aria = "descending",
      tip = "Sorted descending, missing values last"
    ),
    na = list(
      aria = "ascending",
      tip = "Missing values first, then ascending"
    )
  )

  # The header and the body are written as one HTML string rather than as
  # htmltools tags: a 5 x 50 page is some 2,000 tags, and building, walking
  # and printing them cost more than everything else in a render. The string
  # has no indentation, which also keeps stray whitespace out of the cells.
  esc <- function(x) htmltools::htmlEscape(x)
  esc_attr <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

  # The sorted column, if any ("none" has no state).
  sorted_j <- if (!is.null(sort_col) && sort_dir %in% names(sort_states)) {
    match(sort_col, col_names)
  } else {
    NA_integer_
  }
  is_sorted <- !is.na(sorted_j) & seq_len(n_cols) == sorted_j
  sorted <- if (!is.na(sorted_j)) sort_states[[sort_dir]]

  header_class <- paste0(
    "blockr-sortable",
    ifelse(is_sorted, paste0(" blockr-sort-", sort_dir), ""),
    # A numeric column's header follows its numbers to the right.
    ifelse(col_is_numeric, " blockr-th-numeric", "")
  )
  sort_icon_class <- paste0(
    "blockr-sort-icon",
    ifelse(is_sorted, paste0(" blockr-sort-icon-", sort_dir), "")
  )
  sort_attrs <- ifelse(
    is_sorted,
    paste0(' aria-sort="', sorted$aria, '" data-blockr-tooltip="',
           esc_attr(sorted$tip), '"'),
    ""
  )

  # The label sits under the name and is cut by the layout (CSS ellipsis);
  # a cut-off label shows whole in its tooltip. A column without a label
  # keeps an empty line when others have one, so every name sits level.
  label_html <- if (!has_labels) {
    character(n_cols)
  } else {
    ifelse(
      nzchar(col_labels),
      paste0('<span class="blockr-col-label" data-blockr-tooltip="',
             esc_attr(col_labels), '" data-blockr-tooltip-overflow>',
             esc(col_labels), "</span>"),
      paste0('<span class="blockr-col-label blockr-col-label--empty" ',
             'aria-hidden="true">&nbsp;</span>')
    )
  }

  # The sort cue (sort bars, see blockr-table-preview.css) sits on the
  # name's line: after the name, or before it on a right-aligned numeric
  # column so the name stays flush over its numbers. Hidden at rest on an
  # unsorted column, shown muted on hover as what a click would do.
  header_html <- paste0(
    '<thead><tr><th class="blockr-row-number"></th>',
    paste0(
      '<th class="', header_class, '" style="width: ', col_widths_px,
      'px;" data-column="', esc_attr(col_names), '"', sort_attrs, ">",
      '<span class="blockr-col-head"><span class="blockr-col-name">',
      esc(col_names), '</span><span class="', sort_icon_class,
      '"></span></span>', label_html,
      '<span class="blockr-type-label">', esc(col_types), "</span></th>",
      collapse = ""
    ),
    "</tr></thead>"
  )

  # Body cells: `<td>value</td>`. Numeric alignment comes from a rule per
  # column (below), not a class per cell. A value cut off by the column width
  # shows whole in its tooltip, which blockr-table-preview.js gives a cell
  # from its text when the pointer first reaches it.
  start_row_num <- (page - 1L) * page_size
  cells <- lapply(seq_len(n_cols), function(j) {
    ifelse(
      col_na[[j]],
      '<td><span class="blockr-na">NA</span></td>',
      paste0(
        ifelse(col_neg[[j]], '<td class="blockr-negative">', "<td>"),
        esc(formatted[[j]]), "</td>"
      )
    )
  })
  body_html <- if (n_showing > 0L) {
    paste0(
      '<tr><td class="blockr-row-number">', start_row_num + seq_len(n_showing),
      "</td>", do.call(paste0, cells), "</tr>",
      collapse = ""
    )
  } else {
    ""
  }

  # Right-aligned tabular numbers for the numeric columns, one rule for the
  # table, scoped by its id. Runs of adjacent numeric columns become one
  # range selector. The row-number column is child 1.
  table_id <- table_dom_id(sort_input_id, has_ids = !is.null(sort_input) ||
                             !is.null(ns))
  num_style <- numeric_column_style(table_id, which(col_is_numeric) + 1L)

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
    # Cut by the layout (CSS ellipsis), as a column label is; a cut-off
    # label shows whole in its tooltip.
    table_label_tag <- shiny::tags$span(
      class = "blockr-table-label",
      `data-blockr-tooltip` = table_label,
      `data-blockr-tooltip-overflow` = NA,
      shiny::HTML("&middot;&nbsp;"),
      table_label
    )
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
        `aria-label` = "Previous page",
        `data-blockr-tooltip` = "Previous page",
        small_icon("chevron")
      ),
      shiny::tags$button(
        class = paste0("blockr-nav-btn", if (next_disabled) " disabled"),
        disabled = if (next_disabled) "disabled" else NULL,
        `data-direction` = "next",
        `aria-label` = "Next page",
        `data-blockr-tooltip` = "Next page",
        small_icon("chevron")
      )
    )
  )

  # Fixed layout from the FIRST paint: the ths carry explicit widths, so
  # content changes (sort, page) cannot reflow the columns. Width stays 100%
  # via the class: per CSS, a fixed table's used width is max(100%, sum of
  # columns), so narrow tables still fill the panel and wide ones
  # overflow-scroll.
  table_html <- paste0(
    num_style,
    '<div class="blockr-table-wrapper">',
    '<table class="blockr-table" style="table-layout: fixed;">',
    header_html, "<tbody>", body_html, "</tbody></table></div>"
  )

  htmltools::attachDependencies(
    shiny::tagList(
      shiny::tags$div(
        id = table_id,
        class = "blockr-table-container",
        `data-sort-input` = sort_input_id,
        `data-page-input` = page_input_id,
        `data-current-page` = page,
        `data-max-page` = max_page,
        shiny::HTML(table_html),
        footer
      )
    ),
    table_deps()
  )
}

# The container's id, which scopes the table's column rule. Derived from the
# sort input, which is unique per preview when the caller passes ids; the
# legacy shared default gets a counter, so two such tables on one page
# never share a rule.
table_dom_id <- function(sort_input_id, has_ids) {
  id <- paste0("blockr-tbl-", gsub("[^A-Za-z0-9_-]", "-", sort_input_id))
  if (has_ids) {
    return(id)
  }
  table_state$n <- table_state$n + 1L
  paste0(id, "-", table_state$n)
}

table_state <- new.env(parent = emptyenv())
table_state$n <- 0L

# A <style> right-aligning the body cells of the given columns (1-based
# child positions) in the table with id `table_id`, or "" for none.
numeric_column_style <- function(table_id, pos) {
  if (!length(pos)) {
    return("")
  }
  # Split into runs of consecutive positions.
  run <- cumsum(c(1L, diff(pos) != 1L))
  sel <- vapply(split(pos, run), function(p) {
    if (length(p) == 1L) {
      sprintf(":nth-child(%d)", p)
    } else {
      sprintf(":nth-child(n+%d):nth-child(-n+%d)", p[1L], p[length(p)])
    }
  }, character(1))
  paste0(
    "<style>#", table_id, " td:is(", paste(sel, collapse = ","),
    "){text-align:right;font-variant-numeric:tabular-nums}</style>"
  )
}

# The table's dependencies, built once per process: building them reads the
# package version and the icon files.
table_deps <- function() {
  if (is.null(table_state$deps)) {
    table_state$deps <- c(
      list(table_preview_dep()),
      table_preview_tooltip_dep()
    )
  }
  table_state$deps
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

# The shared tooltip (Blockr.tooltip) and its card style, for the sorted
# header's tooltip and the cut-off cells. The card reads the tokens without
# fallbacks, so they come along; the theme layer stays the app's to attach.
# The icons go wherever blockr-ui.js goes, as its menus draw them.
table_preview_tooltip_dep <- function() {
  list(
    tokens_dep(),
    icons_dep(),
    controls_asset("blockr-ui-js", script = "js/blockr-ui.js"),
    controls_asset("blockr-tooltip-css", stylesheet = "css/blockr-tooltip.css")
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
