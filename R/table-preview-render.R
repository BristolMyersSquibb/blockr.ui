# Render glue: the renderUI loop wiring the page engine to the HTML builder.

#' Render a result as the HTML table preview
#'
#' `html_table_render()` returns a [shiny::renderUI()] that shows `result`
#' as the paginated, sortable table preview. `html_table_result()` is the
#' block-flavored wrapper that reads the `page_size` board option first.
#'
#' Sort and page state live in the browser and arrive via Shiny inputs whose
#' ids are derived from the output name (via [shiny::getCurrentOutputInfo()]),
#' so several previews in one module session stay independent. Sorting and
#' pagination are resolved by [table_page()]: in-memory frames use a cached
#' order index, lazy dbplyr tables are counted and paged in the database.
#'
#' @param result A data frame or lazy table (`tbl_lazy`).
#' @param block The block object (used to read board options).
#' @param session The Shiny session.
#' @param page_size Rows per page.
#'
#' @return A [shiny::renderUI()] object.
#' @export
html_table_render <- function(result, session, page_size = 5L) {
  ns <- session$ns

  # One cache per render closure: blockr.core re-runs block_output() (and
  # hence recreates this closure) whenever the block result changes, so the
  # memoized sort index / row count never outlive their result.
  cache <- new.env(parent = emptyenv())
  prev_sort <- NULL

  shiny::renderUI({
    out_name <- tryCatch(
      shiny::getCurrentOutputInfo(session)$name,
      error = function(e) NULL
    )
    instance <- if (is.null(out_name) || !nzchar(out_name)) {
      "result"
    } else {
      out_name
    }
    sort_id <- paste0(instance, "_table_sort")
    page_id <- paste0(instance, "_table_page")

    tryCatch(
      {
        sort_input <- session$input[[sort_id]]
        current_sort <- if (!is.null(sort_input)) {
          list(col = sort_input$col, dir = sort_input$dir)
        } else {
          list(col = NULL, dir = "none")
        }

        page <- session$input[[page_id]]
        page <- if (is.null(page)) 1L else as.integer(page)

        # Reset to page 1 when the sort changes. Server-side on purpose: the
        # JS sets only the sort input, so one click stays one render (a
        # second page-reset input would re-render the output twice).
        if (!identical(current_sort, prev_sort)) {
          prev_sort <<- current_sort
          page <- 1L
        }

        # Extract table label before paging strips it. exact = TRUE so we
        # don't partial-match other attrs (e.g. groupedData's length-2
        # `labels` list, as on datasets::CO2) — that would make the
        # downstream nzchar() in build_html_table() trip the
        # "'length = 2' in coercion to 'logical(1)'" error.
        tbl_label <- attr(result, "label", exact = TRUE)

        pg <- table_page(result, current_sort, page, page_size, cache)

        build_html_table(
          pg$dat,
          pg$total_rows,
          sort_state = current_sort,
          page = pg$page,
          page_size = page_size,
          table_label = tbl_label,
          sort_input = ns(sort_id),
          page_input = ns(page_id)
        )
      },
      error = function(e) {
        shiny::tags$div(
          class = "blockr-table-error",
          style = "color: red; padding: 12px;",
          paste("Error rendering table:", conditionMessage(e))
        )
      }
    )
  })
}

#' @rdname html_table_render
#' @export
html_table_result <- function(result, block, session) {
  page_size <- tryCatch(
    blockr.core::get_board_option_or_default(
      "page_size",
      blockr.core::board_options(block),
      session
    ),
    error = function(e) 5L
  )

  html_table_render(result, session, page_size)
}
