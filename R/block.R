#' Block custom UI
#'
#' @param id Block module id.
#' @param x Board object.
#' @param block Block to generate the UI for.
#' @param ... Generic consistency.
#'
#' @export
#' @rdname block_ui
block_ui.dag_board <- function(id, x, block = NULL, ...) {
  block_card <- function(x, id, ns) {
    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_metadata(x)

    card_tag <- tags$div(
      class = "card",
      width = "100%",
      id = ns(id),
      tags$div(
        class = "row g-0 px-4",
        tags$div(
          class = "col-md-2 d-flex align-items-center justify-content-start",
          blk_icon(blk_info$category, class = "fa-4x")
        ),
        tags$div(
          class = "col-md-10",
          tags$div(
            class = "card-body",
            tags$div(
              class = "d-flex align-items-center justify-content-start card-title gap-2",
              h4(firstup(blk_info$name)),
              tags$small(sprintf("(id: %s)", gsub("block_", "", id))),
              tooltip(
                icon("info-circle"),
                p(
                  icon("lightbulb"),
                  "How to use this block?",
                ),
                p(blk_info$description, ".")
              )
            ),
            # subtitle
            div(
              class = "card-subtitle mb-2 text-body-secondary",
              span(class = "badge bg-secondary", "Type:", blk_info$category),
              span(class = "badge bg-secondary", "Package:", blk_info$package)
            )
          )
        )
      ),
      accordion(
        id = ns(paste0("accordion-", id)),
        multiple = TRUE,
        class = "accordion-flush",
        open = c("outputs", "state"),
        accordion_panel(
          icon = icon("sliders"),
          title = "Block inputs",
          value = "inputs",
          expr_ui(blk_id, x)
        ),
        accordion_panel(
          icon = icon("chart-simple"),
          title = "Block output(s)",
          value = "outputs",
          style = "max-width: 100%; overflow-x: auto;",
          block_ui(blk_id, x)
        ),
        accordion_panel(
          title = "Block code",
          value = "code",
          icon = icon("code"),
        ),
        accordion_panel(
          title = "Block state",
          value = "state",
          icon = icon("bug")
        )
      )
    )
    tagAppendAttributes(card_tag, class = "border border-0 shadow-none")
  }

  ns <- NS(id)
  id <- names(block)
  stopifnot(is.character(id) && length(id) == 1L)
  block <- block[[1]]
  stopifnot(is_block(block))
  block_card(block, id, ns = ns)
}

#' @keywords internal
remove_block_panels <- function(id) {
  stopifnot(is.character(id))
  lapply(id, \(blk) {
    remove_panel("layout", paste0("block-", blk))
  })
}

#' @param blocks Blocks to insert or remove.
#' @rdname block_ui
#' @export
insert_block_ui.dag_board <- function(
  id,
  x,
  blocks = NULL,
  ...
) {
  session <- getDefaultReactiveDomain()
  stopifnot(
    is.character(id),
    length(id) == 1,
    is_board(x),
    !is.null(session)
  )

  ns <- session$ns

  # Handle startup state when blocks is NULL then we look at the board blocks.
  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  # Loop over blocks.
  # This can happen when we restore a board with multiple blocks.
  # Insert all the UI in the hidden offcanvas. Then we can show them
  # on demand ...
  lapply(seq_along(blocks), \(i) {
    blk <- blocks[i]
    blk_ui <- block_ui(id, x, blk)

    insertUI(
      sprintf(
        "#%s .offcanvas-body",
        ns("offcanvas")
      ),
      ui = blk_ui,
      immediate = TRUE
    )
  })

  invisible(x)
}

#' @rdname block_ui
#' @export
remove_block_ui.dag_board <- function(
  id,
  x,
  blocks = NULL,
  ...
) {
  session <- getDefaultReactiveDomain()
  stopifnot(
    is.character(id),
    length(id) == 1,
    is_board(x),
    !is.null(session)
  )

  ns <- session$ns

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  lapply(seq_along(blocks), \(i) {
    blk <- blocks[i]

    # Cleanup any existing UI for this block if we are refreshing the UI
    # from a snapshot.
    removeUI(
      sprintf("#%s", ns(names(blk))),
      immediate = TRUE
    )
  })

  invisible(x)
}

add_block_panel <- function(id, panels) {
  add_panel(
    "layout",
    panel = dockViewR::panel(
      id = sprintf("block-%s", id),
      title = sprintf("Block: %s", id),
      content = tagList(),
      # Don't remove if position is "within": by default,
      # only the visible tab is mounted in the DOM,
      # which means updating one block does not update
      # the linked block UIs and causes many issues.
      renderer = "always",
      # Remove padding and margin to use full space of the panel
      style = list(
        overflow = "auto",
        height = "100%"
      ),
      position = list(
        referencePanel = get_board_option_value(
          "blocks_position"
        )$reference_panel,
        direction = get_board_option_value("blocks_position")$direction
      ),
      remove = list(enable = TRUE, mode = "manual")
    )
  )
}

show_block_panel <- function(id, session) {
  ns <- session$ns
  # Move UI from offcanvas to the new panel
  session$sendCustomMessage(
    "show-block",
    list(
      block_id = sprintf("#%s", ns(id)),
      panel_id = sprintf("#%s", ns(paste0("layout-block-", id)))
    )
  )
}

#' Create/Show a block panel
#'
#' If panel does not exist, create it and move the block UI from
#' offcanvas to the panel container. If it exists, just select it.
#'
#' @param id Block id to show
#' @param parent Parent reactive values.
#' @param session Shiny session object.
#' @rdname block-panel
create_or_show_block_panel <- function(id, parent, session) {
  ns <- session$ns

  # Extract block panels
  all_panels <- get_panels_ids("layout", session)
  block_panels <- get_block_panels(all_panels)

  # If the block panel is already there,
  # just select it.
  if (parent$selected_block %in% block_panels) {
    # Only select panel
    dockViewR::select_panel(
      "layout",
      sprintf("block-%s", parent$selected_block)
    )
  } else {
    # Or add it and move the block UI from offcanvas to the panel container
    add_block_panel(id, all_panels)
    show_block_panel(id, session)
  }
}

#' Hide a block panel
#'
#' Move block from panel to offcanvas-body.
#'
#' @rdname block-panel
hide_block_panel <- function(id, session) {
  ns <- session$ns
  # Remove the block panel when the user clicks on the
  # close button of the panel.
  session$sendCustomMessage(
    "hide-block",
    list(
      offcanvas = sprintf("#%s", ns("offcanvas")),
      block_id = sprintf("#%s", ns(paste0("layout-", id)))
    )
  )
  remove_panel("layout", id)
}

#' Get block info in registry
#'
#' @param x Block object
#' @keywords internal
get_block_metadata <- function(x) {
  stopifnot(is_block(x))

  ctor <- attr(x, "ctor")

  if (is_string(ctor)) {
    blk <- sub("^new_", "", ctor)
    blks <- available_blocks()

    if (blk %in% names(blks)) {
      info <- blks[[blk]]

      res <- list(
        category = attr(info, "category"),
        name = attr(info, "name"),
        description = attr(info, "description"),
        package = attr(info, "package")
      )

      return(res)
    }
  }

  list(
    category = "Uncategorized",
    name = block_name(x),
    description = "No description available",
    package = "local"
  )
}

#' Get the state of a block
#'
#' @param rv Board reactiveValues for read-only usage.
#' @keywords internal
get_blocks_state <- function(rv) {
  stopifnot(is_board(rv$board))
  req(length(board_blocks(rv$board)) > 0)
  lapply(rv$blocks, \(blk) {
    blk$server$result()
  })
}
