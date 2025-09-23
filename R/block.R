#' Block custom UI
#'
#' @param id Block module id.
#' @param x Board object.
#' @param block Block to generate the UI for.
#' @param edit_ui Block edit plugin.
#' @param ... Generic consistency.
#'
#' @export
#' @rdname block_ui
block_ui.dag_board <- function(id, x, block = NULL, edit_ui = NULL, ...) {
  block_card <- function(x, id, ns) {
    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_metadata(x)

    # Edit plugin
    if (!is.null(edit_ui)) {
      edit_ui <- edit_ui$ui(x, NS(blk_id, "edit_block"))
    }

    card_tag <- tags$div(
      class = "card",
      width = "100%",
      id = ns(id),
      tags$div(
        class = "card-body",
        tags$div(
          class = sprintf(
            "card-title d-flex align-items-center justify-content-between gap-2 border-start border-5 border-%s ps-2",
            blk_border_color(blk_info$category)
          ),
          edit_ui$block_name,
          shinyWidgets::checkboxGroupButtons(
            inputId = ns(sprintf("blk_collapse-%s", id)),
            status = "light",
            size = "sm",
            choices = setNames(
              c("input", "output", "code"),
              c(
                sprintf(
                  "<i class='fa fa-sliders' data-bs-toggle='collapse' data-bs-target='#%s' aria-expanded='false' aria-controls='%s'></i>",
                  ns(paste0("input-", id)),
                  ns(paste0("input-", id))
                ),
                sprintf(
                  "<i class='fa fa-line-chart' data-bs-toggle='collapse' data-bs-target='#%s' aria-expanded='false' aria-controls='%s'></i>",
                  ns(paste0("output-", id)),
                  ns(paste0("output-", id))
                ),
                sprintf(
                  "<i class='fa fa-code' data-bs-toggle='collapse' data-bs-target='#%s' aria-expanded='false' aria-controls='%s'></i>",
                  ns(paste0("code-", id)),
                  ns(paste0("code-", id))
                )
              )
            )
          ),
          dropdown_button(
            class = "float-end",
            icon = icon("ellipsis-vertical"),
            size = "sm",
            dropdown_header("... Block actions"),
            dropdown_action_button(
              ns(sprintf("append-%s", blk_id)),
              "Append block",
              icon = icon("plus")
            ),
            dropdown_action_button(
              ns(sprintf("delete-%s", blk_id)),
              "Delete block",
              icon = icon("trash"),
              class = "text-danger"
            ),
            dropdown_divider(),
            div(
              class = "text-muted d-flex justify-content-between",
              p("Package: "),
              p(blk_info$package)
            ),
            div(
              class = "text-muted d-flex justify-content-between",
              p("Type: "),
              p(blk_info$category)
            ),
            div(
              class = "text-muted d-flex justify-content-between",
              p("ID: "),
              p(id)
            )
          )
        ),
        # subtitle
        div(
          class = "card-subtitle text-body-secondary",
          span(class(x)[1]),
          " || ",
          span("id:", id),
          tooltip(
            icon("info-circle"),
            p(
              icon("lightbulb"),
              "How to use this block?",
            ),
            p(blk_info$description, ".")
          )
        ),
        hr(),
        #edit_ui$block_summary,
        div(id = ns(paste0("errors-", id))),
        collapse_container(
          id = ns(paste0("input-", id)),
          expr_ui(blk_id, x)
        ),
        collapse_container(
          id = ns(paste0("output-", id)),
          block_ui(blk_id, x)
        ),
        collapse_container(
          id = ns(paste0("code-", id))
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
remove_block_panels <- function(ids, panels) {
  stopifnot(is.character(ids))
  # Only remove panels that are in the dock
  in_dock <- which(ids %in% get_block_panels(names(panels)))
  if (!length(in_dock)) {
    return(NULL)
  }
  ids <- ids[in_dock]

  lapply(ids, function(id) {
    remove_panel("layout", paste0("block-", id))
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
  lapply(seq_along(blocks), function(i) {
    blk <- blocks[i]
    blk_ui <- block_ui(id, x, blk, ...)

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

  lapply(seq_along(blocks), function(i) {
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
  lapply(rv$blocks, function(blk) {
    blk$server$result()
  })
}
