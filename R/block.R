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
  ns <- NS(id)
  block <- block[[1]]
  stopifnot(is_block(block))
  block_card(x, block, edit_ui, ns = ns)
}

#' @keywords internal
block_card <- function(board, block, edit_ui, ns) {
  id <- block_name_to_id(block)
  blk_id <- ns(paste0("block_", id))
  blk_info <- get_block_metadata(block)

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
      div(
        class = "border-start border-5 ps-2",
        style = sprintf(
          "--bs-border-opacity: 1; border-color: %s !important;",
          blk_border_color(blk_info$category)
        ),
        block_card_title(id, blk_info, edit_ui, ns),
        # subtitle
        block_card_subtitle(board, block, id, blk_info),
      ),
      #edit_ui$block_summary,
      block_card_content(block, id, blk_id, ns)
    )
  )
  tagAppendAttributes(card_tag, class = "border border-0 shadow-none")
}

#' @keywords internal
block_card_title <- function(id, info, edit_ui, ns) {
  tags$div(
    class = "card-title d-flex align-items-center justify-content-between gap-2",
    edit_ui$block_name,
    block_card_toggles(id, ns),
    block_card_dropdown(id, info, ns)
  )
}

#' Block subtitle id method
#'
#' @param x Board.
#' @param id Block id.
#'
#' @export
block_subtitle_id <- function(x, id) {
  UseMethod("block_subtitle_id", x)
}

#' @export
block_subtitle_id.dag_board <- function(x, id) {
  NULL
}

#' @export
block_subtitle_id.md_board <- function(x, id) {
  tagList(
    " || ",
    span("id:", id)
  )
}

#' @keywords internal
block_card_subtitle <- function(board, block, id, info) {
  div(
    class = "card-subtitle text-body-secondary mb-1",
    span(class(block)[1]),
    block_subtitle_id(board, id),
    tags$sup(
      tooltip(
        icon("info-circle"),
        p(
          icon("lightbulb"),
          "How to use this block?",
        ),
        p(info$description, ".")
      )
    )
  )
}

#' @keywords internal
block_card_content <- function(block, id, blk_id, ns) {
  # Hide headers of accordion panels
  accordions <- accordion(
    id = ns(paste0("accordion-", id)),
    multiple = TRUE,
    class = "accordion-flush",
    open = "outputs",
    accordion_panel(
      icon = icon("sliders"),
      title = "Block inputs",
      value = "inputs",
      expr_ui(blk_id, block)
    ),
    accordion_panel(
      icon = icon("chart-simple"),
      title = "Block output(s)",
      value = "outputs",
      style = "max-width: 100%; overflow-x: auto;",
      block_ui(blk_id, block),
      div(id = ns(paste0("outputs-issues-wrapper-", id)))
    ),
    accordion_panel(
      title = "Block code",
      value = "code",
      icon = icon("code"),
    )
  )
  accordions <- htmltools::tagQuery(accordions)$find(
    ".accordion-header"
  )$addAttrs(style = "display: none;")$reset()$find(".accordion-item")$addAttrs(
    style = "border: none;"
  )$allTags()

  tagList(
    div(id = ns(sprintf("errors-block-%s", id))),
    accordions
  )
}

#' @keywords internal
block_card_toggles <- function(id, ns) {
  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns(sprintf("collapse-blk-section-%s", id)),
    status = "light",
    size = "sm",
    choices = setNames(
      c("inputs", "outputs", "code"),
      c(
        "<i class='fa fa-sliders'></i>",
        "<i class='fa fa-line-chart'></i>",
        "<i class='fa fa-code'></i>"
      )
    ),
    selected = "outputs"
  )

  section_toggles$attribs$class <- trimws(gsub(
    "form-group",
    "ms-auto",
    section_toggles$attribs$class
  ))

  section_toggles
}

block_card_dropdown <- function(id, info, ns) {
  dropdown_button(
    class = "float-end",
    icon = icon("ellipsis-vertical", class = "text-dark"),
    size = "sm",
    dropdown_header("BLOCK ACTIONS"),
    dropdown_action_button(
      ns(sprintf("append-%s", id)),
      "Append block",
      icon = icon("plus")
    ),
    dropdown_action_button(
      ns(sprintf("delete-%s", id)),
      "Delete block",
      icon = icon("trash"),
      class = "text-danger"
    ),
    dropdown_divider(),
    div(
      class = "text-muted d-flex justify-content-between",
      p("Package: "),
      p(info$package)
    ),
    div(
      class = "text-muted d-flex justify-content-between",
      p("Type: "),
      p(info$category)
    ),
    div(
      class = "text-muted d-flex justify-content-between",
      p("ID: "),
      p(id)
    )
  )
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
