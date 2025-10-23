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
  attr(block[[1]], "uid") <- names(block)
  block <- block[[1]]
  stopifnot(is_block(block))
  block_card(x, block, edit_ui, ns = ns)
}

#' @keywords internal
block_card <- function(board, block, edit_ui, ns) {
  id <- block_uid(block)
  blk_id <- ns(paste0("block_", id))
  blk_info <- get_block_metadata(block)

  # Edit plugin
  if (!is.null(edit_ui)) {
    edit_ui <- edit_ui$ui(x, NS(blk_id, "edit_block"))
  }

  # if yellow color, use black text, otherwise white for contrasts
  bg_color <- blk_color(blk_info$category)
  icon_color <- if (bg_color == "#F0E442") "text-dark" else "text-white"

  card_tag <- tags$div(
    class = "card",
    width = "100%",
    id = ns(id),
    tags$div(
      class = "card-body",
      div(
        class = "d-flex align-items-stretch gap-3",
        # Icon element
        div(
          class = paste(
            "d-flex align-items-center justify-content-center",
            "flex-shrink-0 rounded-3 shadow-sm"
          ),
          style = sprintf(
            paste0(
              "background: %s;",
              "border: 1px solid rgba(255, 255, 255, 0.2);",
              "width: 60px;",
              "min-height: 100%%;",
              "position: relative;"
            ),
            bg_color
          ),
          div(
            class = icon_color,
            style = "filter: drop-shadow(0 1px 2px rgba(0, 0, 0, 0.3));",
            blk_icon(blk_info$category, class = "xl")
          )
        ),
        # Title section
        div(
          class = "d-flex flex-column justify-content-center flex-grow-1 min-height-0",
          block_card_title(board, block, id, blk_info, edit_ui, ns)
        )
      ),
      #edit_ui$block_summary,
      block_card_content(block, id, blk_id, ns)
    )
  )
  tagAppendAttributes(card_tag, class = "border border-0 shadow-none")
}

#' @keywords internal
block_card_title <- function(board, block, id, info, edit_ui, ns) {
  tags$div(
    class = "d-flex align-items-center justify-content-between w-100",
    # Left side: block name and subtitle
    div(
      class = "flex-grow-1 pe-3",
      div(
        class = "card-title mb-1",
        style = "line-height: 1.3;",
        edit_ui$block_name
      ),
      block_card_subtitle(board, block, id, info)
    ),
    # Right side: toggles and dropdown
    div(
      class = "d-flex align-items-center gap-2 flex-shrink-0",
      block_card_toggles(id, ns),
      block_card_dropdown(id, info, ns)
    )
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
    " | ",
    span("id:", id)
  )
}

#' @keywords internal
block_card_subtitle <- function(board, block, id, info) {
  div(
    class = "text-body-secondary small text-muted",
    style = "line-height: 1.2;",
    span(class(block)[1]),
    block_subtitle_id(board, id),
    tags$sup(
      class = "ms-1",
      tooltip(
        icon("info-circle", style = "color: #9ca3af; font-size: 0.75em;"),
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
    open = c("inputs", "outputs"),
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
    )
  )
  accordions <- htmltools::tagQuery(accordions)$find(
    ".accordion-header"
  )$addAttrs(style = "display: none;")$reset()$find(".accordion-item")$addAttrs(
    style = "border: none;"
  )$allTags()

  tagList(
    div(id = ns(sprintf("errors-block-%s", id)), class = "mt-4"),
    accordions
  )
}

#' @keywords internal
block_card_toggles <- function(id, ns) {
  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns(sprintf("collapse-blk-section-%s", id)),
    status = "light",
    size = "sm",
    choices = set_names(
      c("inputs", "outputs"),
      c(
        "<small>inputs</small>",
        "<small>outputs</small>"
      )
    ),
    individual = TRUE,
    selected = c("inputs", "outputs")
  )

  # Remove the ms-auto class
  section_toggles$attribs$class <- trimws(gsub(
    "form-group|ms-auto",
    "",
    section_toggles$attribs$class
  ))

  section_toggles
}

#' @keywords internal
block_card_dropdown <- function(id, info, ns) {
  # Create a custom dropdown without the default button styling (no bg on hover, ...)
  tags$div(
    class = "dropdown",
    tags$button(
      class = "btn btn-link p-1 border-0 bg-transparent text-muted",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      onmouseover = "this.classList.add('text-dark');",
      onmouseout = "this.classList.remove('text-dark');",
      icon("ellipsis-vertical")
    ),
    tags$ul(
      class = "dropdown-menu dropdown-menu-end shadow-sm rounded-3 border-1",
      style = "min-width: 250px;",
      # Actions header
      tags$li(
        tags$h6(
          class = "dropdown-header text-uppercase fw-semibold small text-secondary",
          style = "font-size: 0.75rem; letter-spacing: 0.5px;",
          "Block Actions"
        )
      ),
      # Block actions
      tags$li(
        tags$button(
          class = "dropdown-item action-button py-2 position-relative text-center",
          type = "button",
          id = ns(sprintf("append-%s", id)),
          style = "padding-left: 2.5rem;",
          tags$span(
            class = "position-absolute start-0 top-50 translate-middle-y ms-3",
            icon("plus", class = "text-success")
          ),
          "Append block"
        )
      ),
      tags$li(
        tags$button(
          class = "dropdown-item action-button py-2 position-relative text-center text-danger",
          type = "button",
          id = ns(sprintf("delete-%s", id)),
          style = "padding-left: 2.5rem;",
          tags$span(
            class = "position-absolute start-0 top-50 translate-middle-y ms-3",
            icon("trash")
          ),
          "Delete block"
        )
      ),
      tags$li(tags$hr(class = "dropdown-divider my-2")),
      # Block details header
      tags$li(
        tags$h6(
          class = "dropdown-header text-uppercase fw-semibold small text-secondary",
          style = "font-size: 0.75rem; letter-spacing: 0.5px;",
          "Block Details"
        )
      ),
      # Block details content
      tags$li(
        tags$div(
          class = "px-3 py-1",
          # Package
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$span("Package", class = "text-muted small"),
            tags$span(info$package, class = "small fw-medium")
          ),
          # Type
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$span("Type", class = "text-muted small"),
            tags$span(info$category, class = "small fw-medium")
          ),
          # ID
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-0",
            tags$span("ID", class = "text-muted small"),
            tags$span(id, class = "small fw-medium font-monospace")
          )
        )
      )
    )
  )
}

#' @keywords internal
remove_block_panels <- function(proxy, ids, panels) {
  stopifnot(is.character(ids))
  # Only remove panels that are in the dock
  in_dock <- which(ids %in% get_block_panels(names(panels)))
  if (!length(in_dock)) {
    return(NULL)
  }
  ids <- ids[in_dock]

  lapply(ids, function(id) {
    remove_panel(proxy, paste0("block-", id))
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

add_block_panel <- function(proxy, id, panels) {
  add_panel(
    proxy,
    panel = dockViewR::panel(
      id = sprintf("block-%s", id),
      title = id,
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
      remove = new_remove_tab_plugin(enable = TRUE, mode = "manual")
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
#' @param proxy Dock proxy.
#' @rdname block-panel
create_or_show_block_panel <- function(proxy, id, parent) {
  session <- proxy$session
  ns <- session$ns

  # Extract block panels
  all_panels <- get_panels_ids(proxy)
  block_panels <- get_block_panels(all_panels)

  # If the block panel is already there,
  # just select it.
  if (parent$selected_block %in% block_panels) {
    # Only select panel
    dockViewR::select_panel(
      proxy,
      sprintf("block-%s", parent$selected_block)
    )
  } else {
    # Or add it and move the block UI from offcanvas to the panel container
    add_block_panel(proxy, id, all_panels)
    show_block_panel(id, session)
  }
}

#' Hide a block panel
#'
#' Move block from panel to offcanvas-body.
#'
#' @rdname block-panel
hide_block_panel <- function(proxy, id) {
  session <- proxy$session
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
  remove_panel(proxy, id)
}

#' Get block info in registry
#'
#' @param x Block object
#' @keywords internal
get_block_metadata <- function(x) {
  stopifnot(is_block(x))

  ctor <- attr(x, "ctor")
  ctor <- attr(ctor, "fun")

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
