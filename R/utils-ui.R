#' Create a Bootstrap Off-Canvas Element
#'
#' Creates an off-canvas element, which is a sliding panel that appears from the
#' edges of the viewport. This is a wrapper for Bootstrap's off-canvas component.
#'
#' @param id Character string. The ID of the off-canvas element. This ID is used to
#'   show/hide the element via JavaScript.
#' @param title Character string. The title to display in the header of the
#'   off-canvas element.
#' @param ... Additional UI elements to include in the body of the off-canvas
#'   element.
#' @param width Character string. Bootstrap width class to apply to the off-canvas
#'   element. Defaults to "w-100" (100% width). Common values include "w-75",
#'   "w-50", etc.
#' @param position Character string. The position from which the off-canvas element
#'   should appear. Must be one of "start" (left), "end" (right), "top", or
#'   "bottom". Defaults to "start".
#'
#' @return A HTML tag object representing the off-canvas element.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'
#'   ui <- page_fillable(
#'     actionButton(
#'      "toggle",
#'      "Toggle offcanvas",
#'      `data-bs-toggle` = "offcanvas",
#'      `data-bs-target` = "#demo",
#'      `aria-controls` = "demo"
#'     ),
#'     off_canvas(
#'       id = "demo",
#'       title = "Settings",
#'       position = "end",
#'       sliderInput("n", "Number", 1, 100, 50)
#'     )
#'   )
#'
#'   server <- function(input, output) {}
#'
#'   shinyApp(ui, server)
#' }
#'
#' @seealso \url{https://getbootstrap.com/docs/5.0/components/offcanvas/}
#' @seealso \url{https://getbootstrap.com/docs/5.0/utilities/sizing/}
#'
#' @export
off_canvas <- function(
  id,
  title,
  ...,
  width = "w-25",
  position = c("start", "top", "bottom", "end")
) {
  position <- match.arg(position)
  label <- rand_names()
  tags$div(
    class = sprintf("offcanvas offcanvas-%s %s", position, width),
    tabindex = "-1",
    id = id,
    `aria-labelledby` = label,
    `data-bs-scroll` = "true",
    tags$div(
      class = "offcanvas-header",
      tags$h5(class = "offcanvas-title", id = label, title),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    tags$div(class = "offcanvas-body", ...)
  )
}

#' Create a Bootstrap dropdown
#'
#' Creates a dropdown menu.
#'
#' @param ... Content.
#' @param icon Icon.
#' @param class Additional CSS classes for the button.
#'
#' @return A HTML tag object representing the dropdown element.
#' @export
dropdown_button <- function(..., icon, class = NULL) {
  tagList(
    tags$button(
      class = if (!is.null(class)) {
        paste("btn btn-link", class)
      } else {
        "btn btn-link btn-dark"
      },
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      icon
    ),
    tags$ul(
      class = "dropdown-menu text-body-secondary p-2",
      ...
    )
  )
}

#' @keywords internal
dropdown_divider <- function() {
  tags$li(tags$hr(class = "dropdown-divider"))
}

#' @keywords internal
dropdown_action_button <- function(id, label, icon = NULL, ...) {
  tags$li(
    actionLink(
      inputId = id,
      label = label,
      icon = icon,
      class = "dropdown-item",
      ...
    )
  )
}

#' @keywords internal
dropdown_header <- function(label) {
  tags$li(tags$h6(class = "dropdown-header", label))
}

#' Create a Bootstrap collapse container.
#'
#' Creates a collapsible element.
#'
#' @param id Unique id.
#' @param ... Content.
#'
#' @return A HTML tag object representing the dropdown element.
#' @keywords internal
collapse_container <- function(id, ...) {
  tags$div(class = "collapse", id = id, ...)
}

#' Get metadata for blocks
#'
#' @param blocks Blocks passed as `blocks` or `block` object
#' @rdname meta
#' @export
blks_metadata <- function(blocks) {
  default_name <- function(x) {
    gsub("_", " ", class(x)[1L])
  }

  if (is_block(blocks)) {
    id <- registry_id_from_block(blocks)
  } else if (is_blocks(blocks)) {
    id <- lapply(blocks, registry_id_from_block)
  } else {
    blockr_abort("Unsupported input type for `blocks`.")
  }

  if (any(lengths(id) == 0L)) {
    cat <- default_category()

    res <- data.frame(
      id = id[lengths(id) == 0L],
      name = chr_ply(blocks[lengths(id) == 0L], default_name),
      description = "not available",
      category = cat,
      icon = default_icon(cat),
      package = "local",
      color = blk_color(cat)
    )

    if (is_blocks(blocks)) {
      rownames(res) <- names(blocks)[lengths(id) == 0L]
    }
  } else {
    res <- NULL
  }

  if (any(lengths(id) > 0L)) {
    reg <- block_metadata(id[lengths(id) > 0L])
    reg <- cbind(reg, color = blk_color(reg$category))

    if (is_blocks(blocks)) {
      rownames(reg) <- names(blocks)[lengths(id) > 0L]
    }

    res <- rbind(res, reg)

    if (is_blocks(blocks)) {
      res <- res[names(blocks), ]
    }
  }

  res
}

#' Get block color based on category
#'
#' @param category Block category
#' @rdname meta
#' @export
blk_color <- function(category) {
  # Okabe-Ito colorblind-friendly palette
  # See: https://jfly.uni-koeln.de/color/
  switch(
    category,
    input = "#0072B2", # Blue
    transform = "#009E73", # Bluish green
    structured = "#56B4E9", # Sky blue
    plot = "#E69F00", # Orange
    table = "#CC79A7", # Reddish purple/pink
    model = "#F0E442", # Yellow (includes AI/ML)
    output = "#D55E00", # Vermilion
    utility = "#CCCCCC", # Light gray
    "#999999" # Medium gray (uncategorized)
  )
}
