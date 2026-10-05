#' Gear and settings tray
#'
#' The gear button and the tray it opens, for a block whose UI is rendered
#' in R. The gear is the last thing in the header row, after any `tools`;
#' the tray opens in flow under it, slides open and closed, and closes on
#' the gear or Escape. `Blockr.gearTray` (in `blockr-ui.js`) does this; a
#' Shiny input binding wires it up, so there is no inline script, and
#' `input[[inputId]]` is `TRUE` while the tray is open. A tray drawn again
#' (a `renderUI()`) keeps the state it had, for the session.
#'
#' The tray holds sections, each a title over a grid of fields. A tray with
#' one section shows no title. A section switched on by a checkbox
#' (`toggle`) keeps the checkbox before its title, and its fields show only
#' while it is checked; the checkbox is an input of its own,
#' `input[[toggle]]`.
#'
#' The fields are Shiny's own inputs, with `updateOn = "blur"` for
#' [shiny::textInput()] and [shiny::numericInput()], so their value changes
#' on Enter or blur only. An open [shiny::selectInput()] closes on Escape
#' before the tray does.
#'
#' @param inputId The gear's id. The tray's is `<inputId>_tray`.
#' @param ... For `gear_tray()`, `tray_section()`s, or fields for a tray of
#'   one section. For `tray_section()`, its fields: Shiny's inputs, or any
#'   other tag. Each goes in a grid cell at the size the design system gives
#'   it: a number or a checkbox takes one column, a multi select the whole
#'   row, anything else two.
#' @param tools Tool buttons ([tool_button()], an [action_menu()]) that
#'   stand left of the gear in the header row.
#' @param label The tray's accessible name.
#'
#' @return `gear_tray()`: an [htmltools::tagList()] of the header row and
#'   the tray, carrying [controls_dep()]. `tray_section()`: a section, for
#'   `gear_tray()` only.
#'
#' @examples
#' gear_tray(
#'   "gear",
#'   tray_section(
#'     "Format",
#'     shiny::selectInput("sep", "Separator", c(",", ";", "\t")),
#'     shiny::checkboxInput("header", "First row is a header", TRUE)
#'   ),
#'   tray_section(
#'     "Skip rows",
#'     shiny::numericInput("skip", "Rows", 0, updateOn = "blur"),
#'     toggle = "skip_on"
#'   )
#' )
#'
#' @export
gear_tray <- function(inputId, ..., tools = NULL, label = "Settings") {

  stopifnot(is_string(inputId), is_string(label))

  sections <- Filter(Negate(is.null), list(...))
  is_section <- vapply(sections, inherits, logical(1L), "blockr_tray_section")

  if (!length(sections)) {
    stop("A tray needs something in it; a block with no options has no ",
         "gear.", call. = FALSE)
  }

  if (!any(is_section)) {
    sections <- list(do.call(tray_section, c(list(NULL), sections)))
  } else if (!all(is_section)) {
    stop("Put every field of a tray with sections in a tray_section().",
         call. = FALSE)
  }

  # One section has no title, unless its title is the checkbox that turns
  # it on.
  if (length(sections) == 1L && is.null(sections[[1L]]$toggle)) {
    sections[[1L]]$title <- NULL
  }

  tray_id <- paste0(inputId, "_tray")

  with_controls(tagList(
    tags$div(
      class = "blockr-gear-header",
      tools,
      tags$button(
        id = inputId,
        type = "button",
        class = "blockr-gear-btn blockr-ui-gear",
        `aria-controls` = tray_id,
        `aria-expanded` = "false",
        `aria-label` = "Settings",
        `data-blockr-tooltip` = "Settings",
        small_icon("gear")
      )
    ),
    tags$div(
      id = tray_id,
      class = "blockr-settings blockr-settings--beak",
      role = "region",
      `aria-label` = label,
      lapply(sections, render_section)
    )
  ))
}

#' @param title The section title, or `NULL`. With `toggle`, the label of
#'   the checkbox that switches the section on.
#' @param toggle The id of a checkbox that switches the section on, or
#'   `NULL` for a section that is always on.
#' @param value Whether the `toggle` checkbox starts checked.
#' @rdname gear_tray
#' @export
tray_section <- function(title = NULL, ..., toggle = NULL, value = FALSE) {

  stopifnot(is.null(title) || is_string(title), is.null(toggle) ||
              is_string(toggle))

  if (!is.null(toggle) && is.null(title)) {
    stop("A section switched on by a checkbox needs a `title`, the ",
         "checkbox's label.", call. = FALSE)
  }

  structure(
    list(
      title = title,
      toggle = toggle,
      value = value,
      fields = rlang::list2(...)
    ),
    class = "blockr_tray_section"
  )
}

render_section <- function(x) {

  title <- if (!is.null(x$toggle)) {
    tags$div(
      class = "blockr-settings__title blockr-settings__title--toggle",
      checkbox_tag(x$toggle, x$title, x$value)
    )
  } else if (!is.null(x$title)) {
    tags$div(class = "blockr-settings__title", x$title)
  }

  fields <- lapply(Filter(Negate(is.null), x$fields), function(f) {
    cls <- if (inherits(f, "shiny.tag")) htmltools::tagGetAttribute(f, "class")
    if (!is.null(cls) && grepl("blockr-settings__field", cls, fixed = TRUE)) {
      f
    } else {
      input_field(field_size(f), f)
    }
  })

  tagList(title, tags$div(class = "blockr-settings__grid", fields))
}

# --- helpers -----------------------------------------------------------------

input_field <- function(size, ...) {
  with_controls(tags$div(
    class = "blockr-settings__field",
    class = switch(size, small = "blockr-settings__field--small",
                   full = "blockr-settings__field--full"),
    ...
  ))
}

# The grid size for a field, read off the inputs in it: one number or one
# checkbox takes one column, a multi select the whole row, anything else two.
field_size <- function(f) {

  if (!inherits(f, c("shiny.tag", "shiny.tag.list"))) {
    return("large")
  }

  # Wrapped, so a field that is a bare input is searched too.
  q <- htmltools::tagQuery(tags$div(f))
  attr_of <- function(sel, name) {
    lapply(q$find(sel)$selectedTags(), htmltools::tagGetAttribute, name)
  }

  types <- unlist(attr_of("input", "type"))
  multiple <- !vapply(attr_of("select", "multiple"), is.null, logical(1L))

  if (length(types) == 1L && types %in% c("number", "checkbox")) {
    "small"
  } else if (any(multiple)) {
    "full"
  } else {
    "large"
  }
}

# The markup of Blockr.checkbox, for a section's toggle, with its check from
# the same icon. Shiny's own checkbox binding reports it.
checkbox_tag <- function(inputId, label, value) {

  stopifnot(is_string(inputId), isTRUE(value) || isFALSE(value))

  tags$label(
    class = "blockr-checkbox",
    tags$input(
      id = inputId,
      type = "checkbox",
      checked = if (value) NA
    ),
    tags$span(class = "blockr-checkbox__box", small_icon("confirm")),
    tags$span(class = "blockr-checkbox__label", label)
  )
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}
