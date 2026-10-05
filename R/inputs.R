#' Select input in the design system
#'
#' The design system's select, `Blockr.Select`, for a block whose UI is
#' rendered in R: single, or multi with tags, as a bordered 42px field. It
#' renders the select's markup, with its label, and a Shiny input binding (in
#' `blockr-inputs.js`, loaded by [controls_dep()]) mounts `Blockr.Select` on
#' it and reports its value as `input[[inputId]]`.
#'
#' A single select without a `placeholder` takes the first choice when
#' `selected` is `NULL`, as [shiny::selectInput()] does; with one, it shows
#' the placeholder until something is picked, and its value is `""`. A multi
#' select's value is a character vector, or `NULL` when nothing is picked.
#'
#' The field comes in the tray's field grid class
#' (`.blockr-settings__field`): a select takes two columns, a multi select
#' the whole row. Text, numbers and checkboxes need no control of their own:
#' use Shiny's, with `updateOn = "blur"` for [shiny::textInput()] and
#' [shiny::numericInput()], so their value changes on Enter or blur only.
#'
#' To change the select from the server, `update_select_input()` takes the
#' arguments of [shiny::updateSelectInput()]. As with Shiny's own inputs, the
#' select then sends its value back, so `input[[inputId]]` follows what it
#' shows. That includes a pick it settles on by itself: new `choices` keep
#' the pick while the list still has it, and otherwise fall back to the first
#' choice, or to the placeholder. A block that copies the input into its
#' state and pushes the state back pushes only when the two differ, so the
#' value sent back does not loop.
#'
#' @param inputId The input's id.
#' @param label The field label (12px, muted, above the control), or `NULL`
#'   for none.
#' @param choices The values to choose from, as a character vector. Names,
#'   where given, are shown muted after the value, as a column's label is.
#' @param selected The initial pick: one value, or several for a multi
#'   select.
#' @param multiple Pick several, shown as tags.
#' @param placeholder Text shown while the field is empty.
#' @param session The Shiny session.
#'
#' @return The field, a [htmltools::tag] carrying [controls_dep()].
#'   `update_select_input()` returns nothing.
#'
#' @examples
#' select_input("col", "Column", c("mpg", "cyl", disp = "Displacement"))
#' select_input("cols", "Columns", c("mpg", "cyl", "disp"),
#'              selected = "mpg", multiple = TRUE)
#'
#' @export
select_input <- function(inputId, label, choices, selected = NULL,
                         multiple = FALSE, placeholder = NULL) {

  stopifnot(
    is_string(inputId),
    isTRUE(multiple) || isFALSE(multiple),
    is.null(placeholder) || is.character(placeholder)
  )

  selected <- if (!is.null(selected)) as.character(selected)

  if (!multiple && length(selected) > 1L) {
    stop("A single select takes one `selected` value.", call. = FALSE)
  }

  input_field(
    if (multiple) "full" else "large",
    field_label(label, inputId),
    tags$div(
      id = inputId,
      class = "blockr-ui-select",
      role = "group",
      `aria-labelledby` = if (!is.null(label)) label_id(inputId),
      `data-multiple` = if (multiple) "true" else "false",
      `data-options` = to_json(select_options(choices)),
      `data-selected` = to_json(as.list(selected)),
      `data-placeholder` = placeholder
    )
  )
}

#' @rdname select_input
#' @export
update_select_input <- function(session = shiny::getDefaultReactiveDomain(),
                                inputId, label = NULL, choices = NULL,
                                selected = NULL) {
  send_update(session, inputId, list(
    choices = if (!is.null(choices)) select_options(choices),
    # A list, so one value still arrives as an array.
    selected = if (!is.null(selected)) as.list(as.character(selected)),
    label = label
  ))
}

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
#' @param inputId The gear's id. The tray's is `<inputId>_tray`.
#' @param ... For `gear_tray()`, `tray_section()`s, or fields for a tray of
#'   one section. For `tray_section()`, its fields: [select_input()],
#'   Shiny's own inputs, or any other tag. Each goes in a grid cell at the
#'   size the design system gives it: a number or a checkbox takes one
#'   column, a multi select the whole row, anything else two.
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
#'     select_input("sep", "Separator", c(",", ";", "\t")),
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
        `data-blockr-tooltip` = "Settings"
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

field_label <- function(label, inputId, ...) {
  if (is.null(label)) {
    return(NULL)
  }
  tags$label(id = label_id(inputId), class = "blockr-label", ..., label)
}

label_id <- function(inputId) paste0(inputId, "-label")

# The grid size for a field that is not one of ours, read off the inputs in
# it: one number or one checkbox takes one column, a multi select the whole
# row, anything else two.
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

# The markup of Blockr.checkbox, for a section's toggle. Shiny's own
# checkbox binding reports it.
checkbox_tag <- function(inputId, label, value) {

  stopifnot(is_string(inputId), isTRUE(value) || isFALSE(value))

  tags$label(
    class = "blockr-checkbox",
    tags$input(
      id = inputId,
      type = "checkbox",
      checked = if (value) NA
    ),
    tags$span(
      class = "blockr-checkbox__box",
      htmltools::HTML(paste0(
        '<svg width="10" height="10" viewBox="0 0 16 16" fill="currentColor">',
        '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-',
        '3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/>',
        "</svg>"
      ))
    ),
    tags$span(class = "blockr-checkbox__label", label)
  )
}

# Blockr.Select's options: a bare value, or a value with its label.
select_options <- function(choices) {

  values <- as.character(unname(choices))
  labels <- names(choices)

  if (is.null(labels)) {
    return(as.list(values))
  }

  unname(Map(
    function(v, l) {
      if (is.na(l) || !nzchar(l) || l == v) v else list(value = v, label = l)
    },
    values, as.character(labels)
  ))
}

send_update <- function(session, inputId, message) {
  stopifnot(is_string(inputId))
  session$sendInputMessage(inputId, Filter(Negate(is.null), message))
  invisible()
}

to_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

# What a select reports: a character vector, NULL for no pick.
select_value <- function(x, shinysession, name) {
  x <- unlist(x)
  if (!length(x)) NULL else as.character(x)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}
