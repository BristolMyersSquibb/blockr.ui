#' Shiny inputs in the design system's controls
#'
#' The shared controls for a block whose UI is rendered in R: each renders
#' the control's markup, with its label, and a Shiny input binding (in
#' `blockr-inputs.js`, loaded by [controls_dep()]) mounts the JS control on
#' it and reports its value as `input[[inputId]]`.
#'
#' * `select_input()` is `Blockr.Select`, single or multi with tags, as a
#'   bordered 42px field. A single select without a `placeholder` takes the
#'   first choice when `selected` is `NULL`, as [shiny::selectInput()] does;
#'   with one, it shows the placeholder until something is picked, and its
#'   value is `""`. A multi select's value is a character vector, or `NULL`
#'   when nothing is picked.
#' * `text_input()` and `number_input()` commit on Enter or blur: the
#'   "Enter" button is armed while the field differs from its value, and
#'   Escape reverts. The input changes only on a commit, never while typing.
#'   A number's value is numeric, `NA` when the field is empty.
#' * `checkbox_input()` is `Blockr.checkbox`: on or off, the label naming
#'   the "on" state.
#' * `segmented_input()` is `Blockr.segmented`, for two or three fixed short
#'   values.
#'
#' Each field comes in the tray's field grid class
#' (`.blockr-settings__field`) at the size the design system gives it:
#' numbers and checkboxes take one column, selects, text and segmented
#' controls two, a multi select the whole row.
#'
#' The `update_*()` functions change a control from the server. The update
#' is not sent back as a new input value: `input[[inputId]]` keeps the last
#' value the user gave until they change it again. Echoing a push loops as
#' soon as two pushes are in flight, so the server keeps track of what it
#' set. Send `selected` with new `choices` when the pick matters; without
#' it, the pick is kept while the new list still has it.
#'
#' @param inputId The input's id.
#' @param label The field label (12px, muted, above the control), or `NULL`
#'   for none. A checkbox's label is the text after its box.
#' @param choices The values to choose from, as a character vector. Names,
#'   where given, are shown muted after the value, as a column's label is.
#'   For `segmented_input()`, names are the segments' text.
#' @param selected The initial pick: one value, or several for a multi
#'   select.
#' @param multiple Pick several, shown as tags.
#' @param placeholder Text shown while the field is empty.
#' @param value The initial value.
#' @param session The Shiny session.
#'
#' @return The field, a [htmltools::tag] carrying [controls_dep()]. The
#'   `update_*()` functions return nothing.
#'
#' @examples
#' select_input("col", "Column", c("mpg", "cyl", disp = "Displacement"))
#' select_input("cols", "Columns", c("mpg", "cyl", "disp"),
#'              selected = "mpg", multiple = TRUE)
#' text_input("name", "Name", placeholder = "data.csv")
#' number_input("n", "Rows", value = 10)
#' checkbox_input("header", "First row is a header", value = TRUE)
#' segmented_input("from", "From", c(First = "head", Last = "tail"))
#'
#' @name inputs
NULL

#' @rdname inputs
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

#' @rdname inputs
#' @export
update_select_input <- function(inputId, choices = NULL, selected = NULL,
                                label = NULL,
                                session = shiny::getDefaultReactiveDomain()) {
  send_update(session, inputId, list(
    choices = if (!is.null(choices)) select_options(choices),
    # A list, so one value still arrives as an array.
    selected = if (!is.null(selected)) as.list(as.character(selected)),
    label = label
  ))
}

#' @rdname inputs
#' @export
text_input <- function(inputId, label, value = "", placeholder = NULL) {

  stopifnot(is_string(inputId), is.character(value), length(value) == 1L)

  commit_field(inputId, label, "large", tags$input(
    id = inputId,
    type = "text",
    class = "blockr-text-input blockr-ui-text",
    value = value,
    placeholder = placeholder,
    autocomplete = "off"
  ))
}

#' @rdname inputs
#' @export
update_text_input <- function(inputId, value = NULL, label = NULL,
                              placeholder = NULL,
                              session = shiny::getDefaultReactiveDomain()) {
  send_update(session, inputId, list(
    value = value,
    label = label,
    placeholder = placeholder
  ))
}

#' @param min,max,step The number's bounds and the step of its arrow keys;
#'   `NA` for none.
#' @rdname inputs
#' @export
number_input <- function(inputId, label, value = NA, min = NA, max = NA,
                         step = NA, placeholder = NULL) {

  stopifnot(is_string(inputId), length(value) == 1L,
            is.na(value) || is.numeric(value))

  num <- function(x) if (!is.na(x)) format(x, scientific = FALSE)

  commit_field(inputId, label, "small", tags$input(
    id = inputId,
    type = "number",
    class = "blockr-text-input blockr-ui-number",
    value = num(value),
    min = num(min),
    max = num(max),
    step = num(step),
    placeholder = placeholder,
    autocomplete = "off"
  ))
}

#' @rdname inputs
#' @export
update_number_input <- function(inputId, value = NULL, label = NULL,
                                session = shiny::getDefaultReactiveDomain()) {
  send_update(session, inputId, list(
    # An empty field, where an NA would reach the page as the text "NA".
    value = if (!is.null(value)) {
      if (is.na(value)) "" else format(value, scientific = FALSE)
    },
    label = label
  ))
}

#' @rdname inputs
#' @export
checkbox_input <- function(inputId, label, value = FALSE) {
  input_field("small", checkbox_tag(inputId, label, value))
}

#' @rdname inputs
#' @export
update_checkbox_input <- function(inputId, value = NULL, label = NULL,
                                  session = shiny::getDefaultReactiveDomain()) {
  send_update(session, inputId, list(value = value, label = label))
}

#' @param size A segmented control's height: `"m"` (42px) in the field grid,
#'   `"xs"` (26px) inside a row or a bar.
#' @rdname inputs
#' @export
segmented_input <- function(inputId, label, choices, selected = NULL,
                            size = c("m", "xs")) {

  size <- match.arg(size)
  stopifnot(is_string(inputId))

  if (!length(choices) %in% 2:3) {
    stop("A segmented control holds two or three values; use ",
         "select_input() for more.", call. = FALSE)
  }

  values <- as.character(unname(choices))
  labels <- names(choices) %||% values
  labels[!nzchar(labels)] <- values[!nzchar(labels)]

  selected <- if (is.null(selected)) values[1L] else as.character(selected)
  stopifnot(length(selected) == 1L, selected %in% values)

  input_field(
    "large",
    field_label(label, inputId),
    tags$div(
      id = inputId,
      class = "blockr-ui-segmented",
      `data-choices` = to_json(unname(Map(
        function(v, l) list(value = v, label = l), values, labels
      ))),
      `data-selected` = to_json(selected),
      `data-size` = if (size == "xs") "xs"
    )
  )
}

#' @rdname inputs
#' @export
update_segmented_input <- function(inputId, selected = NULL, label = NULL,
                                   session = shiny::getDefaultReactiveDomain()) {
  send_update(session, inputId, list(selected = selected, label = label))
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
#'   one section. For `tray_section()`, its fields: the inputs of
#'   [select_input()] and its siblings, or any tag, which is put in a grid
#'   cell of two columns.
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
#'     checkbox_input("header", "First row is a header", TRUE)
#'   ),
#'   tray_section(
#'     "Skip rows",
#'     number_input("skip", "Rows", 0),
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
    if (inherits(f, "shiny.tag") &&
          htmltools::tagHasAttribute(f, "class") &&
          grepl("blockr-settings__field", htmltools::tagGetAttribute(f, "class"),
                fixed = TRUE)) {
      f
    } else {
      tags$div(class = "blockr-settings__field", f)
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

commit_field <- function(inputId, label, size, input) {
  input_field(
    size,
    field_label(label, inputId, `for` = inputId),
    # The Enter button goes inside the field's right edge.
    tags$div(class = "blockr-commit-field", input)
  )
}

field_label <- function(label, inputId, ...) {
  if (is.null(label)) {
    return(NULL)
  }
  tags$label(id = label_id(inputId), class = "blockr-label", ..., label)
}

label_id <- function(inputId) paste0(inputId, "-label")

# The markup of Blockr.checkbox, with the same check.
checkbox_tag <- function(inputId, label, value) {

  stopifnot(is_string(inputId), isTRUE(value) || isFALSE(value))

  tags$label(
    class = "blockr-checkbox",
    tags$input(
      id = inputId,
      type = "checkbox",
      class = "blockr-ui-checkbox",
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
    function(v, l) if (is.na(l) || !nzchar(l) || l == v) v else
      list(value = v, label = l),
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
