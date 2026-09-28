#' Action menu
#'
#' A menu of actions opened by a button: downloads, Rename, Remove. A row does
#' one thing and the menu closes. That is the difference from a select, which
#' sets a value: nothing here is remembered, and there is no current pick.
#'
#' The rows are ordinary Shiny markup, so a [shiny::downloadLink()] or an
#' [shiny::actionLink()] works as it is; [menu_item()] only dresses it as a
#' row. `Blockr.actionMenu` (in `blockr-ui.js`, loaded by [controls_dep()])
#' opens the list on a click on the trigger, places it on the page body with
#' `Blockr.place` so no panel's overflow clips it, and closes it again on a
#' pick, Escape, Tab or a click outside.
#'
#' @param trigger The button that opens the menu, typically a
#'   [tool_button()].
#' @param ... Rows: [menu_item()]s and [menu_section()] titles, in order.
#'   `NULL`s are dropped, so a row can be conditional.
#' @param align `"end"` lines the menu up with the trigger's right edge, for a
#'   trigger in a block's header row, where a menu opening to the right would
#'   leave the block; `"start"` with its left edge.
#'
#' @return A [htmltools::tag] carrying [controls_dep()].
#'
#' @examples
#' action_menu(
#'   tool_button(
#'     htmltools::HTML("&darr;"),
#'     "Download"
#'   ),
#'   menu_section("This patient"),
#'   menu_item(shiny::downloadLink("dl_pptx", "PowerPoint"), meta = ".pptx"),
#'   menu_item(shiny::downloadLink("dl_html", "Web page"), meta = ".html"),
#'   menu_section("This block"),
#'   menu_item(shiny::actionLink("remove", "Remove"), danger = TRUE)
#' )
#'
#' @export
action_menu <- function(trigger, ..., align = c("end", "start")) {

  align <- match.arg(align)

  if (!inherits(trigger, "shiny.tag")) {
    stop("`trigger` must be an HTML tag, such as a tool_button().",
         call. = FALSE)
  }

  rows <- Filter(Negate(is.null), list(...))

  if (!length(rows)) {
    stop("An action menu needs at least one row.", call. = FALSE)
  }

  ok <- vapply(rows, inherits, logical(1L), "blockr_menu_row")

  if (!all(ok)) {
    stop("Every row must come from menu_item() or menu_section().",
         call. = FALSE)
  }

  trigger <- htmltools::tagAppendAttributes(
    trigger,
    class = "blockr-action-menu__trigger",
    `aria-haspopup` = "menu",
    `aria-expanded` = "false"
  )

  menu <- tags$span(
    class = "blockr-action-menu",
    `data-align` = align,
    trigger,
    tags$div(
      class = "blockr-menu",
      role = "menu",
      tabindex = "-1",
      hidden = NA,
      lapply(rows, strip_row_class)
    )
  )

  htmltools::attachDependencies(
    menu,
    htmltools::findDependencies(controls_dep()),
    append = TRUE
  )
}

#' @param x The row's element: a [shiny::downloadLink()], an
#'   [shiny::actionLink()], or any `<a>` or `<button>`. Its text is the row's
#'   label.
#' @param meta Text after the label, muted and right-aligned: the format of a
#'   download (".pptx"), a block ID, or a keyboard shortcut from
#'   [shortcut()].
#' @param icon An icon before the label, as a tag or [htmltools::HTML()].
#' @param danger Draw the row in red. Only for a removal.
#' @param disabled `NULL` for a usable row, or the reason it is not usable:
#'   the row stays in the list, greyed, with the reason as its tooltip.
#'
#' @rdname action_menu
#' @export
menu_item <- function(x, meta = NULL, icon = NULL, danger = FALSE,
                      disabled = NULL) {

  if (!inherits(x, "shiny.tag") || !x$name %in% c("a", "button")) {
    stop("A menu item wraps an <a> or a <button>, such as a downloadLink() ",
         "or an actionLink().", call. = FALSE)
  }

  stopifnot(
    is.null(meta) || (is.character(meta) && length(meta) == 1L) ||
      inherits(meta, "shiny.tag"),
    isTRUE(danger) || isFALSE(danger),
    is.null(disabled) || (is.character(disabled) && length(disabled) == 1L)
  )

  x$children <- list(
    if (!is.null(icon)) tags$span(class = "blockr-menu__icon", icon),
    tags$span(class = "blockr-menu__label", x$children),
    if (!is.null(meta)) tags$span(class = "blockr-menu__meta", meta)
  )

  if (identical(x$name, "button") && is.null(x$attribs$type)) {
    x <- htmltools::tagAppendAttributes(x, type = "button")
  }

  x <- htmltools::tagAppendAttributes(
    x,
    class = "blockr-menu__item",
    class = if (danger) "blockr-menu__item--danger",
    # Its own mark, not aria-disabled alone: Shiny sets and clears
    # aria-disabled on a downloadLink() as its handler binds, which would
    # enable a row the author disabled.
    class = if (!is.null(disabled)) "blockr-menu__item--disabled"
  )

  # Replaced, not appended: a downloadLink() arrives with a tabindex of its
  # own, and two values in one attribute mean nothing.
  x$attribs$tabindex <- NULL
  x$attribs$role <- NULL
  x <- htmltools::tagAppendAttributes(
    x,
    role = "menuitem",
    tabindex = "-1",
    `data-blockr-tooltip` = disabled
  )

  if (!is.null(disabled)) {
    x$attribs[["aria-disabled"]] <- NULL
    x <- htmltools::tagAppendAttributes(x, `aria-disabled` = "true")
  }

  menu_row(x)
}

#' @param title A group title: the rows after it, up to the next title or
#'   divider, belong under it.
#'
#' @rdname action_menu
#' @export
menu_section <- function(title) {
  stopifnot(is.character(title), length(title) == 1L)
  menu_row(tags$div(class = "blockr-menu__title", role = "presentation", title))
}

#' Tool button
#'
#' The design system's icon button: 26px, bare, the icon muted until hover.
#' It has no text, so it carries its name as a tooltip (the light card of
#' `Blockr.tooltip`) and as its accessible name.
#'
#' @param icon The icon, as a tag or [htmltools::HTML()].
#' @param tooltip What the button does, in a few words ("Download").
#' @param ... Further attributes, such as `id`.
#'
#' @return A `<button>` [htmltools::tag] carrying [controls_dep()].
#'
#' @examples
#' tool_button(htmltools::HTML("&darr;"), "Download")
#'
#' @export
tool_button <- function(icon, tooltip, ...) {

  stopifnot(is.character(tooltip), length(tooltip) == 1L, nzchar(tooltip))

  htmltools::attachDependencies(
    tags$button(
      type = "button",
      class = "blockr-tool",
      `aria-label` = tooltip,
      `data-blockr-tooltip` = tooltip,
      ...,
      icon
    ),
    htmltools::findDependencies(controls_dep()),
    append = TRUE
  )
}

# Marks a row so action_menu() can tell it from stray markup; the class is
# dropped again before the tag is rendered.
menu_row <- function(x) {
  class(x) <- c("blockr_menu_row", class(x))
  x
}

strip_row_class <- function(x) {
  class(x) <- setdiff(class(x), "blockr_menu_row")
  x
}
