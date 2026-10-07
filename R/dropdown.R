#' Dropdown
#'
#' A panel that opens under a button and stays open while it is worked in: a
#' search field, a list edited in place, a checkbox. A click outside, Escape
#' or opening another dropdown closes it; a click inside does not. That is
#' the difference from [action_menu()], whose rows each do one thing and
#' close the menu.
#'
#' The panel holds ordinary Shiny markup, inputs and outputs included. On a
#' click on the toggle, `Blockr.dropdown` (in `blockr-ui.js`, loaded by
#' [controls_dep()]) opens it and, while it is open, moves it to the page body
#' and places it with `Blockr.place`, so no panel's overflow clips it; it goes
#' back beside the toggle when it closes. Style the panel through its own
#' classes, as it is not inside the dropdown's ancestors while open.
#'
#' @param toggle The button that opens the panel, as an [htmltools::tag].
#' @param ... The panel's content.
#' @param align Which edge of the toggle the panel lines up with: `"start"`,
#'   the left, or `"end"`, the right, for a toggle at the end of a bar, where
#'   a panel opening to the right would leave the window.
#'
#' @return A [htmltools::tag] carrying [controls_dep()].
#'
#' @examples
#' dropdown(
#'   htmltools::tags$button(type = "button", class = "btn", "Views"),
#'   shiny::textInput("search", "Search"),
#'   shiny::checkboxInput("all", "Show all views")
#' )
#'
#' @export
dropdown <- function(toggle, ..., align = c("start", "end")) {

  align <- match.arg(align)

  if (!inherits(toggle, "shiny.tag")) {
    stop("`toggle` must be an HTML tag, such as a <button>.", call. = FALSE)
  }

  toggle <- htmltools::tagAppendAttributes(
    toggle,
    class = "blockr-dropdown__toggle",
    `aria-expanded` = "false"
  )

  with_controls(
    tags$div(
      class = "blockr-dropdown",
      `data-align` = align,
      toggle,
      tags$div(class = "blockr-dropdown__panel blockr-menu", ...)
    )
  )
}
