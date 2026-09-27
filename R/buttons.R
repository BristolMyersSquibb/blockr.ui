#' Buttons
#'
#' The design system's buttons: main, secondary, quiet and destructive, at
#' 42, 30 or 26px (the `.blockr-btn` classes of `blockr-buttons.css`, loaded
#' by [controls_dep()]).
#'
#' `blockr_button()` is a Shiny action button: `input[[inputId]]` counts its
#' clicks, as for [shiny::actionButton()], and [shiny::updateActionButton()]
#' changes its label, icon and disabled state. `blockr_download_button()` is
#' the same button for a [shiny::downloadHandler()].
#'
#' A plain [shiny::actionButton()] or [shiny::downloadButton()] needs neither:
#' the theme layer ([theme_dep()]) maps Bootstrap's button classes onto the
#' same kinds, so `btn-default` draws as secondary, `btn-primary` as main,
#' `btn-link` as quiet, `btn-danger` as destructive, and `btn-sm` at 30px.
#'
#' @param inputId,outputId The Shiny id.
#' @param label The button's text, or a tag.
#' @param kind `"secondary"` for everything with a frame; `"main"`, the
#'   accent tint, at most once per view; `"quiet"`, text only, for a
#'   low-weight action; `"danger"` only to confirm a removal.
#' @param size `"m"` (42px) beside inputs, `"s"` (30px) in toolbars and
#'   dialogs, `"xs"` (26px) in a header strip next to the gear.
#' @param icon An icon before the label, as a tag or [htmltools::HTML()].
#' @param disabled Start disabled.
#' @param ... Further attributes for the button.
#'
#' @return A [htmltools::tag] carrying [controls_dep()].
#'
#' @examples
#' blockr_button("write", "Write file", kind = "main")
#' blockr_button("clear", "Clear", kind = "quiet", size = "xs")
#' blockr_download_button("csv", "Download", size = "s")
#'
#' @export
blockr_button <- function(inputId, label, kind = c("secondary", "main",
                                                   "quiet", "danger"),
                          size = c("m", "s", "xs"), icon = NULL,
                          disabled = FALSE, ...) {

  stopifnot(is_string(inputId), isTRUE(disabled) || isFALSE(disabled))

  with_controls(
    tags$button(
      id = inputId,
      type = "button",
      class = button_class(kind, size),
      class = "action-button",
      disabled = if (disabled) NA,
      ...,
      button_content(icon, label)
    )
  )
}

#' @rdname blockr_button
#' @export
blockr_download_button <- function(outputId, label = "Download",
                                   kind = c("secondary", "main", "quiet",
                                            "danger"),
                                   size = c("m", "s", "xs"), icon = NULL,
                                   ...) {

  stopifnot(is_string(outputId))

  # Shiny's own downloadButton() markup: disabled until the handler binds.
  with_controls(
    tags$a(
      id = outputId,
      class = button_class(kind, size),
      class = "shiny-download-link disabled",
      href = "",
      target = "_blank",
      download = NA,
      `aria-disabled` = "true",
      tabindex = "-1",
      ...,
      button_content(icon, label)
    )
  )
}

button_class <- function(kind, size) {

  kind <- match.arg(kind, c("secondary", "main", "quiet", "danger"))
  size <- match.arg(size, c("m", "s", "xs"))

  paste0("blockr-btn blockr-btn--", kind, " blockr-btn--", size)
}

# The spans Shiny's action button binding updates in place.
button_content <- function(icon, label) {
  tagList(
    if (!is.null(icon)) tags$span(class = "action-icon", icon),
    tags$span(class = "action-label", label)
  )
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}
