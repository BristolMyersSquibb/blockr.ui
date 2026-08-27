#' Shared blockr stylesheet
#'
#' The `--blockr-*` design tokens and the Bootstrap theme layer that the
#' blockr packages style themselves from, carried as two stylesheets:
#' `blockr-tokens.css` defines the vocabulary in a `:root` block - the
#' grey and blue ramps, font sizes and weights, radii, control heights,
#' shadows and the semantic colours derived from them - and
#' `blockr-theme.css` applies it to the host app.
#'
#' Attach it once, from the app's UI. The theme layer is deliberately
#' unscoped: it restyles Bootstrap typography, labels, form controls,
#' selectize, buttons, tooltips, popovers and the DataTables chrome across
#' the whole page, so an app opts into it rather than picking it up from a
#' component. The token block on its own is inert - nothing is styled by
#' defining a custom property.
#'
#' Consuming packages reference the tokens as `var(--blockr-grey-400,
#' #9ca3af)`, with a literal fallback for hosts that do not attach this
#' dependency.
#'
#' @return An [htmltools::htmlDependency].
#'
#' @examples
#' shiny::fluidPage(
#'   theme_dep(),
#'   shiny::h4("Section"),
#'   shiny::textInput("name", "Name")
#' )
#'
#' @export
theme_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-theme",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = c("css/blockr-tokens.css", "css/blockr-theme.css"),
    all_files = FALSE
  )
}
