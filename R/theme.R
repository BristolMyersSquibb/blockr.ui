#' Shared blockr stylesheet
#'
#' The `--blockr-*` design tokens and the Bootstrap theme layer that the
#' blockr packages style themselves from, carried as two stylesheets:
#' `blockr-tokens.css` defines the vocabulary in a `:root` block - the
#' grey and blue ramps, font sizes and weights, shadows, and the semantic
#' text and surface colours derived from them - and `blockr-theme.css`
#' applies it to the host app.
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

#' Drop Shiny's `:has(> *)` pass-through rules
#'
#' Shiny 1.8.1 styles `uiOutput()` and `conditionalPanel()` containers
#' `display: contents` so their children lay out as direct children of the
#' parent (rstudio/shiny#3957). It guards that on the container being
#' non-empty, and writes the guard as `:has(> *)`. That argument is the
#' universal selector, so any element appearing or disappearing anywhere in
#' the document could flip some ancestor's match. Chrome cannot narrow it
#' into an invalidation set and restyles the whole document on every DOM
#' mutation.
#'
#' On the CEDX board (17.5k elements, 6.4k CSS rules) that is 36ms of style
#' recalculation for a single appended `div`, against 4ms with the two rules
#' gone. It is paid by every block re-render, every keystroke in a picker and
#' every streamed chat token, and it grows with the board because the recalc
#' spans the document, including the panels that are not on screen.
#'
#' Attach it once, at the page level. It is a separate dependency from
#' [theme_dep()] so a host gets the fix whether or not it opts into blockr
#' styling, and because Shiny de-duplicates dependencies by name, attaching
#' it from more than one place is harmless.
#'
#' Deleting the rules is the fix. Overriding `display` on
#' `.shiny-html-output`, which is Shiny's documented escape hatch, leaves the
#' selector in the sheet and keeps the cost.
#'
#' @return An [htmltools::htmlDependency].
#'
#' @examples
#' shiny::fluidPage(shiny_has_perf_dep())
#'
#' @export
shiny_has_perf_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-shiny-has-perf",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    script = "js/shiny-has-perf.js",
    all_files = FALSE
  )
}
