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

#' Strip a redundant `:has(> *)` guard from Shiny's recalculating fade
#'
#' Shiny 1.8.1 styles `uiOutput()` and `conditionalPanel()` containers
#' `display: contents` once they hold children, so those children lay out as
#' direct children of the parent (rstudio/shiny#3957). A pass-through
#' container generates no box and can no longer carry the `.recalculating`
#' fade, so Shiny pushes the opacity down to the children with a companion
#' rule, `div:where(.shiny-html-output):has(> *).recalculating > *`.
#'
#' The companion rule is the one that costs. Its `:has()` sits in non-subject
#' position with a universal subject, and Chrome answers that by restyling the
#' whole document on every DOM insertion. On a 40-block dock board (6.4k
#' elements, 5.5k CSS rules) that is 107ms of style recalculation for a single
#' appended `div`, against 3ms once the guard is gone, and it grows linearly
#' with element count -- including the panels that are not on screen. It is
#' paid by every block re-render, every keystroke in a picker and every
#' streamed chat token. The `display: contents` rule beside it, whose `:has()`
#' is in subject position, measures free and is left alone.
#'
#' The guard is redundant on the companion. A selector shaped
#' `X:has(> *)... > *` picks a descendant of `X`, so `X` necessarily has an
#' element child wherever the subject exists. Removing it preserves both the
#' pass-through layout and the fade.
#'
#' Attach it once, at the page level. It is a separate dependency from
#' [theme_dep()] so a host gets the fix whether or not it opts into blockr
#' styling, and because Shiny de-duplicates dependencies by name, attaching
#' it from more than one place is harmless.
#'
#' Editing the CSSOM is the only route. The cost is bound to the selector
#' being present in the active index, which Chrome consults before the cascade
#' runs, so a rule stripped of every declaration, or aimed at a class present
#' nowhere in the document, still costs full price. Shiny's documented escape
#' hatch, overriding `display` on `.shiny-html-output`, addresses layout and
#' leaves the cost untouched.
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
