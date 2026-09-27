#' Shared blockr stylesheet
#'
#' The `--blockr-*` design tokens and the Bootstrap theme layer that the
#' blockr packages style themselves from, carried as three stylesheets:
#' `blockr-tokens.css` defines the vocabulary in a `:root` block - the
#' colour palette, the meaning tokens built on it (text, backgrounds,
#' borders, status colours), type, radii, control heights and shadows -
#' `blockr-tokens-dark.css` restates them under `data-bs-theme="dark"`,
#' the attribute blockr.core's dark-mode board option sets, and
#' `blockr-theme.css` applies them to the host app.
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
    stylesheet = c(
      "css/blockr-tokens.css",
      "css/blockr-tokens-dark.css",
      "css/blockr-theme.css"
    ),
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

#' Skip the empty input messages Shiny sends after deferred inputs
#'
#' Shiny's input batcher (1.14.0) checks whether a send is already queued but
#' never records that one is, so every deferred `setInput` queues its own
#' send. The first carries all pending inputs and the rest send an empty
#' update. The server runs a full input cycle for each: it walks every output
#' of the session to update its hidden state, then flushes. Mounting a block
#' card sets about a dozen inputs, so a first visit to a 15-block dock view
#' sent 218 messages, 177 of them empty, and the dock's own messages queued
#' behind them for 2 to 4 seconds.
#'
#' The script wraps `Shiny.shinyapp.sendInput` to return early on an empty
#' object. Nothing is lost: the inputs such a send would have carried went
#' out with the first one. Once Shiny records the queued send itself, no
#' empty batch reaches the wrapper.
#'
#' Attach it once, at the page level, like [shiny_has_perf_dep()].
#'
#' @return An [htmltools::htmlDependency].
#'
#' @examples
#' shiny::fluidPage(shiny_input_batch_dep())
#'
#' @export
shiny_input_batch_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-shiny-input-batch",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    script = "js/shiny-input-batch.js",
    all_files = FALSE
  )
}
