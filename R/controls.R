#' Shared block controls
#'
#' The JavaScript and CSS of the controls blockr blocks are built from:
#' `Blockr.Select` (single, multi and menu), the Enter button of a field that
#' commits on Enter, the required-empty cue, the checkbox, the segmented
#' control, the gear tray and the placement routine for floating panels
#' (`Blockr.place`), all on the `window.Blockr` namespace, together with the
#' rows, pills, labels and fields they draw.
#'
#' The stylesheets read the design tokens without fallbacks, so the
#' dependency brings [theme_dep()] along. Attach it from a block's UI;
#' dependencies are de-duplicated by name, so any number of blocks can.
#'
#' The dependency names are the ones blockr.dplyr used while these files
#' lived there (`blockr-select-js`, `blockr-select-css`, `blockr-blocks-css`),
#' so a page never loads two copies of Select.
#'
#' @return An [htmltools::tagList()] of [htmltools::htmlDependency] objects,
#'   in load order.
#'
#' @examples
#' shiny::fluidPage(
#'   controls_dep(),
#'   shiny::div(id = "cols"),
#'   shiny::tags$script(
#'     "Blockr.Select.multi(document.getElementById('cols'),",
#'     "  { options: ['mpg', 'cyl', 'disp'], selected: ['mpg'] });"
#'   )
#' )
#'
#' @export
controls_dep <- function() {
  tagList(
    theme_dep(),
    controls_asset("blockr-ui-js", script = "js/blockr-ui.js"),
    controls_asset("blockr-blocks-css", stylesheet = "css/blockr-blocks.css"),
    controls_asset(
      "blockr-settings-band",
      stylesheet = "css/blockr-settings-band.css"
    ),
    controls_asset("blockr-select-js", script = "js/blockr-select.js"),
    controls_asset("blockr-select-css", stylesheet = "css/blockr-select.css")
  )
}

controls_asset <- function(name, ...) {
  htmltools::htmlDependency(
    name = name,
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    ...,
    all_files = FALSE
  )
}
