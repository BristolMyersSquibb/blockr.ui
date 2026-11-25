#' The names of the theme variables
#'
#' Gives the full names of the theme variables, such as
#' `"--bk-color-primary"`.
#' The theme variables are the CSS variables through which a frontend,
#' or an app's theme, gives the page the look that it likes.
#' The stylesheet of blockr.ui gives each of them a default,
#' for light mode and for dark mode.
#'
#' The names come from the list of theme variables,
#' which the client of blockr.ui declares,
#' and which its build writes into `inst/js/theme-variables.json`.
#' The build preset `@blockr/ui/build` checks every package's build
#' against the same list,
#' so the names in R and in JS never disagree.
#' A minor release of blockr.ui may add a name,
#' and only a major release removes or renames one.
#'
#' @return A character vector of the names, each under the prefix `--bk-`.
#'
#' @examples
#' theme_variables()
#'
#' @export
theme_variables <- function() {
  file <- system.file(
    "js", "theme-variables.json",
    package = "blockr.ui",
    mustWork = TRUE
  )
  jsonlite::read_json(file, simplifyVector = TRUE)$variables
}
