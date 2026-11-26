#' An app's theme
#'
#' An app gives the page a theme of its own:
#' values for some of the theme variables in each mode,
#' and the files of the fonts that it brings.
#' The function `new_blockr_theme()` makes such a theme in R.
#' The app passes it to the `serve()` of blockr.core,
#' which writes the theme into the page's head.
#' The theme's values win over the frontend's values and over the defaults of
#' blockr.ui, whichever stylesheet loads first.
#'
#' Each value of `light` holds in light mode alone,
#' and each value of `dark` in dark mode alone.
#' So a theme with values for light mode alone leaves dark mode to the
#' frontend.
#' A value that holds in both modes, such as a face, goes into both lists.
#'
#' The function refuses a name that [theme_variables()] lacks, naming it.
#' So a misspelt name, or one that a later blockr.ui drops,
#' stops the app as it starts.
#' R writes each value into the page as it is.
#' So the function refuses a value that holds `}` or `<`,
#' which would end the rule or the element that holds it.
#' It checks a value no further.
#'
#' The method of [format()] returns the text of the theme's rules,
#' one for each mode that the theme gives values.
#' Each rule names `:root` twice beside the mode,
#' as in `:root:root[data-bk-mode="light"]`.
#' So the theme's values win over the frontend's values,
#' which the frontend sets under `:root[data-bk-mode="light"]` and
#' `:root[data-bk-mode="dark"]`.
#' The faces of the theme's fonts are core's to write,
#' since core serves their files.
#'
#' @param light,dark The values of light mode and of dark mode.
#'   Each is a list of strings,
#'   each under the full name of a theme variable,
#'   such as `"--bk-color-primary"`.
#'   A value is any CSS value.
#' @param fonts The faces of the theme's fonts,
#'   a list with a face for each file of a font.
#'   A face is a list with the font's family, `family`,
#'   the path of the font's file, `file`,
#'   and the `weight` and the `style` of the face.
#'   The weight and the style default to `"normal"`,
#'   as they do for a face in CSS.
#' @param x For `is_blockr_theme()`, any object,
#'   and for `format()`, a theme.
#' @param ... Ignored.
#'
#' @return The function `new_blockr_theme()` returns an object of class
#' `blockr_theme`, and `is_blockr_theme()` a scalar logical.
#' The method of `format()` returns a string,
#' which is empty for a theme with no values.
#'
#' @examples
#' theme <- new_blockr_theme(
#'   light = list(
#'     "--bk-color-primary" = "#be2bbb",
#'     "--bk-font-sans" = "Inter, sans-serif"
#'   ),
#'   dark = list(
#'     "--bk-color-primary" = "#d66bd3",
#'     "--bk-font-sans" = "Inter, sans-serif"
#'   )
#' )
#'
#' is_blockr_theme(theme)
#'
#' cat(format(theme))
#'
#' # A face for a file of Open Sans that blockr.ui ships
#' new_blockr_theme(
#'   fonts = list(
#'     list(
#'       family = "Open Sans",
#'       file = system.file(
#'         "js", "open-sans-latin-italic.woff2",
#'         package = "blockr.ui"
#'       ),
#'       weight = "300 800",
#'       style = "italic"
#'     )
#'   )
#' )
#'
#' @export
new_blockr_theme <- function(light = list(), dark = list(), fonts = list()) {
  structure(
    list(
      light = check_theme_values(light, "light"),
      dark = check_theme_values(dark, "dark"),
      fonts = lapply(fonts, font_face)
    ),
    class = "blockr_theme"
  )
}

#' @rdname new_blockr_theme
#' @export
is_blockr_theme <- function(x) {
  inherits(x, "blockr_theme")
}

#' @rdname new_blockr_theme
#' @export
format.blockr_theme <- function(x, ...) {
  attribute <- theme_list()[["modeAttribute"]]
  rules <- c(
    theme_rule(x[["light"]], attribute, "light"),
    theme_rule(x[["dark"]], attribute, "dark")
  )
  paste(rules, collapse = "\n\n")
}

# The rule that sets the values of one mode, or none where the mode has no
# values. Its selector names `:root` twice beside the mode attribute, which
# gives it a specificity of (0,3,0). A frontend sets its values under `:root`
# and the mode attribute, a specificity of (0,2,0). Both sit outside any
# layer, while blockr.ui's defaults sit in one. So the app's values win over
# the frontend's and over blockr.ui's defaults, whichever stylesheet loads
# first.
theme_rule <- function(values, attribute, mode) {

  if (!length(values)) {
    return(character())
  }

  paste0(
    ":root:root[", attribute, "=\"", mode, "\"] {\n",
    paste0("  ", names(values), ": ", unlist(values), ";\n", collapse = ""),
    "}"
  )
}

# The values of one mode, as `new_blockr_theme()` takes them: a list of
# strings, each under the name of a theme variable.
check_theme_values <- function(values, mode) {

  if (!is.list(values) || is.object(values)) {
    theme_abort(
      "The argument `", mode, "` is expected to be a list of values, ",
      "each under the name of a theme variable.",
      class = "blockr_theme_values_invalid"
    )
  }

  if (!length(values)) {
    return(list())
  }

  nms <- names(values)

  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    theme_abort(
      "Each value in `", mode, "` is expected under the name of a theme ",
      "variable.",
      class = "blockr_theme_values_invalid"
    )
  }

  unknown <- setdiff(nms, theme_variables())

  if (length(unknown)) {
    theme_abort(
      "The argument `", mode, "` names ", and_list(dQuote(unknown, FALSE)),
      ", which `theme_variables()` lacks.",
      class = "blockr_theme_variable_unknown"
    )
  }

  strings <- vapply(values, is_theme_string, logical(1L))

  if (!all(strings)) {
    theme_abort(
      "The argument `", mode, "` gives no string as the value of ",
      and_list(dQuote(nms[!strings], FALSE)), ".",
      class = "blockr_theme_values_invalid"
    )
  }

  unsafe <- grepl("[}<]", unlist(values))

  if (any(unsafe)) {
    theme_abort(
      "The argument `", mode, "` gives ", and_list(dQuote(nms[unsafe], FALSE)),
      " a value that holds `}` or `<`. ",
      "Either would end the rule or the element that holds it.",
      class = "blockr_theme_value_unsafe"
    )
  }

  values
}

# A face of the theme's fonts, with "normal" for a weight or a style that it
# lacks, as for a face in CSS.
font_face <- function(font) {
  defaults <- list(weight = "normal", style = "normal")
  c(font, defaults[setdiff(names(defaults), names(font))])
}

# Whether `x` is a string that a theme can write into the page.
is_theme_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

# The strings of `x` joined for a message, as in "a, b and c".
and_list <- function(x) {

  if (length(x) < 2L) {
    return(x)
  }

  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

# An error of class `class`, beside "blockr_error", which blockr.core's
# `blockr_abort()` gives its errors as well.
theme_abort <- function(..., class) {
  stop(
    errorCondition(
      paste0(...),
      class = c(class, "blockr_error"),
      call = NULL
    )
  )
}
