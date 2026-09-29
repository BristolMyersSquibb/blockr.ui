#' A keyboard shortcut hint
#'
#' Writes a shortcut once, for every platform: keys joined by `"+"`, with
#' `"Mod"` for Command on a Mac and Ctrl elsewhere. The hint carries both
#' forms and blockr.ui shows the one that applies (design system, "Keyboard
#' shortcuts"): `"Mod+Shift+S"` reads ⌘⇧S on a Mac and Ctrl+Shift+S
#' elsewhere, and `"Mod+Enter"` reads ⌘↵ or Ctrl+↵. Put it in a menu row's
#' meta slot ([menu_item()]) or after a button's label.
#'
#' @param keys The keys, such as `"Mod+S"`. Named keys: `Mod`, `Shift`,
#'   `Alt`, `Ctrl`, `Enter` and `Esc`; any other single character is shown
#'   in upper case.
#'
#' @return A `<span>`, with [controls_dep()] attached.
#'
#' @examples
#' shortcut("Mod+Shift+S")
#'
#' @export
shortcut <- function(keys) {

  stopifnot(is.character(keys), length(keys) == 1L, nzchar(keys))

  parts <- strsplit(keys, "+", fixed = TRUE)[[1L]]

  with_controls(
    tags$span(
      class = "blockr-shortcut",
      tags$span(class = "blockr-shortcut__mac", shortcut_text(parts, TRUE)),
      tags$span(class = "blockr-shortcut__other", shortcut_text(parts, FALSE))
    )
  )
}

shortcut_text <- function(parts, mac) {

  names <- if (mac) {
    c(Mod = "\u2318", Shift = "\u21e7", Alt = "\u2325", Ctrl = "\u2303",
      Enter = "\u21b5", Esc = "Esc")
  } else {
    c(Mod = "Ctrl", Shift = "Shift", Alt = "Alt", Ctrl = "Ctrl",
      Enter = "\u21b5", Esc = "Esc")
  }

  keys <- vapply(
    parts,
    function(k) {
      if (k %in% names(names)) names[[k]]
      else if (nchar(k) == 1L) toupper(k)
      else k
    },
    character(1L),
    USE.NAMES = FALSE
  )

  paste(keys, collapse = if (mac) "" else "+")
}
