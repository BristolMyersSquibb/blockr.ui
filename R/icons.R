#' @param name The icon: `"chevron"`, `"remove"` (a tag's x, 10px), `"x"` (a
#'   row's remove button, 14px), `"grip"` (a drag handle), `"plus"`,
#'   `"minus"`, `"trash"`, `"sliders"`, `"check"` (a menu's current item),
#'   `"confirm"` (a committed field, a picked option), `"code"`, `"gear"`,
#'   `"eye"` (a preview toggle), `"dots"` (a "…" menu's trigger), `"info"`,
#'   `"warning"`, `"maximize"` or `"restore"` (a dock group's maximise
#'   button).
#'
#' @return For `small_icon()`, the icon's `<svg>` element, as
#'   [htmltools::HTML()].
#'
#' @examples
#' tool_button(small_icon("gear"), "Settings")
#'
#' @rdname controls_dep
#' @export
small_icon <- function(name) {

  stopifnot(is.character(name), length(name) == 1L)

  icons <- icon_set()

  if (!name %in% names(icons)) {
    stop("There is no icon \"", name, "\". The icons are ",
         paste0("\"", names(icons), "\"", collapse = ", "), ".",
         call. = FALSE)
  }

  htmltools::HTML(icons[[name]])
}

# The small icons, one file each in inst/assets/icons and no other copy:
# small_icon() reads them for markup built in R, and controls_dep() writes
# them into the page as Blockr.icons. The files are read once per process.
icon_set <- function() {

  if (is.null(controls_cache$icons)) {

    files <- list.files(
      pkg_file("assets", "icons"),
      pattern = "\\.svg$",
      full.names = TRUE
    )

    controls_cache$icons <- blockr.core::set_names(
      blockr.core::chr_ply(files, read_icon),
      sub("\\.svg$", "", basename(files))
    )
  }

  controls_cache$icons
}

# A file holds the icon's note as a comment and its svg a tag a line, so the
# icon is the file without the comment and the whitespace between tags. The
# tests in tests/js read the files the same way.
read_icon <- function(file) {
  svg <- paste(readLines(file, warn = FALSE), collapse = "\n")
  svg <- gsub("<!--[\\s\\S]*?-->", "", svg, perl = TRUE)
  trimws(gsub(">\\s+<", "><", svg, perl = TRUE))
}

icons_dep <- function() {
  controls_asset(
    "blockr-icons",
    head = paste0("<script>", icons_js(), "</script>")
  )
}

# The statement that sets Blockr.icons, an icon a line. Each name is quoted,
# as a file's name need not be a JavaScript identifier, and every "</" is
# written "<\/", so no icon can close the script it sits in.
icons_js <- function() {

  icons <- icon_set()
  keys <- encodeString(names(icons), quote = "\"")
  svg <- gsub("</", "<\\/", encodeString(icons, quote = "\""), fixed = TRUE)

  paste0(
    "(window.Blockr = window.Blockr || {}).icons = {\n",
    paste0("  ", keys, ": ", svg, collapse = ",\n"),
    "\n};"
  )
}
