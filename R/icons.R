#' @param name The icon: `"chevron"`, `"remove"` (a tag's x, 10px), `"x"` (a
#'   row's remove button, 14px), `"plus"`, `"trash"`, `"sliders"`, `"check"`
#'   (a menu's current item), `"confirm"` (a committed field, a picked
#'   option), `"code"` or `"gear"`.
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

  if (!name %in% names(icon_set)) {
    stop("There is no icon \"", name, "\". The icons are ",
         paste0("\"", names(icon_set), "\"", collapse = ", "), ".",
         call. = FALSE)
  }

  htmltools::HTML(icon_set[[name]])
}

# The small icons, one copy of each: small_icon() reads them for markup built
# in R, and controls_dep() writes them into the page as Blockr.icons.
icon_set <- c(
  # The one chevron (design system, "Chevrons"): 1.4px at every size, so the
  # stroke does not scale when a pill draws it at 10px.
  chevron = paste0(
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" ',
    'stroke="currentColor" stroke-width="1.4" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true">',
    '<polyline points="3 4.5 6 7.5 9 4.5" vector-effect="non-scaling-stroke">',
    "</polyline></svg>"
  ),
  # A tag's x: a thin stroke, like every small icon (design system, "Small
  # icons"); at 1.5 it read bold beside the row's remove button.
  remove = paste0(
    '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" ',
    'stroke="currentColor" stroke-width="1" stroke-linecap="round" ',
    'aria-hidden="true">',
    '<line x1="2.5" y1="2.5" x2="7.5" y2="7.5"></line>',
    '<line x1="7.5" y1="2.5" x2="2.5" y2="7.5"></line></svg>'
  ),
  # A row's remove button: the same thin stroke as `remove`, at 14px. It was
  # Bootstrap's filled x, which read heavier than every other small icon.
  x = paste0(
    '<svg width="14" height="14" viewBox="0 0 14 14" fill="none" ',
    'stroke="currentColor" stroke-width="1" stroke-linecap="round" ',
    'aria-hidden="true">',
    '<line x1="3.5" y1="3.5" x2="10.5" y2="10.5"></line>',
    '<line x1="10.5" y1="3.5" x2="3.5" y2="10.5"></line></svg>'
  ),
  plus = paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16" aria-hidden="true">',
    '<path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 0',
    'v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>'
  ),
  # Menu icons, drawn at 1.25 stroke so the few rows that carry one read as
  # one set: the bin of a destructive row, the sliders of a row that opens a
  # mode ("Manage pages").
  trash = paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true">',
    '<path d="M2.5 4.5h11M6.5 4.5V3h3v1.5M4 4.5l.7 9h6.6l.7-9"></path></svg>'
  ),
  sliders = paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round" ',
    'aria-hidden="true">',
    '<path d="M2 4h12M2 8h12M2 12h12"></path>',
    '<circle cx="5" cy="4" r="1.6" ',
    'fill="var(--blockr-color-bg-raised)"></circle>',
    '<circle cx="10" cy="8" r="1.6" ',
    'fill="var(--blockr-color-bg-raised)"></circle>',
    '<circle cx="6" cy="12" r="1.6" ',
    'fill="var(--blockr-color-bg-raised)"></circle></svg>'
  ),
  # The current item's mark in a menu: a thin check, like the other small
  # icons.
  check = paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
    'stroke="currentColor" stroke-width="1.5" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true">',
    '<path d="M3.5 8.5l3 3 6-7"></path></svg>'
  ),
  confirm = paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16" aria-hidden="true">',
    '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0',
    "l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0",
    '"/></svg>'
  ),
  code = paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16" aria-hidden="true">',
    '<path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294z',
    "M4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708",
    "l-3.5-3.5a.5.5 0 0 1 0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 ",
    "0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 .708.708l3.5-3.5a.5.5 0 0 0 ",
    '0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>'
  ),
  gear = paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16" aria-hidden="true">',
    '<path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 0 0 ',
    "1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82",
    ".023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 ",
    "1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311",
    "-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1",
    "-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-.705 1.987",
    "-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 1.4",
    "-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283",
    "-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872z",
    'M8 10.93a2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
  )
)

icons_dep <- function() {
  controls_asset(
    "blockr-icons",
    head = paste0("<script>", icons_js(), "</script>")
  )
}

# The statement that sets Blockr.icons, an icon a line. The JavaScript tests
# run it from the snapshot test-icons.R keeps. Every "</" is written "<\/",
# so no icon can close the script it sits in.
icons_js <- function() {

  svg <- gsub("</", "<\\/", encodeString(icon_set, quote = "\""), fixed = TRUE)

  paste0(
    "(window.Blockr = window.Blockr || {}).icons = {\n",
    paste0("  ", names(icon_set), ": ", svg, collapse = ",\n"),
    "\n};"
  )
}
