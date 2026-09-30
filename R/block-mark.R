#' Block mark
#'
#' A block's mark is its glyph in its category colour on an 18% tint of that
#' colour (design system, "The block's mark and bare mode"). It comes in four
#' sizes: 32px in the dock header, 24px in list rows, 20px in the compact
#' header and 16px in a tab. The glyph is half the mark plus 2px, and the
#' corner radius a quarter of it.
#'
#' In HTML, `block_mark()` draws the mark with the `.blockr-mark` class, which
#' colours it from the category's token, `--blockr-category-<category>`. The
#' mark on a `Blockr.menu` row is the same element at 24px. For a place that
#' takes an image rather than markup, such as the DAG's canvas,
#' `block_mark_svg()` draws the mark as a standalone SVG. An image cannot read
#' the tokens, so the SVG carries its colour as a literal, which
#' `category_color()` looks up from the tokens file. Both tints are
#' translucent, so the mark sits at 18% on whatever surface is under it, in
#' either scheme.
#'
#' @param icon The block's glyph, as SVG markup: the `icon` column of
#'   [blockr.core::block_metadata()].
#' @param category The block's category: the `category` column of
#'   [blockr.core::block_metadata()]. A category outside
#'   [blockr.core::suggested_categories()] takes the colour of
#'   `"uncategorized"`.
#' @param size The side of the mark in pixels. The HTML mark comes in the
#'   four sizes: 24 (a list row), 32 (the dock header), 20 (the compact
#'   header) and 16 (a tab). The SVG takes any size.
#' @param ... Further attributes of the mark, such as its tooltip.
#'
#' @return A `<span>` [htmltools::tag] carrying [controls_dep()] from
#'   `block_mark()`. From `block_mark_svg()`, the SVG as [htmltools::HTML()],
#'   or with `uri = TRUE` a `data:` URI of it, to use as an image's source.
#'   From `category_color()`, one hex colour per category.
#'
#' @examples
#' meta <- blockr.core::block_metadata(blockr.core::new_dataset_block())
#'
#' block_mark(meta$icon, meta$category, size = 32)
#' block_mark_svg(meta$icon, meta$category, size = 48, uri = TRUE)
#' category_color(c("input", "plot", "uncategorized"))
#'
#' @export
block_mark <- function(icon, category, size = 24, ...) {

  stopifnot(
    blockr.core::is_string(icon),
    blockr.core::is_string(category),
    is.numeric(size), length(size) == 1L, size %in% c(16, 20, 24, 32)
  )

  with_controls(
    tags$span(
      class = "blockr-mark",
      class = if (size != 24) paste0("blockr-mark--", size),
      `data-category` = category,
      ...,
      htmltools::HTML(icon)
    )
  )
}

#' @param uri Return a `data:` URI of the SVG rather than the SVG itself.
#'
#' @rdname block_mark
#' @export
block_mark_svg <- function(icon, category, size = 24, uri = FALSE) {

  stopifnot(
    blockr.core::is_string(icon),
    blockr.core::is_string(category),
    is.numeric(size), length(size) == 1L, size > 0,
    isTRUE(uri) || isFALSE(uri)
  )

  color <- category_color(category)
  glyph <- size / 2 + 2

  svg <- sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%1$s\" ",
      "height=\"%1$s\" viewBox=\"0 0 %1$s %1$s\">",
      "<rect width=\"%1$s\" height=\"%1$s\" rx=\"%2$s\" fill=\"%3$s\" ",
      "fill-opacity=\"0.18\"/>",
      "<svg x=\"%4$s\" y=\"%4$s\" width=\"%5$s\" height=\"%5$s\" ",
      "viewBox=\"%6$s\" fill=\"%3$s\" color=\"%3$s\">%7$s</svg>",
      "</svg>"
    ),
    size, size / 4, color, (size - glyph) / 2, glyph,
    glyph_view_box(icon), glyph_body(icon)
  )

  if (uri) {
    return(
      paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE))
    )
  }

  htmltools::HTML(svg)
}

#' @rdname block_mark
#' @export
category_color <- function(category) {

  stopifnot(is.character(category))

  colors <- category_colors()
  res <- colors[category]
  res[is.na(res)] <- colors[["uncategorized"]]

  unname(res)
}

# Read from the tokens file rather than written out again, so an SVG mark
# cannot drift from the stylesheet.
category_colors <- function() {

  if (is.null(mark_cache$colors)) {

    css <- readLines(
      pkg_file("assets", "css", "blockr-tokens.css"),
      warn = FALSE
    )
    hits <- regmatches(
      css,
      regexec("--blockr-category-([a-z]+):\\s*(#[0-9a-f]{6});", css)
    )
    hits <- hits[lengths(hits) == 3L]

    mark_cache$colors <- blockr.core::set_names(
      blockr.core::chr_xtr(hits, 3L),
      blockr.core::chr_xtr(hits, 2L)
    )
  }

  mark_cache$colors
}

mark_cache <- new.env(parent = emptyenv())

# A glyph whose tag gives no grid is taken to be on bsicons' 16px one.
glyph_view_box <- function(icon) {

  tag <- regmatches(icon, regexpr("<svg\\b[^>]*>", icon, perl = TRUE))
  box <- regmatches(tag, regexec("viewBox=[\"']([^\"']*)[\"']", tag))

  if (length(box) && length(box[[1L]]) == 2L) box[[1L]][2L] else "0 0 16 16"
}

# The glyph's own <svg> tag sizes and colours it for inline HTML, so the
# mark's inner <svg> replaces it.
glyph_body <- function(icon) {
  sub("(?s)^\\s*<svg\\b[^>]*>(.*)</svg>\\s*$", "\\1", icon, perl = TRUE)
}
