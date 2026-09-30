glyph <- paste0(
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" ",
  "style=\"height:1em;width:1em;fill:currentColor;\" aria-hidden=\"true\">",
  "<path d=\"M0 0h16v16H0z\"></path></svg>"
)

test_that("block_mark() draws the glyph at a size, coloured by category", {

  attrib <- htmltools::tagGetAttribute
  mark <- block_mark(glyph, "plot")

  expect_identical(attrib(mark, "class"), "blockr-mark")
  expect_identical(attrib(mark, "data-category"), "plot")
  expect_match(as.character(mark), "<path d=\"M0 0h16v16H0z\">", fixed = TRUE)

  # The 24px mark is the plain class; the other sizes add their modifier.
  for (size in c(32, 20, 16)) {
    expect_identical(
      attrib(block_mark(glyph, "plot", size = size), "class"),
      paste0("blockr-mark blockr-mark--", size)
    )
  }

  expect_identical(
    attrib(
      block_mark(glyph, "plot", `data-blockr-tooltip` = "Chart"),
      "data-blockr-tooltip"
    ),
    "Chart"
  )

  expect_error(block_mark(glyph, "plot", size = 48))
  expect_error(block_mark(glyph, c("plot", "table")))
})

test_that("block_mark() brings the stylesheet that draws it", {

  deps <- htmltools::findDependencies(block_mark(glyph, "plot"))

  expect_true(
    "blockr-blocks-css" %in% blockr.core::chr_xtr(deps, "name")
  )
})

test_that("each size of the mark follows the spec's rule", {

  # The glyph is half the mark plus 2px, and the corner radius a quarter of
  # it (design system, "The block's mark and bare mode").
  css <- read_css(
    system.file("assets", "css", "blockr-blocks.css", package = "blockr.ui")
  )

  rule <- function(selector) {
    body <- regmatches(
      css,
      regexec(paste0("\\Q", selector, "\\E\\s*\\{([^}]*)\\}"), css, perl = TRUE)
    )[[1L]][2L]
    parts <- strsplit(trimws(strsplit(body, ";")[[1L]]), "\\s*:\\s*")
    parts <- parts[lengths(parts) == 2L]
    blockr.core::set_names(
      blockr.core::chr_xtr(parts, 2L),
      blockr.core::chr_xtr(parts, 1L)
    )
  }

  for (size in c(24, 32, 20, 16)) {

    decl <- rule(
      if (size == 24) ".blockr-mark" else paste0(".blockr-mark--", size)
    )

    expect_identical(
      decl[c("width", "height", "font-size", "border-radius")],
      blockr.core::set_names(
        paste0(c(size, size, size / 2 + 2, size / 4), "px"),
        c("width", "height", "font-size", "border-radius")
      ),
      label = paste0("the ", size, "px mark")
    )
  }
})

test_that("every category token colours the mark, and every category has one", {

  tokens <- blockr_tokens()
  tokens <- tokens[startsWith(names(tokens), "--blockr-category-")]
  categories <- sub("^--blockr-category-", "", names(tokens))

  expect_setequal(categories, names(blockr.core::suggested_categories()))

  # The unmarked mark takes `uncategorized`, and each other category has its
  # rule, reading its own token.
  selectors <- css_selectors(
    system.file("assets", "css", "blockr-blocks.css", package = "blockr.ui")
  )
  coloured <- sub(
    "^\\.blockr-mark\\[data-category=\"([a-z]+)\"\\]$",
    "\\1",
    grep("^\\.blockr-mark\\[data-category=", selectors, value = TRUE)
  )

  expect_setequal(coloured, setdiff(categories, "uncategorized"))

  expect_identical(
    category_color(categories),
    unname(tokens)
  )
})

test_that("category_color() gives an unknown category the default colour", {

  expect_identical(
    category_color(c("input", "not-a-category", NA)),
    c("#0072b2", "#999999", "#999999")
  )
  expect_identical(category_color(character()), character())
})

test_that("block_mark_svg() draws the same mark as an image", {

  svg <- as.character(block_mark_svg(glyph, "plot", size = 32))

  expect_match(
    svg,
    "^<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"32\""
  )

  # The tint: the category colour at 18%, its corners rounded by a quarter of
  # the size.
  expect_match(
    svg,
    paste0(
      "<rect width=\"32\" height=\"32\" rx=\"8\" fill=\"#e69f00\" ",
      "fill-opacity=\"0.18\"/>"
    ),
    fixed = TRUE
  )

  # The glyph: half the mark plus 2px, centred, in the category colour, on
  # its own grid, without the size and colour its tag carries for HTML.
  expect_match(
    svg,
    paste0(
      "<svg x=\"7\" y=\"7\" width=\"18\" height=\"18\" viewBox=\"0 0 16 16\" ",
      "fill=\"#e69f00\" color=\"#e69f00\"><path d=\"M0 0h16v16H0z\"></path>",
      "</svg></svg>$"
    )
  )
  expect_no_match(svg, "1em", fixed = TRUE)
})

test_that("block_mark_svg() keeps a glyph's own grid", {

  wide <- "<svg viewBox='0 0 24 24'><circle r=\"12\"/></svg>"

  expect_match(
    as.character(block_mark_svg(wide, "input")),
    "viewBox=\"0 0 24 24\" fill=\"#0072b2\"",
    fixed = TRUE
  )
})

test_that("block_mark_svg() returns a data URI that decodes to the SVG", {

  svg <- block_mark_svg(glyph, "table", size = 48)
  uri <- block_mark_svg(glyph, "table", size = 48, uri = TRUE)

  expect_type(uri, "character")
  expect_match(uri, "^data:image/svg\\+xml,%3Csvg%20")
  expect_no_match(uri, "[<>\"# ]")
  expect_identical(
    utils::URLdecode(sub("^data:image/svg\\+xml,", "", uri)),
    as.character(svg)
  )
})

test_that("block_mark_svg() draws a real block's glyph", {

  meta <- blockr.core::block_metadata(blockr.core::new_dataset_block())
  svg <- as.character(block_mark_svg(meta$icon, meta$category))

  expect_match(svg, "<path d=", fixed = TRUE)
  expect_match(svg, "fill=\"#0072b2\"", fixed = TRUE)
  expect_no_match(svg, "class=\"bi", fixed = TRUE)
})
