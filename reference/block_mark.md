# Block mark

A block's mark is its glyph in its category colour on an 18% tint of
that colour (design system, "The block's mark and bare mode"). It comes
in four sizes: 32px in the dock header, 24px in list rows, 20px in the
compact header and 16px in a tab. The glyph is half the mark plus 2px,
and the corner radius a quarter of it.

## Usage

``` r
block_mark(icon, category, size = 24, ...)

block_mark_svg(icon, category, size = 24, uri = FALSE)

category_color(category)
```

## Arguments

- icon:

  The block's glyph, as SVG markup: the `icon` column of
  [`blockr.core::block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.html).

- category:

  The block's category: the `category` column of
  [`blockr.core::block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.html).
  A category outside
  [`blockr.core::suggested_categories()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.html)
  takes the colour of `"uncategorized"`.

- size:

  The side of the mark in pixels. The HTML mark comes in the four sizes:
  24 (a list row), 32 (the dock header), 20 (the compact header) and 16
  (a tab). The SVG takes any size.

- ...:

  Further attributes of the mark, such as its tooltip.

- uri:

  Return a `data:` URI of the SVG rather than the SVG itself.

## Value

A `<span>`
[htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
carrying
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md)
from `block_mark()`. From `block_mark_svg()`, the SVG as
[`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html),
or with `uri = TRUE` a `data:` URI of it, to use as an image's source.
From `category_color()`, one hex colour per category.

## Details

In HTML, `block_mark()` draws the mark with the `.blockr-block-mark`
class, which colours it from the category's token,
`--blockr-category-<category>`. The mark on a `Blockr.menu` row is the
same element at 24px. For a place that takes an image rather than
markup, such as the DAG's canvas, `block_mark_svg()` draws the mark as a
standalone SVG. An image cannot read the tokens, so the SVG carries its
colour as a literal, which `category_color()` looks up from the tokens
file. Both tints are translucent, so the mark sits at 18% on whatever
surface is under it, in either scheme.

## Examples

``` r
meta <- blockr.core::block_metadata(blockr.core::new_dataset_block())

block_mark(meta$icon, meta$category, size = 32)
#> <span class="blockr-block-mark blockr-block-mark--32" data-category="input"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-database " style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img" ><path d="M4.318 2.687C5.234 2.271 6.536 2 8 2s2.766.27 3.682.687C12.644 3.125 13 3.627 13 4c0 .374-.356.875-1.318 1.313C10.766 5.729 9.464 6 8 6s-2.766-.27-3.682-.687C3.356 4.875 3 4.373 3 4c0-.374.356-.875 1.318-1.313ZM13 5.698V7c0 .374-.356.875-1.318 1.313C10.766 8.729 9.464 9 8 9s-2.766-.27-3.682-.687C3.356 7.875 3 7.373 3 7V5.698c.271.202.58.378.904.525C4.978 6.711 6.427 7 8 7s3.022-.289 4.096-.777A4.92 4.92 0 0 0 13 5.698ZM14 4c0-1.007-.875-1.755-1.904-2.223C11.022 1.289 9.573 1 8 1s-3.022.289-4.096.777C2.875 2.245 2 2.993 2 4v9c0 1.007.875 1.755 1.904 2.223C4.978 15.71 6.427 16 8 16s3.022-.289 4.096-.777C13.125 14.755 14 14.007 14 13V4Zm-1 4.698V10c0 .374-.356.875-1.318 1.313C10.766 11.729 9.464 12 8 12s-2.766-.27-3.682-.687C3.356 10.875 3 10.373 3 10V8.698c.271.202.58.378.904.525C4.978 9.71 6.427 10 8 10s3.022-.289 4.096-.777A4.92 4.92 0 0 0 13 8.698Zm0 3V13c0 .374-.356.875-1.318 1.313C10.766 14.729 9.464 15 8 15s-2.766-.27-3.682-.687C3.356 13.875 3 13.373 3 13v-1.302c.271.202.58.378.904.525C4.978 12.71 6.427 13 8 13s3.022-.289 4.096-.777c.324-.147.633-.323.904-.525Z"></path></svg></span>
block_mark_svg(meta$icon, meta$category, size = 48, uri = TRUE)
#> [1] "data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20width%3D%2248%22%20height%3D%2248%22%20viewBox%3D%220%200%2048%2048%22%3E%3Crect%20width%3D%2248%22%20height%3D%2248%22%20rx%3D%2212%22%20fill%3D%22%230072b2%22%20fill-opacity%3D%220.18%22%2F%3E%3Csvg%20x%3D%2211%22%20y%3D%2211%22%20width%3D%2226%22%20height%3D%2226%22%20viewBox%3D%220%200%2016%2016%22%20fill%3D%22%230072b2%22%20color%3D%22%230072b2%22%3E%3Cpath%20d%3D%22M4.318%202.687C5.234%202.271%206.536%202%208%202s2.766.27%203.682.687C12.644%203.125%2013%203.627%2013%204c0%20.374-.356.875-1.318%201.313C10.766%205.729%209.464%206%208%206s-2.766-.27-3.682-.687C3.356%204.875%203%204.373%203%204c0-.374.356-.875%201.318-1.313ZM13%205.698V7c0%20.374-.356.875-1.318%201.313C10.766%208.729%209.464%209%208%209s-2.766-.27-3.682-.687C3.356%207.875%203%207.373%203%207V5.698c.271.202.58.378.904.525C4.978%206.711%206.427%207%208%207s3.022-.289%204.096-.777A4.92%204.92%200%200%200%2013%205.698ZM14%204c0-1.007-.875-1.755-1.904-2.223C11.022%201.289%209.573%201%208%201s-3.022.289-4.096.777C2.875%202.245%202%202.993%202%204v9c0%201.007.875%201.755%201.904%202.223C4.978%2015.71%206.427%2016%208%2016s3.022-.289%204.096-.777C13.125%2014.755%2014%2014.007%2014%2013V4Zm-1%204.698V10c0%20.374-.356.875-1.318%201.313C10.766%2011.729%209.464%2012%208%2012s-2.766-.27-3.682-.687C3.356%2010.875%203%2010.373%203%2010V8.698c.271.202.58.378.904.525C4.978%209.71%206.427%2010%208%2010s3.022-.289%204.096-.777A4.92%204.92%200%200%200%2013%208.698Zm0%203V13c0%20.374-.356.875-1.318%201.313C10.766%2014.729%209.464%2015%208%2015s-2.766-.27-3.682-.687C3.356%2013.875%203%2013.373%203%2013v-1.302c.271.202.58.378.904.525C4.978%2012.71%206.427%2013%208%2013s3.022-.289%204.096-.777c.324-.147.633-.323.904-.525Z%22%3E%3C%2Fpath%3E%3C%2Fsvg%3E%3C%2Fsvg%3E"
category_color(c("input", "plot", "uncategorized"))
#> [1] "#0072b2" "#e69f00" "#999999"
```
