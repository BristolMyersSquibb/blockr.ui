# Tool button

The design system's icon button: 26px, bare, the icon muted until hover.
It has no text, so it carries its name as a tooltip (the light card of
`Blockr.tooltip`) and as its accessible name.

## Usage

``` r
tool_button(icon, tooltip, ...)
```

## Arguments

- icon:

  The icon, as a tag or
  [`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

- tooltip:

  What the button does, in a few words ("Download").

- ...:

  Further attributes, such as `id`.

## Value

A `<button>`
[htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
carrying
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md).

## Examples

``` r
tool_button(htmltools::HTML("&darr;"), "Download")
#> <button type="button" class="blockr-tool" aria-label="Download" data-blockr-tooltip="Download">&darr;</button>
```
