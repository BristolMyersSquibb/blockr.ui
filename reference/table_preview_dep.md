# Table-preview assets

`table_preview_dep()` returns the
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
carrying the table-preview CSS and JS;
[`build_html_table()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/build_html_table.md)
attaches it automatically. `table_preview_css()` returns the base CSS as
an inline `shiny::tags$style` for hosts that layer delta CSS on top of
it (e.g. blockr.viz's html table block); it reads the same file the
dependency serves, so there is a single source.

## Usage

``` r
table_preview_dep()

table_preview_css()
```

## Value

An
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
or a `shiny::tags$style` element.
