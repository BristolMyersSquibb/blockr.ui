
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.ui

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![status](https://github.com/BristolMyersSquibb/blockr.ui/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.ui/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.ui/graph/badge.svg)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.ui)
[![CRAN
status](https://www.r-pkg.org/badges/version/blockr.ui)](https://CRAN.R-project.org/package=blockr.ui)
<!-- badges: end -->

User-interface primitives shared across the
[blockr](https://blockr.site/) ecosystem. The package currently provides
one: a paginated, sortable HTML table display for block results, which
any board can opt into and any package can build its own preview on top
of.

## Installation

You can install the development version of blockr.ui from
[GitHub](https://github.com/BristolMyersSquibb/blockr.ui) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.ui")
```

## The table display

Package blockr.core renders a block’s result through whatever object the
`blockr.tabular_display` option names, so opting a whole board into the
HTML table is one line before the app starts:

``` r
options(blockr.tabular_display = blockr.ui::html_table_display)
```

Data, parser and transform blocks then preview through a table that
sorts and pages on the client, keeps its scroll position across a
re-render, and shows column types (plus ADaM-style labels where the data
carries them) under each header. Each block’s preview sorts and pages
independently.

A worked board is in `inst/examples/table-preview`:

``` r
shiny::runApp(
  system.file("examples", "table-preview", package = "blockr.ui")
)
```

For a lazy (dbplyr) result the page fetch is pushed to the database
rather than collected up front, so paging a large table stays cheap.

## Building your own preview

The pieces behind the display are exported, so a package that wants a
different chrome around the same table need not re-derive any of it:

| Function              | Does                                                         |
|-----------------------|--------------------------------------------------------------|
| `table_page()`        | fetches one page of a result, sorted — local or lazy         |
| `build_html_table()`  | renders a page as an HTML table with headers and type labels |
| `column_widths_px()`  | measures column widths so headers and cells line up          |
| `apply_table_sort()`  | applies a sort spec to a data frame                          |
| `html_table_render()` | wires the above into a Shiny output                          |
| `table_preview_dep()` | the stylesheet and script the table needs                    |

## Code of Conduct

Please note that the blockr.ui project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
