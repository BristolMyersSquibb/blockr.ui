# Base stylesheet for a page without Bootstrap

What a blockr page needs from Bootstrap when it does not load it: the
reset of the browser's defaults, the body face (Open Sans, bundled), and
the look of the markup Shiny's own inputs emit, which keeps Bootstrap's
class names (`form-group`, `form-control`, `checkbox`, `btn`) whatever
the page loads. The button rules read the `--bs-btn-*` properties that
[`theme_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/theme_dep.md)
sets per kind, so a button looks the same on either kind of page.

## Usage

``` r
base_dep()
```

## Value

An
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
of
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects: the font, the base sheet, then the tokens and the theme layer
of
[`theme_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/theme_dep.md).

## Details

The base sheet is written against the theme layer, so it brings
[`theme_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/theme_dep.md)
along, after itself: the theme restyles over it as it does over
Bootstrap. Attach it once, from a page that does not load Bootstrap; a
[`theme_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/theme_dep.md)
attached elsewhere on the page is the same dependency. On a page that
does load Bootstrap it restyles what Bootstrap already styles and must
be left out.

## Examples

``` r
htmltools::tagList(
  base_dep(),
  shiny::textInput("name", "Name")
)
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="name-label" for="name">Name</label>
#>   <input id="name" type="text" class="shiny-input-text form-control" value="" data-update-on="change"/>
#> </div>
```
