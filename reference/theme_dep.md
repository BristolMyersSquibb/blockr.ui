# Shared blockr stylesheet

The `--blockr-*` design tokens and the Bootstrap theme layer that the
blockr packages style themselves from, as two dependencies. The tokens
define the vocabulary in a `:root` block: the colour palette, the
meaning tokens built on it (text, backgrounds, borders, status colours),
type, radii, control heights and shadows. The file
`blockr-tokens-dark.css` restates them under `data-bs-theme="dark"`, the
attribute blockr.core's dark-mode board option sets. On their own the
tokens are inert: nothing is styled by defining a custom property. The
theme layer, `blockr-theme.css`, applies them to the host app.

## Usage

``` r
theme_dep()
```

## Value

An
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
of two
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects: the tokens, then the theme layer.

## Details

The theme layer is deliberately unscoped: it restyles Bootstrap
typography, labels, form controls, selectize, buttons, tooltips,
popovers and the DataTables chrome across the whole page. An app opts
into it by attaching `theme_dep()` once, from its UI, as blockr.dock's
board page does. A component brings only the tokens: with `theme_dep()`
in the page the app gets the full look, and without it the controls of
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md)
keep theirs and the app keeps its own.

Packages that read the tokens without attaching them write a literal
fallback, as in `var(--blockr-color-text-muted, #6b7280)`.

## Examples

``` r
# The full look: the theme layer restyles the app's heading and field.
shiny::fluidPage(
  theme_dep(),
  controls_dep(),
  shiny::h4("Section"),
  shiny::textInput("name", "Name")
)
#> <div class="container-fluid">
#>   <h4>Section</h4>
#>   <div class="form-group shiny-input-container">
#>     <label class="control-label" id="name-label" for="name">Name</label>
#>     <input id="name" type="text" class="shiny-input-text form-control" value="" data-update-on="change"/>
#>   </div>
#> </div>

# The controls keep their look, and the app keeps its own.
shiny::fluidPage(
  controls_dep(),
  shiny::h4("Section"),
  shiny::textInput("name", "Name")
)
#> <div class="container-fluid">
#>   <h4>Section</h4>
#>   <div class="form-group shiny-input-container">
#>     <label class="control-label" id="name-label" for="name">Name</label>
#>     <input id="name" type="text" class="shiny-input-text form-control" value="" data-update-on="change"/>
#>   </div>
#> </div>
```
