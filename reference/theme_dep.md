# Shared blockr stylesheet

The `--blockr-*` design tokens and the Bootstrap theme layer that the
blockr packages style themselves from, carried as three stylesheets:
`blockr-tokens.css` defines the vocabulary in a `:root` block - the
colour palette, the meaning tokens built on it (text, backgrounds,
borders, status colours), type, radii, control heights and shadows -
`blockr-tokens-dark.css` restates them under `data-bs-theme="dark"`, the
attribute blockr.core's dark-mode board option sets, and
`blockr-theme.css` applies them to the host app.

## Usage

``` r
theme_dep()
```

## Value

An
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html).

## Details

Attach it once, from the app's UI. The theme layer is deliberately
unscoped: it restyles Bootstrap typography, labels, form controls,
selectize, buttons, tooltips, popovers and the DataTables chrome across
the whole page, so an app opts into it rather than picking it up from a
component. The token block on its own is inert - nothing is styled by
defining a custom property.

Consuming packages reference the tokens as
`var(--blockr-grey-400, #9ca3af)`, with a literal fallback for hosts
that do not attach this dependency.

## Examples

``` r
shiny::fluidPage(
  theme_dep(),
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
