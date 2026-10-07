# Dropdown

A panel that opens under a button and stays open while it is worked in:
a search field, a list edited in place, a checkbox. A click outside,
Escape or opening another dropdown closes it; a click inside does not.
That is the difference from
[`action_menu()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/action_menu.md),
whose rows each do one thing and close the menu.

## Usage

``` r
dropdown(toggle, ..., align = c("start", "end"))
```

## Arguments

- toggle:

  The button that opens the panel, as an
  [htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html).

- ...:

  The panel's content.

- align:

  Which edge of the toggle the panel lines up with: `"start"`, the left,
  or `"end"`, the right, for a toggle at the end of a bar, where a panel
  opening to the right would leave the window.

## Value

A
[htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
carrying
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md).

## Details

The panel holds ordinary Shiny markup, inputs and outputs included. On a
click on the toggle, `Blockr.dropdown` (in `blockr-ui.js`, loaded by
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md))
opens it and, while it is open, moves it to the page body and places it
with `Blockr.place`, so no panel's overflow clips it; it goes back
beside the toggle when it closes. Style the panel through its own
classes, as it is not inside the dropdown's ancestors while open.

## Examples

``` r
dropdown(
  htmltools::tags$button(type = "button", class = "btn", "Views"),
  shiny::textInput("search", "Search"),
  shiny::checkboxInput("all", "Show all views")
)
#> <div class="blockr-dropdown" data-align="start">
#>   <button aria-expanded="false" class="btn blockr-dropdown__toggle" type="button">Views</button>
#>   <div class="blockr-dropdown__panel blockr-menu">
#>     <div class="form-group shiny-input-container">
#>       <label class="control-label" id="search-label" for="search">Search</label>
#>       <input id="search" type="text" class="shiny-input-text form-control" value="" data-update-on="change"/>
#>     </div>
#>     <div class="form-group shiny-input-container">
#>       <div class="checkbox">
#>         <label>
#>           <input id="all" type="checkbox" class="shiny-input-checkbox"/>
#>           <span>Show all views</span>
#>         </label>
#>       </div>
#>     </div>
#>   </div>
#> </div>
```
