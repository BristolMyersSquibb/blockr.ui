# Shared block controls

The JavaScript and CSS of the controls blockr blocks are built from:
`Blockr.Select` (single, multi and menu), `Blockr.Input` (the code field
with column and function completions), the Enter button of a field that
commits on Enter, the required-empty cue, the checkbox, the segmented
control, the gear tray, the tooltip (`Blockr.tooltip`), the menus of
actions (`Blockr.menu` for those built in JavaScript,
`Blockr.actionMenu` for those
[`action_menu()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/action_menu.md)
builds in R) and the placement routine for floating panels
(`Blockr.place`), all on the `window.Blockr` namespace, together with
the rows, pills, labels, fields and buttons (`.blockr-btn`) they draw.

## Usage

``` r
controls_dep()
```

## Value

An
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
of
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects, in load order.

## Details

The stylesheets read the design tokens without fallbacks, so the
dependency brings the tokens along, but not the theme layer that
restyles the rest of the page: an app opts into that with
[`theme_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/theme_dep.md).
Attach it from a block's UI; dependencies are de-duplicated by name, so
any number of blocks can.

The dependency names are the ones blockr.dplyr used while these files
lived there (`blockr-select-js`, `blockr-select-css`,
`blockr-blocks-css`, `blockr-input-js`, `blockr-input-css`), so a page
carries one copy of each. Of two dependencies with one name, htmltools
keeps the higher version, so while blockr.dplyr still ships its own
copies, a page that has both uses blockr.dplyr's.

## Examples

``` r
shiny::fluidPage(
  controls_dep(),
  shiny::div(id = "cols"),
  shiny::tags$script(
    "Blockr.Select.multi(document.getElementById('cols'),",
    "  { options: ['mpg', 'cyl', 'disp'], selected: ['mpg'] });"
  )
)
#> <div class="container-fluid">
#>   <div id="cols"></div>
#>   <script>
#>     Blockr.Select.multi(document.getElementById('cols'),
#>       { options: ['mpg', 'cyl', 'disp'], selected: ['mpg'] });
#>   </script>
#> </div>
```
