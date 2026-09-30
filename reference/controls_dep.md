# Shared block controls

The JavaScript and CSS of the controls blockr blocks are built from:
`Blockr.Select` (single, multi and menu), `Blockr.Input` (the code field
with column and function completions), the Enter button of a field that
commits on Enter, the required-empty cue, the checkbox, the segmented
control, the gear tray, the tooltip (`Blockr.tooltip`), the menus of
actions (`Blockr.menu` for those built in JavaScript,
`Blockr.actionMenu` for those
[`action_menu()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/action_menu.md)
builds in R), the placement routine for floating panels (`Blockr.place`)
and the small icons (`Blockr.icons`), all on the `window.Blockr`
namespace, together with the rows, pills, labels, fields and buttons
(`.blockr-btn`) they draw.

## Usage

``` r
controls_dep()

small_icon(name)
```

## Arguments

- name:

  The icon: `"chevron"`, `"remove"` (a tag's x, 10px), `"x"` (a row's
  remove button, 14px), `"grip"` (a drag handle), `"plus"`, `"trash"`,
  `"sliders"`, `"check"` (a menu's current item), `"confirm"` (a
  committed field, a picked option), `"code"` or `"gear"`.

## Value

For `controls_dep()`, an
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
of
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects, in load order.

For `small_icon()`, the icon's `<svg>` element, as
[`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

## Details

The small icons are not in `blockr-ui.js`: the dependency writes them
into the page's head ahead of it, from the list `small_icon()` reads, so
markup built in R and markup built in JavaScript draw one copy of each,
and `small_icon("gear")` is `Blockr.icons.gear`. An icon draws in
`currentColor`, so it takes the colour of the text around it, and is
hidden from screen readers: the control that holds it carries the name,
as
[`tool_button()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/tool_button.md)
does.

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

tool_button(small_icon("gear"), "Settings")
#> <button type="button" class="blockr-tool" aria-label="Settings" data-blockr-tooltip="Settings"><svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16" aria-hidden="true"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg></button>
```
