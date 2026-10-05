# Gear and settings tray

The gear button and the tray it opens, for a block whose UI is rendered
in R. The gear is the last thing in the header row, after any `tools`;
the tray opens in flow under it, slides open and closed, and closes on
the gear or Escape. `Blockr.gearTray` (in `blockr-ui.js`) does this; a
Shiny input binding wires it up, so there is no inline script, and
`input[[id]]` is `TRUE` while the tray is open. A tray drawn again (a
`renderUI()`) keeps the state it had, for the session.

## Usage

``` r
gear_tray(id, ..., tools = NULL, label = "Settings")

tray_section(title = NULL, ..., toggle = NULL, value = FALSE)
```

## Arguments

- id:

  The gear's id, an input id. The tray's is `<id>_tray`.

- ...:

  For `gear_tray()`, `tray_section()`s, or fields for a tray of one
  section. For `tray_section()`, its fields: Shiny's inputs, or any
  other tag. Each goes in a cell of the tray's grid, which gives Shiny's
  inputs the design system's sizes: a number or a checkbox takes one
  column, a multi select the whole row, anything else two.

- tools:

  Tool buttons
  ([`tool_button()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/tool_button.md),
  an
  [`action_menu()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/action_menu.md))
  that stand left of the gear in the header row.

- label:

  The tray's accessible name.

- title:

  The section title, or `NULL`. With `toggle`, the label of the checkbox
  that switches the section on.

- toggle:

  The id of a checkbox that switches the section on, or `NULL` for a
  section that is always on.

- value:

  Whether the `toggle` checkbox starts checked.

## Value

`gear_tray()`: an
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
of the header row and the tray, carrying
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md).
`tray_section()`: a section, for `gear_tray()` only.

## Details

The tray holds sections, each a title over a grid of fields. A tray with
one section shows no title. A section switched on by a checkbox
(`toggle`) keeps the checkbox before its title, and its fields show only
while it is checked; the checkbox is an input of its own,
`input[[toggle]]`.

The fields are Shiny's own inputs, with `updateOn = "blur"` for
[`shiny::textInput()`](https://rdrr.io/pkg/shiny/man/textInput.html) and
[`shiny::numericInput()`](https://rdrr.io/pkg/shiny/man/numericInput.html),
so their value changes on Enter or blur only. An open
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
closes on Escape before the tray does.

## Examples

``` r
gear_tray(
  "gear",
  tray_section(
    "Format",
    shiny::selectInput("sep", "Separator", c(",", ";", "\t")),
    shiny::checkboxInput("header", "First row is a header", TRUE)
  ),
  tray_section(
    "Skip rows",
    shiny::numericInput("skip", "Rows", 0, updateOn = "blur"),
    toggle = "skip_on"
  )
)
#> <div class="blockr-gear-header">
#>   <button id="gear" type="button" class="blockr-gear-btn blockr-ui-gear" aria-controls="gear_tray" aria-expanded="false" aria-label="Settings" data-blockr-tooltip="Settings"><svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" viewBox="0 0 16 16" aria-hidden="true"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg></button>
#> </div>
#> <div id="gear_tray" class="blockr-settings blockr-settings--beak" role="region" aria-label="Settings">
#>   <div class="blockr-settings__title">Format</div>
#>   <div class="blockr-settings__grid">
#>     <div class="blockr-settings__field">
#>       <div class="form-group shiny-input-container">
#>         <label class="control-label" id="sep-label" for="sep">Separator</label>
#>         <div>
#>           <select id="sep" class="shiny-input-select"><option value="," selected>,</option>
#> <option value=";">;</option>
#> <option value="   ">   </option></select>
#>           <script type="application/json" data-for="sep" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>         </div>
#>       </div>
#>     </div>
#>     <div class="blockr-settings__field">
#>       <div class="form-group shiny-input-container">
#>         <div class="checkbox">
#>           <label>
#>             <input id="header" type="checkbox" class="shiny-input-checkbox" checked="checked"/>
#>             <span>First row is a header</span>
#>           </label>
#>         </div>
#>       </div>
#>     </div>
#>   </div>
#>   <div class="blockr-settings__title blockr-settings__title--toggle">
#>     <div class="form-group shiny-input-container">
#>       <div class="checkbox">
#>         <label>
#>           <input id="skip_on" type="checkbox" class="shiny-input-checkbox"/>
#>           <span>Skip rows</span>
#>         </label>
#>       </div>
#>     </div>
#>   </div>
#>   <div class="blockr-settings__grid">
#>     <div class="blockr-settings__field">
#>       <div class="form-group shiny-input-container">
#>         <label class="control-label" id="skip-label" for="skip">Rows</label>
#>         <input id="skip" type="number" class="shiny-input-number form-control" value="0" data-update-on="blur"/>
#>       </div>
#>     </div>
#>   </div>
#> </div>
```
