# Action menu

A menu of actions opened by a button: downloads, Rename, Remove. A row
does one thing and the menu closes. That is the difference from a
select, which sets a value: nothing here is remembered, and there is no
current pick.

## Usage

``` r
action_menu(trigger, ..., align = c("end", "start"))

menu_item(x, meta = NULL, icon = NULL, danger = FALSE, disabled = NULL)

menu_section(title)

menu_divider()
```

## Arguments

- trigger:

  The button that opens the menu, typically a
  [`tool_button()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/tool_button.md).

- ...:

  Rows: `menu_item()`s, `menu_section()` titles and `menu_divider()`
  rules, in order. `NULL`s are dropped, so a row can be conditional.

- align:

  `"end"` lines the menu up with the trigger's right edge, for a trigger
  in a block's header row, where a menu opening to the right would leave
  the block; `"start"` with its left edge.

- x:

  The row's element: a
  [`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html),
  an
  [`shiny::actionLink()`](https://rdrr.io/pkg/shiny/man/actionButton.html),
  or any `<a>` or `<button>`. Its text is the row's label.

- meta:

  Text after the label, muted and right-aligned: the format of a
  download (".pptx"), a block ID, or a keyboard shortcut from
  [`shortcut()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/shortcut.md).

- icon:

  An icon before the label, as a tag or
  [`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

- danger:

  A removal: the row turns red under the pointer or keyboard focus, and
  stays plain at rest.

- disabled:

  `NULL` for a usable row, or the reason it is not usable: the row stays
  in the list, greyed, with the reason as its tooltip.

- title:

  A group title: the rows after it, up to the next title or divider,
  belong under it.

## Value

A
[htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
carrying
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md).

## Details

The rows are ordinary Shiny markup, so a
[`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)
or an
[`shiny::actionLink()`](https://rdrr.io/pkg/shiny/man/actionButton.html)
works as it is; `menu_item()` only dresses it as a row.
`Blockr.actionMenu` (in `blockr-ui.js`, loaded by
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md))
opens the list on a click on the trigger, places it on the page body
with `Blockr.place` so no panel's overflow clips it, and closes it again
on a pick, Escape, Tab or a click outside.

A download row works once Shiny has bound its handler, which it does
when the menu first shows, so for a moment on that first open the row is
still inert. To have it ready from the start, set
`shiny::outputOptions(output, "<id>", suspendWhenHidden = FALSE)` for
its output.

`menu_divider()` draws a rule between groups that have no title, such as
the ordinary actions and a destructive one after them.

## Examples

``` r
action_menu(
  tool_button(
    htmltools::HTML("&darr;"),
    "Download"
  ),
  menu_section("This patient"),
  menu_item(shiny::downloadLink("dl_pptx", "PowerPoint"), meta = ".pptx"),
  menu_item(shiny::downloadLink("dl_html", "Web page"), meta = ".html"),
  menu_section("This block"),
  menu_item(shiny::actionLink("remove", "Remove"), danger = TRUE)
)
#> <span class="blockr-action-menu" data-align="end">
#>   <button aria-expanded="false" aria-haspopup="menu" aria-label="Download" class="blockr-tool blockr-action-menu__trigger" data-blockr-tooltip="Download" type="button">&darr;</button>
#>   <div class="blockr-menu" role="menu" tabindex="-1" hidden>
#>     <div class="blockr-menu__title" role="presentation">This patient</div>
#>     <a aria-disabled="true" class="shiny-download-link disabled blockr-menu__item" download href="" id="dl_pptx" role="menuitem" tabindex="-1" target="_blank">
#>       <span class="blockr-menu__label">PowerPoint</span>
#>       <span class="blockr-menu__meta">.pptx</span>
#>     </a>
#>     <a aria-disabled="true" class="shiny-download-link disabled blockr-menu__item" download href="" id="dl_html" role="menuitem" tabindex="-1" target="_blank">
#>       <span class="blockr-menu__label">Web page</span>
#>       <span class="blockr-menu__meta">.html</span>
#>     </a>
#>     <div class="blockr-menu__title" role="presentation">This block</div>
#>     <a class="action-button action-link blockr-menu__item blockr-menu__item--danger" href="#" id="remove" role="menuitem" tabindex="-1"><span class="blockr-menu__label"><span class="action-label">Remove</span></span></a>
#>   </div>
#> </span>
```
