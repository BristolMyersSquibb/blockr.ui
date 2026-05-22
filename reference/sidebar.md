# Sidebar primitive

A Bootstrap-free slide-in panel mirroring
[`shiny::showModal()`](https://rdrr.io/pkg/shiny/man/showModal.html) /
[`shiny::removeModal()`](https://rdrr.io/pkg/shiny/man/showModal.html)
in spirit. `sidebar_ui()` mounts an empty panel once per page;
`show_sidebar()` and `hide_sidebar()` drive its content from any
[`moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
body, exactly like a modal.

## Usage

``` r
sidebar_ui(
  id,
  ui = NULL,
  title = NULL,
  side = c("right", "left"),
  width = "360px",
  mode = c("overlay", "push")
)

sidebar_dep()

show_sidebar(
  id,
  ui = NULL,
  title = NULL,
  session = shiny::getDefaultReactiveDomain()
)

hide_sidebar(id, session = shiny::getDefaultReactiveDomain())

sidebar_state(id, session = shiny::getDefaultReactiveDomain())

keep_or_hide_sidebar(
  id,
  ui,
  title = NULL,
  session = shiny::getDefaultReactiveDomain()
)
```

## Arguments

- id:

  A character scalar. The DOM id used for the panel element. The same id
  is what `show_sidebar()` / `hide_sidebar()` target and what R reads as
  `input[[id]]` to observe `{open, pinned}` state.

- ui:

  A Shiny tag tree to render inside the panel body. Pre-rendered via
  [`htmltools::renderTags()`](https://rstudio.github.io/htmltools/reference/renderTags.html)
  so dependencies travel with the content.

- title:

  Optional title shown in the panel header. `NULL` clears it.

- side:

  Which side of the viewport the panel slides in from.

- width:

  A CSS length applied to the panel as `--blockr-sidebar-panel-width`.
  The CSS bundle uses that variable for the panel's `width`.

- mode:

  One of `"overlay"` (default) or `"push"`. In `"overlay"` mode the
  panel hovers above the page and the rest of the layout is untouched.
  In `"push"` mode the bundled CSS adds
  `padding-<side>: var(--blockr-sidebar-width)` on `<body>` whenever the
  panel is open, so the page content shifts aside instead of being
  covered. The choice is independent of pin state.

- session:

  The Shiny session. Defaults to the calling reactive domain - same as
  [`shiny::showModal()`](https://rdrr.io/pkg/shiny/man/showModal.html).

## Value

- `sidebar_ui()` returns an
  [htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html)
  with `sidebar_dep()` attached.

- `sidebar_dep()` returns an
  [htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html).

- `show_sidebar()` and `hide_sidebar()` are called for their side effect
  (a custom message to the client) and return `invisible(NULL)`.

- `sidebar_state()` returns a list `list(open, pinned)` of logicals,
  read from the root session's input. Useful when an action handler
  wants to decide whether to chain (re-render the form) or close the
  panel after a successful confirm: pinned panels should stay open per
  the user's opt-in, unpinned panels close.

- `keep_or_hide_sidebar()` is the convenience built on top of
  `sidebar_state()` for the action-handler confirm flow: when the panel
  is pinned it re-shows with the supplied (fresh) `ui` so the user can
  chain another action; otherwise it hides the panel.

## Details

Each panel id is a single concern: `show_sidebar()` replaces the panel's
body and title in place, and pin state is bound to the panel element.
Re-showing with fresh content from the same caller is the supported
pattern (see `keep_or_hide_sidebar()`). Two unrelated concerns should
each mount their own `sidebar_ui(id)` - reusing one id from different
callers will silently overwrite a pinned panel's content the next time
`show_sidebar()` is called against it.

A panel can be opened in two complementary ways. The original
"modal-like" form is `show_sidebar(id, ui = <tags>)` from the server,
rendered on demand. The "offcanvas-like" form pre-renders the body at
`sidebar_ui()` time (pass `ui` and optionally `title`) and opens
client-side: any element on the page carrying
`data-blockr-sidebar-target = "<id>"` toggles the matching panel on
click, with no R round-trip. This is the right model for a static
settings panel triggered by a navbar icon - the click latency stays at
one frame instead of a full Shiny round-trip. The two forms coexist on
the same DOM mount: a later `show_sidebar(id, ui = ...)` replaces the
pre-rendered body if the consumer later wants dynamic behaviour, and
`show_sidebar(id)` (no `ui`) opens a pre-rendered panel from the server
without re-shipping the HTML.

## Examples

``` r
if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    actionButton("open", "Open sidebar"),
    sidebar_ui("panel", side = "right")
  )
  server <- function(input, output, session) {
    observeEvent(input$open, {
      show_sidebar(
        "panel",
        title = "Hello",
        ui = tagList(
          textInput("name", "Name"),
          actionButton("ok", "OK")
        )
      )
    })
    observeEvent(input$ok, hide_sidebar("panel"))
  }
  shinyApp(ui, server)
}
```
