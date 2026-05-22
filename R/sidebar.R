#' Sidebar primitive
#'
#' A Bootstrap-free slide-in panel mirroring [shiny::showModal()] /
#' [shiny::removeModal()] in spirit. `sidebar_ui()` mounts an empty panel
#' once per page; `show_sidebar()` and `hide_sidebar()` drive its content
#' from any `moduleServer()` body, exactly like a modal.
#'
#' Each panel id is a single concern: `show_sidebar()` replaces the
#' panel's body and title in place, and pin state is bound to the panel
#' element. Re-showing with fresh content from the same caller is the
#' supported pattern (see [keep_or_hide_sidebar()]). Two unrelated
#' concerns should each mount their own `sidebar_ui(id)` - reusing one
#' id from different callers will silently overwrite a pinned panel's
#' content the next time `show_sidebar()` is called against it.
#'
#' @param id A character scalar. The DOM id used for the panel element. The
#'   same id is what `show_sidebar()` / `hide_sidebar()` target and what R
#'   reads as `input[[id]]` to observe `{open, pinned}` state.
#' @param side Which side of the viewport the panel slides in from.
#' @param width A CSS length applied to the panel as
#'   `--blockr-sidebar-panel-width`. The CSS bundle uses that variable for
#'   the panel's `width`.
#' @param mode One of `"overlay"` (default) or `"push"`. In `"overlay"`
#'   mode the panel hovers above the page and the rest of the layout is
#'   untouched. In `"push"` mode the bundled CSS adds
#'   `padding-<side>: var(--blockr-sidebar-width)` on `<body>` whenever
#'   the panel is open, so the page content shifts aside instead of being
#'   covered. The choice is independent of pin state.
#' @param ui A Shiny tag tree to render inside the panel body. Pre-rendered
#'   via [htmltools::renderTags()] so dependencies travel with the content.
#' @param title Optional title shown in the panel header. `NULL` clears it.
#' @param session The Shiny session. Defaults to the calling reactive
#'   domain - same as [shiny::showModal()].
#'
#' @return
#' - `sidebar_ui()` returns an [htmltools::tag] with `sidebar_dep()`
#'   attached.
#' - `sidebar_dep()` returns an [htmltools::htmlDependency].
#' - `show_sidebar()` and `hide_sidebar()` are called for their side
#'   effect (a custom message to the client) and return `invisible(NULL)`.
#' - `sidebar_state()` returns a list `list(open, pinned)` of logicals,
#'   read from the root session's input. Useful when an action handler
#'   wants to decide whether to chain (re-render the form) or close the
#'   panel after a successful confirm: pinned panels should stay open
#'   per the user's opt-in, unpinned panels close.
#' - `keep_or_hide_sidebar()` is the convenience built on top of
#'   `sidebar_state()` for the action-handler confirm flow: when the
#'   panel is pinned it re-shows with the supplied (fresh) `ui` so the
#'   user can chain another action; otherwise it hides the panel.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     actionButton("open", "Open sidebar"),
#'     sidebar_ui("panel", side = "right")
#'   )
#'   server <- function(input, output, session) {
#'     observeEvent(input$open, {
#'       show_sidebar(
#'         "panel",
#'         title = "Hello",
#'         ui = tagList(
#'           textInput("name", "Name"),
#'           actionButton("ok", "OK")
#'         )
#'       )
#'     })
#'     observeEvent(input$ok, hide_sidebar("panel"))
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @name sidebar
NULL

#' @rdname sidebar
#' @export
sidebar_ui <- function(id, ui = NULL, title = NULL,
                       side = c("right", "left"), width = "360px",
                       mode = c("overlay", "push")) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(width), length(width) == 1L, nzchar(width),
    is.null(title) || (is.character(title) && length(title) == 1L)
  )
  side <- match.arg(side)
  mode <- match.arg(mode)

  # Body slot: empty by default ("modal-like" use; populate later via
  # show_sidebar()) or pre-rendered from `ui` ("offcanvas-like" use; the
  # panel is ready to open with no server round-trip via the data-attribute
  # trigger or `show_sidebar(id)` with no `ui`).
  body_children <- list()
  body_deps <- list()
  if (!is.null(ui)) {
    rendered <- htmltools::renderTags(ui)
    body_children <- list(htmltools::HTML(rendered$html))
    body_deps <- rendered$dependencies
  }

  panel <- shiny::tags$div(
    id = id,
    class = "blockr-sidebar",
    `data-side` = side,
    `data-mode` = mode,
    `aria-hidden` = "true",
    tabindex = "-1",
    style = paste0("--blockr-sidebar-panel-width: ", width, ";"),
    shiny::tags$header(
      class = "blockr-sidebar-header",
      shiny::tags$h2(class = "blockr-sidebar-title", title),
      shiny::tags$div(
        class = "blockr-sidebar-actions",
        shiny::tags$button(
          type = "button",
          class = "blockr-sidebar-btn blockr-sidebar-pin",
          `aria-label` = "Pin sidebar",
          `aria-pressed` = "false",
          title = "Pin",
          pin_icon()
        ),
        shiny::tags$button(
          type = "button",
          class = "blockr-sidebar-btn blockr-sidebar-close",
          `aria-label` = "Close sidebar",
          title = "Close",
          "\u00d7"
        )
      )
    ),
    do.call(
      shiny::tags$div,
      c(
        list(
          class = "blockr-sidebar-body",
          role = "region",
          `aria-live` = "polite"
        ),
        body_children
      )
    )
  )

  htmltools::attachDependencies(panel, c(list(sidebar_dep()), body_deps))
}

#' @rdname sidebar
#' @export
sidebar_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-sidebar",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = "css/blockr-sidebar.css",
    script = "js/blockr-sidebar.js",
    all_files = FALSE
  )
}

#' @rdname sidebar
#' @export
show_sidebar <- function(id, ui = NULL, title = NULL,
                         session = shiny::getDefaultReactiveDomain()) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.null(title) || (is.character(title) && length(title) == 1L)
  )
  root <- root_session(session)

  # Two modes:
  #   * `ui` non-NULL: full content-swap (modal-like). Pre-render via
  #     renderTags and ship html + deps.
  #   * `ui = NULL`: open-only. The body was either pre-rendered at
  #     `sidebar_ui(ui = ...)` time or left in place by an earlier show.
  #     Skip the html / deps fields so the JS skips the swap sequence.
  payload <- list(action = "show")
  if (!is.null(ui)) {
    rendered <- htmltools::renderTags(ui)
    payload$html <- rendered$html
    payload$dependencies <- lapply(
      htmltools::resolveDependencies(rendered$dependencies),
      shiny::createWebDependency
    )
  }
  if (!is.null(title)) {
    payload$title <- title
  }
  root$sendInputMessage(id, payload)
  invisible(NULL)
}

#' @rdname sidebar
#' @export
hide_sidebar <- function(id, session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  root <- root_session(session)

  root$sendInputMessage(id, list(action = "hide"))
  invisible(NULL)
}

#' @rdname sidebar
#' @export
sidebar_state <- function(id, session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  root <- root_session(session)

  # Snapshot read: callers (typically `observeEvent` confirm handlers)
  # want the current value without creating a reactive dependency on it.
  state <- shiny::isolate(root$input[[id]])
  if (is.null(state)) {
    list(open = FALSE, pinned = FALSE)
  } else {
    state
  }
}

#' @rdname sidebar
#' @export
keep_or_hide_sidebar <- function(id, ui, title = NULL,
                                 session = shiny::getDefaultReactiveDomain()) {
  if (isTRUE(sidebar_state(id, session = session)$pinned)) {
    show_sidebar(id, ui = ui, title = title, session = session)
  } else {
    hide_sidebar(id, session = session)
  }
}

# Inline SVG pushpin (Bootstrap-icons "pin"). Swapped in for an earlier
# `▣` ("WHITE SQUARE WITH ROUNDED CORNERS") that no font rendered as
# a recognisable pin. SVG keeps the icon legible across platforms with
# no extra dep, and `fill="currentColor"` lets the existing pinned-state
# colour rule (`.blockr-sidebar-pinned .blockr-sidebar-pin`) drive it.
pin_icon <- function() {
  shiny::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    fill = "currentColor",
    `aria-hidden` = "true",
    shiny::tags$path(
      d = paste0(
        "M9.828.722a.5.5 0 0 1 .354.146l4.95 4.95a.5.5 0 0 1 0 .707",
        "c-.48.48-1.072.588-1.503.588-.177 0-.335-.018-.46-.039",
        "l-3.134 3.134a5.927 5.927 0 0 1 .16 1.013c.046.702-.032 ",
        "1.687-.72 2.375a.5.5 0 0 1-.707 0l-2.829-2.828-3.182 ",
        "3.182c-.195.195-1.219.902-1.414.707-.195-.195.512-1.22",
        ".707-1.414l3.182-3.182-2.828-2.829a.5.5 0 0 1 0-.707",
        "c.688-.688 1.673-.767 2.375-.72a5.922 5.922 0 0 1 1.013",
        ".16l3.134-3.133a2.772 2.772 0 0 1-.04-.461c0-.43.108-",
        "1.022.589-1.503a.5.5 0 0 1 .353-.146z"
      )
    )
  )
}

# Walk to the root session so `sendInputMessage(id, ...)` targets the
# absolute DOM id, not the calling module's namespaced id. The panel
# id passed to `sidebar_ui()` is a free-form DOM id chosen by the
# caller and is what `show_sidebar()` / `hide_sidebar()` target.
root_session <- function(session) {
  if (is.null(session)) {
    rlang::abort(
      paste(
        "`show_sidebar()` / `hide_sidebar()` must be called from",
        "within a Shiny session."
      ),
      class = "blockr_ui_no_session"
    )
  }
  session$rootScope()
}
