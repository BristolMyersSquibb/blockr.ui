#' Sidebar primitive
#'
#' A Bootstrap-free slide-in panel mirroring [shiny::showModal()] /
#' [shiny::removeModal()] in spirit. `sidebar_ui()` mounts an empty panel
#' once per page; `show_sidebar()` and `hide_sidebar()` drive its content
#' from any `moduleServer()` body, exactly like a modal.
#'
#' @param id A character scalar. The DOM id used for the panel element. The
#'   same id is what `show_sidebar()` / `hide_sidebar()` target and what R
#'   reads as `input[[id]]` to observe `{open, pinned}` state.
#' @param side Which side of the viewport the panel slides in from.
#' @param width A CSS length applied to the panel as
#'   `--blockr-sidebar-panel-width`. The CSS bundle uses that variable for
#'   the panel's `width`.
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
sidebar_ui <- function(id, side = c("right", "left"), width = "360px") {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(width), length(width) == 1L, nzchar(width)
  )
  side <- match.arg(side)

  panel <- shiny::tags$div(
    id = id,
    class = "blockr-sidebar",
    `data-side` = side,
    `aria-hidden` = "true",
    tabindex = "-1",
    style = paste0("--blockr-sidebar-panel-width: ", width, ";"),
    shiny::tags$header(
      class = "blockr-sidebar-header",
      shiny::tags$h2(class = "blockr-sidebar-title"),
      shiny::tags$div(
        class = "blockr-sidebar-actions",
        shiny::tags$button(
          type = "button",
          class = "blockr-sidebar-btn blockr-sidebar-pin",
          `aria-label` = "Pin sidebar",
          `aria-pressed` = "false",
          title = "Pin",
          "\u25a3"
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
    shiny::tags$div(
      class = "blockr-sidebar-body",
      role = "region",
      `aria-live` = "polite"
    )
  )

  htmltools::attachDependencies(panel, sidebar_dep())
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
show_sidebar <- function(id, ui, title = NULL,
                         session = shiny::getDefaultReactiveDomain()) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.null(title) || (is.character(title) && length(title) == 1L)
  )
  root <- root_session(session)

  rendered <- htmltools::renderTags(ui)
  root$sendInputMessage(id, list(
    action = "show",
    html = rendered$html,
    dependencies = lapply(
      htmltools::resolveDependencies(rendered$dependencies),
      shiny::createWebDependency
    ),
    title = title
  ))
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
