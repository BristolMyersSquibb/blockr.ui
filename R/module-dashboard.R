#' Dashboard UI
#' @param id Module id.
#' @rdname dashboard
dashboard_ui <- function(id, board, ...) {
  ns <- NS(id)
  div(
    id = ns("dashboard_zoom_target"),
    style = "zoom: 1;",
    dockViewOutput(ns("dock"), height = "100vh")
  )
}

#' Dashboard grid server
#'
#' @param board Board reactiveValues. Read-only.
#' @param update Update reactiveVal to signal change to the board.
#' @param session Shiny session.
#' @param parent Parent global reactiveValues.
#' @param ... Extra parameters.
#' @rdname dashboard
#' @export
dashboard_server <- function(
  id,
  board,
  update,
  session,
  parent,
  ...
) {
  moduleServer(
    id,
    function(input, output, session) {
      isolate(
        {
          parent$in_grid <- list()
          parent$added_to_dashboard <- NULL
          parent$removed_from_dashboard <- NULL
        }
      )

      input <- session$input
      ns <- session$ns
      output <- session$output

      res <- reactiveVal()

      observeEvent(
        {
          # Should not trigger on restore, only when the dashboard changes
          req(is.null(parent$refreshed))
          input$dock_state
        },
        {
          res(input$dock_state)
        }
      )

      # Whenever a new block is created, we initialise its grid state
      observeEvent(parent$added_block, {
        parent$in_grid[[block_uid(parent$added_block)]] <- FALSE
      })

      # Removed block(s) must not be referenced in the grid and
      # the panel must be removed from the dock.
      observeEvent(parent$removed_block, {
        lapply(parent$removed_block, function(removed) {
          # Signal to remove panel from dock.
          # Panel will be removed by manage_dashboard.
          parent$in_grid[[removed]] <- NULL
          if (paste0("block-", removed) %in% get_panels_ids("dock", session)) {
            remove_panel("dock", paste0("block-", removed))
          }
        })
      })

      # Initialise outputs for any existing block in the grid
      # TBD: this is not needed yet as we can't initialise a board
      # with existing blocks in the grid. Uncomment the code
      # below when we can do that.
      #lapply(
      #  isolate(parent$in_grid),
      #  function(id) {
      #    generate_dashboard_blk_output(
      #      id,
      #      board,
      #      session
      #    )
      #  }
      #)

      # Add panel to dashboard + handle secondary output
      observeEvent(
        {
          req(
            parent$added_to_dashboard,
            parent$in_grid[[parent$added_to_dashboard]]
          )
        },
        {
          add_blk_panel_to_dashboard(
            parent$added_to_dashboard,
            board,
            session
          )
          generate_dashboard_blk_output(
            parent$added_to_dashboard,
            board,
            session
          )
          parent$added_to_dashboard <- NULL
        }
      )

      # Toggle state for each selected block and update the state
      observeEvent(
        {
          req(parent$removed_from_dashboard)
          parent$in_grid[[parent$removed_from_dashboard]]
        },
        {
          # Remove output from dock
          remove_blk_from_dashboard(parent$removed_from_dashboard, session)
          parent$removed_from_dashboard <- NULL
        }
      )

      output$dock <- renderDockView({
        dock_view(
          panels = list(), # TBD handle when we initalise from a non empty dock
          # TBD: handle theme from global app options
          theme = "replit"
        )
      })

      # Handle zoom on grid element
      observeEvent(get_board_option_value("dashboard_zoom"), {
        handle_dashboard_zoom(session)
      })

      res
    }
  )
}

new_dashboard_zoom_option <- function(
  value = blockr_option("dashboard_zoom", 1),
  ...
) {
  new_board_option(
    id = "dashboard_zoom",
    default = value,
    ui = function(id) {
      numericInput(
        NS(id, "dashboard_zoom"),
        "Dashboard zoom factor",
        value,
        min = 0.5,
        max = 2,
        step = 0.1
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("dashboard_zoom", session),
        {
          updateNumericInput(
            session,
            "dashboard_zoom",
            value = get_board_option_value("dashboard_zoom", session)
          )
        }
      )
    },
    transform = function(x) as.numeric(x),
    ...
  )
}

#' @export
validate_board_option.dashboard_zoom_option <- function(x) {
  val <- board_option_value(NextMethod())

  if (!is_number(val) || val <= 0) {
    abort(
      "Expecting `dashboard_zoom` to represent a scalar positive number.",
      class = "board_options_dashboard_zoom_invalid"
    )
  }

  invisible(x)
}

#' @include context-menu.R
add_to_dashboard_ctxm <- new_context_menu_entry(
  name = "Add to dashboard",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      if (current.id === undefined) return;
      Shiny.setInputValue('%s', current.id, {priority: 'event'});
    }",
      ns("add_to_dashboard")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$add_to_dashboard,
      {
        parent$added_to_dashboard <- input$add_to_dashboard
        parent$in_grid[[parent$added_to_dashboard]] <- TRUE
      }
    )
  },
  condition = function(board, parent, target) {
    target$type == "node" &&
      (!target$id %in% names(parent$in_grid) || !parent$in_grid[[target$id]])
  }
)

#' @include context-menu.R
remove_from_dashboard_ctxm <- new_context_menu_entry(
  name = "Remove from dashboard",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      if (current.id === undefined) return;
      Shiny.setInputValue('%s', current.id, {priority: 'event'});
    }",
      ns("remove_from_dashboard")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$remove_from_dashboard,
      {
        parent$removed_from_dashboard <- input$remove_from_dashboard
        parent$in_grid[[parent$removed_from_dashboard]] <- FALSE
      }
    )
  },
  condition = function(board, parent, target) {
    target$type == "node" &&
      target$id %in% names(parent$in_grid) &&
      parent$in_grid[[target$id]]
  }
)

#' @export
#' @rdname board-module
new_dashboard_module <- function(id = "dashboard", title = "Dashboard") {
  new_board_module(
    dashboard_ui,
    dashboard_server,
    on_restore = function(board, parent, session, ...) {
      restore_dashboard(board$board, board, parent, session)
    },
    id = id,
    title = title,
    context_menu = list(
      add_to_dashboard_ctxm,
      remove_from_dashboard_ctxm
    ),
    options = new_dashboard_zoom_option(category = "Dashboard options"),
    class = "dashboard_module"
  )
}
