restore_dashboard <- function(board, rv, parent, session) {
  parent$in_grid <- list()
  ids <- names(rv$blocks)
  # Find blocks that should be in the dock
  in_grid_ids <- find_blocks_ids(rv$board, parent, session)

  # Don't restore if no blocks
  if (!length(ids)) {
    return(NULL)
  }

  # When the dock was empty, we still need to initialise the block state
  # and all values are false
  if (!length(in_grid_ids)) {
    lapply(ids, function(id) {
      parent$in_grid[[id]] <- FALSE
    })
    return(NULL)
  }

  # Otherwise we spread elements between the dock and the network
  not_in_grid <- which(!(ids %in% in_grid_ids))

  # Restore dock layout (panels positions and sizes). No need
  # to cleanup before
  dockViewR::restore_dock("dock", parent$module_state$dashboard())

  # Regenerate the output for the block
  lapply(in_grid_ids, function(id) {
    parent$in_grid[[id]] <- TRUE
    generate_dashboard_blk_output(id, rv, session)
  })

  lapply(ids[not_in_grid], function(id) {
    parent$in_grid[[id]] <- FALSE
  })
}

generate_dashboard_blk_output <- function(id, rv, session) {
  output <- session$output
  out_name <- sprintf(
    "dock-%s-result",
    id
  )

  observeEvent(
    {
      req(id %in% board_block_ids(rv$board))
      reactiveValuesToList(rv$blocks[[id]]$server$cond)
      block_render_trigger(
        board_blocks(rv$board)[[id]],
        session = session
      )
      rv$blocks[[id]]$server$result()
    },
    {
      output[[out_name]] <- block_output(
        rv$blocks[[id]]$block,
        {
          # Provide user feedback in the dashboard
          # to explain why an output is blank. shiny.emptystate
          # could also be a more polished alternative ...
          validate(
            need(
              rv$blocks[[id]]$server$result(),
              "Not data available. Please update the pipeline."
            )
          )
          rv$blocks[[id]]$server$result()
        },
        session
      )
    },
    ignoreNULL = FALSE
  )
}

add_blk_panel_to_dashboard <- function(id, rv, session) {
  ns <- session$ns
  dock_blk_ui <- block_ui(
    ns(
      sprintf(
        "dock-%s",
        id
      )
    ),
    rv$blocks[[id]]$block
  )

  add_panel(
    "dock",
    sprintf("block_%s", id),
    panel = dockViewR::panel(
      id = sprintf("block-%s", id),
      title = sprintf("Block: %s", id),
      content = dock_blk_ui
    )
  )
}

remove_blk_from_dashboard <- function(id, session) {
  output <- session$output
  out_name <- sprintf("dock-%s-result", id)
  remove_panel("dock", sprintf("block-%s", id))
  output[[out_name]] <- NULL
}

find_blocks_ids <- function(
  board,
  parent,
  session
) {
  state <- parent$module_state$dashboard()
  if (!length(state) || !length(state$panels)) {
    return(NULL)
  }
  gsub("block-", "", names(state$panels))
}

handle_dashboard_zoom <- function(session) {
  ns <- session$ns
  session$sendCustomMessage(
    "update-dashboard-zoom",
    list(
      id = sprintf("#%s", ns("dashboard_zoom_target")),
      zoom = get_board_option_value("dashboard_zoom", session = session)
    )
  )
}
