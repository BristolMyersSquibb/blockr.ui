#' Main ui for blockr2
#'
#' The board is composed of 2 views pointing to the network module
#' or the grid/dashboard module.
#'
#' @param id Unique id.
#' @param board Board object.
#' @param plugins Board plugins.
#' @rdname main
#' @export
main_ui <- function(id, board, board_id, plugins = board_plugins(board)) {
  board_ui(NS(id, board_id), board, plugins)
}

create_app_state <- function(board) {
  reactiveValues(
    cold_start = length(board_blocks(board)) == 0L,
    refreshed = NULL,
    network = list(),
    app_layout = list(),
    # Blocks/nodes
    append_block = FALSE,
    added_block = NULL,
    removed_block = NULL,
    selected_block = NULL,
    unselected_block = NULL,
    # Edges
    cancelled_edge = NULL,
    added_edge = NULL,
    removed_edge = NULL,
    # Stacks
    added_stack = NULL,
    stack_added_block = NULL,
    stack_removed_block = NULL,
    removed_stack = NULL,
    stacks = NULL,
    # scoutbar
    open_scoutbar = FALSE,
    scoutbar = list(
      trigger = NULL,
      action = NULL,
      value = NULL,
      is_open = FALSE
    ),
    # For snapshots
    save_board = FALSE,
    backup_list = list(),
    # For code generation
    display_code = FALSE,
    # Dashboard
    module_state = list()
  )
}

#' Main server function
#'
#' Server module for board.
#'
#' @param board_id Board ID.
#'
#' @rdname main
#' @export
main_server <- function(id, board, board_id, plugins = board_plugins(board)) {
  modules <- board_modules(board)

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      app_state <- create_app_state(board)

      # For shinytest2 (don't remove)
      exportTestValues(res = process_app_state(app_state))

      # Board module
      res <- board_server(
        board_id,
        board,
        plugins = plugins,
        callbacks = c(
          lapply(modules, board_module_server),
          list(
            # Callback to signal other modules that the restore is done.
            # This allows to restore each part in the correct order.
            on_board_restore = board_restore,
            manage_scoutbar = manage_scoutbar,
            layout = build_layout(modules, plugins),
            update_block_ui = update_block_ui
          )
        ),
        parent = app_state
      )

      isolate(
        {
          app_state$module_state <- res[names(modules)]
        }
      )
    }
  )
}
