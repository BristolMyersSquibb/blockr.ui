#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID.
#' @param board Reactive values object, containing board informations.
#' @param update Reactive value object to initiate board updates.
#' @param parent App state
#' @param ... Extra arguments passed from parent scope. Useful to communicate
#' between plugins and surface information at the top level (for testing ...).
#'
#' @return NULL.
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(id, board, update, parent, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Adding a block, we update the rv$added so the graph is updated
      # in the links plugin
      observeEvent(
        {
          req(parent$scoutbar$action == "add_block")
          parent$scoutbar$value
        },
        {
          # Allow to create block with custom parameters
          if (is_blocks(parent$scoutbar$value)) {
            new_blk <- parent$scoutbar$value
          } else {
            new_id <- rand_names(board_block_ids(board$board))
            new_blk <- create_block(
              parent$scoutbar$value,
              name = id_to_sentence_case(new_id)
            )
            new_blk <- as_blocks(set_names(list(new_blk), new_id))
          }

          update(
            list(blocks = list(add = new_blk))
          )
          parent$added_block <- new_blk[[1]]
          attr(parent$added_block, "uid") <- names(new_blk)
        }
      )

      # Remove block (triggered from the links module or from this module)
      observeEvent(parent$removed_block, {
        update(
          list(blocks = list(rm = parent$removed_block))
        )
      })

      # When a edge creation was cancelled
      observeEvent(parent$cancelled_edge, {
        update(
          list(blocks = list(rm = parent$cancelled_edge))
        )
      })

      # Reset added_block (no need to keep some old state)
      observeEvent(update()$blocks$rm, {
        parent$added_block <- NULL
      })

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_block
#' @export
add_rm_block_ui <- function(id, board) {
  NULL
}
