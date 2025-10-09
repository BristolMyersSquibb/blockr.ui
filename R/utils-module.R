#' Create a board module
#'
#' Extend a DAG board by adding modules.
#'
#' @param ui,server UI and server functions
#' @param on_restore Function called when restoring the board state.
#' @param id,title Module ID and title
#' @param context_menu List of context menu entries
#' @param position Panel position
#' @param options Module options (see [blockr.core::new_board_options()]).
#' @param class (Optional) additional class(es)
#'
#' @export
#' @rdname board-module
new_board_module <- function(
  ui,
  server,
  on_restore = function(board, parent, session, ...) {
    TRUE
  },
  id,
  title,
  context_menu = list(),
  position = NULL,
  options = new_board_options(),
  class = character()
) {
  if (is_context_menu_entry(context_menu)) {
    context_menu <- list(context_menu)
  }

  stopifnot(
    is.function(ui),
    is.function(server),
    is.function(on_restore),
    is_string(id),
    is_string(title),
    is.list(context_menu),
    all(lgl_ply(context_menu, is_context_menu_entry))
  )

  structure(
    list(
      server = server,
      ui = ui,
      on_restore = on_restore,
      context_menu = context_menu,
      options = as_board_options(options)
    ),
    id = id,
    title = title,
    position = position,
    class = c(class, "board_module"),
    ctor = deparse(sys.call(sys.parent())[[1]]),
    ctor_pkg = pkg_name(parent.frame())
  )
}

is_board_module <- function(x) {
  inherits(x, "board_module")
}

board_module_server <- function(x) {
  stopifnot(is_board_module(x))
  srv_func <- x[["server"]]
  formals(srv_func)[["id"]] <- board_module_id(x)
  srv_func
}

board_module_ui <- function(x) {
  stopifnot(is_board_module(x))
  x[["ui"]]
}

board_module_options <- function(x) {
  stopifnot(is_board_module(x))
  x[["options"]]
}

board_module_id <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "id")
}

board_module_title <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "title")
}

board_module_context_menu <- function(x) {
  stopifnot(is_board_module(x))
  x[["context_menu"]]
}

board_module_position <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "position")
}

board_module_on_restore <- function(x) {
  stopifnot(is_board_module(x))
  x[["on_restore"]]
}

board_module_positions <- function(x) {
  res <- lapply(x, board_module_position)

  nul <- lgl_ply(res, is.null)

  if (nul[[1L]]) {
    res[[1L]] <- list(referencePanel = "dag", direction = "right")
  }

  if (any(nul[-1L])) {
    res[setdiff(which(nul), 1L)] <- rep(
      list(
        list(referencePanel = board_module_id(x[[1L]]), direction = "within")
      ),
      sum(nul[-1L])
    )
  }

  res
}

call_board_module_ui <- function(x, ...) {
  board_module_ui(x)(...)
}

board_modules <- function(board) {
  stopifnot(is_dag_board(board))

  modules <- board[["modules"]]

  stopifnot(
    is.list(modules),
    all(lgl_ply(modules, is_board_module)),
    length(unique(chr_ply(modules, board_module_id))) == length(modules)
  )

  names(modules) <- chr_ply(modules, board_module_id)

  modules
}
