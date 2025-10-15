#' @export
blockr_ser.dag_board <- function(
  x,
  blocks = NULL,
  options = NULL,
  network = NULL,
  layout = NULL,
  modules = NULL,
  ...
) {
  list(
    object = class(x),
    board = NextMethod(),
    network = network,
    layout = layout,
    modules = modules,
    version = as.character(pkg_version())
  )
}

#' @export
blockr_ser.board_module <- function(x, state, ...) {
  list(
    object = class(x),
    payload = list(
      state = state[[attr(x, "id")]]
    ),
    constructor = attr(x, "ctor"),
    package = attr(x, "ctor_pkg"),
    version = as.character(pkg_version())
  )
}

#' @export
blockr_deser.dag_board <- function(x, data, ...) {
  list(
    board = NextMethod(data = data[["board"]]),
    # Other elements that are not part of the board
    # and need to be restored at the top level
    network = data[["network"]],
    layout = data[["layout"]],
    modules = data[["modules"]]
  )
}

#' @export
serialize_board.dag_board <- function(
  x,
  blocks,
  parent,
  ...,
  session = get_session()
) {
  blocks <- lapply(
    lst_xtr(blocks, "server", "state"),
    lapply,
    reval_if
  )

  opts <- lapply(
    set_names(nm = names(as_board_options(x))),
    get_board_option_or_null,
    session
  )

  blockr_ser(
    x,
    blocks = blocks,
    options = opts,
    network = parent$network,
    layout = parent$app_layout,
    modules = lapply(
      board_modules(x),
      blockr_ser,
      state = lapply(parent$module_state, reval)
    )
  )
}

#' @export
restore_board.dag_board <- function(
  x,
  new,
  result,
  parent,
  ...,
  session = get_session()
) {
  tryCatch(
    {
      tmp_res <- blockr_deser(new)
      tmp_res$board[["modules"]] <- lapply(tmp_res$modules, function(mod) {
        fun <- get(
          mod$constructor,
          envir = asNamespace(mod$package),
          mode = "function"
        )
        res <- fun()
        attr(res, "ctor") <- mod$constructor
        res
      })
      result(tmp_res$board)
      # Update parent node, grid, selected, mode
      # that were stored in the JSON but not part of the board object.
      parent$network <- tmp_res$network
      parent$app_layout <- tmp_res$layout

      mods <- intersect(names(parent$module_state), names(tmp_res$modules))

      miss <- setdiff(names(parent$module_state), names(tmp_res$modules))
      xtra <- setdiff(names(tmp_res$modules), names(parent$module_state))

      if (length(miss)) {
        showNotification(
          paste0(
            "Attempting to restore a board with missing module settings for: ",
            paste_enum(miss),
            ". These will be reset."
          ),
          duration = NA,
          type = "warning",
          session = session
        )
      }

      if (length(xtra)) {
        showNotification(
          paste0(
            "Attempting to restore a board with extra module settings for: ",
            paste_enum(xtra),
            ". These will be ignored."
          ),
          duration = NA,
          type = "warning",
          session = session
        )
      }

      for (mod in mods) {
        parent$module_state[[mod]](tmp_res$modules[[mod]]$payload$state)
      }
    },
    error = function(e) {
      showNotification(
        paste0(
          "Error restoring snapshot. It is possible that you are trying to ",
          "restore an old state that is not compatible with the current ",
          "version."
        ),
        tags$details(
          tags$summary("Details"),
          tags$small(e$message)
        ),
        duration = NA,
        type = "error",
        session = session
      )
    }
  )
}
