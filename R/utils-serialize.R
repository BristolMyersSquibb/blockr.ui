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

  mods <- lapply(data[["modules"]], blockr_deser)
  mods <- lapply(mods, function(mod) {
    mod[["options"]] <- new_board_options()
    mod
  })

  as_dag_board(
    c(
      NextMethod(data = data[["board"]]),
      # Other elements that are not part of the board
      # and need to be restored at the top level
      list(
        network = data[["network"]],
        layout = data[["layout"]],
        modules = mods
      )
    )
  )
}

#' @export
blockr_deser.board_module <- function(x, data, ...) {

  ctor <- get(
    data[["constructor"]],
    envir = asNamespace(data[["package"]]),
    mode = "function",
    inherits = FALSE
  )

  res <- do.call(ctor, list())

  attr(res, "state") <- data[["payload"]][["state"]]

  res
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

      mod_states <- lapply(tmp_res[["modules"]], attr, "state")

      tmp_res[["modules"]] <- lapply(
        tmp_res[["modules"]],
        `attr<-`,
        "state",
        NULL
      )

      result(tmp_res)

      mods <- intersect(names(parent$module_state), names(mod_states))

      miss <- setdiff(names(parent$module_state), names(mod_states))
      xtra <- setdiff(names(mod_states), names(parent$module_state))

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
        parent$module_state[[mod]](mod_states[[mod]])
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
