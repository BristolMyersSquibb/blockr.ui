#' Create a context menu entry
#'
#' Adds a new entry to the context menu of a board.
#'
#' @param name Name of the context menu entry.
#' @param js JavaScript code to execute when the entry is selected.
#' @param action Action to perform when the entry is selected.
#' @param condition Condition to determine if the entry should be shown.
#' @param id Unique identifier for the context menu entry.
#' Infered from `name` if not provided.
#' @rdname context-menu
#' @export
new_context_menu_entry <- function(
  name,
  js,
  action = NULL,
  condition = TRUE,
  id = tolower(gsub(" +", "_", name))
) {
  if (is.null(action)) {
    action <- function(...) NULL
  }

  if (isTRUE(condition)) {
    condition <- function(...) TRUE
  }

  if (is_string(js)) {
    js_string <- js
    js <- function(...) js_string
  }

  stopifnot(
    is.function(action),
    is.function(condition),
    is.function(js),
    is_string(id),
    is_string(name)
  )

  structure(
    list(condition = condition, action = action, js = js),
    name = name,
    id = id,
    class = "context_menu_entry"
  )
}

is_context_menu_entry <- function(x) {
  inherits(x, "context_menu_entry")
}

context_menu_entry_id <- function(x) attr(x, "id")

context_menu_entry_name <- function(x) attr(x, "name")

context_menu_entry_condition <- function(x, ...) {
  x[["condition"]](...)
}

context_menu_entry_action <- function(x, ...) {
  if (!is_context_menu_entry(x)) {
    validate_context_menu_entries(x)

    for (i in x) {
      context_menu_entry_action(i, ...)
    }

    return(invisible(NULL))
  }

  x[["action"]](...)

  invisible(NULL)
}

context_menu_entry_js <- function(x, ns = NULL) {
  if (!is_context_menu_entry(x)) {
    validate_context_menu_entries(x)

    res <- paste(
      chr_ply(x, context_menu_entry_js, ns = ns),
      collapse = " else "
    )

    return(
      paste0("(value, target, current) => {\n", res, "\n}")
    )
  }

  if (is.null(ns)) {
    ns <- NS(NULL)
  }

  paste0(
    "if (value === '",
    context_menu_entry_id(x),
    "') {\n(",
    x[["js"]](ns),
    ")(value, target, current)\n}"
  )
}

build_context_menu <- function(x, ...) {
  if (!is_context_menu_entry(x)) {
    validate_context_menu_entries(x)

    return(
      Filter(not_null, lapply(x, build_context_menu, ...))
    )
  }

  if (!context_menu_entry_condition(x, ...)) {
    return(NULL)
  }

  list(name = context_menu_entry_name(x), value = context_menu_entry_id(x))
}

validate_context_menu_entries <- function(x) {
  stopifnot(
    is.list(x),
    all(lgl_ply(x, is_context_menu_entry)),
    anyDuplicated(chr_ply(x, context_menu_entry_id)) == 0L
  )

  invisible(x)
}

#' @param x Object
#' @rdname context-menu
#' @export
context_menu_items <- function(x) {

  if (missing(x)) {

    res <- list(
      create_edge_ctxm,
      remove_node_ctxm,
      remove_edge_ctxm,
      append_node_ctxm,
      create_stack_ctxm,
      remove_stack_ctxm,
      add_block_ctxm
    )

    return(res)
  }

  UseMethod("context_menu_items")
}

#' @rdname context-menu
#' @export
context_menu_items.board_module <- function(x) {

  res <- board_module_context_menu(x)

  if (is_context_menu_entry(res)) {
    return(list(res))
  }

  res <- validate_context_menu_entries(res)

  res
}

#' @rdname context-menu
#' @export
context_menu_items.list <- function(x) {

  res <- lapply(x, context_menu_items)

  is_ctxm <- lgl_ply(x, is_context_menu_entry)

  res[is_ctxm] <- lapply(res[is_ctxm], list)

  for (i in res) {
    validate_context_menu_entries(i)
  }

  do.call("c", res)
}

#' @rdname context-menu
#' @export
context_menu_items.dag_board <- function(x) {
  c(
    context_menu_items(),
    context_menu_items(board_modules(x))
  )
}
