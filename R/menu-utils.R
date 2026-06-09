# Shared helpers for the instance-backed menus (stack, link). These
# menus own validation of their committed payload and keep themselves in
# sync with the board, so the consumer (e.g. blockr.dock) shrinks to a
# thin adapter. We reuse blockr.core where it already has a helper
# (`is_string()`, `notify()`) and only add what it lacks.

#' Validate a hex colour string
#'
#' Tests whether `x` is a single `#rgb` (3-digit) or `#rrggbb` (6-digit)
#' hex colour string.
#' `blockr.core` has no colour predicate, so this one lives in
#' `blockr.ui`; it is exported so consumers (e.g. `blockr.dock`) validate
#' colours against the same rule the stack menu uses instead of
#' re-implementing it.
#'
#' @param x Object to test.
#'
#' @return A length-1 logical.
#'
#' @examples
#' is_hex_color("#66c2a5")
#' is_hex_color("nope")
#'
#' @export
is_hex_color <- function(x) {
  blockr.core::is_string(x) && grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", x)
}

# A fresh id: a non-empty string not already taken by `existing`. Shared
# by the stack-id and link-id validators.
is_new_id <- function(id, existing) {
  blockr.core::is_string(id) && nzchar(id) && !(id %in% existing)
}

# Normalise a menu-server argument that may arrive as a reactive, a bare
# value, or `NULL` into a zero-arg accessor. Passing a reactive opts the
# menu into live board sync; a bare value / `NULL` is read once and never
# changes.
as_arg_reactive <- function(x) {
  if (shiny::is.reactive(x)) {
    return(x)
  }
  function() x
}

# `board_*_ids()` guarded against a `NULL` board (the snapshot / no-sync
# path passes `board = NULL`).
safe_ids <- function(board, fun) {
  if (is.null(board)) character() else fun(board)
}
