#' Demo app
#'
#' Run demo app
#'
#' @param ... Forwarded to \link[blockr.core]{new_board}.
#'
#' @rdname run_demo_app
#' @export
run_demo_app <- function(...) {
  serve(
    new_dag_board(...),
    "main"
  )
}
