dropNulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}

`%OR%` <- function(x, y) {
  if (is.null(x)) y else x
}

block_uid <- function(x) {
  stopifnot(inherits(x, "block"))
  attr(x, "uid")
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

block_name_to_id <- function(x) {
  gsub(" ", "_", tolower(block_name(x)))
}

v_rule <- function() {
  shiny::tags$div(class = "vr")
}

lst_xtr_reval <- function(x, ...) {
  lapply(lst_xtr(x, ...), reval)
}

reval <- function(x) {
  x()
}

reval_if <- function(x) if (is.function(x)) x() else x

pkg_file <- function(...) {
  system.file(..., package = "blockr.core")
}

is_pkg_avail <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Useful for shinytest2
#' Pre-process reactiveValues results
#'
#' @keywords internal
process_app_state <- function(state) {
  stopifnot(is.list(state))

  lapply(
    set_names(nm = names(state)),
    function(nme) {
      if (nme == "network") {
        # drop the x and y coords as this may not be reproducible
        if (length(state[[nme]]$nodes) || length(state[[nme]]$combos)) {
          if (length(state[[nme]]$nodes)) {
            state[[nme]]$nodes <- lapply(
              state[[nme]]$nodes,
              function(node) {
                node$x <- NULL
                node$y <- NULL
                node$style$x <- NULL
                node$style$y <- NULL
                node
              }
            )
          }

          if (length(state[[nme]]$combos)) {
            state[[nme]]$combos <- lapply(
              state[[nme]]$combos,
              function(combo) {
                combo$x <- NULL
                combo$y <- NULL
                combo$style$x <- NULL
                combo$style$y <- NULL
                combo
              }
            )
          }
          state[[nme]]
        } else {
          state[[nme]]
        }
      } else if (nme == "backup_list") {
        state[[nme]] <- NULL
        state[[nme]]
      } else if (nme == "app_layout") {
        if (length(state[[nme]]$panels) > 0) {
          state[[nme]]$panels <- lapply(
            state[[nme]]$panels,
            function(panel) {
              panel$params <- NULL
              panel
            }
          )
        }
        state[[nme]]
      } else if (nme == "module_state") {
        # This contains unevaluated reactiveValues which should not be in a snapshot
        state[[nme]] <- NULL
        state[[nme]]
      } else {
        state[[nme]]
      }
    }
  )
}

globalVariables(c("x", "y"))
