.onLoad <- function(libname, pkgname) {
  # select_input() reports through this type; force, so load_all() can run
  # again in the same session.
  shiny::registerInputHandler("blockr.ui.select", select_value, force = TRUE)
}
