#' @importFrom blockr.core pkg_name
NULL

pkg_file <- function(..., pkg = parent.frame()) {
  system.file(..., package = pkg_name(pkg))
}

# The dependency builders run on every control render (each control attaches
# controls_dep()), and packageVersion() reads the package's metadata from disk
# on every call. The version cannot change while the process runs, so read it
# once.
ui_version <- function() {
  if (is.null(pkg_cache$version)) {
    pkg_cache$version <- utils::packageVersion("blockr.ui")
  }
  pkg_cache$version
}

pkg_cache <- new.env(parent = emptyenv())
