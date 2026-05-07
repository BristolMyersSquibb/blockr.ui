#' @importFrom blockr.core pkg_name
NULL

pkg_file <- function(..., pkg = parent.frame()) {
  system.file(..., package = pkg_name(pkg))
}
