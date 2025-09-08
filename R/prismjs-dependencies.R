#' prismjs dependencies utils
#'
#' @description This function attaches prismjs. dependencies to the given tag
#'
#' @param tag Element to attach the dependencies.
#'
#' @importFrom htmltools tagList htmlDependency
#' @export
add_prismjs_deps <- function(tag) {
  prismjs_deps <- htmlDependency(
    #nolint start
    name = "prismjs",
    version = "1.30.0",
    src = c(file = "prismjs-1.30.0"),
    script = c("js/components/prism-r.min.js", "js/plugins/autoloader.min.js"),
    stylesheet = "css/themes/prism.min.css",
    package = "blockr.ui",
    head = "<script>Prism.plugins.autoloader.languages_path = 'https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/';</script>;
    "
  )
  #nolint end
  tagList(tag, prismjs_deps)
}
