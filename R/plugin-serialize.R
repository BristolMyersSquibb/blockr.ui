#' Ser/deser module UI
#'
#' @param id module ID.
#' @param board The initial `board` object
#' @rdname ser_deser
#' @export
ser_deser_ui <- function(id, board) {
  import_btn <- htmltools::tagQuery(
    fileInput(
      NS(id, "restore"),
      buttonLabel = "Import",
      label = "",
      placeholder = "Select a board file"
    )
  )$find(".btn")$addClass("btn-sm")$reset()$find(".input-group")$addClass(
    "input-group-sm"
  )$allTags()

  # To fix CSS alignement issues with bootstrap
  import_btn$attribs$class <- gsub(
    "form-group",
    "custom-form-group",
    import_btn$attribs$class
  )

  div(
    class = "d-flex justify-content-center align-items-center gap-1",
    import_btn,
    downloadButton(
      NS(id, "serialize"),
      "Export",
      class = "btn-sm",
      icon = icon("file-export"),
    )
  )
}
