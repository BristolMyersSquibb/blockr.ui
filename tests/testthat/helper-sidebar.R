# Build a `shiny::MockShinySession` that captures every `sendInputMessage`
# call into a list bound to the test's environment. `MockShinySession`'s
# default implementation is a no-op, so we must override on the instance to
# observe what `show_sidebar()` / `hide_sidebar()` actually emit.
mock_session_with_capture <- function() {
  sess <- shiny::MockShinySession$new()
  captured <- list()
  sess$sendInputMessage <- function(input_id, message) {
    captured[[length(captured) + 1L]] <<- list(
      id = input_id,
      message = message
    )
  }
  list(
    session = sess,
    messages = function() captured
  )
}
