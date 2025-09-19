seq_gen <- function(...) {

  gen <- coro::async_generator(
    function(x) {
      for (elt in x) {
        coro::yield(elt)
      }
    }
  )

  gen(paste0(c(...), " "))
}

mock_chat <- function(name = "test", model = "mock",
                      url = "https://test.com", ...) {

  function(system_prompt = NULL, params = NULL) {

    Chat <- asNamespace("ellmer")[["Chat"]]

    MockChat <- R6::R6Class(
      "MockChat",
      inherit = Chat,
      public = list(...)
    )

    MockChat$new(ellmer::Provider(name, model, url))
  }
}

call_tool <- function(self, tool_name, ...) {
  fun <- self$get_tools()[[tool_name]]
  fun(...)
}
