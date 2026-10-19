schema <- list(
  type = "object",
  properties = list(
    columns = list(type = "array", items = list(type = "string"),
                   default = list()),
    exclude = list(type = "boolean", default = FALSE),
    operator = list(type = "string", enum = list("&", "|"), default = "&"),
    conditions = list(
      type = "array",
      items = list(
        type = "object",
        properties = list(
          values = list(type = "array", items = list(type = "string"))
        )
      ),
      default = list()
    )
  )
)

test_that("coerce_state fills defaults and keeps arrays as lists", {
  expect_identical(
    coerce_state(list(), schema),
    list(columns = list(), exclude = FALSE, operator = "&",
         conditions = list())
  )
  s <- coerce_state(list(columns = "a", exclude = "TRUE", operator = "^"),
                    schema)
  expect_identical(s$columns, list("a"))
  expect_true(s$exclude)
  expect_identical(s$operator, "&")
  expect_identical(coerce_state(list(columns = c("a", "b")), schema)$columns,
                   list("a", "b"))
})

test_that("coerce_state canonicalises records", {
  flat <- list(type = "values", column = "x", values = "a")
  s <- coerce_state(list(conditions = flat), schema)
  expect_identical(
    s$conditions,
    list(list(type = "values", column = "x", values = list("a")))
  )
  expect_identical(
    coerce_state(list(conditions = c("a", "b")), schema)$conditions,
    list()
  )
  named <- list(a = list(type = "expr", expr = "x > 1"))
  expect_identical(
    coerce_state(list(conditions = named), schema)$conditions,
    list(list(type = "expr", expr = "x > 1"))
  )
})

spec <- list(
  name = "test",
  inputs = "data",
  schema = schema,
  state = coerce_state(list(columns = list("a")), schema),
  expr = function(s) length(s$columns),
  requests = list(echo = function(x) toupper(x))
)

test_that("the server answers ready, submit and request", {
  sent <- list()
  record <- function(msg) sent[[length(sent) + 1L]] <<- msg
  data <- shiny::reactive(data.frame(a = 1:3, b = c("x", "y", "")))
  shiny::testServer(
    js_block_server,
    args = list(inputs = list(data = data), spec = spec, send = record),
    {

      # nothing before ready
      expect_length(sent, 0L)

      session$setInputs(block = list(messages = list(list(type = "ready"))))
      expect_identical(sent[[1]]$state, spec$state)
      expect_identical(
        vapply(sent[[1]]$context$inputs$data$columns, `[[`, "", "type"),
        c("integer", "character")
      )

      session$setInputs(block = list(messages = list(
        list(type = "submit", state = list(columns = list("a", "b")))
      )))
      expect_identical(session$returned$state$columns(), list("a", "b"))
      expect_identical(session$returned$expr(), 2L)
      # no echo of what the client sent
      expect_length(sent, 1L)

      # an external write goes to the client
      session$returned$state$exclude(TRUE)
      session$flushReact()
      expect_true(sent[[2]]$state$exclude)

      session$setInputs(block = list(messages = list(
        list(type = "request", seq = 1, name = "column_values",
             args = list(input = "data", column = "b")),
        list(type = "request", seq = 2, name = "echo",
             args = list(x = "hi")),
        list(type = "request", seq = 3, name = "nope", args = list())
      )))
      expect_identical(sent[[3]]$seq, 1)
      expect_identical(sent[[3]]$result$values, list("", "x", "y"))
      expect_true(sent[[3]]$result$hasEmpty)
      expect_identical(sent[[4]]$result, "HI")
      expect_match(sent[[5]]$error, "unknown request")
    }
  )
})
