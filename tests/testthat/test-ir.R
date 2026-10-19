ir_col <- function(x) list(col = x)
ir_call <- function(f, ...) list(call = f, args = list(...))

test_that("filter steps translate to dplyr::filter", {
  pred <- list(
    ir_call("%in%", ir_col("Species"), list(list = list("setosa", "virginica"))),
    ir_call(">", ir_col("Sepal.Length"), list(lit = 5))
  )
  e <- ir_to_expr(list(op = "filter", pred = pred))
  expect_identical(
    deparse1(e),
    deparse1(quote(dplyr::filter(.(data), Species %in% c("setosa", "virginica"),
                                 Sepal.Length > 5)))
  )
  expect_identical(ir_to_expr(list(op = "filter", pred = list())), quote(.(data)))
})

test_that("code nodes are parsed, other languages refused", {
  e <- ir_expr_to_r(list(code = "lubridate::year(d) > 2020", lang = "r"))
  expect_identical(e, quote(lubridate::year(d) > 2020))
  expect_error(ir_expr_to_r(list(code = "x > 1", lang = "sql")), "sql")
})

test_that("functions get blockx's namespaces, operators stay bare", {
  e <- ir_expr_to_r(list(
    call = "str_detect", args = list(ir_col("x"), list(lit = "a"))
  ))
  expect_identical(e, quote(stringr::str_detect(x, "a")))
  expect_identical(ir_expr_to_r(ir_call("neg", ir_col("x"))), quote(-x))
  expect_identical(ir_expr_to_r(ir_call("!", ir_call("is.na", ir_col("x")))),
                   quote(!is.na(x)))
  expect_identical(
    ir_expr_to_r(list(call = "mean", args = list(ir_col("x")),
                      named = list(na.rm = list(lit = TRUE)))),
    quote(mean(x, na.rm = TRUE))
  )
})

test_that("literals keep their type, NULL is NA", {
  expect_identical(ir_expr_to_r(list(list = list("007", "1"))), c("007", "1"))
  expect_identical(ir_expr_to_r(list(list = list(1, 2))), c(1, 2))
  expect_identical(ir_expr_to_r(list(lit = NULL)), NA)
})

test_that("preserve_order arranges by the picked values", {
  e <- ir_to_expr(list(
    op = "filter",
    pred = list(ir_call("%in%", ir_col("Species"),
                        list(list = list("virginica", "setosa")))),
    preserve_order = TRUE
  ))
  out <- eval(eval(call("bquote", e, list(data = quote(iris)))))
  expect_identical(as.character(unique(out$Species)), c("virginica", "setosa"))
})

test_that("select steps translate, with exclude and distinct", {
  expect_identical(
    deparse1(ir_to_expr(list(op = "select", columns = c("a", "b")))),
    "dplyr::select(.(data), a, b)"
  )
  expect_identical(
    deparse1(ir_to_expr(list(op = "select", columns = "a", exclude = TRUE,
                              distinct = TRUE))),
    "dplyr::distinct(dplyr::select(.(data), -c(a)))"
  )
  expect_identical(ir_to_expr(list(op = "select", columns = character())),
                   quote(.(data)))
})

test_that("IR arrays are normalized so they stay arrays", {
  s <- list(type = "object", properties = list(
    pred = list(type = "array", items = list(format = "ir"), default = list())
  ))
  out <- coerce_state(list(pred = list(
    call = "%in%", args = list(list(col = "x"), list(list = "a"))
  )), s)
  expect_identical(out$pred[[1]]$args[[2]]$list, list("a"))
  expect_match(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)),
               '"list":["a"]', fixed = TRUE)
})

test_that("& and | take two or more arguments", {
  e <- ir_expr_to_r(list(call = "|", args = list(
    list(col = "a"), list(col = "b"), list(col = "c")
  )))
  expect_identical(e, quote(a | b | c))
})
