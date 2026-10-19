#' The block IR in R
#'
#' A block whose state is IR (`"plan"` in its `block.json`) has no expression
#' builder of its own: its state, with the `op` the manifest names, is a step
#' of the IR blockx runs, and `ir_to_expr()` turns it into a dplyr call. One
#' translator serves every block; blockx turns the same IR into SQL.
#'
#' Expressions are lists:
#'
#' * `list(col = "x")`: a column
#' * `list(lit = 1)`: a literal, `NULL` for `NA`
#' * `list(list = c("a", "b"))`: literals, the right-hand side of `%in%`
#' * `list(call = "f", args = list(...), named = list(...))`: an operator
#'   (`+`, `==`, `%in%`, `&`, `!`, `neg` for unary minus, ...) or a function
#'   by its R name
#' * `list(code = "...", lang = "r")`: code the IR does not express, as typed.
#'   R parses it; blockx parses it when it can and otherwise runs the block in
#'   R.
#'
#' Steps (`op`) so far: `filter` (`pred`, a list of expressions, ANDed;
#' `preserve_order`), `select` (`columns`, `exclude`, `distinct`).
#'
#' @param step A step: `op` and its fields.
#' @param data The symbol the step's input is bound to.
#' @return For `ir_to_expr()`, a call with `.(data)` for the input, as
#'   blockr.core's `bquoted` expressions have it.
#' @export
ir_to_expr <- function(step, data = "data") {
  input <- call(".", as.name(data))
  switch(
    step$op %||% "",
    filter = ir_filter(step, input),
    select = ir_select(step, input),
    stop("unknown IR step \"", step$op, "\"")
  )
}

ir_filter <- function(step, input) {
  pred <- lapply(step$pred %||% list(), ir_expr_to_r)
  if (!length(pred)) {
    return(input)
  }
  out <- as.call(c(quote(dplyr::filter), input, pred))
  if (isTRUE(step$preserve_order)) {
    # the rows in the order the values were picked: the first %in% that keeps
    # rows (an exclusion has no order)
    pick <- ir_first_in(step$pred)
    if (!is.null(pick)) {
      out <- as.call(list(
        quote(dplyr::arrange), out,
        call("match", as.name(pick$col), pick$values)
      ))
    }
  }
  out
}

ir_first_in <- function(pred) {
  for (e in pred) {
    if (identical(e$call, "%in%")) {
      return(list(col = e$args[[1L]]$col, values = ir_lits(e$args[[2L]]$list)))
    }
    if (identical(e$call, "|")) {
      found <- ir_first_in(e$args)
      if (!is.null(found)) return(found)
    }
  }
  NULL
}

ir_select <- function(step, input) {
  cols <- lapply(as.character(unlist(step$columns)), as.name)
  out <- if (!length(cols)) {
    input
  } else if (isTRUE(step$exclude)) {
    as.call(list(quote(dplyr::select), input, call("-", as.call(c(quote(c), cols)))))
  } else {
    as.call(c(quote(dplyr::select), input, cols))
  }
  if (isTRUE(step$distinct)) out <- as.call(list(quote(dplyr::distinct), out))
  out
}

# Functions blockx writes with their namespace (blockx shared/dplyr.ts, NS).
ir_namespaced <- c(
  if_else = "dplyr", case_when = "dplyr", coalesce = "dplyr",
  between = "dplyr", str_detect = "stringr", str_c = "stringr",
  str_sub = "stringr", str_to_upper = "stringr", str_to_lower = "stringr",
  n = "dplyr", n_distinct = "dplyr", first = "dplyr", last = "dplyr",
  median = "stats", sd = "stats", lag = "dplyr", lead = "dplyr",
  row_number = "dplyr", min_rank = "dplyr", dense_rank = "dplyr",
  percent_rank = "dplyr", ntile = "dplyr"
)

ir_operators <- c(
  "+", "-", "*", "/", "^", "%%", "==", "!=", ">", ">=", "<", "<=", "%in%",
  "&", "|", "!", "~"
)

#' @rdname ir_to_expr
#' @param e An IR expression.
#' @return For `ir_expr_to_r()`, the R language object.
#' @export
ir_expr_to_r <- function(e) {
  if (!is.list(e)) {
    stop("an IR expression is a list")
  }
  if ("col" %in% names(e)) {
    return(as.name(e$col))
  }
  if ("lit" %in% names(e)) {
    return(ir_lit(e$lit))
  }
  if ("list" %in% names(e)) {
    return(ir_lits(e$list))
  }
  if ("code" %in% names(e)) {
    lang <- e$lang %||% "r"
    if (!identical(lang, "r")) {
      stop("code in ", lang, " cannot run in R")
    }
    return(str2lang(e$code))
  }
  f <- e$call
  if (is.null(f)) {
    stop("not an IR expression")
  }
  args <- lapply(e$args %||% list(), ir_expr_to_r)
  if (identical(f, "neg")) {
    return(call("-", args[[1L]]))
  }
  fn <- if (f %in% ir_operators) {
    as.name(f)
  } else if (f %in% names(ir_namespaced)) {
    call("::", as.name(ir_namespaced[[f]]), as.name(f))
  } else {
    as.name(f)
  }
  named <- lapply(e$named %||% list(), ir_expr_to_r)
  as.call(c(fn, args, named))
}

ir_lit <- function(x) {
  x <- unlist(x)
  if (is.null(x) || !length(x)) NA else x[[1L]]
}

# A list of literals as one atomic vector: strings stay strings ("007" is not
# 7), NULL is NA.
ir_lits <- function(x) {
  vals <- lapply(as.list(x), function(v) if (is.null(v)) NA else v)
  if (!length(vals)) return(character())
  unlist(vals)
}

# The canonical R form of an IR expression, as it goes to the client: every
# `args` and `list` a list, so a length-1 one stays an array.
ir_normalize <- function(e) {
  if (!is.list(e)) {
    return(e)
  }
  if ("list" %in% names(e)) {
    e$list <- as.list(e$list)
  }
  if ("args" %in% names(e)) {
    e$args <- lapply(unname(as.list(e$args)), ir_normalize)
  }
  if ("named" %in% names(e)) {
    e$named <- lapply(as.list(e$named), ir_normalize)
  }
  e
}
