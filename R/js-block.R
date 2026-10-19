#' Blocks with a JavaScript UI
#'
#' `new_js_block()` builds a block whose controls are a JavaScript UI and
#' whose state is a JSON object, from the block's folder in a package:
#'
#' ```
#' inst/blocks/<name>/
#'   block.json      name, inputs, state schema, requests, js and css files
#'   <name>.js       the UI: Blockr.defineBlock({name, mount(el, host)})
#'   <name>.css
#' ```
#'
#' The same folder runs in blockx, which hosts the UI without Shiny. The
#' contract both hosts follow is in `blockr.design`,
#' `open/blockx-ready-blocks/contract.md`.
#'
#' The UI talks to R through one input and one message per block. The client
#' sends `{messages: [...]}` on the block's input, each message one of
#' `{type: "ready"}`, `{type: "submit", state}` and
#' `{type: "request", seq, name, args}`. R answers with
#' `session$sendInputMessage()`: `{state, context}` (either or both), or
#' `{seq, result}` / `{seq, error}`. R sends nothing before the client's
#' `ready`, which comes when the element binds, so no message can reach a
#' block whose script has not loaded.
#'
#' The state is kept as one `reactiveVal` per field, which blockr.core
#' serializes and external controllers write to. Every value that enters, from
#' the constructor or the client, is coerced to the manifest's schema, so an
#' array is always an unnamed list and goes to the client as a JSON array.
#'
#' @param name Block name, the folder under `inst/blocks`.
#' @param state Named list, the constructor's arguments. Its names must be the
#'   constructor's formals (other than `...`), for save and restore.
#' @param expr Function of the state list returning a `bquoted` expression.
#' @param class Block class.
#' @param pkg Package holding the block's folder.
#' @param new Block constructor of blockr.core, e.g.
#'   [blockr.core::new_transform_block()].
#' @param expr_type,external_ctrl,allow_empty_state Passed to `new`.
#' @param ctor,ctor_pkg Passed to `new`; the default records the calling
#'   constructor.
#' @param ... Passed to `new`.
#'
#' @return A block object.
#' @export
new_js_block <- function(name, state, expr, class,
                         pkg = environmentName(topenv(parent.frame())),
                         new = blockr.core::new_transform_block,
                         expr_type = "bquoted", external_ctrl = TRUE,
                         allow_empty_state = TRUE,
                         ctor = sys.parent(), ctor_pkg = NULL, ...) {

  manifest <- js_block_manifest(name, pkg)
  state <- coerce_state(state, manifest$state)

  spec <- list(
    name = name,
    inputs = js_block_inputs(manifest),
    schema = manifest$state,
    state = state,
    expr = expr,
    requests = js_block_requests(manifest)
  )

  # blockr.core reads the initial state from the server's enclosing
  # environment, by the constructor's formal names, and the block's inputs
  # from the server's formals.
  env <- list2env(state, parent = environment(js_block_server))
  env$.blockr_spec <- spec

  server <- rlang::new_function(
    stats::setNames(
      c(alist(id = ), rep(alist(x = ), length(spec$inputs))),
      c("id", spec$inputs)
    ),
    quote(
      js_block_server(id, mget(.blockr_spec$inputs, envir = environment()),
                      .blockr_spec)
    ),
    env
  )

  new(
    server,
    js_block_ui(name, pkg, manifest),
    class = class,
    ctor = ctor,
    ctor_pkg = ctor_pkg,
    expr_type = expr_type,
    external_ctrl = external_ctrl,
    allow_empty_state = allow_empty_state,
    ...
  )
}

# `send` is for tests: shiny's mock session records no input messages.
js_block_server <- function(id, inputs, spec, send = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    fields <- names(spec$state)

    r_fields <- lapply(
      stats::setNames(nm = fields),
      function(f) shiny::reactiveVal(spec$state[[f]])
    )

    r_state <- shiny::reactive(
      lapply(stats::setNames(nm = fields), function(f) r_fields[[f]]())
    )

    context <- shiny::reactive(
      list(
        inputs = lapply(inputs, function(dat) {
          # an input throws while its upstream is unset or failing
          tryCatch(list(columns = js_block_columns(dat())),
                   error = function(e) NULL)
        })
      )
    )

    # `ready`: the client has bound and can take messages. `client`: the
    # state it last submitted, which is not sent back to it. `sent`: what it
    # was last sent.
    sync <- new.env(parent = emptyenv())
    sync$ready <- FALSE
    sync$client <- NULL
    sync$state <- NULL
    sync$context <- NULL

    if (is.null(send)) {
      send <- function(msg) session$sendInputMessage("block", msg)
    }

    send_update <- function(state = NULL, context = NULL) {
      msg <- list()
      if (!is.null(state) && !identical(state, sync$state)) {
        sync$state <- msg$state <- state
      }
      if (!is.null(context) && !identical(context, sync$context)) {
        sync$context <- msg$context <- context
      }
      if (length(msg)) send(msg)
    }

    receive <- function(msg) {
      switch(
        msg$type %||% "",
        ready = {
          sync$ready <- TRUE
          # a re-bound element starts from scratch
          sync$state <- sync$context <- NULL
          send_update(r_state(), context())
        },
        submit = {
          state <- coerce_state(msg$state, spec$schema)
          sync$client <- sync$state <- state
          for (f in fields) r_fields[[f]](state[[f]])
        },
        request = send(
          c(list(seq = msg$seq), js_block_answer(msg, spec, inputs))
        )
      )
    }

    shiny::observeEvent(input$block, {
      for (msg in input$block$messages) receive(msg)
    })

    shiny::observeEvent(r_state(), {
      state <- r_state()
      if (sync$ready && !identical(state, sync$client)) {
        send_update(state = state)
      }
    })

    shiny::observeEvent(context(), {
      if (sync$ready) send_update(context = context())
    })

    list(
      expr = shiny::reactive(spec$expr(r_state())),
      state = r_fields
    )
  })
}

js_block_answer <- function(msg, spec, inputs) {
  tryCatch(
    {
      args <- msg$args %||% list()
      result <- if (identical(msg$name, "column_values")) {
        dat <- inputs[[args$input %||% "data"]]
        if (is.null(dat)) stop("no input ", args$input)
        js_block_column_values(
          dat(), args$column,
          limit = blockr.core::blockr_option("max_column_values", 1000),
          query = args$query %||% ""
        )
      } else {
        fn <- spec$requests[[msg$name]]
        if (is.null(fn)) stop("unknown request \"", msg$name, "\"")
        do.call(fn, args)
      }
      list(result = result)
    },
    error = function(e) list(error = conditionMessage(e))
  )
}

js_block_ui <- function(name, pkg, manifest) {
  force(name)
  dep <- htmltools::htmlDependency(
    name = paste0(pkg, "-", name, "-block"),
    version = utils::packageVersion(pkg),
    src = block_path(pkg, "blocks", name),
    script = unlist(manifest$js),
    stylesheet = unlist(manifest$css)
  )
  function(id) {
    htmltools::tagList(
      controls_dep(),
      js_block_dep(),
      dep,
      htmltools::div(
        class = "block-container",
        htmltools::div(
          id = shiny::NS(id, "block"),
          class = "blockr-block",
          `data-block` = name
        )
      )
    )
  }
}

#' @rdname new_js_block
#' @return For `js_block_dep()`, the dependencies of the block host:
#'   `Blockr.defineBlock()` and the helpers blocks share (`blockr-block.js`),
#'   and the Shiny host (`blockr-block-shiny.js`).
#' @export
js_block_dep <- function() {
  if (is.null(controls_cache$block_deps)) {
    controls_cache$block_deps <- htmltools::tagList(
      controls_asset("blockr-block-js", script = "js/blockr-block.js"),
      controls_asset("blockr-block-shiny-js",
                     script = "js/blockr-block-shiny.js")
    )
  }
  controls_cache$block_deps
}

#' @rdname new_js_block
#' @return For `js_block_manifest()`, the parsed `block.json`.
#' @export
js_block_manifest <- function(name, pkg) {
  key <- paste(pkg, name, sep = "::")
  if (is.null(manifest_cache[[key]])) {
    path <- block_path(pkg, "blocks", name, "block.json")
    if (!nzchar(path)) {
      stop("no inst/blocks/", name, "/block.json in package ", pkg)
    }
    manifest_cache[[key]] <- jsonlite::read_json(path, simplifyVector = FALSE)
  }
  manifest_cache[[key]]
}

manifest_cache <- new.env(parent = emptyenv())

# system.file() of another package, also when that package is loaded with
# pkgload::load_all(), whose system.file() shim only covers the package's own
# calls: its files are then under inst/ of the namespace path.
block_path <- function(pkg, ...) {
  root <- getNamespaceInfo(asNamespace(pkg), "path")
  for (path in c(file.path(root, ...), file.path(root, "inst", ...))) {
    if (file.exists(path)) return(path)
  }
  ""
}

js_block_inputs <- function(manifest) {
  inputs <- unlist(manifest$inputs)
  if (is.null(inputs)) "data" else inputs
}

# The manifest names each request's R function as "pkg::fun".
js_block_requests <- function(manifest) {
  reqs <- manifest$requests %||% list()
  stats::setNames(
    lapply(reqs, function(r) {
      fn <- strsplit(r$r, "::", fixed = TRUE)[[1L]]
      getExportedValue(fn[1L], fn[2L])
    }),
    vapply(reqs, `[[`, character(1L), "name")
  )
}

# ---- state schema --------------------------------------------------------

#' Coerce a block state to its schema
#'
#' The schema is a JSON Schema subset: `type` (`object`, `array`, `string`,
#' `boolean`, `number`, `integer`), `properties`, `items`, `enum` and
#' `default`. The canonical R form of a JSON array is an unnamed list, so
#' it is sent back as an array whatever its length.
#'
#' The top level gets every property, a missing one its default. A nested
#' object keeps the fields it has, each coerced if the schema names it. A
#' record that is not an object is dropped from its array, and a single
#' record where an array of records is expected is wrapped.
#'
#' @param x State, as a named list.
#' @param schema The `state` entry of a block's manifest.
#'
#' @return The coerced state.
#' @export
coerce_state <- function(x, schema) {
  props <- schema$properties
  x <- if (is.list(x)) x else list()
  lapply(
    stats::setNames(nm = names(props)),
    function(p) coerce_value(x[[p]], props[[p]])
  )
}

coerce_value <- function(x, s) {
  if (is.null(x)) {
    return(schema_default(s))
  }
  type <- s$type
  if (is.null(type)) {
    return(x)
  }
  switch(
    type,
    array = coerce_array(x, s$items %||% list()),
    object = coerce_object(x, s) %||% schema_default(s),
    {
      v <- coerce_scalar(x, type)
      if (is.null(v) || (!is.null(s$enum) && !v %in% unlist(s$enum))) {
        schema_default(s)
      } else {
        v
      }
    }
  )
}

coerce_array <- function(x, items) {
  if (identical(items$type, "object")) {
    # an atomic vector holds no records
    if (!is.list(x)) {
      return(list())
    }
    # one flat record
    if (!is.null(names(x)) && !all(vapply(x, is.list, logical(1L)))) {
      x <- list(x)
    }
    x <- Filter(is.list, unname(x))
    return(lapply(x, coerce_object, s = items))
  }
  x <- if (is.list(x)) unname(x) else as.list(x)
  if (is.null(items$type)) {
    return(x)
  }
  Filter(Negate(is.null), lapply(x, coerce_scalar, type = items$type))
}

coerce_object <- function(x, s) {
  if (!is.list(x) || (length(x) && is.null(names(x)))) {
    return(NULL)
  }
  for (p in intersect(names(s$properties), names(x))) {
    if (!is.null(x[[p]])) x[[p]] <- coerce_value(x[[p]], s$properties[[p]])
  }
  x
}

coerce_scalar <- function(x, type) {
  if (is.list(x)) {
    if (length(x) != 1L || is.list(x[[1L]])) return(NULL)
    x <- x[[1L]]
  }
  if (length(x) < 1L) {
    return(NULL)
  }
  x <- x[[1L]]
  v <- switch(
    type,
    string = as.character(x),
    boolean = as.logical(x),
    number = suppressWarnings(as.numeric(x)),
    integer = suppressWarnings(as.integer(x)),
    x
  )
  if (is.na(v)) NULL else v
}

schema_default <- function(s) {
  if (!is.null(s$default)) {
    return(s$default)
  }
  switch(
    s$type %||% "",
    array = list(),
    object = stats::setNames(list(), character()),
    string = "",
    boolean = FALSE,
    NULL
  )
}

# ---- the host's view of the data ------------------------------------------

# Columns of an input for the context: name, type and label.
js_block_columns <- function(df) {
  if (inherits(df, "tbl_lazy")) {
    df <- dplyr::collect(utils::head(df, 0L))
  }
  lapply(colnames(df), function(col) {
    vals <- df[[col]]
    list(name = col, type = column_type(vals), label = column_label(vals))
  })
}

# Factors, dates and the rest filter by value, so they report as character.
column_type <- function(vals) {
  if (is.logical(vals)) {
    "logical"
  } else if (is.integer(vals)) {
    "integer"
  } else if (is.numeric(vals)) {
    "numeric"
  } else {
    "character"
  }
}

column_label <- function(x) {
  lbl <- attr(x, "label")
  if (is.character(lbl) && length(lbl) == 1L && !is.na(lbl)) lbl else ""
}

# The `column_values` request: the distinct values of one column, at most
# `limit` of them, filtered by `query`. `truncated` says whether the full list
# is longer than `limit`, whatever the query, so the client stays in search
# mode while narrowing. Moved from blockr.dplyr's build_column_values().
js_block_column_values <- function(df, col, limit = NULL, query = "") {
  if (!col %in% colnames(df)) {
    stop("no column ", col)
  }
  if (inherits(df, "tbl_lazy")) {
    return(column_values_lazy(df, col, limit = limit, query = query))
  }
  vals <- df[[col]]
  type <- column_type(vals)

  info <- list(
    name = col,
    type = type,
    hasNA = anyNA(vals),
    label = column_label(vals)
  )

  cap_values <- function(uv) {
    total <- length(uv)
    truncated <- !is.null(limit) && length(uv) > limit
    if (nzchar(query %||% "")) {
      uv <- uv[grepl(tolower(query), tolower(as.character(uv)), fixed = TRUE)]
    }
    if (!is.null(limit) && length(uv) > limit) uv <- uv[seq_len(limit)]
    list(values = uv, total = total, truncated = truncated)
  }

  if (type %in% c("numeric", "integer")) {
    non_na <- vals[!is.na(vals)]
    if (length(non_na) > 0) {
      info$min <- min(non_na)
      info$max <- max(non_na)
    }
    capped <- cap_values(sort(unique(non_na)))
    info$uniqueValues <- as.list(capped$values)
  } else {
    # sort() before as.character(): factors sort by level order
    capped <- cap_values(as.character(sort(unique(vals[!is.na(vals)]))))
    info$values <- as.list(capped$values)
    # `vals == ""` fails on dates, which R coerces "" to
    info$hasEmpty <- if (is.character(vals) || is.factor(vals)) {
      any(vals == "", na.rm = TRUE)
    } else {
      FALSE
    }
  }
  info$total <- capped$total
  info$truncated <- capped$truncated
  info
}

# The same on a remote table, without pulling a whole column: numeric columns
# get their range in one query, and at most `limit + 1` distinct values come
# back, the extra one telling that the list is cut.
column_values_lazy <- function(df, col, limit = NULL, query = "") {
  df <- dplyr::ungroup(df)
  template <- dplyr::collect(utils::head(df, 0L))
  vals0 <- template[[col]]
  type <- column_type(vals0)
  sym <- rlang::sym(col)

  info <- list(name = col, type = type, hasNA = FALSE,
               label = column_label(vals0))

  numeric_col <- type %in% c("numeric", "integer")

  if (numeric_col) {
    rng <- dplyr::collect(
      dplyr::summarise(
        df,
        ..lo = min(!!sym, na.rm = TRUE),
        ..hi = max(!!sym, na.rm = TRUE)
      )
    )
    if (nrow(rng) && is.finite(rng$..lo)) {
      info$min <- rng$..lo
      info$max <- rng$..hi
    }
  }

  q <- df
  if (nzchar(query %||% "")) {
    pattern <- gsub("([][{}()+*^$|\\\\.?-])", "\\\\\\1", tolower(query))
    q <- dplyr::filter(q, grepl(!!pattern, tolower(as.character(!!sym))))
  }
  distinct_q <- dplyr::distinct(dplyr::transmute(q, ..v = !!sym))
  cap <- if (is.null(limit)) Inf else limit
  pulled <- if (is.finite(cap)) {
    dplyr::collect(utils::head(distinct_q, cap + 1L))
  } else {
    dplyr::collect(distinct_q)
  }

  raw <- pulled$..v[!is.na(pulled$..v)]
  truncated <- is.finite(cap) && length(raw) > cap

  uv <- if (numeric_col) sort(raw) else sort(as.character(raw))
  if (truncated) uv <- uv[seq_len(cap)]
  if (numeric_col) {
    info$uniqueValues <- as.list(uv)
  } else {
    info$values <- as.list(uv)
    info$hasEmpty <- FALSE
  }
  info$total <- if (truncated) NA_integer_ else length(uv)
  info$truncated <- truncated
  info
}
