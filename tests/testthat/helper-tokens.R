blockr_tokens <- function() {

  definitions <- css_definitions(
    read_css(
      system.file("assets", "css", "blockr-tokens.css", package = "blockr.ui")
    )
  )

  blockr.core::set_names(
    blockr.core::chr_ply(
      names(definitions),
      resolve_token,
      definitions = definitions
    ),
    names(definitions)
  )
}

token_references <- function(pkg, path = NULL) {

  root <- css_root(pkg, path)

  sites <- do.call(
    rbind,
    lapply(
      list.files(root, pattern = "\\.css$", recursive = TRUE),
      file_var_sites,
      root = root
    )
  )

  if (is.null(sites)) {
    sites <- data.frame(
      file = character(),
      line = integer(),
      token = character(),
      fallback = character()
    )
  }

  annotate_references(sites, blockr_tokens())
}

expect_tokens_agree <- function(pkg, path = NULL) {

  sites <- token_references(pkg, path)
  defined <- defined_references(sites, pkg)
  wrong <- defined[!is.na(defined$agrees) & !defined$agrees, ]

  testthat::expect(
    nrow(wrong) == 0L,
    paste0(
      "Fallback literals in '", pkg, "' disagree with the tokens they ",
      "back:\n",
      paste0(
        "  ", wrong$file, ":", wrong$line, "  ", wrong$token,
        " is ", wrong$value, ", fallback writes ", wrong$fallback,
        collapse = "\n"
      )
    )
  )

  invisible(sites)
}

expect_tokens_reachable <- function(app, pkg, path = NULL) {

  sites <- token_references(pkg, path)
  tokens <- unique(defined_references(sites, pkg)$token)

  unresolved <- unlist(app$get_js(unresolved_tokens_js(tokens)))

  testthat::expect(
    length(unresolved) == 0L,
    paste0(
      "Tokens referenced by '", pkg, "' resolve to nothing in the running ",
      "app:\n",
      paste0("  ", unresolved, collapse = "\n"),
      "\nAttach blockr.ui::theme_dep() from the app's UI."
    )
  )

  invisible(tokens)
}

expect_theme_attached <- function(ui) {

  name <- theme_dep()$name
  attached <- blockr.core::chr_xtr(
    htmltools::renderTags(ui)$dependencies,
    "name"
  )

  testthat::expect(
    name %in% attached,
    paste0(
      "The '", name, "' dependency is not attached, so the shared tokens ",
      "never reach this UI. Add blockr.ui::theme_dep() to it."
    )
  )

  invisible(ui)
}

unresolved_tokens_js <- function(tokens) {
  paste0(
    "['", paste(tokens, collapse = "','"), "'].filter(",
    "n => getComputedStyle(document.documentElement)",
    ".getPropertyValue(n).trim() === '')"
  )
}

defined_references <- function(sites, pkg) {

  defined <- sites[!is.na(sites$value), ]

  if (!nrow(defined)) {
    stop("Package '", pkg, "' references no token that blockr.ui defines.")
  }

  defined
}

annotate_references <- function(sites, tokens) {

  value <- unname(tokens[sites$token])
  fallback <- blockr.core::chr_ply(sites$fallback, resolve_css_value, tokens)

  data.frame(
    sites,
    value = value,
    agrees = normalize_css_value(fallback) == normalize_css_value(value),
    row.names = NULL
  )
}

css_root <- function(pkg, path) {

  root <- if (is.null(path)) system.file(package = pkg) else path

  if (!nzchar(root)) {
    stop("Package '", pkg, "' is not installed.")
  }

  if (!dir.exists(root)) {
    stop("No directory at '", root, "'.")
  }

  root
}

file_var_sites <- function(file, root) {

  css <- read_css(file.path(root, file))
  sites <- var_sites(css)

  data.frame(
    file = rep(file, nrow(sites)),
    line = css_lines(css, sites$start),
    token = sites$token,
    fallback = sites$fallback
  )
}

css_lines <- function(css, at) {

  breaks <- gregexpr("\n", css, fixed = TRUE)[[1L]]

  if (breaks[1L] < 0L) {
    return(rep(1L, length(at)))
  }

  findInterval(at, breaks) + 1L
}

normalize_css_value <- function(value) {

  literal <- gsub("\\s*([,()])\\s*", "\\1", gsub("\\s+", " ", trimws(value)))

  hex <- gregexpr(
    "#(?:[0-9a-f]{8}|[0-9a-f]{6}|[0-9a-f]{4}|[0-9a-f]{3})(?![0-9a-f])",
    literal,
    perl = TRUE,
    ignore.case = TRUE
  )

  regmatches(literal, hex) <- lapply(regmatches(literal, hex), expand_hex)

  replace(literal, is.na(value), NA_character_)
}

expand_hex <- function(hex) {

  short <- nchar(hex) < 6L
  hex[short] <- paste0("#", gsub("(.)", "\\1\\1", substring(hex[short], 2L)))

  tolower(hex)
}

read_css <- function(file) {
  mask_css_comments(paste(readLines(file, warn = FALSE), collapse = "\n"))
}

mask_css_comments <- function(css) {

  comments <- gregexpr("(?s)/\\*.*?\\*/", css, perl = TRUE)

  regmatches(css, comments) <- lapply(
    regmatches(css, comments),
    gsub,
    pattern = "[^\n]",
    replacement = " "
  )

  css
}

css_definitions <- function(css) {

  declarations <- regmatches(
    css,
    gregexpr("--blockr-[a-z0-9-]+\\s*:[^;}]*", css, perl = TRUE)
  )[[1L]]

  colon <- regexpr(":", declarations, fixed = TRUE)
  name <- trimws(substr(declarations, 1L, colon - 1L))
  value <- trimws(substring(declarations, colon + 1L))

  last <- !duplicated(name, fromLast = TRUE)

  blockr.core::set_names(value[last], name[last])
}

var_sites <- function(css) {

  hits <- gregexpr("var\\(\\s*--blockr-[a-z0-9-]+", css, perl = TRUE)[[1L]]

  if (hits[1L] < 0L) {
    return(
      data.frame(
        start = integer(),
        end = integer(),
        token = character(),
        fallback = character()
      )
    )
  }

  chars <- strsplit(css, "", fixed = TRUE)[[1L]]
  depth <- cumsum((chars == "(") - (chars == ")"))

  start <- as.integer(hits)
  end <- blockr.core::int_ply(start + 3L, closing_paren, depth = depth)

  sites <- data.frame(
    start = start,
    end = end,
    token = sub(
      "^var\\(\\s*",
      "",
      substring(css, start, start + attr(hits, "match.length") - 1L)
    ),
    fallback = blockr.core::chr_ply(
      substring(css, start + 4L, end - 1L),
      var_fallback
    )
  )[!is.na(end), ]

  row.names(sites) <- NULL

  sites
}

closing_paren <- function(open, depth) {

  after <- which(depth == depth[open] - 1L)
  after <- after[after > open]

  if (length(after)) after[1L] else NA_integer_
}

var_fallback <- function(inner) {

  chars <- strsplit(inner, "", fixed = TRUE)[[1L]]
  depth <- cumsum((chars == "(") - (chars == ")"))
  comma <- which(chars == "," & depth == 0L)

  if (!length(comma)) {
    return(NA_character_)
  }

  trimws(substring(inner, comma[1L] + 1L))
}

resolve_token <- function(name, definitions) {

  if (!name %in% names(definitions)) {
    return(NA_character_)
  }

  resolve_css_value(definitions[[name]], definitions, name)
}

resolve_css_value <- function(value, definitions, seen = character()) {

  if (is.na(value)) {
    return(NA_character_)
  }

  repeat {

    sites <- var_sites(value)

    if (!nrow(sites)) {
      return(trimws(value))
    }

    resolved <- resolve_reference(
      sites$token[1L],
      sites$fallback[1L],
      definitions,
      seen
    )

    if (is.na(resolved)) {
      return(NA_character_)
    }

    value <- paste0(
      substr(value, 1L, sites$start[1L] - 1L),
      resolved,
      substring(value, sites$end[1L] + 1L)
    )
  }
}

resolve_reference <- function(token, fallback, definitions, seen) {

  if (token %in% seen) {
    return(NA_character_)
  }

  if (token %in% names(definitions)) {
    return(resolve_css_value(definitions[[token]], definitions, c(seen, token)))
  }

  resolve_css_value(fallback, definitions, seen)
}

consumer_css <- function(..., envir = parent.frame()) {

  root <- withr::local_tempdir(.local_envir = envir)
  files <- list(...)

  for (file in names(files)) {
    writeLines(files[[file]], file.path(root, file))
  }

  root
}

stub_driver <- function(unresolved) {
  list(get_js = function(script) as.list(unresolved))
}
