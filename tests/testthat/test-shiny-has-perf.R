# The guard pattern is read out of the shipped script rather than repeated
# here, so the table below tests what actually runs in the browser. The
# surrounding split-and-strip is mirrored from `stripGuard()`; keep the two in
# step when either changes.
guard_pattern <- function() {

  js <- readLines(
    system.file("assets", "js", "shiny-has-perf.js", package = "blockr.ui"),
    warn = FALSE
  )

  line <- grep("var REDUNDANT_GUARD", js, value = TRUE, fixed = TRUE)
  stopifnot(length(line) == 1L)

  sub("^[^/]*/(.*)/;\\s*$", "\\1", line)
}

strip_guard <- function(selector, pattern = guard_pattern()) {
  parts <- trimws(strsplit(selector, ",", fixed = TRUE)[[1]])
  shiny_owned <- grepl("shiny-html-output", parts, fixed = TRUE) |
    grepl("shiny-conditional--shown", parts, fixed = TRUE)
  redundant <- grepl(pattern, parts, perl = TRUE) & shiny_owned
  parts[redundant] <- gsub(":has(> *)", "", parts[redundant], fixed = TRUE)
  paste(parts, collapse = ", ")
}

test_that("shiny_has_perf_dep ships the script it documents", {

  dep <- shiny_has_perf_dep()

  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$script, "js/shiny-has-perf.js")

  assets <- system.file("assets", package = "blockr.ui")

  expect_true(file.exists(file.path(assets, dep$script)))
})

test_that("the guard is stripped from Shiny's recalculating fade", {

  fade <- paste(
    "[data-display-if].shiny-conditional--shown:has(> *).recalculating > *",
    "div:where(.shiny-html-output):has(> *).recalculating > *",
    sep = ", "
  )

  expect_identical(
    strip_guard(fade),
    paste(
      "[data-display-if].shiny-conditional--shown.recalculating > *",
      "div:where(.shiny-html-output).recalculating > *",
      sep = ", "
    )
  )
})

test_that("the pass-through guard is load-bearing and left alone", {

  # Dropping `:has(> *)` here would apply `display: contents` to empty
  # containers, which collapses the dock host to a blank page.
  pass_through <- paste(
    "[data-display-if].shiny-conditional--shown:has(> *)",
    "div:where(.shiny-html-output):has(> *)",
    sep = ", "
  )

  expect_identical(strip_guard(pass_through), pass_through)
})

test_that("only provably redundant guards are stripped", {

  # Redundant: reaching a descendant of the guarded element proves that
  # element has an element child, so the guard never changes the match set.
  expect_identical(
    strip_guard("div:where(.shiny-html-output):has(> *) .recalculating > *"),
    "div:where(.shiny-html-output) .recalculating > *"
  )

  keep <- c(
    # Guard in subject position: nothing downstream proves a child exists.
    "div:where(.shiny-html-output):has(> *).recalculating",
    # Sibling combinators walk out of the element entirely.
    "div:where(.shiny-html-output):has(> *) ~ .foo > *",
    "div:where(.shiny-html-output):has(> *) + .foo > *",
    # Subject is named rather than universal: outside the measured case.
    "div:where(.shiny-html-output):has(> *) > .foo",
    # Not one of the two containers Shiny styles.
    ".dv-groupview:has(> *).active > *",
    "[data-shiny-busy-spinners] .recalculating:has(>*)"
  )

  for (selector in keep) {
    expect_identical(strip_guard(selector), selector)
  }
})

test_that("Shiny still nests the fade inside the guard we target", {

  skip_if_not_installed("shiny")

  scss <- system.file(
    "www", "shared", "shiny_scss", "shiny.bootstrap5.scss",
    package = "shiny"
  )
  skip_if(scss == "", "Shiny sass sources not installed")

  src <- paste(readLines(scss, warn = FALSE), collapse = "\n")

  # An upstream fix, or a restructuring of this block, makes the shipped
  # script a silent no-op. Fail here rather than in a profiler.
  expect_match(
    src,
    "&:has\\(> \\*\\)\\s*\\{[^}]*&\\.recalculating > \\*",
    perl = TRUE
  )
})
