shipped <- function(dir, pattern) {
  list.files(
    system.file("assets", dir, package = "blockr.ui"),
    pattern = pattern,
    full.names = TRUE
  )
}

layers <- c("sticky", "fixed", "offcanvas", "modal", "menu", "tooltip", "toast")

test_that("a z-index names a layer, or orders siblings within -1 to 3", {

  values <- blockr.core::unlst(
    lapply(shipped("css", "\\.css$"), file_z_values),
    use_names = TRUE
  )
  local <- suppressWarnings(as.integer(values))

  named <- grepl("^var\\(--blockr-z-[a-z]+\\)$", values)
  sibling <- !is.na(local) & local >= -1L & local <= 3L
  stray <- !(named | sibling | values == "auto")

  expect_gt(sum(named), 0L)
  expect_identical(paste0(names(values), ": ", values)[stray], character())
})

test_that("the scripts leave stacking to the stylesheets", {

  src <- lapply(shipped("js", "\\.js$"), readLines, warn = FALSE)

  expect_false(any(grepl("zIndex|z-index", blockr.core::unlst(src))))
})

test_that("the layers keep Bootstrap's order and values", {

  tokens <- blockr_tokens()
  z <- as.integer(tokens[paste0("--blockr-z-", layers)])

  expect_false(anyNA(z))
  expect_true(all(diff(z) > 0L))

  scss <- system.file(
    "lib", "bs5", "scss", "_variables.scss",
    package = "bslib"
  )

  skip_if_not(nzchar(scss), "bslib's Bootstrap sources are not installed")

  src <- readLines(scss, warn = FALSE)
  stack <- regmatches(src, regexec("^\\$zindex-([a-z-]+):\\s*([0-9]+)", src))
  stack <- stack[lengths(stack) == 3L]

  bootstrap <- blockr.core::set_names(
    as.integer(blockr.core::chr_xtr(stack, 3L)),
    blockr.core::chr_xtr(stack, 2L)
  )

  expect_identical(z, unname(bootstrap[sub("^menu$", "popover", layers)]))
})
