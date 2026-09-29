test_that("text on the accent tint is text-accent-strong", {

  # text-accent measures under 4.5:1 on the tint, and more so on its hover
  # step; a rule that paints the tint and colours its text says so.
  files <- list.files(
    system.file("assets", "css", package = "blockr.ui"),
    pattern = "\\.css$",
    full.names = TRUE
  )

  rules <- blockr.core::unlst(
    lapply(files, function(file) {
      css <- read_css(file)
      blocks <- regmatches(css, gregexpr("[^{}]*\\{[^{}]*\\}", css))[[1L]]
      blockr.core::set_names(blocks, rep(basename(file), length(blocks)))
    }),
    use_names = TRUE
  )

  tint <- grepl("--blockr-color-bg-accent-subtle", rules, fixed = TRUE)
  plain <- grepl("var(--blockr-color-text-accent)", rules, fixed = TRUE)
  selector <- trimws(sub("\\{[\\s\\S]*$", "", rules, perl = TRUE))

  expect_identical(
    paste0(names(rules), ": ", selector)[tint & plain],
    character()
  )
})
