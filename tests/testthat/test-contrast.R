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

test_that("icons, status borders and the logo clear 3:1 on the surface", {

  # What marks a control or its state (an icon, a status border, the status
  # dot drawn in one) needs 3:1 against the surface (WCAG 1.4.11), which the
  # Chrome check's axe does not cover. The disabled colour is exempt, as it is
  # for text. The logo's green is held to it too, because the logo in
  # blockr.dock's navbar is the board's busy indicator.
  marks <- c(
    paste0(
      "--blockr-color-",
      c(
        "text-muted", "text-default", "border-accent",
        "border-danger", "border-warning", "border-success"
      )
    ),
    "--blockr-logo"
  )

  for (scheme in c("light", "dark")) {

    tokens <- blockr_tokens(scheme)
    ratios <- blockr.core::dbl_ply(
      tokens[marks],
      contrast_ratio,
      tokens[["--blockr-color-bg-surface"]]
    )

    expect_identical(
      sprintf("%s %s: %.2f:1", scheme, marks, ratios)[ratios < 3],
      character()
    )
  }
})
