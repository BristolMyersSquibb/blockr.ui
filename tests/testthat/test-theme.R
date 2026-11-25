test_that("new_blockr_theme() makes a theme that is_blockr_theme() tells", {
  theme <- new_blockr_theme()

  expect_true(is_blockr_theme(theme))
  expect_s3_class(theme, "blockr_theme", exact = TRUE)
  expect_identical(theme$light, list())
  expect_identical(theme$dark, list())
  expect_identical(theme$fonts, list())

  expect_false(is_blockr_theme(list()))
  expect_false(is_blockr_theme(structure(list(), class = c("theme", "gg"))))
})

test_that("format() writes a rule for each mode that the theme gives values", {
  theme <- new_blockr_theme(
    light = list(
      "--bk-color-primary" = "#be2bbb",
      "--bk-font-sans" = "Inter, sans-serif"
    ),
    dark = list(
      "--bk-color-primary" = "#d66bd3",
      "--bk-font-sans" = "Inter, sans-serif"
    )
  )

  expect_identical(
    format(theme),
    paste(
      ":root:root[data-bk-mode=\"light\"] {",
      "  --bk-color-primary: #be2bbb;",
      "  --bk-font-sans: Inter, sans-serif;",
      "}",
      "",
      ":root:root[data-bk-mode=\"dark\"] {",
      "  --bk-color-primary: #d66bd3;",
      "  --bk-font-sans: Inter, sans-serif;",
      "}",
      sep = "\n"
    )
  )

  expect_identical(
    format(new_blockr_theme(dark = list("--bk-radius" = "0"))),
    ":root:root[data-bk-mode=\"dark\"] {\n  --bk-radius: 0;\n}"
  )
})

test_that("format() writes nothing for a theme with no values", {
  expect_identical(format(new_blockr_theme()), "")

  fonts <- list(list(family = "Inter", file = "inter-latin.woff2"))
  expect_identical(format(new_blockr_theme(fonts = fonts)), "")
})

test_that("new_blockr_theme() refuses a name that theme_variables() lacks", {
  expect_error(
    new_blockr_theme(light = list("--bk-colour-primary" = "#be2bbb")),
    "`light` names \"--bk-colour-primary\", which `theme_variables()` lacks",
    fixed = TRUE,
    class = "blockr_theme_variable_unknown"
  )
  expect_error(
    new_blockr_theme(
      dark = list(
        "--bk-color-primary" = "#d66bd3",
        "color-primary" = "#d66bd3",
        "--bk-gap" = "1rem"
      )
    ),
    "\"color-primary\" and \"--bk-gap\"",
    fixed = TRUE,
    class = "blockr_theme_variable_unknown"
  )
})

test_that("new_blockr_theme() refuses a value that holds } or <", {
  expect_error(
    new_blockr_theme(light = list("--bk-radius" = "0 } body { color: red")),
    "\"--bk-radius\" a value that holds",
    fixed = TRUE,
    class = "blockr_theme_value_unsafe"
  )
  expect_error(
    new_blockr_theme(dark = list("--bk-font-sans" = "</style><script>")),
    class = "blockr_theme_value_unsafe"
  )
  expect_no_error(
    new_blockr_theme(
      light = list("--bk-shadow-sm" = "0 1px 2px rgb(0 0 0 / 5%)")
    )
  )
})

test_that("new_blockr_theme() takes a list of strings by name for each mode", {
  expect_error(
    new_blockr_theme(light = c("--bk-radius" = "0")),
    class = "blockr_theme_values_invalid"
  )
  expect_error(
    new_blockr_theme(light = list("--bk-radius" = "0", "0.25rem")),
    class = "blockr_theme_values_invalid"
  )
  expect_error(
    new_blockr_theme(
      dark = list("--bk-z-index-overlay" = 50, "--bk-radius" = NA_character_)
    ),
    "as the value of \"--bk-z-index-overlay\" and \"--bk-radius\"",
    fixed = TRUE,
    class = "blockr_theme_values_invalid"
  )
})

test_that("a face takes the weight and the style of a face in CSS", {
  theme <- new_blockr_theme(
    fonts = list(
      list(family = "Inter", file = "inter-latin.woff2"),
      list(
        family = "Inter",
        file = "inter-latin-italic.woff2",
        weight = "100 900",
        style = "italic"
      )
    )
  )

  expect_identical(
    theme$fonts,
    list(
      list(
        family = "Inter",
        file = "inter-latin.woff2",
        weight = "normal",
        style = "normal"
      ),
      list(
        family = "Inter",
        file = "inter-latin-italic.woff2",
        weight = "100 900",
        style = "italic"
      )
    )
  )
})
