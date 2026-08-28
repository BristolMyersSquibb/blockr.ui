test_that("the claimed vocabulary changes only deliberately", {

  tokens <- blockr_tokens()

  expect_snapshot(cat(paste(names(tokens), tokens, sep = ": "), sep = "\n"))
})

test_that("every claimed token resolves to a literal", {

  tokens <- blockr_tokens()

  expect_false(any(is.na(tokens)))
  expect_identical(tokens[["--blockr-color-text-primary"]], "#111827")
  expect_identical(tokens[["--blockr-grey-900"]], "#111827")
})

test_that("var() sites survive nested parens and nested var()", {

  nested <- var_sites(
    "a { box-shadow: var(--blockr-shadow-lg, 0 8px 24px rgba(1, 2, 3, 0.16)); }"
  )

  expect_identical(nested$token, "--blockr-shadow-lg")
  expect_identical(nested$fallback, "0 8px 24px rgba(1, 2, 3, 0.16)")

  inner <- var_sites("a { color: var(--blockr-a, var(--blockr-b, #fff)); }")

  expect_identical(inner$token, c("--blockr-a", "--blockr-b"))
  expect_identical(inner$fallback, c("var(--blockr-b, #fff)", "#fff"))

  bare <- var_sites("a { color: var(--blockr-grey-50); }")

  expect_identical(bare$fallback, NA_character_)
})

test_that("commented-out CSS is neither read nor counted", {

  css <- read_css(
    withr::local_tempfile(
      lines = c(
        "/* --blockr-ghost: #000;",
        "   color: var(--blockr-ghost, #000); */",
        "a { color: var(--blockr-real, #fff); }"
      )
    )
  )

  expect_named(css_definitions(css), character())
  expect_identical(var_sites(css)$token, "--blockr-real")
  expect_identical(css_lines(css, var_sites(css)$start), 3L)
})

test_that("a reference resolves through the chain, a cycle does not", {

  definitions <- css_definitions(
    ":root {
       --blockr-old: var(--blockr-new);
       --blockr-new: #fff;
       --blockr-loop: var(--blockr-loop);
     }"
  )

  expect_identical(resolve_token("--blockr-old", definitions), "#fff")
  expect_identical(resolve_token("--blockr-loop", definitions), NA_character_)
  expect_identical(resolve_token("--blockr-gone", definitions), NA_character_)
  expect_identical(
    resolve_css_value("var(--blockr-absent, 2px)", definitions),
    "2px"
  )
  expect_identical(
    resolve_css_value("var(--blockr-absent)", definitions),
    NA_character_
  )
})

test_that("resolution rewrites every site and keeps the text around them", {

  definitions <- css_definitions(":root { --blockr-b: #dc2626; }")

  expect_identical(
    resolve_css_value("var(--blockr-b) 0 var(--blockr-b)", definitions),
    "#dc2626 0 #dc2626"
  )
  expect_identical(
    resolve_css_value(
      "1px var(--blockr-a, var(--blockr-b, #fff)) solid",
      definitions
    ),
    "1px #dc2626 solid"
  )
})

test_that("the last declaration of a name wins", {

  definitions <- css_definitions(
    ":root { --blockr-x: 1px; } :root { --blockr-x: 2px; }"
  )

  expect_identical(definitions, c("--blockr-x" = "2px"))
})
