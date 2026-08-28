test_that("token_references annotates every site it finds", {

  root <- consumer_css(
    "widget.css" = c(
      ".a { color: var(--blockr-color-error, #DC2626); }",
      ".b { color: var(--blockr-color-primary, #3b82f6); }",
      ".c { color: var(--blockr-color-negative, #f43f5e); }",
      ".d { color: var(--blockr-grey-50); }"
    )
  )

  expect_identical(
    token_references("fixture", root),
    data.frame(
      file = rep("widget.css", 4L),
      line = 1:4,
      token = c(
        "--blockr-color-error",
        "--blockr-color-primary",
        "--blockr-color-negative",
        "--blockr-grey-50"
      ),
      fallback = c("#DC2626", "#3b82f6", "#f43f5e", NA),
      value = c("#dc2626", "#2563eb", NA, "#f9fafb"),
      agrees = c(TRUE, FALSE, NA, NA)
    )
  )
})

test_that("hex spelling and whitespace are the only equivalences", {

  expect_identical(
    normalize_css_value(c("#FFF", "#ffffff", "#FFFF", "#ffffffff")),
    c("#ffffff", "#ffffff", "#ffffffff", "#ffffffff")
  )

  expect_identical(
    normalize_css_value(c("  rgba(0,0,0, 0.1)  ", "rgba(0, 0, 0, 0.1)")),
    rep("rgba(0,0,0,0.1)", 2L)
  )

  expect_false(
    identical(normalize_css_value("13px"), normalize_css_value("0.8125rem"))
  )

  expect_identical(normalize_css_value(NA_character_), NA_character_)
})

test_that("expect_tokens_agree reports the disagreeing site", {

  root <- consumer_css(
    "widget.css" = c(
      ".a { color: var(--blockr-color-error, #dc2626); }",
      "",
      ".b { color: var(--blockr-color-primary, #3b82f6); }"
    )
  )

  expect_failure(
    expect_tokens_agree("fixture", root),
    "widget.css:3  --blockr-color-primary is #2563eb, fallback writes #3b82f6"
  )
})

test_that("a name this package does not define is reported, never asserted", {

  root <- consumer_css(
    "widget.css" = c(
      ".a { color: var(--blockr-grey-50, #f9fafb); }",
      ".b { color: var(--blockr-focus-ring, 0 0 0 3px rgba(1, 2, 3, 0.4)); }"
    )
  )

  expect_success(expect_tokens_agree("fixture", root))
  expect_identical(
    token_references("fixture", root)$value,
    c("#f9fafb", NA)
  )
})

test_that("a consumer reading none of the vocabulary is an error", {

  root <- consumer_css("widget.css" = ".a { color: var(--blockr-nope, red); }")

  expect_error(expect_tokens_agree("fixture", root), "references no token")
  expect_error(
    expect_tokens_reachable(stub_driver(NULL), "fixture", root),
    "references no token"
  )
})

test_that("stylesheets are located through the installed package", {

  expect_error(css_root("blockr.nonesuch", NULL), "is not installed")
  expect_error(css_root("fixture", "/no/such/place"), "No directory at")
})

test_that("the reachability script reads each name off the root element", {

  expect_identical(
    unresolved_tokens_js(c("--blockr-grey-50", "--blockr-grey-100")),
    paste0(
      "['--blockr-grey-50','--blockr-grey-100'].filter(",
      "n => getComputedStyle(document.documentElement)",
      ".getPropertyValue(n).trim() === '')"
    )
  )
})

test_that("expect_tokens_reachable fails on the names the app returns", {

  root <- consumer_css(
    "widget.css" = ".a { color: var(--blockr-grey-50, #f9fafb); }"
  )

  expect_success(
    expect_tokens_reachable(stub_driver(character()), "fixture", root)
  )
  expect_failure(
    expect_tokens_reachable(stub_driver("--blockr-grey-50"), "fixture", root),
    "--blockr-grey-50"
  )
})

test_that("expect_theme_attached sees the dependency in a rendered UI", {

  expect_success(expect_theme_attached(htmltools::tagList(theme_dep())))
  expect_failure(
    expect_theme_attached(htmltools::div("no tokens here")),
    "is not attached"
  )
})

test_that("no fallback in this package's own stylesheets has drifted", {
  expect_success(expect_tokens_agree("blockr.ui"))
})
