test_that("blockr_button() is an action button in the .blockr-btn classes", {

  btn <- blockr_button("go", "Write file", kind = "main", size = "s")
  html <- htmltools::renderTags(btn)$html

  expect_match(
    html,
    'class="blockr-btn blockr-btn--main blockr-btn--s action-button"',
    fixed = TRUE
  )
  expect_match(html, 'id="go" type="button"', fixed = TRUE)
  expect_match(html, '<span class="action-label">Write file</span>',
               fixed = TRUE)
  expect_no_match(html, "disabled")

  expect_identical(
    htmltools::tagGetAttribute(blockr_button("go", "Go"), "class"),
    "blockr-btn blockr-btn--secondary blockr-btn--m action-button"
  )

  off <- blockr_button("go", "Go", icon = htmltools::HTML("<svg></svg>"),
                       disabled = TRUE)
  html <- htmltools::renderTags(off)$html
  expect_match(html, "disabled", fixed = TRUE)
  expect_match(html, '<span class="action-icon"><svg></svg></span>',
               fixed = TRUE)

  expect_error(blockr_button("go", "Go", kind = "primary"))
  expect_error(blockr_button("go", "Go", size = "l"))
})

test_that("blockr_download_button() is Shiny's download link, dressed", {

  html <- htmltools::renderTags(
    blockr_download_button("csv", size = "xs", kind = "quiet")
  )$html

  expect_match(
    html,
    "blockr-btn blockr-btn--quiet blockr-btn--xs shiny-download-link disabled",
    fixed = TRUE
  )
  expect_match(html, 'aria-disabled="true"', fixed = TRUE)
  expect_match(html, ">Download</span>", fixed = TRUE)
})

test_that("buttons bring the controls along", {

  names <- vapply(
    htmltools::findDependencies(blockr_button("go", "Go")),
    `[[`, character(1L), "name"
  )

  expect_true(all(c("blockr-tokens", "blockr-buttons-css") %in% names))
})

test_that("the theme maps every Bootstrap button kind it names", {

  css <- paste(
    readLines(system.file("assets", "css", "blockr-theme.css",
                          package = "blockr.ui")),
    collapse = "\n"
  )

  for (cls in c("btn-primary", "btn-default", "btn-secondary",
                "btn-outline-secondary", "btn-light", "btn-link",
                "btn-danger")) {
    expect_match(css, paste0(":root:root .btn.", cls, "[ ,{]"), info = cls)
  }
  expect_match(css, ":root .btn-sm {", fixed = TRUE)

  # The solid accent button is gone.
  expect_no_match(css, "text-on-accent|color: #ffffff")
})
