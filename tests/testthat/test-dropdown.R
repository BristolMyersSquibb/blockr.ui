test_that("dropdown() puts the toggle and the panel in its wrapper", {

  dd <- dropdown(
    htmltools::tags$button(type = "button", "Views"),
    shiny::textInput("search", "Search"),
    NULL,
    align = "end"
  )

  html <- htmltools::renderTags(dd)$html

  expect_match(html, 'class="blockr-dropdown" data-align="end"', fixed = TRUE)
  expect_match(
    html,
    'type="button" class="blockr-dropdown__toggle" aria-expanded="false"',
    fixed = TRUE
  )
  expect_match(html, 'class="blockr-dropdown__panel blockr-menu"',
               fixed = TRUE)

  expect_match(html, 'id="search"', fixed = TRUE)

  expect_true(
    "blockr-ui-js" %in%
      blockr.core::chr_xtr(htmltools::findDependencies(dd), "name")
  )

  expect_identical(
    htmltools::tagGetAttribute(
      dropdown(htmltools::tags$button("Views")),
      "data-align"
    ),
    "start"
  )
})

test_that("dropdown() wants a tag to toggle it", {
  expect_error(dropdown("Views"), "must be an HTML tag")
})
