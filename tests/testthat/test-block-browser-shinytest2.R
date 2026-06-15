# End-to-end test of inst/examples/block-browser/app.R. Covers the
# user-facing flows the unit tests can't reach: live search filter,
# click-to-add-with-defaults, and open-edit-add via the in-card button.

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_cran()

app_path <- system.file("examples/block-browser", package = "blockr.ui")
if (!nzchar(app_path)) {
  app_path <- file.path("..", "..", "inst", "examples", "block-browser")
}
skip_if_not(dir.exists(app_path), "block-browser example app not found")

card_sel <- function(type) {
  paste0(".blockr-block-browser-card[data-block-type=", type, "]")
}

count_cards <- function(app, extra_selector) {
  app$get_js(sprintf(
    "document.querySelectorAll('.blockr-block-browser-card%s').length",
    extra_selector
  ))
}

n_visible <- function(app) count_cards(app, ":not(.hidden)")

card_has_class <- function(app, type, klass) {
  app$get_js(sprintf(
    "document.querySelector('%s').classList.contains('%s')",
    card_sel(type), klass
  ))
}

click_js <- function(app, selector) {
  app$run_js(sprintf("document.querySelector('%s').click()", selector))
  app$wait_for_idle(300)
}

set_search <- function(app, query) {
  app$run_js(sprintf(paste0(
    "(function() {",
    "  var el = document.querySelector('.blockr-block-browser-search');",
    "  el.value = '%s';",
    "  el.dispatchEvent(new Event('input', { bubbles: true }));",
    "})()"
  ), query))
  app$wait_for_idle(300)
}

test_that("search filters cards live", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "block-browser-search",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_add")
  app$wait_for_idle(500)

  total <- n_visible(app)
  expect_true(total >= 1)

  set_search(app, "data")
  expect_true(n_visible(app) >= 1)
  expect_true(n_visible(app) < total)

  set_search(app, "")
  expect_equal(n_visible(app), total)
})

test_that("clicking a card body adds it immediately with defaults", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "block-browser-click-add",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_add")
  app$wait_for_idle(500)

  # A plain header click adds without expanding the card.
  click_js(app, paste0(
    card_sel("dataset_block"), " .blockr-block-browser-card-header"
  ))
  expect_false(card_has_class(app, "dataset_block", "card-expanded"))

  # The single-block commit fires with the clicked block's type and a
  # non-empty default id.
  expect_equal(app$get_value(output = "commit_type"), "dataset_block")
  expect_true(nzchar(app$get_value(output = "commit_id")))
})

test_that("chevron opens the form; the in-card button adds with edits", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "block-browser-edit-add",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_add")
  app$wait_for_idle(500)

  # Chevron expands the card without adding.
  click_js(app, paste0(
    card_sel("head_block"), " .blockr-block-browser-card-chevron"
  ))
  expect_true(card_has_class(app, "head_block", "card-expanded"))
  expect_equal(app$get_value(output = "commit_type"), "")

  # Type a custom id, then click the in-card add button.
  app$run_js(sprintf(paste0(
    "(function(){var el=document.querySelector(",
    "'%s .blockr-block-browser-field-id input');",
    "el.value='my_custom_id';})()"
  ), card_sel("head_block")))
  click_js(app, paste0(
    card_sel("head_block"), " .blockr-block-browser-card-add"
  ))

  expect_equal(app$get_value(output = "commit_type"), "head_block")
  expect_equal(app$get_value(output = "commit_id"), "my_custom_id")
})

test_that("clicking inside the form does not add the block", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "block-browser-form-noadd",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_add")
  app$wait_for_idle(500)

  click_js(app, paste0(
    card_sel("dataset_block"), " .blockr-block-browser-card-chevron"
  ))
  # Clicking the id input must not trigger a commit.
  click_js(app, paste0(
    card_sel("dataset_block"), " .blockr-block-browser-field-id input"
  ))
  expect_equal(app$get_value(output = "commit_type"), "")
})

test_that("the pre-rendered add browser is not rebuilt across opens", {
  # The add browser's body is pre-rendered once into #add_panel and the
  # button only toggles the panel. Tagging the live DOM node and finding
  # the tag intact after a close / reopen proves it was not re-rendered.
  app <- shinytest2::AppDriver$new(
    app_path, name = "block-browser-prerender",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  add_browser <- "#add_panel .blockr-block-browser"

  app$click("open_add")
  app$wait_for_idle(500)
  app$run_js(sprintf(
    "document.querySelector('%s').dataset.persist = 'yes'", add_browser
  ))

  click_js(app, "#add_panel .blockr-sidebar-close")
  app$click("open_add")
  app$wait_for_idle(500)

  expect_equal(
    app$get_js(sprintf(
      "document.querySelector('%s').dataset.persist", add_browser
    )),
    "yes"
  )
})
