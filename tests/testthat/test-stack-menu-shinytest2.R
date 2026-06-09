# End-to-end test of inst/examples/stack-menu/app.R. Covers the
# user-facing flows the unit tests can't reach: live search, click
# toggles a card and the selection set, selection survives a search
# query that hides a selected card, swatch click updates the hex input,
# and confirm publishes a spec (with the nonce advancing on re-fires).

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_cran()

app_path <- system.file("examples/stack-menu", package = "blockr.ui")
if (!nzchar(app_path)) {
  app_path <- file.path("..", "..", "inst", "examples", "stack-menu")
}
skip_if_not(dir.exists(app_path), "stack-menu example app not found")

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
n_selected <- function(app) count_cards(app, ".card-selected")

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

set_text_input <- function(app, id, value) {
  app$run_js(sprintf(paste0(
    "(function() {",
    "  var el = document.getElementById('%s');",
    "  el.value = '%s';",
    "  el.dispatchEvent(new Event('input', { bubbles: true }));",
    "  el.dispatchEvent(new Event('change', { bubbles: true }));",
    "})()"
  ), id, value))
  app$wait_for_idle(300)
}

test_that("create flow: search, toggle, confirm publishes the spec", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "stack-menu-create",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_create")
  app$wait_for_idle(500)

  total <- n_visible(app)
  expect_true(total >= 2)

  set_search(app, "data")
  filtered <- n_visible(app)
  expect_true(filtered >= 1 && filtered < total)

  set_search(app, "")
  expect_equal(n_visible(app), total)

  # Toggle two cards.
  click_js(app, card_sel("a"))
  click_js(app, card_sel("b"))
  expect_equal(n_selected(app), 2)

  # Selection survives a search that hides one of the selected cards.
  set_search(app, "zzzzz_no_match")
  expect_equal(n_visible(app), 0)
  expect_true(card_has_class(app, "a", "card-selected"))
  set_search(app, "")
  expect_equal(n_selected(app), 2)

  # Fill name + commit.
  ns_name <- app$get_js(
    "document.querySelector('.blockr-stack-menu input[id$=\"-stack_name\"]').id"
  )
  set_text_input(app, ns_name, "MyStack")

  click_js(app, ".blockr-stack-menu-confirm")

  expect_equal(app$get_value(output = "commit_count"), "1")
  blocks <- strsplit(
    app$get_value(output = "commit_blocks"), ",", fixed = TRUE
  )[[1]]
  expect_setequal(blocks, c("a", "b"))
  expect_equal(app$get_value(output = "commit_name"), "MyStack")

  # Repeat confirm re-fires (nonce advances).
  click_js(app, ".blockr-stack-menu-confirm")
  expect_equal(app$get_value(output = "commit_count"), "2")
})

test_that("colour picker: moving the hue slider updates the hex input", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "stack-menu-palette",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_create")
  app$wait_for_idle(500)

  initial <- app$get_js(
    "document.querySelector('.blockr-stack-menu-hex').value"
  )
  expect_match(initial, "^#[0-9a-fA-F]{6}$")

  # Drive the hue slider programmatically and fire the input event so
  # the JS HSL -> hex recompute runs.
  app$run_js(paste0(
    "(function(){",
    "  var s = document.querySelector('.blockr-stack-menu-hue');",
    "  s.value = '210';",
    "  s.dispatchEvent(new Event('input', { bubbles: true }));",
    "})()"
  ))
  app$wait_for_idle(300)

  picked <- app$get_js(
    "document.querySelector('.blockr-stack-menu-hex').value"
  )
  expect_match(picked, "^#[0-9a-fA-F]{6}$")
  expect_false(identical(tolower(picked), tolower(initial)))

  # Commit and verify the colour reaches the server.
  click_js(app, card_sel("a"))
  click_js(app, ".blockr-stack-menu-confirm")
  expect_equal(tolower(app$get_value(output = "commit_color")), tolower(picked))
})

test_that("edit flow: pre-selects stack members and surfaces them", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "stack-menu-edit",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_edit")
  app$wait_for_idle(500)

  # `s1` contains d and e per the example app; both must be present and
  # pre-selected.
  expect_true(card_has_class(app, "d", "card-selected"))
  expect_true(card_has_class(app, "e", "card-selected"))

  # Deselect one and confirm.
  click_js(app, card_sel("d"))
  expect_false(card_has_class(app, "d", "card-selected"))
  click_js(app, ".blockr-stack-menu-confirm")

  blocks <- strsplit(
    app$get_value(output = "commit_blocks"), ",", fixed = TRUE
  )[[1]]
  expect_setequal(blocks, "e")
  # The committed stacks object is keyed by the edited stack's id.
  expect_equal(app$get_value(output = "commit_id"), "s1")
})
