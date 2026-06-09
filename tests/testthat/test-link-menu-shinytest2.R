# End-to-end test of inst/examples/link-menu/app.R. Covers the
# user-facing flows the unit tests can't reach: live search, click
# commits an OUTGOING / INCOMING link with the correct orientation,
# chevron toggle, in-card "Add link" with edited values, and the
# live `pool-update` that hides a just-wired card without
# re-rendering.

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_cran()

app_path <- system.file("examples/link-menu", package = "blockr.ui")
if (!nzchar(app_path)) {
  app_path <- file.path("..", "..", "inst", "examples", "link-menu")
}
skip_if_not(dir.exists(app_path), "link-menu example app not found")

card_sel <- function(type, dir = NULL) {
  if (is.null(dir)) {
    paste0(".blockr-block-browser-card[data-block-type=", type, "]")
  } else {
    paste0(
      ".blockr-link-menu-direction[data-direction=", dir,
      "] .blockr-block-browser-card[data-block-type=", type, "]"
    )
  }
}

n_visible <- function(app) {
  app$get_js(paste0(
    "document.querySelectorAll(",
    "'.blockr-block-browser-card:not(.hidden)'",
    ").length"
  ))
}

click_js <- function(app, selector) {
  app$run_js(sprintf("document.querySelector('%s').click()", selector))
  app$wait_for_idle(400)
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

test_that("search filters cards live across both sections", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-search",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_m")
  app$wait_for_idle(500)

  total <- n_visible(app)
  expect_true(total >= 2)

  set_search(app, "head")
  filtered <- n_visible(app)
  expect_true(filtered >= 1 && filtered < total)

  set_search(app, "")
  expect_equal(n_visible(app), total)
})

test_that("OUTGOING card commit composes source = anchor, target = card", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-outgoing",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_a")
  app$wait_for_idle(500)

  click_js(app, card_sel("h", "outgoing"))

  expect_equal(app$get_value(output = "commit_source"), "a")
  expect_equal(app$get_value(output = "commit_target"), "h")
  expect_equal(app$get_value(output = "commit_count"), "1")
})

test_that("INCOMING card commit composes source = card, target = anchor", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-incoming",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Anchor m has incoming candidates (a, h, r since they can source it).
  app$click("open_m")
  app$wait_for_idle(500)

  click_js(app, card_sel("a", "incoming"))

  expect_equal(app$get_value(output = "commit_source"), "a")
  expect_equal(app$get_value(output = "commit_target"), "m")
  expect_equal(app$get_value(output = "commit_count"), "1")
})

test_that("chevron toggle expands the card without committing", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-chevron",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_a")
  app$wait_for_idle(500)

  app$run_js(sprintf(paste0(
    "document.querySelector('%s .blockr-block-browser-card-chevron')",
    ".click()"
  ), card_sel("h", "outgoing")))
  app$wait_for_idle(300)

  expanded <- app$get_js(sprintf(
    "document.querySelector('%s').classList.contains('card-expanded')",
    card_sel("h", "outgoing")
  ))
  expect_true(expanded)
  expect_equal(app$get_value(output = "commit_count"), "0")
})

test_that("in-card Add link commits with edited values", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-incard",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_a")
  app$wait_for_idle(500)

  # Open m's card (arity 2; has the block_input picker).
  app$run_js(sprintf(paste0(
    "document.querySelector('%s .blockr-block-browser-card-chevron')",
    ".click()"
  ), card_sel("m", "outgoing")))
  app$wait_for_idle(300)

  # Edit Link ID + switch the port to "y".
  link_sel <- ".blockr-block-browser-field-link-id input"
  port_sel <- ".blockr-block-browser-field-block-input select"
  app$run_js(sprintf(paste0(
    "(function() {",
    "  var card = document.querySelector('%s');",
    "  var link_id = card.querySelector('%s');",
    "  link_id.value = 'my_link';",
    "  link_id.dispatchEvent(new Event('input', { bubbles: true }));",
    "  var port = card.querySelector('%s');",
    "  port.value = 'y';",
    "  port.dispatchEvent(new Event('change', { bubbles: true }));",
    "})()"
  ), card_sel("m", "outgoing"), link_sel, port_sel))
  app$wait_for_idle(300)

  click_js(
    app,
    sprintf("%s .blockr-block-browser-card-add", card_sel("m", "outgoing"))
  )

  expect_equal(app$get_value(output = "commit_source"), "a")
  expect_equal(app$get_value(output = "commit_target"), "m")
  expect_equal(app$get_value(output = "commit_link_id"), "my_link")
  expect_equal(app$get_value(output = "commit_block_input"), "y")
})

test_that("live pool-update hides just-wired card without re-rendering", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-pool-update",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$click("open_a")
  app$wait_for_idle(500)

  before <- n_visible(app)
  expect_true(before >= 2)

  # Commit a link to `h` (its only port).
  click_js(app, card_sel("h", "outgoing"))
  expect_equal(app$get_value(output = "commit_count"), "1")

  # The h card should now be hidden (its only port got wired).
  app$wait_for_idle(500)
  after <- n_visible(app)
  expect_lt(after, before)

  h_hidden <- app$get_js(sprintf(
    "document.querySelector('%s').classList.contains('hidden')",
    card_sel("h", "outgoing")
  ))
  expect_true(h_hidden)

  # The remaining cards (m, r) should still be visible and clickable.
  expect_gt(after, 0)
})

test_that("card-body click after pool-update wires to the first free port", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-default-port",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Anchor a (dataset): m is OUTGOING with both ports x / y free.
  app$click("open_a")
  app$wait_for_idle(500)

  # First commit via card-body click: takes the default port "x".
  click_js(app, card_sel("m", "outgoing"))
  app$wait_for_idle(600)
  expect_equal(app$get_value(output = "commit_block_input"), "x")

  # Second commit via card-body click (no chevron open). The pool-
  # update should have refreshed m's select to only ["y"]; the JS
  # snaps the select's value to "y" so this card-body click commits
  # with port "y", not "x".
  click_js(app, card_sel("m", "outgoing"))
  app$wait_for_idle(600)
  expect_equal(app$get_value(output = "commit_block_input"), "y")
  expect_equal(app$get_value(output = "commit_count"), "2")
})

test_that("pool-update refreshes per-card block-input options", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-input-refresh",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Anchor a (dataset): m is OUTGOING with both ports free.
  app$click("open_a")
  app$wait_for_idle(500)

  # Open m's card and commit a link on port "x".
  app$run_js(sprintf(paste0(
    "document.querySelector('%s .blockr-block-browser-card-chevron')",
    ".click()"
  ), card_sel("m", "outgoing")))
  app$wait_for_idle(300)

  options_before <- app$get_js(sprintf(paste0(
    "Array.from(document.querySelectorAll(",
    "'%s .blockr-block-browser-field-block-input select option'",
    ")).map(function(o) { return o.value; })"
  ), card_sel("m", "outgoing")))
  expect_setequal(options_before, c("x", "y"))

  port_sel <- sprintf(
    "%s .blockr-block-browser-field-block-input select",
    card_sel("m", "outgoing")
  )
  app$run_js(sprintf(paste0(
    "(function(){var s=document.querySelector('%s');",
    "s.value='x';",
    "s.dispatchEvent(new Event('change', { bubbles: true }));})()"
  ), port_sel))
  app$wait_for_idle(200)

  click_js(
    app,
    sprintf("%s .blockr-block-browser-card-add", card_sel("m", "outgoing"))
  )
  app$wait_for_idle(600)

  # The pool-update should have refreshed m's block-input options to
  # only contain "y" now; the field itself is hidden (one-option
  # dropdown is useless chrome).
  options_after <- app$get_js(sprintf(paste0(
    "Array.from(document.querySelectorAll(",
    "'%s .blockr-block-browser-field-block-input select option'",
    ")).map(function(o) { return o.value; })"
  ), card_sel("m", "outgoing")))
  expect_setequal(options_after, "y")

  field_hidden <- app$get_js(sprintf(paste0(
    "document.querySelector(",
    "'%s .blockr-block-browser-field-block-input'",
    ").style.display === 'none'"
  ), card_sel("m", "outgoing")))
  expect_true(field_hidden)
})

test_that("repeat commits re-fire (nonce advances)", {
  app <- shinytest2::AppDriver$new(
    app_path, name = "link-menu-repeat",
    load_timeout = 20 * 1000, timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Anchor `a`, click the h card, then click the m card; each click
  # should bump the commit count.
  app$click("open_a")
  app$wait_for_idle(500)

  click_js(app, card_sel("h", "outgoing"))
  expect_equal(app$get_value(output = "commit_count"), "1")

  app$wait_for_idle(400)
  click_js(app, card_sel("m", "outgoing"))
  expect_equal(app$get_value(output = "commit_count"), "2")
})
