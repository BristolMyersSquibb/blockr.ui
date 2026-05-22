# End-to-end test of inst/examples/minimal/app.R covering the same user
# interactions as the chromote-driven test plan: pin / unpin / Esc /
# outside-click / X-button, plus `input$main_sidebar` state.
#
# Skipped on CRAN, on CI without Chrome, and when shinytest2 isn't
# installed. Mirrors the test plan from PR #2.

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_cran()

app_path <- system.file("examples/minimal", package = "blockr.ui")
if (!nzchar(app_path)) {
  app_path <- file.path("..", "..", "inst", "examples", "minimal")
}
skip_if_not(dir.exists(app_path), "minimal example app not found on disk")

# Helper: read the panel's open/pinned state through Shiny's input cache.
panel_state <- function(app) {
  app$get_value(input = "main_sidebar")
}

panel_class <- function(app) {
  app$get_js("document.getElementById('main_sidebar').className")
}

dispatch_outside_click <- function(app) {
  # Synthesize a `mousedown` outside the panel so the document-level
  # outside-click handler fires (the panel's listener is on `mousedown`,
  # not `click`).
  app$run_js(paste0(
    "document.body.dispatchEvent(",
    "new MouseEvent('mousedown', { bubbles: true })",
    ")"
  ))
  app$wait_for_idle(500)
}

press_esc <- function(app) {
  app$run_js(paste0(
    "document.activeElement.dispatchEvent(",
    "new KeyboardEvent('keydown', { key: 'Escape', bubbles: true })",
    ")"
  ))
  app$wait_for_idle(500)
}

# Click an element in the running app via JS — keeps the long
# `document.querySelector(...).click()` calls inside a single helper so the
# call sites stay under 80 columns.
click_js <- function(app, selector) {
  app$run_js(sprintf("document.querySelector(%s).click()", shQuote(selector)))
  app$wait_for_idle(500)
}

test_that("mode = 'push' adds .blockr-html-pushed-* and a width on open", {
  push_app <- system.file("examples/push", package = "blockr.ui")
  if (!nzchar(push_app)) {
    push_app <- file.path("..", "..", "inst", "examples", "push")
  }
  skip_if_not(dir.exists(push_app), "push example app not found on disk")

  app <- shinytest2::AppDriver$new(
    push_app,
    name = "sidebar-push-mode",
    load_timeout = 20 * 1000,
    timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  html_class_initial <- app$get_js("document.documentElement.className")
  expect_false(grepl("blockr-html-pushed", html_class_initial %||% ""))

  app$click("open")
  app$wait_for_idle(500)

  html_class_open <- app$get_js("document.documentElement.className")
  expect_match(html_class_open, "blockr-html-pushed-right")

  width_open <- app$get_js(paste0(
    "document.documentElement.style.",
    "getPropertyValue('--blockr-sidebar-width')"
  ))
  expect_match(width_open, "[0-9]+px")
  expect_false(width_open == "0px")

  click_js(app, "#main_sidebar .blockr-sidebar-close")

  html_class_closed <- app$get_js("document.documentElement.className")
  expect_false(grepl("blockr-html-pushed", html_class_closed %||% ""))
  expect_equal(
    app$get_js(paste0(
      "document.documentElement.style.",
      "getPropertyValue('--blockr-sidebar-width')"
    )),
    "0px"
  )
})

test_that("two push sidebars constrain body between them, not under one", {
  push_both <- system.file("examples/push-both", package = "blockr.ui")
  if (!nzchar(push_both)) {
    push_both <- file.path("..", "..", "inst", "examples", "push-both")
  }
  skip_if_not(
    dir.exists(push_both),
    "push-both example app not found on disk"
  )

  app <- shinytest2::AppDriver$new(
    push_both,
    name = "sidebar-push-both",
    load_timeout = 20 * 1000,
    timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  width_var <- function(name) {
    app$get_js(sprintf(
      "document.documentElement.style.getPropertyValue('%s')", name
    ))
  }

  # Open left first: only left class + left width.
  app$click("open_left")
  app$wait_for_idle(500)
  html_class <- app$get_js("document.documentElement.className")
  expect_match(html_class, "blockr-html-pushed-left")
  expect_false(grepl("blockr-html-pushed-right", html_class))
  expect_match(width_var("--blockr-sidebar-width-left"), "[0-9]+px")
  expect_false(width_var("--blockr-sidebar-width-left") == "0px")
  expect_equal(width_var("--blockr-sidebar-width-right"), "0px")

  # Open right while left stays open: both classes + both widths.
  app$click("open_right")
  app$wait_for_idle(500)
  html_class <- app$get_js("document.documentElement.className")
  expect_match(html_class, "blockr-html-pushed-left")
  expect_match(html_class, "blockr-html-pushed-right")
  expect_false(width_var("--blockr-sidebar-width-left") == "0px")
  expect_false(width_var("--blockr-sidebar-width-right") == "0px")

  # Close right; left persists.
  click_js(app, "#right_sidebar .blockr-sidebar-close")
  html_class <- app$get_js("document.documentElement.className")
  expect_match(html_class, "blockr-html-pushed-left")
  expect_false(grepl("blockr-html-pushed-right", html_class))
  expect_false(width_var("--blockr-sidebar-width-left") == "0px")
  expect_equal(width_var("--blockr-sidebar-width-right"), "0px")
})

test_that("minimal example: pin / unpin / Esc / outside-click / X-button", {
  app <- shinytest2::AppDriver$new(
    app_path,
    name = "sidebar-minimal",
    load_timeout = 20 * 1000,
    timeout = 10 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # 1. Initial: closed.
  expect_equal(panel_state(app), list(open = FALSE, pinned = FALSE))

  # 2. Open.
  app$click("open")
  app$wait_for_idle(500)
  expect_equal(panel_state(app), list(open = TRUE, pinned = FALSE))
  expect_match(panel_class(app), "blockr-sidebar-open")

  pin_sel   <- "#main_sidebar .blockr-sidebar-pin"
  close_sel <- "#main_sidebar .blockr-sidebar-close"

  # 3. Pin.
  click_js(app, pin_sel)
  expect_equal(panel_state(app), list(open = TRUE, pinned = TRUE))

  # 4. Unpin.
  click_js(app, pin_sel)
  expect_equal(panel_state(app), list(open = TRUE, pinned = FALSE))

  # 5. Esc closes when not pinned.
  press_esc(app)
  expect_equal(panel_state(app)$open, FALSE)

  # 6. Esc with pin: still open.
  app$click("open")
  app$wait_for_idle(500)
  click_js(app, pin_sel)
  press_esc(app)
  expect_equal(panel_state(app), list(open = TRUE, pinned = TRUE))

  # 7. X button closes (overrides pin).
  click_js(app, close_sel)
  expect_equal(panel_state(app)$open, FALSE)

  # 8. Re-open + outside click closes (not pinned).
  app$click("open")
  app$wait_for_idle(500)
  dispatch_outside_click(app)
  expect_equal(panel_state(app)$open, FALSE)

  # 9. Re-open + pin + outside click stays open.
  app$click("open")
  app$wait_for_idle(500)
  click_js(app, pin_sel)
  dispatch_outside_click(app)
  expect_equal(panel_state(app), list(open = TRUE, pinned = TRUE))
})
