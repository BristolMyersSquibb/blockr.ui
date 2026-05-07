test_that("sidebar_ui markup is Bootstrap-free", {
  panel <- sidebar_ui("main_sidebar")
  html <- as.character(panel)

  expect_false(grepl("\\boffcanvas\\b", html, ignore.case = TRUE))
  expect_false(grepl("class=\"[^\"]*\\bmodal\\b", html, ignore.case = TRUE))
  expect_false(grepl("data-bs-[a-z-]+=", html))
})

test_that("sidebar_ui sets initial state attributes", {
  panel <- sidebar_ui("main_sidebar", side = "left", width = "400px")
  html <- as.character(panel)

  expect_match(html, "id=\"main_sidebar\"", fixed = TRUE)
  expect_match(html, "data-side=\"left\"", fixed = TRUE)
  expect_match(html, "width: 400px", fixed = TRUE)
  expect_match(html, "aria-hidden=\"true\"", fixed = TRUE)
  expect_match(html, "class=\"blockr-sidebar\"", fixed = TRUE)
})

test_that("sidebar_ui attaches sidebar_dep() automatically", {
  panel <- sidebar_ui("main_sidebar")
  deps <- htmltools::findDependencies(panel)
  dep_names <- vapply(deps, function(x) x$name, character(1))

  expect_true("blockr-sidebar" %in% dep_names)
})

test_that("sidebar_ui rejects malformed inputs", {
  expect_error(sidebar_ui(""))
  expect_error(sidebar_ui(NULL))
  expect_error(sidebar_ui(c("a", "b")))
  expect_error(sidebar_ui("ok", side = "top"))
  expect_error(sidebar_ui("ok", width = ""))
})

test_that("sidebar_dep references the bundled CSS and JS files", {
  dep <- sidebar_dep()

  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$name, "blockr-sidebar")
  expect_identical(dep$stylesheet, "css/blockr-sidebar.css")
  expect_identical(dep$script, "js/blockr-sidebar.js")

  src_dir <- system.file(dep$src$file, package = dep$package)
  expect_true(nzchar(src_dir))
  expect_true(
    file.exists(file.path(src_dir, "css/blockr-sidebar.css"))
  )
  expect_true(
    file.exists(file.path(src_dir, "js/blockr-sidebar.js"))
  )
})

test_that("show_sidebar emits a `show` input message with HTML, deps, title", {
  ctx <- mock_session_with_capture()

  show_sidebar(
    "main_sidebar",
    title = "Add block",
    ui = shiny::tagList(
      shiny::textInput("block_name", "Name"),
      shiny::actionButton("confirm", "Add")
    ),
    session = ctx$session
  )

  msgs <- ctx$messages()
  expect_length(msgs, 1L)

  expect_identical(msgs[[1L]]$id, "main_sidebar")
  payload <- msgs[[1L]]$message
  expect_identical(payload$action, "show")
  expect_identical(payload$title, "Add block")
  expect_type(payload$html, "character")
  expect_match(payload$html, "id=\"block_name\"", fixed = TRUE)
  expect_match(payload$html, "id=\"confirm\"", fixed = TRUE)
  expect_type(payload$dependencies, "list")
})

test_that("show_sidebar forwards htmltools dependencies", {
  ctx <- mock_session_with_capture()

  ui_with_dep <- htmltools::attachDependencies(
    shiny::tags$div("hello"),
    htmltools::htmlDependency(
      name = "fake-dep",
      version = "1.0.0",
      src = c(href = "https://example.com/"),
      script = "fake.js"
    )
  )

  show_sidebar("main_sidebar", ui = ui_with_dep, session = ctx$session)

  payload <- ctx$messages()[[1L]]$message
  dep_names <- vapply(payload$dependencies, function(x) x$name, character(1))
  expect_true("fake-dep" %in% dep_names)
})

test_that("hide_sidebar emits a single `hide` input message", {
  ctx <- mock_session_with_capture()

  hide_sidebar("main_sidebar", session = ctx$session)

  msgs <- ctx$messages()
  expect_length(msgs, 1L)
  expect_identical(msgs[[1L]]$id, "main_sidebar")
  expect_identical(msgs[[1L]]$message, list(action = "hide"))
})

test_that("show_sidebar / hide_sidebar dispatch via the root session", {
  # A nested session-proxy must still target the absolute DOM id, not its
  # own namespaced id. `MockShinySession$makeScope("mod_a")` mimics what
  # Shiny does for moduleServer().
  ctx <- mock_session_with_capture()
  nested <- ctx$session$makeScope("mod_a")

  show_sidebar(
    "main_sidebar",
    ui = shiny::tags$div("hi"),
    session = nested
  )
  hide_sidebar("main_sidebar", session = nested)

  msgs <- ctx$messages()
  expect_length(msgs, 2L)
  # Both calls reached the root session and used the absolute id.
  expect_identical(msgs[[1L]]$id, "main_sidebar")
  expect_identical(msgs[[2L]]$id, "main_sidebar")
})

test_that("show_sidebar / hide_sidebar error outside a Shiny session", {
  expect_error(
    show_sidebar("main_sidebar", ui = shiny::div(), session = NULL),
    class = "blockr_ui_no_session"
  )
  expect_error(
    hide_sidebar("main_sidebar", session = NULL),
    class = "blockr_ui_no_session"
  )
})
