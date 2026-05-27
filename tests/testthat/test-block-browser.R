test_that("block_browser_ui renders one card per registered block", {
  panel <- block_browser_ui("mod_a", NULL, mode = "add")
  html <- as.character(htmltools::renderTags(panel)$html)

  cards <- gregexpr("data-block-type=\"[^\"]+\"", html)[[1]]
  expect_equal(length(cards), length(blockr.core::available_blocks()))
})

test_that("each card carries metadata data-attributes", {
  panel <- block_browser_ui("mod_a", NULL, mode = "add")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-name=", fixed = TRUE)
  expect_match(html, "data-description=", fixed = TRUE)
  expect_match(html, "data-package=", fixed = TRUE)
  expect_match(html, "data-category=", fixed = TRUE)
})

test_that("cards group under category sections with Uncategorized fallback", {
  panel <- block_browser_ui("mod_a", NULL, mode = "add")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "blockr-block-browser-category", fixed = TRUE)
  cats <- regmatches(html, gregexpr("data-category=\"[^\"]+\"", html))[[1]]
  expect_true(length(cats) > 0)
})

test_that("each card has an add button with a mode-specific label", {
  for (m in c("add", "append", "prepend")) {
    panel <- block_browser_ui("mod_a", NULL, mode = m,
                              trigger_id = if (m == "add") NULL else "x")
    html <- as.character(htmltools::renderTags(panel)$html)
    expect_match(html, "blockr-block-browser-card-add", fixed = TRUE)
    label <- switch(m, add = ">Add<", append = ">Append<",
                    prepend = ">Prepend<")
    expect_match(html, label, fixed = TRUE)
  }
})

test_that("the browser has no multiselect footer / connection / chip UI", {
  panel <- block_browser_ui("mod_a", NULL, mode = "add")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_false(grepl("blockr-block-browser-footer", html, fixed = TRUE))
  expect_false(grepl("blockr-block-browser-connection", html, fixed = TRUE))
  expect_false(grepl("blockr-block-browser-chips", html, fixed = TRUE))
  expect_false(grepl("blockr-block-browser-card-order", html, fixed = TRUE))
})

test_that("root id is NS(id)('commit') - the input-binding target", {
  panel <- block_browser_ui("mod_a", NULL, mode = "append", trigger_id = "x")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id=\"mod_a-commit\"", fixed = TRUE)
  # The ad-hoc data-commit-input attribute is gone (binding uses el.id).
  expect_false(grepl("data-commit-input", html, fixed = TRUE))
})

test_that("data-mode reflects the requested mode", {
  for (m in c("add", "append", "prepend")) {
    panel <- block_browser_ui("mod_a", NULL, mode = m,
                              trigger_id = if (m == "add") NULL else "x")
    html <- as.character(htmltools::renderTags(panel)$html)
    expect_match(html, paste0("data-mode=\"", m, "\""), fixed = TRUE)
  }
})

test_that("prepend with finite-arity target stamps the integer arity", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))
  panel <- block_browser_ui("mod_a", board, mode = "prepend", trigger_id = "m")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-target-arity=\"2\"", fixed = TRUE)
})

test_that("prepend with arity-1 target stamps '1'", {
  h <- blockr.core::new_head_block()
  board <- blockr.core::new_board(blocks = list(h = h))
  panel <- block_browser_ui("mod_a", board, mode = "prepend", trigger_id = "h")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-target-arity=\"1\"", fixed = TRUE)
})

test_that("prepend with variadic target stamps 'inf'", {
  r <- blockr.core::new_rbind_block()
  board <- blockr.core::new_board(blocks = list(r = r))
  panel <- block_browser_ui("mod_a", board, mode = "prepend", trigger_id = "r")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-target-arity=\"inf\"", fixed = TRUE)
})

test_that("add and append never stamp data-target-arity", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))

  panel_add <- block_browser_ui("mod_a", board, mode = "add")
  expect_false(grepl("data-target-arity", as.character(panel_add)))

  panel_app <- block_browser_ui("mod_a", board, mode = "append",
                                trigger_id = "m")
  expect_false(grepl("data-target-arity", as.character(panel_app)))
})

test_that("append / prepend render a context subtitle; add does not", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))

  for (mode in c("append", "prepend")) {
    panel <- block_browser_ui("mod_a", board, mode = mode, trigger_id = "m")
    html <- as.character(htmltools::renderTags(panel)$html)
    expect_match(html, "blockr-block-browser-context", fixed = TRUE,
                 info = paste0("mode=", mode))
  }

  panel_add <- block_browser_ui("mod_a", board, mode = "add")
  html_add <- as.character(htmltools::renderTags(panel_add)$html)
  expect_false(grepl("blockr-block-browser-context", html_add, fixed = TRUE))
})

test_that("per-card form field ids are namespaced by id + block type", {
  panel <- block_browser_ui("mod_a", NULL, mode = "add")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id=\"mod_a-new_dataset_block_id\"", fixed = TRUE)
  expect_match(html, "id=\"mod_a-new_dataset_block_title\"", fixed = TRUE)
})

test_that("default block ids are unique and avoid the board's existing ids", {
  existing <- c("aaa", "bbb")
  board <- blockr.core::new_board(
    blocks = stats::setNames(
      list(blockr.core::new_dataset_block(),
           blockr.core::new_dataset_block()),
      existing
    )
  )
  panel <- block_browser_ui("mod_a", board, mode = "add")
  html <- as.character(htmltools::renderTags(panel)$html)

  vals <- regmatches(
    html,
    gregexpr("id=\"mod_a-[a-z_]+_id\" value=\"([^\"]+)\"", html)
  )[[1]]
  ids <- sub(".*value=\"([^\"]+)\"$", "\\1", vals)
  expect_true(length(ids) >= 1)
  expect_equal(length(ids), length(unique(ids)))      # unique among cards
  expect_false(any(ids %in% existing))                 # avoid board ids
})

test_that("per-card forms never register Shiny-bound inputs", {
  panel <- block_browser_ui("mod_a", NULL, mode = "append", trigger_id = "x")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_false(grepl("class=\"[^\"]*shiny-bound-input", html))
  expect_false(grepl("class=\"[^\"]*shiny-input-", html))
})

test_that("block_browser_dep references the bundled CSS and JS files", {
  dep <- block_browser_dep()
  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$name, "blockr-block-browser")
  expect_identical(dep$stylesheet, "css/blockr-block-browser.css")
  expect_identical(dep$script, "js/blockr-block-browser.js")

  asset_root <- system.file("assets", package = "blockr.ui")
  expect_true(
    file.exists(file.path(asset_root, "css", "blockr-block-browser.css"))
  )
  expect_true(
    file.exists(file.path(asset_root, "js", "blockr-block-browser.js"))
  )
})

test_that("block_browser_ui attaches the dependency", {
  panel <- block_browser_ui("mod_a", NULL, mode = "add")
  deps <- htmltools::findDependencies(panel)
  dep_names <- vapply(deps, function(x) x$name, character(1))
  expect_true("blockr-block-browser" %in% dep_names)
})

test_that("block_browser_server returns the committed spec without nonce", {
  shiny::testServer(
    block_browser_server,
    args = list(id = "browser"),
    {
      session$setInputs(
        commit = list(type = "new_dataset_block", id = "foo",
                      title = NULL, link_id = NULL, block_input = NULL,
                      target_input = NULL, nonce = 1)
      )
      out <- session$returned()
      expect_equal(out$type, "new_dataset_block")
      expect_equal(out$id, "foo")
      expect_null(out$nonce)
    }
  )
})

test_that("malformed inputs are rejected", {
  expect_error(block_browser_ui(character(0), NULL, mode = "add"))
  expect_error(block_browser_ui(c("a", "b"), NULL, mode = "add"))
  expect_error(block_browser_ui("mod_a", NULL, mode = "noop"))
  expect_error(block_browser_ui("mod_a", NULL, mode = "append",
                                trigger_id = c("a", "b")))
})
