test_that("block_browser_ui renders one card per registered block", {
  panel <- block_browser_ui("mod_a", NULL)
  html <- as.character(htmltools::renderTags(panel)$html)

  cards <- gregexpr("data-block-type=\"[^\"]+\"", html)[[1]]
  expect_equal(length(cards), length(blockr.core::available_blocks()))
})

test_that("each card carries metadata data-attributes", {
  panel <- block_browser_ui("mod_a", NULL)
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-name=", fixed = TRUE)
  expect_match(html, "data-description=", fixed = TRUE)
  expect_match(html, "data-package=", fixed = TRUE)
  expect_match(html, "data-category=", fixed = TRUE)
})

test_that("collapsed cards expose the description as a band and a title hint", {
  panel <- block_browser_ui("mod_a", NULL)
  html <- as.character(htmltools::renderTags(panel)$html)

  # The full description lives in the expanded card's description band ...
  expect_match(html, "blockr-block-browser-card-descr-band", fixed = TRUE)
  # ... and a collapsed card root additionally carries it as a native
  # `title` hint (the JS toggle removes this on expand).
  expect_match(
    html, "class=\"blockr-block-browser-card\"[^>]*title=\"", perl = TRUE
  )
})

test_that("cards group under category sections with Uncategorized fallback", {
  panel <- block_browser_ui("mod_a", NULL)
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "blockr-block-browser-category", fixed = TRUE)
  cats <- regmatches(html, gregexpr("data-category=\"[^\"]+\"", html))[[1]]
  expect_true(length(cats) > 0)
})

test_that("the add button label matches the flow", {
  # The button carries the leading plus glyph then the label, so match
  # the add-button class followed (after the svg) by the verb.
  add_label <- function(ui) {
    html <- as.character(htmltools::renderTags(ui)$html)
    flat <- gsub("\\s+", " ", html)
    sub(
      paste0(
        ".*blockr-block-browser-card-add[^>]*> ?",
        "(?:<svg.*?</svg> ?)?([A-Za-z]+).*"
      ),
      "\\1", flat, perl = TRUE
    )
  }
  expect_equal(add_label(block_browser_ui("a", NULL)), "Add")
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))
  expect_equal(
    add_label(block_browser_ui("a", board, append_to("m"))), "Append"
  )
  expect_equal(
    add_label(block_browser_ui("a", board, prepend_to("m"))), "Prepend"
  )
})

test_that("the browser has no multiselect footer / connection / chip UI", {
  panel <- block_browser_ui("mod_a", NULL)
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_false(grepl("blockr-block-browser-footer", html, fixed = TRUE))
  expect_false(grepl("blockr-block-browser-connection", html, fixed = TRUE))
  expect_false(grepl("blockr-block-browser-chips", html, fixed = TRUE))
  expect_false(grepl("blockr-block-browser-card-order", html, fixed = TRUE))
})

test_that("root id is NS(id)('commit') - the input-binding target", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))
  panel <- block_browser_ui("mod_a", board, append_to("m"))
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id=\"mod_a-commit\"", fixed = TRUE)
  expect_false(grepl("data-commit-input", html, fixed = TRUE))
})

test_that("data-mode reflects the resolved flow", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))

  add <- as.character(block_browser_ui("a", NULL))
  app <- as.character(block_browser_ui("a", board, append_to("m")))
  pre <- as.character(block_browser_ui("a", board, prepend_to("m")))

  expect_match(add, "data-mode=\"add\"", fixed = TRUE)
  expect_match(app, "data-mode=\"append\"", fixed = TRUE)
  expect_match(pre, "data-mode=\"prepend\"", fixed = TRUE)
})

test_that("append_to / prepend_to build validated bb_target descriptors", {
  expect_s3_class(append_to("x"), "bb_target")
  expect_s3_class(prepend_to("x"), "bb_target")
  expect_identical(append_to("x")$mode, "append")
  expect_identical(prepend_to("y")$id, "y")
  expect_error(append_to(""))
  expect_error(prepend_to(c("a", "b")))
})

test_that("prepend stamps data-target-arity from the target block", {
  for (tc in list(
    list(blk = blockr.core::new_merge_block(), arity = "2"),
    list(blk = blockr.core::new_head_block(),  arity = "1"),
    list(blk = blockr.core::new_rbind_block(), arity = "inf")
  )) {
    board <- blockr.core::new_board(blocks = list(t = tc$blk))
    html <- as.character(block_browser_ui("a", board, prepend_to("t")))
    expect_match(html, paste0("data-target-arity=\"", tc$arity, "\""),
                 fixed = TRUE)
  }
})

test_that("add and append never stamp data-target-arity", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))

  expect_false(grepl("data-target-arity", as.character(
    block_browser_ui("a", board)
  )))
  expect_false(grepl("data-target-arity", as.character(
    block_browser_ui("a", board, append_to("m"))
  )))
})

test_that("append / prepend render a context subtitle; add does not", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))

  for (target in list(append_to("m"), prepend_to("m"))) {
    html <- as.character(block_browser_ui("a", board, target))
    expect_match(html, "blockr-block-browser-context", fixed = TRUE)
  }
  expect_false(grepl("blockr-block-browser-context",
                     as.character(block_browser_ui("a", board)), fixed = TRUE))
})

test_that("only the flow's fields are rendered", {
  m <- blockr.core::new_merge_block()   # arity 2
  h <- blockr.core::new_head_block()    # arity 1
  board <- blockr.core::new_board(blocks = list(m = m, h = h))

  has_field <- function(html, suffix) {
    grepl(paste0("blockr-block-browser-field-", suffix), html, fixed = TRUE)
  }

  add <- as.character(block_browser_ui("a", board))
  expect_true(has_field(add, "id"))
  expect_true(has_field(add, "title"))
  expect_false(has_field(add, "link-id"))
  expect_false(has_field(add, "block-input"))
  expect_false(has_field(add, "target-input"))

  app <- as.character(block_browser_ui("a", board, append_to("m")))
  expect_true(has_field(app, "link-id"))
  expect_true(has_field(app, "block-input"))
  expect_false(has_field(app, "target-input"))

  pre2 <- as.character(block_browser_ui("a", board, prepend_to("m")))
  expect_true(has_field(pre2, "link-id"))
  expect_true(has_field(pre2, "target-input"))    # arity 2 -> shown
  expect_false(has_field(pre2, "block-input"))

  pre1 <- as.character(block_browser_ui("a", board, prepend_to("h")))
  expect_true(has_field(pre1, "link-id"))
  expect_false(has_field(pre1, "target-input"))   # arity 1 -> hidden
})

test_that("append candidates: arity 0 filtered, named + variadic kept", {
  # head_block:    arity 1 named ("data")   -> kept, picker hidden.
  # merge_block:   arity 2 named ("x","y")  -> kept, picker shown.
  # rbind_block:   arity NA (variadic)      -> kept, picker hidden
  #                                            (slot is generated by the
  #                                             dock at append time).
  # dataset_block: arity 0                  -> dropped entirely.
  src <- blockr.core::new_dataset_block()
  board <- blockr.core::new_board(blocks = list(s = src))

  html <- as.character(block_browser_ui("a", board, append_to("s")))

  expect_match(html, "data-block-type=\"head_block\"", fixed = TRUE)
  expect_false(grepl("id=\"a-head_block_block_input\"",
                     html, fixed = TRUE))

  expect_match(html, "id=\"a-merge_block_block_input\"", fixed = TRUE)

  # Variadic block stays in the catalogue but has no rendered picker
  # (block_inputs() is character(0); the dock generates a slot).
  expect_match(html, "data-block-type=\"rbind_block\"", fixed = TRUE)
  expect_false(grepl("id=\"a-rbind_block_block_input\"",
                     html, fixed = TRUE))

  # arity 0 -> filtered out.
  expect_false(grepl("data-block-type=\"dataset_block\"",
                     html, fixed = TRUE))
})

test_that("target_input picker excludes slots taken by incoming links", {
  # Dataset -> Merge.x leaves only Merge.y free. Prepending to Merge
  # must not show `x` as a target_input option, and (with only one
  # free slot) the picker collapses entirely.
  d <- blockr.core::new_dataset_block()
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(
    blocks = list(d = d, m = m),
    links = list(l1 = blockr.core::new_link(from = "d", to = "m",
                                            input = "x"))
  )

  html <- as.character(block_browser_ui("a", board, prepend_to("m")))

  # Only `y` is free -> picker hidden (length(target_inputs) <= 1).
  expect_false(grepl("blockr-block-browser-field-target-input",
                     html, fixed = TRUE))

  # Sanity: if no link exists yet, both slots are free -> picker shown.
  board_free <- blockr.core::new_board(blocks = list(m = m))
  html_free <- as.character(block_browser_ui("a", board_free,
                                             prepend_to("m")))
  expect_true(grepl("blockr-block-browser-field-target-input",
                    html_free, fixed = TRUE))
  # And the dropdown options contain both x and y.
  expect_match(html_free, "<option value=\"x\">", fixed = TRUE)
  expect_match(html_free, "<option value=\"y\">", fixed = TRUE)
})

test_that("per-card form field ids are namespaced by id + block type", {
  panel <- block_browser_ui("mod_a", NULL)
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id=\"mod_a-dataset_block_id\"", fixed = TRUE)
  expect_match(html, "id=\"mod_a-dataset_block_title\"", fixed = TRUE)
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
  panel <- block_browser_ui("mod_a", board)
  html <- as.character(htmltools::renderTags(panel)$html)

  vals <- regmatches(
    html,
    gregexpr("id=\"mod_a-[a-z_]+_id\" value=\"([^\"]+)\"", html)
  )[[1]]
  ids <- sub(".*value=\"([^\"]+)\"$", "\\1", vals)
  expect_true(length(ids) >= 1)
  expect_equal(length(ids), length(unique(ids)))
  expect_false(any(ids %in% existing))
})

test_that("per-card forms never register Shiny-bound inputs", {
  m <- blockr.core::new_merge_block()
  board <- blockr.core::new_board(blocks = list(m = m))
  panel <- block_browser_ui("mod_a", board, append_to("m"))
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
  panel <- block_browser_ui("mod_a", NULL)
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
        commit = list(type = "dataset_block", id = "foo",
                      title = NULL, link_id = NULL, block_input = NULL,
                      target_input = NULL, nonce = 1)
      )
      out <- session$returned()
      expect_equal(out$type, "dataset_block")
      expect_equal(out$id, "foo")
      expect_null(out$nonce)
    }
  )
})

test_that("malformed inputs are rejected", {
  expect_error(block_browser_ui(character(0), NULL))
  expect_error(block_browser_ui(c("a", "b"), NULL))
  expect_error(block_browser_ui("mod_a", NULL, target = "not-a-target"))
})
