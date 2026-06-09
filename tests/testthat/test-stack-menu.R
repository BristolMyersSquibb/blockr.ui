two_block_board <- function() {
  blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        b = blockr.core::new_head_block()
      )
    )
  )
}

board_with_stack <- function(stack_blocks = "a") {
  blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        b = blockr.core::new_head_block()
      )
    ),
    stacks = list(s1 = blockr.core::new_stack(stack_blocks))
  )
}

card_ids <- function(panel) {
  html <- as.character(htmltools::renderTags(panel)$html)
  m <- regmatches(html, gregexpr("data-block-type=\"[^\"]+\"", html))[[1]]
  sub("data-block-type=\"([^\"]+)\"", "\\1", m)
}

test_that("create mode renders one card per eligible board block", {
  panel <- stack_menu_ui("mod", two_block_board())
  expect_setequal(card_ids(panel), c("a", "b"))

  html <- as.character(htmltools::renderTags(panel)$html)
  expect_match(html, "data-mode=\"create\"", fixed = TRUE)
  expect_match(html, "id=\"mod-commit\"", fixed = TRUE)
  expect_false(grepl("card-selected", html, fixed = TRUE))
})

test_that("cards carry the block-browser data-* attributes", {
  panel <- stack_menu_ui("mod", two_block_board())
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-name=", fixed = TRUE)
  expect_match(html, "data-description=", fixed = TRUE)
  expect_match(html, "data-package=", fixed = TRUE)
  expect_match(html, "data-category=", fixed = TRUE)
})

test_that("eligible pool excludes blocks already in a stack", {
  panel <- stack_menu_ui("mod", board_with_stack("a"))
  expect_setequal(card_ids(panel), "b")
})

test_that("edit mode pre-selects the stack's blocks", {
  panel <- stack_menu_ui("mod", board_with_stack("a"), target = "s1")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-mode=\"edit\"", fixed = TRUE)
  expect_setequal(card_ids(panel), c("a", "b"))
  pat <- paste0(
    "card-selected[^\"]*\"[^>]*data-block-type=\"a\"",
    "|data-block-type=\"a\"[^>]*data-selected=\"true\""
  )
  expect_match(html, pat)
})

test_that("edit mode augments the pool with the target stack's blocks", {
  # `a` is in s1, so available_stack_blocks returns only `b`; edit must
  # still surface `a` so the user can deselect it.
  panel <- stack_menu_ui("mod", board_with_stack("a"), target = "s1")
  expect_true("a" %in% card_ids(panel))
})

test_that("edit mode pre-fills name and color", {
  brd <- board_with_stack("a")
  panel <- stack_menu_ui("mod", brd, target = "s1")
  html <- as.character(htmltools::renderTags(panel)$html)

  pat <- paste0(
    "id=\"mod-stack_name\"[^>]*value=\"Stack\"",
    "|value=\"Stack\"[^>]*id=\"mod-stack_name\""
  )
  expect_match(html, pat)
})

test_that("create mode renders all three fields visibly top-level", {
  panel <- stack_menu_ui("mod", two_block_board())
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id=\"mod-stack_name\"", fixed = TRUE)
  expect_match(html, "id=\"mod-stack_color\"", fixed = TRUE)
  expect_match(html, "id=\"mod-stack_id\"", fixed = TRUE)
  # No collapsible toggle.
  expect_false(grepl("<details", html, fixed = TRUE))
  expect_false(grepl("<summary", html, fixed = TRUE))
  expect_false(grepl("blockr-stack-menu-advanced", html, fixed = TRUE))
})

test_that("create mode pre-fills a non-empty default stack name", {
  panel <- stack_menu_ui("mod", two_block_board())
  html <- as.character(htmltools::renderTags(panel)$html)

  # The default is a random rand_names() value, so assert the input
  # carries a non-empty value (>= 1 char) rather than a fixed string.
  expect_match(html, "id=\"mod-stack_name\"[^>]*value=\"[^\"]+\"")
})

test_that("edit mode omits the stack_id input entirely", {
  panel <- stack_menu_ui("mod", board_with_stack("a"), target = "s1")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_false(grepl("id=\"mod-stack_id\"", html, fixed = TRUE))
  expect_false(grepl("<details", html, fixed = TRUE))
  expect_match(html, "id=\"mod-stack_color\"", fixed = TRUE)
})

test_that("card id subtitle is prefixed with 'id:'", {
  panel <- stack_menu_ui("mod", two_block_board())
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id: a", fixed = TRUE)
  expect_match(html, "id: b", fixed = TRUE)
})

test_that("color field carries the hue / lightness sliders + hex input", {
  panel <- stack_menu_ui("mod", two_block_board())
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "blockr-stack-menu-hue", fixed = TRUE)
  expect_match(html, "blockr-stack-menu-lightness", fixed = TRUE)
  expect_match(html, "blockr-stack-menu-hex", fixed = TRUE)
  # The old swatch palette must be gone.
  expect_false(grepl("blockr-stack-menu-palette", html, fixed = TRUE))
})

test_that("cards drop description + package badge and show id subtitle", {
  panel <- stack_menu_ui("mod", two_block_board())
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_false(
    grepl("blockr-block-browser-card-description", html, fixed = TRUE)
  )
  expect_false(
    grepl("blockr-block-browser-card-package", html, fixed = TRUE)
  )
  expect_match(html, "blockr-stack-menu-card-id", fixed = TRUE)
})

test_that("unknown target id raises a classed error", {
  expect_error(
    stack_menu_ui("mod", board_with_stack("a"), target = "no_such"),
    class = "blockr_ui_stack_menu_unknown_target"
  )
})

test_that("malformed inputs are rejected", {
  expect_error(stack_menu_ui(NULL, NULL))
  expect_error(stack_menu_ui("", NULL))
  expect_error(stack_menu_ui(c("a", "b"), NULL))
  expect_error(stack_menu_ui("mod", NULL, target = ""))
  expect_error(stack_menu_ui("mod", NULL, target = character()))
})

test_that("stack_menu_dep references the bundled CSS / JS", {
  d <- stack_menu_dep()
  expect_s3_class(d, "html_dependency")
  expect_identical(d$name, "blockr-stack-menu")
  expect_identical(d$stylesheet, "css/blockr-stack-menu.css")
  expect_identical(d$script, "js/blockr-stack-menu.js")
})

test_that("rendered panel resolves both block-browser and stack-menu deps", {
  panel <- stack_menu_ui("mod", NULL)
  deps <- htmltools::findDependencies(panel)
  names <- vapply(deps, function(d) d$name, character(1L))
  expect_true("blockr-block-browser" %in% names)
  expect_true("blockr-stack-menu" %in% names)
})

test_that("stack_menu_server returns an id-keyed dock_stacks object", {
  skip_if_not_installed("blockr.dock")
  shiny::testServer(stack_menu_server, args = list(id = "m"), {
    session$setInputs(
      "stack_name" = "Imports",
      "stack_color" = "#a8dcef",
      "stack_id" = "stk_x",
      "commit" = list(blocks = c("a", "b"), nonce = 1L)
    )
    v <- session$returned()
    expect_true(blockr.core::is_stacks(v))
    expect_named(v, "stk_x")
    stk <- v[["stk_x"]]
    expect_identical(blockr.core::stack_blocks(stk), c("a", "b"))
    expect_identical(blockr.core::stack_name(stk), "Imports")
    expect_identical(attr(stk, "color"), "#a8dcef")
  })
})

test_that("stack_menu_server re-fires on repeat commits (nonce advances)", {
  skip_if_not_installed("blockr.dock")
  shiny::testServer(stack_menu_server, args = list(id = "m"), {
    fired <- 0L
    session$setInputs(
      "stack_name" = "S",
      "stack_color" = "#aabbcc",
      "stack_id" = "s1"
    )

    observeEvent(session$returned(), {
      fired <<- fired + 1L
    })

    session$setInputs("commit" = list(blocks = c("a"), nonce = 1L))
    session$flushReact()
    session$setInputs("commit" = list(blocks = c("a"), nonce = 2L))
    session$flushReact()

    expect_gte(fired, 2L)
  })
})

test_that("stack_menu_server malformed id rejected", {
  expect_error(stack_menu_server(NULL))
  expect_error(stack_menu_server(""))
  expect_error(stack_menu_server(c("a", "b")))
})

test_that("stack_menu_server validates the commit when a board is supplied", {
  board <- blockr.core::new_board(
    blockr.core::as_blocks(list(a = blockr.core::new_dataset_block("iris"))),
    stacks = blockr.core::stacks(s1 = "a")
  )
  shiny::testServer(
    stack_menu_server,
    args = list(id = "m", board = shiny::reactive(board)),
    {
      fired <- 0L
      observeEvent(session$returned(), fired <<- fired + 1L)

      # Duplicate id -> rejected (committed never fires).
      session$setInputs(
        "stack_name" = "S", "stack_color" = "#aabbcc", "stack_id" = "s1",
        "commit" = list(blocks = c("a"), nonce = 1L)
      )
      session$flushReact()
      expect_identical(fired, 0L)

      # Empty name -> rejected.
      session$setInputs(
        "stack_name" = "", "stack_id" = "s2",
        "commit" = list(blocks = c("a"), nonce = 2L)
      )
      session$flushReact()
      expect_identical(fired, 0L)

      # Bad colour -> rejected.
      session$setInputs(
        "stack_name" = "S", "stack_color" = "nope", "stack_id" = "s2",
        "commit" = list(blocks = c("a"), nonce = 3L)
      )
      session$flushReact()
      expect_identical(fired, 0L)

      # All valid -> fires once with the spec.
      session$setInputs(
        "stack_name" = "S", "stack_color" = "#aabbcc", "stack_id" = "s2",
        "commit" = list(blocks = c("a"), nonce = 4L)
      )
      session$flushReact()
      expect_identical(fired, 1L)
      v <- session$returned()
      expect_true(blockr.core::is_stacks(v))
      expect_named(v, "s2")
    }
  )
})

test_that("edit-mode board sync survives the edited stack being removed", {
  rv <- shiny::reactiveValues(
    board = blockr.core::new_board(
      blockr.core::as_blocks(list(a = blockr.core::new_dataset_block("iris"))),
      stacks = blockr.core::stacks(s1 = "a")
    )
  )
  shiny::testServer(
    stack_menu_server,
    args = list(id = "m", board = shiny::reactive(rv$board), target = "s1"),
    {
      session$flushReact()
      # Remove the edited stack: the board observer must not abort in
      # `lookup_stack()` - it skips the sync instead.
      rv$board <- blockr.core::new_board(
        blockr.core::as_blocks(list(a = blockr.core::new_dataset_block("iris")))
      )
      expect_no_error(session$flushReact())
    }
  )
})

test_that("stack_sync_payload tracks the board's eligible blocks", {
  board <- blockr.core::new_board(
    blockr.core::as_blocks(list(
      a = blockr.core::new_dataset_block("iris"),
      b = blockr.core::new_head_block()
    ))
  )
  payload <- stack_sync_payload(board, NULL)
  expect_identical(payload$type, "menu:sync")
  ids <- vapply(payload$cards, `[[`, character(1L), "id")
  expect_setequal(ids, c("a", "b"))
  # Each card carries rendered markup so the client can insert it.
  expect_true(all(nzchar(vapply(payload$cards, `[[`, character(1L), "html"))))

  # Remove a block -> its card drops out of the next payload.
  board2 <- blockr.core::new_board(
    blockr.core::as_blocks(list(a = blockr.core::new_dataset_block("iris")))
  )
  ids2 <- vapply(stack_sync_payload(board2, NULL)$cards, `[[`,
                 character(1L), "id")
  expect_identical(ids2, "a")
})
