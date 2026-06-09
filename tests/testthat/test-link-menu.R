small_board <- function() {
  blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        b = blockr.core::new_head_block(),
        m = blockr.core::new_merge_block()
      )
    )
  )
}

board_with_link <- function(...) {
  blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        b = blockr.core::new_head_block(),
        m = blockr.core::new_merge_block()
      )
    ),
    links = list(
      l1 = blockr.core::new_link(from = "a", to = "b", input = "data")
    )
  )
}

variadic_board <- function() {
  blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        r = blockr.core::new_rbind_block()
      )
    )
  )
}

card_ids <- function(panel) {
  html <- as.character(htmltools::renderTags(panel)$html)
  m <- regmatches(html, gregexpr("data-block-type=\"[^\"]+\"", html))[[1]]
  sub("data-block-type=\"([^\"]+)\"", "\\1", m)
}

card_directions <- function(panel) {
  html <- as.character(htmltools::renderTags(panel)$html)
  m <- regmatches(html, gregexpr("data-direction=\"[^\"]+\"", html))[[1]]
  sub("data-direction=\"([^\"]+)\"", "\\1", m)
}

# ---- link_eligible_pools() --------------------------------------------

test_that("dataset anchor: outgoing populated, incoming empty", {
  pools <- link_eligible_pools(small_board(), "a")
  expect_setequal(pools$outgoing, c("b", "m"))
  expect_identical(pools$incoming, character())
})

test_that("merge anchor: both directions populated", {
  pools <- link_eligible_pools(small_board(), "m")
  expect_setequal(pools$outgoing, "b")
  expect_setequal(pools$incoming, c("a", "b"))
})

test_that("variadic anchor: both directions populated", {
  pools <- link_eligible_pools(variadic_board(), "r")
  expect_setequal(pools$outgoing, character())
  expect_setequal(pools$incoming, "a")
})

test_that("existing link fills target: it drops from OUTGOING pool", {
  pools <- link_eligible_pools(board_with_link(), "a")
  # b is now full (its single port is wired). m still has 2 free ports.
  expect_setequal(pools$outgoing, "m")
})

test_that("free_inputs map keys by target id and shrinks after wiring", {
  pools <- link_eligible_pools(small_board(), "a")
  # OUTGOING targets: b (arity 1, free) -> "data"; m (arity 2, both
  # free) -> c("x", "y"). a itself isn't a target (it's the source).
  expect_setequal(names(pools$free_inputs), c("b", "m"))
  expect_setequal(pools$free_inputs[["m"]], c("x", "y"))

  brd <- blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        b = blockr.core::new_head_block(),
        m = blockr.core::new_merge_block()
      )
    ),
    links = list(
      l1 = blockr.core::new_link(from = "a", to = "m", input = "x")
    )
  )
  pools2 <- link_eligible_pools(brd, "a")
  # m's x slot got wired; the free_inputs for m drops to just "y".
  expect_setequal(pools2$free_inputs[["m"]], "y")
})

test_that("free_inputs for INCOMING is keyed by the anchor", {
  pools <- link_eligible_pools(small_board(), "m")
  # m is the anchor; INCOMING cards (a, b) all wire into m, so the
  # only target is m -> its free ports.
  expect_true("m" %in% names(pools$free_inputs))
  expect_setequal(pools$free_inputs[["m"]], c("x", "y"))
})

test_that("eligibility filters out cycle-creating candidates (direct)", {
  # h -> r exists. With anchor=r, OUTGOING must not include h (adding
  # r -> h would close the cycle h -> r -> h).
  brd <- blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        h = blockr.core::new_head_block(),
        r = blockr.core::new_rbind_block()
      )
    ),
    links = list(
      l1 = blockr.core::new_link(from = "h", to = "r", input = "1")
    )
  )
  pools <- link_eligible_pools(brd, "r")
  expect_false("h" %in% pools$outgoing)

  # INCOMING is fine: variadic r still accepts another h -> r on a
  # fresh slot (no new cycle).
  expect_true("h" %in% pools$incoming)
})

test_that("eligibility filters out cycle-creating candidates (transitive)", {
  # a -> h -> r. With anchor=r, OUTGOING must drop both h (direct
  # ancestor) and a (transitive ancestor).
  brd <- blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        a = blockr.core::new_dataset_block(),
        h = blockr.core::new_head_block(),
        r = blockr.core::new_rbind_block()
      )
    ),
    links = list(
      l1 = blockr.core::new_link(from = "a", to = "h", input = "data"),
      l2 = blockr.core::new_link(from = "h", to = "r", input = "1")
    )
  )
  pools <- link_eligible_pools(brd, "r")
  expect_false("h" %in% pools$outgoing)
  expect_false("a" %in% pools$outgoing)
})

test_that("INCOMING filter excludes blocks the anchor already reaches", {
  # h -> r exists. With anchor=h, INCOMING must not include r (adding
  # r -> h would close the cycle).
  brd <- blockr.core::new_board(
    blocks = blockr.core::as_blocks(
      list(
        h = blockr.core::new_head_block(),
        r = blockr.core::new_rbind_block()
      )
    ),
    links = list(
      l1 = blockr.core::new_link(from = "h", to = "r", input = "1")
    )
  )
  pools <- link_eligible_pools(brd, "h")
  expect_false("r" %in% pools$incoming)
})

test_that("free_inputs for variadic target returns character()", {
  pools <- link_eligible_pools(variadic_board(), "a")
  # r is variadic; no named ports, the consumer generates a fresh
  # slot at link-build time.
  expect_identical(pools$free_inputs[["r"]], character())
})

test_that("link_eligible_pools rejects an unknown anchor", {
  expect_error(
    link_eligible_pools(small_board(), "no_such"),
    class = "blockr_ui_link_menu_unknown_anchor"
  )
})

# ---- link_menu_ui() ----------------------------------------------------

test_that("anchor with free outputs only renders OUTGOING only", {
  panel <- link_menu_ui("mod", small_board(), anchor = "a")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "data-mode|data-anchor=\"a\"", perl = TRUE)
  expect_match(html, "id=\"mod-commit\"", fixed = TRUE)
  expect_setequal(unique(card_directions(panel)), "outgoing")
  expect_setequal(card_ids(panel), c("b", "m"))
  expect_match(html, ">Output to<", fixed = TRUE)
  expect_false(grepl(">Input from<", html, fixed = TRUE))
})

test_that("anchor with free inputs renders BOTH directions", {
  panel <- link_menu_ui("mod", small_board(), anchor = "m")
  html <- as.character(htmltools::renderTags(panel)$html)

  dirs <- unique(card_directions(panel))
  expect_setequal(dirs, c("outgoing", "incoming"))
  expect_match(html, ">Input from<", fixed = TRUE)
  expect_match(html, ">Output to<", fixed = TRUE)
  # INPUT FROM precedes OUTPUT TO (left-to-right data-flow reading).
  expect_lt(
    regexpr(">Input from<", html, fixed = TRUE),
    regexpr(">Output to<", html, fixed = TRUE)
  )
})

test_that("variadic anchor renders both sections", {
  panel <- link_menu_ui("mod", variadic_board(), anchor = "r")
  dirs <- unique(card_directions(panel))
  # outgoing is empty here (only `a` is left, dataset has no inputs);
  # incoming surfaces `a`.
  expect_setequal(dirs, "incoming")
})

test_that("isolated anchor renders empty-state, no direction sections", {
  brd <- blockr.core::new_board(
    blocks = blockr.core::as_blocks(list(a = blockr.core::new_dataset_block()))
  )
  panel <- link_menu_ui("mod", brd, anchor = "a")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "is-empty", fixed = TRUE)
  expect_length(card_ids(panel), 0L)
  expect_false(grepl("data-direction=", html, fixed = TRUE))
})

test_that("unknown anchor id raises a classed error", {
  expect_error(
    link_menu_ui("mod", small_board(), anchor = "no_such"),
    class = "blockr_ui_link_menu_unknown_anchor"
  )
})

test_that("cards carry id: <block_id> subtitle (no description / badge)", {
  panel <- link_menu_ui("mod", small_board(), anchor = "a")
  html <- as.character(htmltools::renderTags(panel)$html)

  expect_match(html, "id: b", fixed = TRUE)
  expect_match(html, "id: m", fixed = TRUE)
  expect_false(
    grepl("blockr-block-browser-card-description", html, fixed = TRUE)
  )
  expect_false(
    grepl("blockr-block-browser-card-package", html, fixed = TRUE)
  )
})

test_that("arity-1 target card hides the Block input picker", {
  panel <- link_menu_ui("mod", small_board(), anchor = "a")
  html <- as.character(htmltools::renderTags(panel)$html)

  # b is arity-1: no block_input field for the b card.
  expect_false(
    grepl("id=\"mod-card_b_block_input\"", html, fixed = TRUE)
  )
  # b still has a link_id field.
  expect_match(html, "id=\"mod-card_b_link_id\"", fixed = TRUE)
})

test_that("arity > 1 target card renders the Block input picker", {
  panel <- link_menu_ui("mod", small_board(), anchor = "a")
  html <- as.character(htmltools::renderTags(panel)$html)

  # m is arity-2 with both ports free.
  expect_match(html, "id=\"mod-card_m_block_input\"", fixed = TRUE)
})

test_that("variadic target hides the Block input picker", {
  panel <- link_menu_ui("mod", variadic_board(), anchor = "a")
  html <- as.character(htmltools::renderTags(panel)$html)

  # r is variadic - no block_input picker.
  expect_false(
    grepl("id=\"mod-card_r_block_input\"", html, fixed = TRUE)
  )
})

test_that("INCOMING picker reflects the anchor's free slots", {
  panel <- link_menu_ui("mod", small_board(), anchor = "m")
  html <- as.character(htmltools::renderTags(panel)$html)

  # In INCOMING cards (target = anchor = m, arity 2), the per-card
  # block_input select should be present on each incoming card,
  # populated with m's port names ("x", "y").
  expect_match(html, "id=\"mod-card_a_block_input\"", fixed = TRUE)
  expect_match(html, "value=\"x\"", fixed = TRUE)
  expect_match(html, "value=\"y\"", fixed = TRUE)
})

test_that("malformed inputs rejected", {
  brd <- small_board()
  expect_error(link_menu_ui(NULL, brd, "a"))
  expect_error(link_menu_ui("", brd, "a"))
  expect_error(link_menu_ui(c("a", "b"), brd, "a"))
  expect_error(link_menu_ui("mod", brd, ""))
  expect_error(link_menu_ui("mod", brd, character()))
})

test_that("link_menu_dep references the bundled CSS / JS", {
  d <- link_menu_dep()
  expect_s3_class(d, "html_dependency")
  expect_identical(d$name, "blockr-link-menu")
  expect_identical(d$stylesheet, "css/blockr-link-menu.css")
  expect_identical(d$script, "js/blockr-link-menu.js")
})

test_that("rendered panel resolves both block-browser and link-menu deps", {
  panel <- link_menu_ui("mod", small_board(), anchor = "a")
  deps <- htmltools::findDependencies(panel)
  names <- vapply(deps, function(d) d$name, character(1L))
  expect_true("blockr-block-browser" %in% names)
  expect_true("blockr-link-menu" %in% names)
})

# ---- link_menu_server() -----------------------------------------------

test_that("server returns a ready-to-apply links object", {
  shiny::testServer(link_menu_server, args = list(id = "mod"), {
    session$setInputs(
      commit = list(
        source = "a", target = "b",
        link_id = "lk1", block_input = "data",
        nonce = 1L
      )
    )
    v <- session$returned()
    expect_true(blockr.core::is_links(v))
    expect_named(v, "lk1")
    expect_identical(v$from, "a")
    expect_identical(v$to, "b")
    expect_identical(v$input, "data")
  })
})

test_that("server re-fires on repeat commits (nonce advances)", {
  shiny::testServer(link_menu_server, args = list(id = "mod"), {
    fired <- 0L
    observeEvent(session$returned(), {
      fired <<- fired + 1L
    })
    session$setInputs(
      commit = list(
        source = "a", target = "b", link_id = "lk",
        block_input = "data", nonce = 1L
      )
    )
    session$flushReact()
    session$setInputs(
      commit = list(
        source = "a", target = "b", link_id = "lk",
        block_input = "data", nonce = 2L
      )
    )
    session$flushReact()
    expect_gte(fired, 2L)
  })
})

test_that("link_menu_server malformed id rejected", {
  expect_error(link_menu_server(NULL))
  expect_error(link_menu_server(""))
  expect_error(link_menu_server(c("a", "b")))
})

test_that("server validates the link id when a board is supplied", {
  board <- blockr.core::new_board(
    blockr.core::as_blocks(list(
      a = blockr.core::new_dataset_block("iris"),
      b = blockr.core::new_head_block()
    )),
    links = blockr.core::links(id = "ab", from = "a", to = "b")
  )
  shiny::testServer(
    link_menu_server,
    args = list(id = "mod", board = shiny::reactive(board), anchor = "a"),
    {
      fired <- 0L
      observeEvent(session$returned(), fired <<- fired + 1L)

      # Duplicate link id -> rejected (committed never fires).
      session$setInputs(commit = list(
        source = "a", target = "b", link_id = "ab", nonce = 1L
      ))
      session$flushReact()
      expect_identical(fired, 0L)

      # Fresh link id -> fires once with a ready links object.
      session$setInputs(commit = list(
        source = "a", target = "b", link_id = "fresh",
        block_input = "data", nonce = 2L
      ))
      session$flushReact()
      expect_identical(fired, 1L)
      v <- session$returned()
      expect_true(blockr.core::is_links(v))
      expect_named(v, "fresh")
    }
  )
})

test_that("server resolves the first free named input when none is picked", {
  board <- small_board()
  shiny::testServer(
    link_menu_server,
    args = list(id = "mod", board = shiny::reactive(board), anchor = "a"),
    {
      # m is arity 2 (inputs x, y), nothing wired yet -> first free is x.
      session$setInputs(commit = list(
        source = "a", target = "m", link_id = "lk", nonce = 1L
      ))
      session$flushReact()
      v <- session$returned()
      expect_true(blockr.core::is_links(v))
      expect_identical(v$to, "m")
      expect_identical(v$input, "x")
    }
  )
})

test_that("server generates a numeric slot for a variadic target", {
  board <- variadic_board()
  shiny::testServer(
    link_menu_server,
    args = list(id = "mod", board = shiny::reactive(board), anchor = "a"),
    {
      session$setInputs(commit = list(
        source = "a", target = "r", link_id = "lk", nonce = 1L
      ))
      session$flushReact()
      expect_identical(session$returned()$input, "1")
    }
  )
})

test_that("link_sync_payload tracks eligibility across a board change", {
  ns <- shiny::NS("mod")
  blocks <- blockr.core::as_blocks(list(
    a = blockr.core::new_dataset_block("iris"),
    b = blockr.core::new_head_block()
  ))

  # a -> b wired: b's only input is taken, so anchor `a` has no OUTGOING
  # target (and a dataset has no input, so no INCOMING) -> no cards.
  wired <- blockr.core::new_board(
    blocks, links = blockr.core::links(id = "ab", from = "a", to = "b")
  )
  p1 <- link_sync_payload(wired, "a", ns)
  expect_identical(p1$type, "menu:sync")
  expect_false("b" %in% vapply(p1$cards, `[[`, character(1L), "id"))

  # Remove the link: `b` frees up and reappears as an OUTGOING card,
  # carrying its direction + rendered markup for client-side insertion.
  free <- blockr.core::new_board(blocks)
  p2 <- link_sync_payload(free, "a", ns)
  card_b <- Filter(function(c) identical(c$id, "b"), p2$cards)
  expect_length(card_b, 1L)
  expect_identical(card_b[[1L]]$direction, "outgoing")
  expect_true(nzchar(card_b[[1L]]$html))
})
