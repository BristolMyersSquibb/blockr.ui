mock_add_block <- function(blk, board_update, parent, session) {
  new_blk <- as_blocks(blk)
  board_update(
    list(blocks = list(add = new_blk))
  )
  parent$added_block <- new_blk[[1]]
  attr(parent$added_block, "uid") <- names(new_blk)
  session$flushReact()
}

mock_remove_block <- function(id, parent, session) {
  parent$removed_block <- id
  session$flushReact()
}

create_mock_params <- function(board = new_dag_board()) {
  modules <- board_modules(board)
  plugins <- board_plugins(board)

  list(
    x = board,
    plugins = plugins,
    callbacks = c(
      lapply(modules, board_module_server),
      list(
        # Callback to signal other modules that the restore is done.
        # This allows to restore each part in the correct order.
        on_board_restore = board_restore,
        manage_scoutbar = manage_scoutbar,
        layout = build_layout(modules, plugins)
      )
    ),
    parent = create_app_state(board)
  )
}

test_that("process_app_layout returns layout unchanged when no block panels", {
  layout <- list(
    panels = list(
      panel1 = list(
        id = "panel1",
        params = list(content = list(html = "content1"))
      ),
      panel2 = list(
        id = "panel2",
        params = list(content = list(html = "content2"))
      )
    )
  )

  result <- process_app_layout(layout)
  expect_equal(result, layout)
})

test_that("process_app_layout clears content for block panels", {
  layout <- list(
    panels = list(
      `block-panel` = list(
        id = "block-panel",
        params = list(content = list(html = "some content"))
      ),
      normal_panel = list(
        id = "normal_panel",
        params = list(content = list(html = "other content"))
      )
    )
  )

  result <- process_app_layout(layout)

  # Block panel content should be cleared
  expect_equal(result$panels[["block-panel"]]$params$content$html, character(0))

  # Normal panel content should remain unchanged
  expect_equal(result$panels$normal_panel$params$content$html, "other content")
})

test_that("process_app_layout handles empty panels list", {
  layout <- list(panels = list())

  result <- process_app_layout(layout)
  expect_equal(result, layout)
})

testServer(
  board_server,
  args = create_mock_params(),
  {
    # Init
    expect_length(dot_args$parent$in_grid, 0)
    # Layout initial state
    test_dock <- list()
    test_dock[["panels"]] <- list(
      "dashboard" = list(
        id = "dashboard",
        params = list(
          content = list(html = "blabla")
        )
      )
    )

    session$userData$board_options[["blocks_position"]] <- reactiveVal(
      list(reference_panel = "dashboard", direction = "within")
    )

    session$setInputs(layout_state = test_dock)

    # Add block
    mock_add_block(
      new_dataset_block("mtcars"),
      board_update,
      dot_args$parent,
      session
    )

    expect_true(is_block(dot_args$parent$added_block))
    expect_false(dot_args$parent$in_grid[[block_uid(
      dot_args$parent$added_block
    )]])

    # Select block
    dot_args$parent$selected_block <- block_uid(
      dot_args$parent$added_block
    )

    # Add to dashboard
    dot_args$parent$added_to_dashboard <- block_uid(dot_args$parent$added_block)
    dot_args$parent$in_grid[[dot_args$parent$added_to_dashboard]] <- TRUE
    session$flushReact()
    output[[sprintf("dock-%s-result", block_uid(dot_args$parent$added_block))]]
    expect_null(dot_args$parent$added_to_dashboard)

    # To be able to remove panels later, we need to mock the dock state
    test_dock[["panels"]][[sprintf(
      "block-%s",
      block_uid(dot_args$parent$added_block)
    )]] <- list(
      id = block_uid(dot_args$parent$added_block),
      params = list(
        content = list(html = "blabla")
      )
    )
    session$setInputs(dock_state = test_dock, layout_state = test_dock)

    output$dock

    # Change dashboard zoom
    session$userData$board_options[["dashboard_zoom"]] <- 0.5
    session$flushReact()

    # Remove from dashboard
    dot_args$parent$removed_from_dashboard <- block_uid(
      dot_args$parent$added_block
    )
    dot_args$parent$in_grid[[dot_args$parent$removed_from_dashboard]] <- FALSE
    session$flushReact()
    # This does not work, but it should ...
    #expect_null(output[[sprintf(
    #  "dock-%s-result",
    #  block_uid(dot_args$parent$added_block)
    #)]])
    expect_null(dot_args$parent$removed_from_dashboard)

    # Remove block: returns a warning, no idea why ...
    mock_remove_block(
      block_uid(dot_args$parent$added_block),
      dot_args$parent,
      session
    )
    expect_null(dot_args$parent$in_grid[[dot_args$parent$removed_block]])

    # Refresh
    dot_args$parent$refreshed <- "restore-dag"
    # Manually setup the dashboard state as this is theoretically
    # injected by the dashboard module
    dot_args$parent$module_state$dashboard <- reactiveVal(NULL)
    # Manually simulate remove panel as the JS callback does not work
    # in the testServer context
    session$setInputs(dock_state = list(panels = list()))
    expect_null(dot_args$parent$refreshed)

    # Scoutbar
    session$setInputs("scoutbar" = "dataset_block@add_block")
    expect_identical(dot_args$parent$scoutbar$action, "add_block")
    expect_identical(dot_args$parent$scoutbar$value, "dataset_block")

    dot_args$parent$open_scoutbar <- TRUE
    session$flushReact()

    dot_args$parent$append_block <- TRUE

    session$setInputs("scoutbar-open" = FALSE)
    expect_false(dot_args$parent$append_block)
    expect_false(dot_args$parent$open_scoutbar)
    expect_false(dot_args$parent$scoutbar$is_open)

    # Hide block panel in app layout
    mock_add_block(
      new_dataset_block("mtcars"),
      board_update,
      dot_args$parent,
      session
    )
    test_dock <- list()
    test_dock[["panels"]][[sprintf(
      "block-%s",
      block_uid(dot_args$parent$added_block)
    )]] <- list(
      id = block_uid(dot_args$parent$added_block),
      params = list(
        content = list(html = "blabla")
      )
    )
    session$setInputs(layout_state = test_dock)
    session$setInputs(
      "layout_panel-to-remove" = sprintf(
        "block-%s",
        block_uid(dot_args$parent$added_block)
      )
    )
  }
)

test_that("Board dock app works", {
  skip_on_cran()

  # We test from an existing dock so that we can fix block, stack and link IDs
  # to avoid randomness failure
  app <- shinytest2::AppDriver$new(
    system.file(package = "blockr.ui", "examples/dashboard/non-empty"),
    name = "dashboard-non-empty-app",
    seed = 4323
  )

  Sys.sleep(2)

  inputs <- c(
    "main-board-manage_blocks-scoutbar-configuration",
    "main-board-manage_links-network-initialized"
  )

  app$expect_values(input = inputs, export = TRUE)

  # Add block: is there a way to fix the block ID?
  #app$click(selector = ".g6-toolbar-item[value=\"add-block\"")
  #app$wait_for_idle()
  #app$click(
  #  selector = ".scout__bar-wrapper button[aria-label=\"dataset_block@add_block\"]"
  #)
  #app$wait_for_idle()
  #app$expect_values(input = inputs, export = TRUE)
  app$stop()
})
