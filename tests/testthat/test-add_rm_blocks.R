testServer(
  add_rm_block_server,
  args = c(
    generate_plugin_args(new_dag_board()),
    list(
      parent = reactiveValues(
        scoutbar = NULL,
        added_block = NULL,
        removed_block = NULL,
        cancelled_edge = NULL
      )
    )
  ),
  {
    # Test adding new block
    parent$scoutbar <- list(
      action = "add_block",
      value = "dataset_block"
    )
    session$flushReact()
    expect_identical(parent$scoutbar$value, "dataset_block")
    expect_s3_class(parent$added_block, "dataset_block")
    expect_type(attr(parent$added_block, "uid"), "character")
    expect_s3_class(update()$blocks$add, "blocks")

    # Test removing block
    parent$removed_block <- attr(parent$added_block, "uid")
    session$flushReact()
    expect_identical(update()$blocks$rm, parent$removed_block)

    # Test edge cancellation
    parent$cancelled_edge <- "test2"
    session$flushReact()
    expect_identical(update()$blocks$rm, "test2")

    # Verify added_block is reset after removal
    expect_null(parent$added_block)
  }
)

test_that("add_rm_bloc_ui works", {
  ui <- add_rm_block_ui("mod")
  expect_null(ui)
})
