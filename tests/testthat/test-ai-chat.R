test_that("setup_chat_provider", {
  # Test basic functionality with chat_openai
  result <- setup_chat_provider(
    prompt = "You are a helpful assistant"
  )
  expect_s3_class(result, "Chat")
  expect_identical(result$get_system_prompt(), "You are a helpful assistant")
})

test_that("setup_chat_task creates ExtendedTask", {
  result <- setup_chat_task()
  expect_s3_class(result, "ExtendedTask")
})

test_that("new_chat_module works", {
  mod <- new_chat_module()
  expect_s3_class(mod, "board_module")
  expect_identical(board_module_id(mod), "blockr_assistant")
  expect_identical(board_module_title(mod), "AI chat")
  expect_null(board_module_position(mod))
  expect_s3_class(board_module_options(mod), "board_options")
  res <- board_module_positions(list(mod))[[1]]
  expect_type(res, "list")
  expect_identical(res$referencePanel, "dag")
  expect_identical(res$direction, "right")
})

test_that("chat_mod_ui works", {
  ui <- chat_mod_ui("test")
  expect_s3_class(ui, "shiny.tag")
})

testServer(
  chat_mod_srv,
  args = list(
    board = reactiveValues(
      blocks = list(),
      board = new_dag_board(),
      board_id = "board",
      inputs = list(),
      links = list(),
      stacks = list()
    ),
    update = reactiveVal(),
    parent = reactiveValues(
      append_block = FALSE,
      scoutbar = list(action = NULL, value = NULL)
    )
  ),
  {
    expect_null(provider())
    expect_null(app_request())
    expect_null(res())
    expect_null(parent$ai_chat)
    expect_length(stackable_blocks(), 0)

    session$flushReact()

    output$chat_ui
    expect_s3_class(provider(), "Chat")
    expect_true(length(provider()$get_tools()) > 0)
    # Simulate a chat input
    session$setInputs(
      `prompt_user_input` = "Import iris data"
    )
    expect_true(parent$ai_chat)

    # Call provider to invoke tools
    provider()$chat("Import iris data")
    expect_true("add_new_dataset_block" %in% names(provider()$get_tools()))
    session$flushReact()
    expect_identical(parent$scoutbar$action, "add_block")
    expect_identical(block_name(parent$scoutbar$value[[1]]), "Dataset block")
    expect_snapshot(app_request())

    # TBD: add block to board manually
    board_blocks(board$board) <- c(
      board_blocks(board$board),
      parent$scoutbar$value
    )
    provider()$chat("Remove block aaaa.")
    expect_snapshot(app_request())

    session$setInputs(
      `prompt_clean` = 0
    )
  }
)

test_that("Chat app works", {
  skip_on_cran()

  # We test from an existing dock so that we can fix block, stack and link IDs
  # to avoid randomness failure
  app <- shinytest2::AppDriver$new(
    system.file(package = "blockr.ui", "examples/ai-chat/simple"),
    name = "simple-ai-chat",
    seed = 4323
  )

  Sys.sleep(4)

  inputs <- c("main-board-chat-prompt_user_input")
  app$expect_values(input = inputs, export = TRUE)
  app$set_inputs(
    `main-board-chat-prompt_user_input` = "Import iris data",
    # Don't remove: shinychat text area field has `data-shiny-no-bind-input` HTML attribute
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$stop()
})
