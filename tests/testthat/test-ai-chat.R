test_that("setup_chat_provider", {
  # Test basic functionality with chat_openai
  result <- setup_chat_provider(
    provider = "openai",
    prompt = "You are a helpful assistant"
  )
  expect_s3_class(result, "Chat")
  expect_identical(result$get_system_prompt(), "You are a helpful assistant")
})

test_that("setup_chat_task creates ExtendedTask", {
  result <- setup_chat_task()
  expect_s3_class(result, "ExtendedTask")
})

test_that("init_chat_message works", {
  res <- init_chat_message(get_ellmer_chat_providers()[[1]])
  expect_type(res, "list")
  expect_named(res, c("role", "content"))
})

test_that("get_ellmer_chat_providers works", {
  res <- get_ellmer_chat_providers()
  expect_true(is.character(res))
})

test_that("Chat app works", {
  skip_on_cran()

  # We test from an existing dock so that we can fix block, stack and link IDs
  # to avoid randomness failure
  app <- shinytest2::AppDriver$new(
    system.file(package = "blockr.ui", "examples/ai-chat/simple"),
    name = "simple-ai-chat",
    seed = 4323
  )

  app$wait_for_idle()

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
