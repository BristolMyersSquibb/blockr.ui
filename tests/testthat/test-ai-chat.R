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

    llm_opt <- withr::with_options(
      list(
        blockr.chat_function = function(system_prompt = NULL, params = NULL) {
          ellmer::chat_openai(system_prompt = system_prompt, params = params)
        }
      ),
      new_llm_model_option()
    )

    board_option_to_userdata <- get(
      "board_option_to_userdata",
      envir = asNamespace("blockr.core"),
      inherits = FALSE,
      mode = "function"
    )

    board_option_to_userdata(llm_opt, board$board, session)

    session$flushReact()

    output$chat_ui
    expect_s3_class(provider(), "Chat")
    expect_true(length(provider()$get_tools()) > 0)
    # Simulate a chat input
    session$setInputs(
      `prompt_user_input` = "Import iris data"
    )
    expect_true(parent$ai_chat)
  }
)
