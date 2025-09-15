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
        blockr.chat_function = mock_chat(
          stream_async = function(msg, ...) {
            if (identical(msg, "Import iris data")) {
              call_tool(self, "create_block_tool_factory",
                ctor = "new_dataset_block"
              )
              call_tool(self, "add_new_dataset_block",
                name = "dataset_block",
                append = FALSE,
                parms = list(dataset = "iris", package = "datasets")
              )
              seq_gen("Successfully", "added a block", "that loads \"iris\".")
            } else if (grepl("^Remove block", msg)) {
              blk <- sub("^Remove block ", "", msg)
              res <- call_tool(self, "remove_block", id = blk)
              if ("error" %in% names(res)) {
                seq_gen("Could not", "remove block", blk)
              } else {
                seq_gen("Successfully", "removed block", blk)
              }
            } else {
              stop("Unknown request: ", msg)
            }
          }
        )
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
      prompt_user_input = "Import iris data"
    )

    session$flushReact()

    while (append_stream_task$status() == "running") {
      session$elapse(250)
    }

    expect_identical(append_stream_task$status(), "success")
    expect_snapshot(app_request())

    expect_identical(parent$scoutbar$action, "add_block")
    expect_s3_class(parent$scoutbar$value, "blocks")
    expect_length(parent$scoutbar$value, 1L)

    names(parent$scoutbar$value) <- "iris_data"

    board_blocks(board$board) <- c(
      board_blocks(board$board),
      parent$scoutbar$value
    )

    session$setInputs(
      prompt_user_input = "Remove block iris_data"
    )
    session$flushReact()

    while (append_stream_task$status() == "running") {
      session$elapse(250)
    }

    expect_snapshot(app_request())

    to_rm <- which(names(board_blocks(board$board)) == "iris_data")
    board_blocks(board$board) <- board_blocks(board$board)[-to_rm]

    session$setInputs(
      prompt_user_input = "Remove block iris_data"
    )
    session$flushReact()

    while (append_stream_task$status() == "running") {
      session$elapse(250)
    }

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
    seed = 4323,
    options = list(
      blockr.chat_function = mock_chat(
        stream_async = function(msg, ...) {
          if (identical(msg, "Import iris data")) {
            call_tool(self, "create_block_tool_factory",
              ctor = "new_dataset_block"
            )
            call_tool(self, "add_new_dataset_block",
              name = "dataset_block",
              append = FALSE,
              parms = list(dataset = "iris", package = "datasets")
            )
            seq_gen("Successfully", "added a block", "that loads \"iris\".")
          } else {
            stop("Unknown request: ", msg)
          }
        }
      )
    )
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
