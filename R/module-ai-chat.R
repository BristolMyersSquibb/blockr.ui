#' Chat module UI
#' @rdname ai-chat
chat_mod_ui <- function(id, board, ...) {
  ns <- NS(NS(id, "chat"))
  card(
    card_header(
      class = "d-flex justify-content-between",
      "blockr.ui assistant",
      actionLink(
        ns("prompt_clean"),
        icon("trash")
      )
    ),
    chat_ui(
      id = ns("prompt"),
      messages = list(
        list(
          role = "assistant",
          content = "Hi! I'll help you to build blockr.ui pipeline with OpenAI's `gpt-4o`. 
          You can start with: Import iris data, select the Species 
          column and stack the 2 blocks. 
          Then filter Species to only account for 'virginica'. 
          Add the new block to the previous stack."
        )
      )
    )
  )
}

#' Chat module server
#'
#' @inheritParams dashboard_server
#' @rdname ai-chat
chat_mod_srv <- function(board, update, session, parent, ...) {
  moduleServer(
    "chat",
    function(input, output, session) {
      # Maybe we want to replace this by ellmer::chat to allow for any provider. The provider name
      # could be passed as a board option or so ...
      openai <- setup_chat_provider()
      res <- manage_chat(openai, parent, session)
      chat_restore("prompt", openai)

      create_block_names_tool(openai)

      # Request to store tool results
      app_request <- reactiveVal(NULL)
      create_block_tool_factory(openai, app_request, board, parent, session)
      create_remove_block_tool(openai, app_request, board, parent, session)

      stackable_blocks <- reactive(get_stackable_blocks(board))

      create_add_stack_tool(
        openai,
        stackable_blocks,
        app_request,
        board,
        parent,
        session
      )
      create_get_stackable_blocks_tool(openai, stackable_blocks)
      create_get_stack_ids_tool(openai, board)
      create_add_block_to_stack_tool(
        openai,
        stackable_blocks,
        app_request,
        board,
        parent,
        session
      )

      res
    }
  )
}

#' @export
#' @rdname board-module
new_chat_module <- function(id = "blockr_assistant", title = "AI chat") {
  new_board_module(
    chat_mod_ui,
    chat_mod_srv,
    id = id,
    title = title,
    context_menu = list(
      # TBD: do something better here ...
      new_context_menu_entry(
        name = "Open AI chat",
        js = "() => {
          console.log('Hello world')
        }",
        action = function(input, output, session, board, update, parent) {
          # TBD
        }
      )
    ),
    class = "chat_module"
  )
}
