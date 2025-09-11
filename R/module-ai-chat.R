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
    uiOutput(ns("chat_ui"))
  )
}

#' Chat module server
#'
#' @inheritParams dashboard_server
#' @rdname ai-chat
chat_mod_srv <- function(id = "chat", board, update, session, parent, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # Dynamic provider support through board options
      provider <- reactiveVal(NULL)
      # Request to store tool results
      app_request <- reactiveVal(NULL)

      # Dynamic UI: this also somehow fixes an issue with shinychat
      # if the provider choosen does not have enough credits
      # then the chat isn't stuck and can be reset by changing the provider
      output$chat_ui <- renderUI({
        req(provider())
        provider <- provider()$get_provider()
        provider <- sprintf(
          "%s/%s",
          provider@name,
          provider@model
        )
        chat_ui(
          id = ns("prompt"),
          messages = list(init_chat_message(provider))
        )
      })

      # Update provider, tools
      observeEvent(
        TRUE,
        {
          # Note: there is no-need to reset tools
          provider(setup_chat_provider())

          create_block_names_tool(provider)
          create_block_tool_factory(
            provider,
            app_request,
            board,
            parent,
            session
          )
          create_remove_block_tool(
            provider,
            app_request,
            board,
            parent,
            session
          )
          create_add_stack_tool(
            provider,
            stackable_blocks,
            app_request,
            board,
            parent,
            session
          )
          create_stackable_blocks_tool(provider, stackable_blocks)
          create_get_stack_ids_tool(provider, board)
          create_add_block_to_stack_tool(
            provider,
            stackable_blocks,
            app_request,
            board,
            parent,
            session
          )
          create_add_to_dash_tool(
            provider,
            app_request,
            board,
            parent,
            session
          )
          create_remove_from_dash_tool(
            provider,
            app_request,
            board,
            parent,
            session
          )
        },
        once = TRUE
      )

      res <- manage_chat(provider, parent, session)
      # We probably can comment this below
      #chat_restore("prompt", provider())

      # Useful info to create stacks
      stackable_blocks <- reactive(get_stackable_blocks(board))

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
