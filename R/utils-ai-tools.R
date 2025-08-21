#' Utility functions to create AI tools for blockr.ui
#'
#' @param provider AI provider function to use from ellmer.
#' @param prompt System prompt to use for the AI provider. Best
#' to create a markdown file with instructions.
#' @param ... Additional arguments to pass to the provider.
#' @keywords internal
setup_chat_provider <- function(
  provider = chat_openai,
  prompt = readLines(system.file(
    "examples/ai-chat/rules.md",
    package = "blockr.ui"
  )),
  ...
) {
  stopifnot(is.function(provider))
  provider(
    system_prompt = prompt,
    ...
  )
}

#' Create a task to append chat messages
#' @keywords internal
setup_chat_task <- function() {
  ExtendedTask$new(
    function(client, ui_id, user_input) {
      promises::then(
        promises::promise_resolve(client$stream_async(
          user_input,
          tool_mode = "sequential",
          stream = "content"
        )),
        function(stream) {
          chat_append(ui_id, stream)
        }
      )
    }
  )
}

#' Create a chat server callbacks
#' @param provider AI provider object. See \url{setup_chat_provider}.
#' @param parent Parent global reactive values object.
#' @param session Shiny session object.
manage_chat <- function(provider, parent, session) {
  input <- session$input

  # Necessary to handle AI mode
  isolate({
    parent$ai_chat <- FALSE
  })

  append_stream_task <- setup_chat_task()

  observeEvent(input$prompt_user_input, {
    # Switch to AI mode. This is used to avoid certain behavior
    # that normally happen in the app when it is manually driven
    # by a human. With AI this is slightly different and some of
    # these action should not happen.
    parent$ai_chat <- TRUE
    append_stream_task$invoke(provider, "prompt", input$prompt_user_input)
  })

  observeEvent(input$prompt_clean, {
    chat_clear("prompt")
    # This also erase the chat memory and not just the UI
    #openai$set_turns(list())
  })

  observeEvent(res(), {
    # Once the response is received, we signal the app
    # that the AI chat is done.
    parent$ai_chat <- FALSE
    # Need to reset scoutbar
    parent$scoutbar$action <- parent$scoutbar$value <- NULL
  })

  res <- reactive({
    if (append_stream_task$status() == "success") {
      provider$last_turn()
    }
  })

  return(res)
}

#' Create a block tool factory
#'
#' create a tool for a specific block type constructor.
#'
#' @param provider AI provider object.
#' @param app_request Reactive value containing the tool answer. Maybe useful
#' for post processing.
#' @param board Board object. Not used yet but may be useful.
#' @param parent Parent reactive values object.
#' @param session Shiny session object.
#' @export
#' @rdname ai-chat-tool
create_block_tool_factory <- function(
  provider,
  app_request,
  board,
  parent,
  session
) {
  block_tool_factory <- tool(
    function(ctor) {
      # Extract constructor parameters
      if (grepl("llm", ctor)) {
        # LLM only have question field
        parms <- pairlist(question = character())
      } else {
        parms <- get_block_parameters(ctor)
      }

      # Create tool arguments based on constructor parameters
      tool_args <- lapply(names(parms), function(name) {
        if (is.language(parms[[name]])) {
          parms[[name]] <- eval(parms[[name]])
        }
        switch(
          typeof(parms[[name]]),
          character = type_array(type_string()),
          integer = type_array(type_integer()),
          numeric = type_array(type_number()),
          logical = type_array(type_boolean()),
          list = type_object(),
          NULL
        )
      })
      names(tool_args) <- names(parms)

      # Create the specific block tool
      block_tool <- tool(
        function(name, append = FALSE, parms = list()) {
          dat <- list(
            name = name,
            append = append,
            parms = parms
          )

          # Needs a reactive context... will happen once
          observeEvent(TRUE, {
            parent$scoutbar$action <- "add_block"
            parent$scoutbar$value <- dat
            if (dat$append) {
              parent$append_block <- TRUE
            }
          })
          return(app_request(list(action = "add_block", data = dat)))
        },
        name = paste0("add_", ctor),
        description = paste(
          "Add a",
          ctor,
          "with specific parameters"
        ),
        arguments = list(
          name = type_string(
            "Name of the block to be created. Typically like *_block where '*' is
                the block type (dataset, select, ...) and without the new_ prefix.
                Valid names are given by the `available_block_names` tool."
          ),
          append = type_boolean(
            "Whether to append to previous block. Default to FALSE."
          ),
          parms = do.call(
            "type_object",
            c(
              .description = "Parameters for the block constructor. 
                  Each parameter type is inferred from the default values of 
                  the block constructor.",
              tool_args,
              .required = FALSE
            )
          )
        )
      )
      provider$register_tool(block_tool)
      return(NULL)
    },
    name = "create_block_tool_factory",
    description = "Create a tool for a given block type.",
    arguments = list(
      ctor = type_string(
        "Block constructor to create tool for (e.g., 'new_dataset_block', 'new_select_block').
            Valid names are given by the `available_block_names` tool."
      )
    )
  )

  provider$register_tool(block_tool_factory)
}

#' Create a tool to list available block names
#' @rdname ai-chat-tool
#' @export
create_block_names_tool <- function(provider) {
  available_block_names <- tool(
    function() {
      names(blockr.core::available_blocks())
    },
    name = "available_block_names",
    description = "Returns a vector of strings containing registered block names.",
    arguments = list()
  )

  provider$register_tool(available_block_names)
}

#' Create a tool to remove a block by its id
#' @rdname ai-chat-tool
#' @export
create_remove_block_tool <- function(
  provider,
  app_request,
  board,
  parent,
  session
) {
  remove_block <- tool(
    function(id) {
      # Needs a reactive context... will happen once
      observeEvent(TRUE, {
        if (!(id %in% board_block_ids(board$board))) {
          showNotification(
            paste("Block with id", id, "does not exist."),
            type = "error"
          )
          return(FALSE)
        }
        parent$removed_block <- id
      })
      return(app_request(list(action = "remove_block", data = id)))
    },
    name = "remove_block",
    description = "Remove a block by its id.",
    arguments = list(
      id = type_string(
        "Id of the block to be removed."
      )
    )
  )

  provider$register_tool(remove_block)
}

#' Create a tool to add a stack with given blocks
#'
#' @param stackable_blocks Reactive expression returning a vector of blocks
#' that can be stacked.
#' @rdname ai-chat-tool
#' @export
create_add_stack_tool <- function(
  provider,
  stackable_blocks,
  app_request,
  board,
  parent,
  session
) {
  create_stack <- tool(
    function(blocks) {
      # Needs a reactive context... will happen once
      observeEvent(TRUE, {
        if (any(!(blocks %in% board_block_ids(board$board)))) {
          showNotification(
            "Some blocks do not exist in the board.",
            type = "error"
          )
          return(FALSE)
        }

        if (any(!(blocks %in% stackable_blocks()))) {
          non_stackable <- blocks[!(blocks %in% stackable_blocks())]
          showNotification(
            paste(
              "Some blocks are already in stacks and cannot be stacked again:",
              paste(non_stackable, collapse = ", ")
            ),
            type = "error"
          )
          return(FALSE)
        }
        parent$added_stack <- blocks
      })
      return(app_request(list(
        action = "add_stack",
        data = list(blocks = blocks)
      )))
    },
    name = "create_stack",
    description = "Create a stack with a given name and blocks.",
    arguments = list(
      blocks = type_array(type_string(
        "Ids of the blocks to include in the stack. IDs must exist in the board. You
            can call `get_stackable_blocks` to get the list of available block IDs."
      ))
    )
  )

  provider$register_tool(create_stack)
}

#' Create a tool to get stackable blocks
#' @rdname ai-chat-tool
#' @export
create_get_stackable_blocks_tool <- function(provider, stackable_blocks) {
  get_stackable_blocks <- tool(
    function() {
      isolate(stackable_blocks())
    },
    name = "get_stackable_blocks",
    description = "Returns a vector of strings containing the ids of 
        all blocks in the board that don't belong to a stack.",
    arguments = list()
  )

  provider$register_tool(get_stackable_blocks)
}

#' Create a tool to get stack ids
#' @rdname ai-chat-tool
#' @export
create_get_stack_ids_tool <- function(provider, board) {
  get_stack_ids <- tool(
    function() {
      # Needs a reactive context... will happen once
      isolate(names(board_stacks(board$board)))
    },
    name = "get_stack_ids",
    description = "Returns a vector of strings containing the 
        ids of all stacks in the board.",
    arguments = list()
  )

  provider$register_tool(get_stack_ids)
}

#' Create a tool to add a block to an existing stack
#' @rdname ai-chat-tool
#' @export
create_add_block_to_stack_tool <- function(
  provider,
  stackable_blocks,
  app_request,
  board,
  parent,
  session
) {
  add_block_to_stack <- tool(
    function(stack_id, block_id) {
      # Needs a reactive context... will happen once
      observeEvent(TRUE, {
        if (!(stack_id %in% names(board_stacks(board$board)))) {
          showNotification(
            paste("Stack with id", stack_id, "does not exist."),
            type = "error"
          )
          return(FALSE)
        }
        if (!(block_id %in% board_block_ids(board$board))) {
          showNotification(
            paste("Block with id", block_id, "does not exist."),
            type = "error"
          )
          return(FALSE)
        }

        if (!(block_id %in% stackable_blocks())) {
          showNotification(
            paste("Block with id", block_id, "is already in a stack."),
            type = "error"
          )
          return(FALSE)
        }

        # Feedback for the board
        parent$stack_added_node <- list(
          stack_id = stack_id,
          node_id = block_id
        )

        # Feedback for the graph
        add_nodes_to_stack(
          sprintf("combo-%s", stack_id),
          block_id,
          board,
          parent,
          session
        )
      })
      return(app_request(list(
        action = "add_block_to_stack",
        data = list(stack_id = stack_id, node_id = block_id)
      )))
    },
    name = "add_block_to_stack",
    description = "Add a block to an existing stack.",
    arguments = list(
      stack_id = type_string(
        "Id of the stack to add the block to. Must be a valid
          id returned by `get_stack_ids`."
      ),
      block_id = type_string(
        "Id of the block to add to the stack. 
          A valid ID returned by `get_stackable_blocks`."
      )
    )
  )
  provider$register_tool(add_block_to_stack)
}

#' @keywords internal
get_block_parameters <- function(ctor) {
  parms <- formals(ctor)
  parms[setdiff(names(parms), "...")]
}

#' @keywords internal
get_stackable_blocks <- function(board) {
  block_ids <- board_block_ids(board$board)
  cannot_stack <- unlist(
    lapply(board_stacks(board$board), stack_blocks),
    use.names = FALSE
  )
  if (!length(cannot_stack)) {
    return(block_ids)
  }
  block_ids[!(block_ids %in% cannot_stack)]
}
