#' @keywords internal
init_chat_message <- function(provider) {
  list(
    role = "assistant",
    content = sprintf(
      "Hi! I'll help you to build blockr.ui pipeline with %s. 
      You can start with: Import iris data, select the Species 
      column and stack the 2 blocks. 
      Then filter Species to only account for 'virginica'. 
      Add the new block to the previous stack.",
      provider
    )
  )
}

#' Utility functions to create AI tools for blockr.ui
#'
#' @keywords internal
system_prompt <- function() {
  readLines(
    system.file("examples/ai-chat/rules.md", package = "blockr.ui")
  )
}

#' Create a task to append chat messages
#' @param session Shiny session object.
#' @keywords internal
setup_chat_task <- function(session) {
  ExtendedTask$new(
    function(client, ui_id, user_input) {
      promises::then(
        promises::promise_resolve(client$stream_async(
          user_input,
          tool_mode = "sequential",
          stream = "content"
        )),
        function(stream) {
          chat_append(ui_id, stream, session = session)
        }
      )
    }
  )
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
          # Setting required = FALSE to avoid having
          # wrong values from the AI.
          character = type_array(
            type_string(required = FALSE),
            required = FALSE
          ),
          integer = type_array(
            type_integer(required = FALSE),
            required = FALSE
          ),
          numeric = type_array(type_number(required = FALSE), required = FALSE),
          logical = type_array(
            type_boolean(required = FALSE),
            required = FALSE
          ),
          list = type_object(.required = FALSE),
          NULL
        )
      })
      names(tool_args) <- names(parms)

      # Create the specific block tool
      block_tool <- tool(
        function(name, append = FALSE, parms = list()) {

          id <- rand_names(isolate(board_block_ids(board$board)))

          if (!"name" %in% names(params)) {
            params <- c(params, list(name = id_to_sentence_case(id)))
          }

          dat <- list(
            name = name,
            append = append,
            parms = dropNulls(parms)
          )

          res <- list(action = "add_block", data = dat)
          app_request(res)

          # Check that the block can be built.
          new_blk <- tryCatch(
            do.call(create_block, c(list(id = dat$name), dat$parms)),
            error = function(e) {
              res$error <- paste("Error while adding block:", e$message)
              app_request(res)
              return(res)
            }
          )

          # Needs a reactive context
          isolate(
            {
              parent$scoutbar$action <- "add_block"
              if (is_block(new_blk)) {
                parent$scoutbar$value <- as_blocks(set_names(list(new_blk), id))
              }
              if (dat$append) {
                parent$append_block <- TRUE
              }
            }
          )

          res
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
      isolate(provider()$register_tool(block_tool))
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

  provider()$register_tool(block_tool_factory)
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

  provider()$register_tool(available_block_names)
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
      res <- list(action = "remove_block", data = id)
      app_request(res)

      # Check that id is valid
      res <- check_block_id(id, board, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }

      parent$removed_block <- id

      return(res)
    },
    name = "remove_block",
    description = "Remove a block by its id.",
    arguments = list(
      id = type_string(
        "Id of the block to be removed."
      )
    )
  )

  provider()$register_tool(remove_block)
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
      res <- list(
        action = "add_stack",
        data = list(blocks = blocks)
      )
      app_request(res)

      block_ids <- isolate(board_block_ids(board$board))
      if (any(!(blocks %in% block_ids))) {
        invalid_ids <- blocks[!(blocks %in% block_ids)]
        res$error <- paste(
          "Some block ids do not exist in the board:",
          paste(invalid_ids, collapse = ", ")
        )
        app_request(res)
        return(res)
      }

      stackable_blocks <- isolate(stackable_blocks())

      if (any(!(blocks %in% stackable_blocks))) {
        non_stackable <- blocks[!(blocks %in% stackable_blocks)]
        res$error <- paste(
          "Some blocks are already in stacks and cannot be stacked again:",
          paste(non_stackable, collapse = ", ")
        )
        app_request(res)
        return(res)
      }

      # Needs a reactive context... will happen once
      observeEvent(TRUE, {
        parent$added_stack <- blocks
      })
      return(res)
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

  provider()$register_tool(create_stack)
}

#' Create a tool to get stackable blocks
#' @rdname ai-chat-tool
#' @export
create_stackable_blocks_tool <- function(provider, stackable_blocks) {
  get_stackable_blocks <- tool(
    function() {
      isolate(stackable_blocks())
    },
    name = "get_stackable_blocks",
    description = "Returns a vector of strings containing the ids of 
        all blocks in the board that don't belong to a stack.",
    arguments = list()
  )

  provider()$register_tool(get_stackable_blocks)
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

  provider()$register_tool(get_stack_ids)
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

      res <- list(
        action = "add_block_to_stack",
        data = list(stack_id = stack_id, node_id = block_id)
      )
      app_request(res)

      res <- check_stack_id(stack_id, board, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }
      res <- check_block_id(block_id, board, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }

      if (!(block_id %in% isolate(stackable_blocks()))) {
        res$error <- paste("Block with id", block_id, "is already in a stack.")
        app_request(res)
        return(res)
      }

      observeEvent(TRUE, {
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
      return(res)
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
  provider()$register_tool(add_block_to_stack)
}

#' Create a tool to add a block to the dashboard
#' @rdname ai-chat-tool
#' @export
create_add_to_dash_tool <- function(
  provider,
  app_request,
  board,
  parent,
  session
) {
  add_block_to_dashboard <- tool(
    function(block_id) {
      # Needs a reactive context... will happen once
      res <- list(action = "add_to_dashboard", data = block_id)
      app_request(res)

      res <- check_block_id(block_id, board, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }

      # Block should not be already in the dashboard
      res <- check_block_grid_status(block_id, parent, FALSE, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }

      observeEvent(TRUE, {
        parent$added_to_dashboard <- block_id
        parent$in_grid[[block_id]] <- TRUE
      })
      return(res)
    },
    name = "add_block_to_dashboard",
    description = "Add a block to the dashboard module.",
    arguments = list(
      block_id = type_string(
        "Id of the block to add to the dashboard."
      )
    )
  )

  provider()$register_tool(add_block_to_dashboard)
}

#' Create a tool to remove a block from the dashboard
#' @rdname ai-chat-tool
#' @export
create_remove_from_dash_tool <- function(
  provider,
  app_request,
  board,
  parent,
  session
) {
  remove_block_from_dashboard <- tool(
    function(block_id) {
      res <- list(action = "remove_from_dashboard", data = block_id)
      app_request(res)

      res <- check_block_id(block_id, board, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }

      # Block should be already in the dashboard
      res <- check_block_grid_status(block_id, parent, TRUE, res, app_request)
      if (!is.null(res$error)) {
        return(res)
      }

      observeEvent(TRUE, {
        parent$removed_from_dashboard <- block_id
        parent$in_grid[[block_id]] <- FALSE
      })
      return(res)
    },
    name = "remove_block_from_dashboard",
    description = "Remove a block from the dashboard module.",
    arguments = list(
      block_id = type_string(
        "Id of the block to remove from the dashboard.."
      )
    )
  )

  provider()$register_tool(remove_block_from_dashboard)
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

#' @keywords internal
check_block_id <- function(id, board, res, app_request) {
  block_ids <- isolate(board_block_ids(board$board))
  if (!(id %in% block_ids)) {
    res$error <- paste("Block with id", id, "does not exist.")
    app_request(res)
  }
  res
}

#' @keywords internal
check_stack_id <- function(id, board, res, app_request) {
  stack_names <- isolate(names(board_stacks(board$board)))
  if (!(id %in% stack_names)) {
    res$error <- paste("Stack with id", id, "does not exist.")
    app_request(res)
  }
  res
}

#' @keywords internal
check_block_grid_status <- function(
  id,
  parent,
  should_be = TRUE,
  res,
  app_request
) {
  is_in_grid <- isolate(parent$in_grid[[id]])

  if (is_in_grid == should_be) {
    app_request(res)
  } else {
    res$error <- if (should_be) {
      paste("Block with id", id, "is already in the dashboard.")
    } else {
      paste("Block with id", id, "is not in the dashboard.")
    }
  }
  res
}
