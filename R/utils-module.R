#' Create a board module
#'
#' Extend a DAG board by adding modules.
#'
#' @param ui,server UI and server functions
#' @param id,title Module ID and title
#' @param context_menu List of context menu entries
#' @param position Panel position
#' @param class (Optional) additional class(es)
#'
#' @export
#' @rdname board-module
new_board_module <- function(
  ui,
  server,
  id,
  title,
  context_menu = list(),
  position = NULL,
  options = new_board_options(),
  class = character()
) {
  if (is_context_menu_entry(context_menu)) {
    context_menu <- list(context_menu)
  }

  stopifnot(
    is.function(ui),
    is.function(server),
    is_string(id),
    is_string(title),
    is.list(context_menu),
    all(lgl_ply(context_menu, is_context_menu_entry))
  )

  structure(
    list(
      server = server,
      ui = ui,
      context_menu = context_menu,
      options = as_board_options(options)
    ),
    id = id,
    title = title,
    position = position,
    class = c(class, "board_module")
  )
}

is_board_module <- function(x) {
  inherits(x, "board_module")
}

board_module_server <- function(x) {
  stopifnot(is_board_module(x))
  x[["server"]]
}

board_module_ui <- function(x) {
  stopifnot(is_board_module(x))
  x[["ui"]]
}

board_module_options <- function(x) {
  stopifnot(is_board_module(x))
  x[["options"]]
}

board_module_id <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "id")
}

board_module_title <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "title")
}

board_module_context_menu <- function(x) {
  stopifnot(is_board_module(x))
  x[["context_menu"]]
}

board_module_position <- function(x) {
  stopifnot(is_board_module(x))
  attr(x, "position")
}

board_module_positions <- function(x) {
  res <- lapply(x, board_module_position)

  nul <- lgl_ply(res, is.null)

  if (nul[[1L]]) {
    res[[1L]] <- list(referencePanel = "dag", direction = "right")
  }

  if (any(nul[-1L])) {
    res[setdiff(which(nul), 1L)] <- rep(
      list(
        list(referencePanel = board_module_id(x[[1L]]), direction = "within")
      ),
      sum(nul[-1L])
    )
  }

  res
}

call_board_module_ui <- function(x, ...) {
  board_module_ui(x)(...)
}

create_edge_ctxm <- new_context_menu_entry(
  name = "Create edge",
  js = "(value, target, current) => {
      if (current.id === undefined) return;
      const graphId = `${target.closest('.g6').id}`;
      const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
      graph.updateBehavior({
        key: 'create-edge', // Specify the behavior to update
        enable: true,
      });
      // Select node
      graph.setElementState(current.id, 'selected');
      // Disable drag node as it is incompatible with edge creation
      graph.updateBehavior({ key: 'drag-element', enable: false });
      graph.updateBehavior({ key: 'drag-element-force', enable: false });
    }",
  condition = function(board, parent, target) {
    target$type == "node"
  },
  id = "create_edge"
)

remove_node_ctxm <- new_context_menu_entry(
  name = "Remove node",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
        if (current.id === undefined) return;
        Shiny.setInputValue('%s', current.id);
      }",
      ns("remove_node")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$remove_node,
      {
        parent$removed_block <- input$remove_node
      }
    )
  },
  condition = function(board, parent, target) {
    target$type == "node"
  }
)

remove_edge_ctxm <- new_context_menu_entry(
  name = "Remove edge",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      if (current.id === undefined) return;
      Shiny.setInputValue('%s', current.id);
      const graphId = `${target.closest('.g6').id}`;
      const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
      graph.removeEdgeData([current.id]);
      graph.draw();
    }",
      ns("remove_edge")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$remove_edge,
      {
        update(
          list(
            links = list(rm = input$remove_edge)
          )
        )
      }
    )
  },
  condition = function(board, parent, target) {
    target$type == "edge"
  }
)

append_node_ctxm <- new_context_menu_entry(
  name = "Append node",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      Shiny.setInputValue('%s', true, {priority: 'event'});
    }",
      ns("append_node")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$append_node,
      {
        if (is.null(parent$selected_block)) {
          return(NULL)
        }
        parent$scoutbar$trigger <- "links"
        if (isFALSE(parent$append_block)) {
          parent$append_block <- TRUE
        }
      }
    )
  },
  condition = function(board, parent, target) {
    target$type == "node"
  }
)

create_stack_ctxm <- new_context_menu_entry(
  name = "Create stack",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      Shiny.setInputValue('%s', true, {priority: 'event'});
    }",
      ns("create_stack")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$create_stack,
      {
        show_stack_actions(
          board,
          session
        )
      }
    )
  },
  condition = function(board, parent, target) {
    target$type == "canvas"
  }
)

remove_stack_ctxm <- new_context_menu_entry(
  name = "Remove stack",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      if (current.id === undefined) return;
      Shiny.setInputValue('%s', current.id);
    }",
      ns("remove_stack")
    )
  },
  action = function(input, output, session, board, update, parent) {
    observeEvent(
      input$remove_stack,
      unstack_nodes(parent, session)
    )
  },
  condition = function(board, parent, target) {
    target$type == "combo"
  }
)

add_block_ctxm <- new_context_menu_entry(
  name = "Add block",
  js = function(ns) {
    sprintf(
      "(value, target, current) => {
      Shiny.setInputValue('%s', true, {priority: 'event'});
    }",
      ns("add_block")
    )
  },
  condition = function(board, parent, target) {
    target$type == "canvas"
  }
)
