#' @param modules Further modules to pass.
#' @param class Additional class(es).
#' @param options Board options (see [dag_board_options()]).
#' @rdname run_demo_app
#' @export
new_dag_board <- function(
  ...,
  modules = new_dashboard_module(),
  options = dag_board_options(),
  class = character()
) {
  if (is_board_module(modules)) {
    modules <- list(modules)
  }

  stopifnot(is.list(modules), all(lgl_ply(modules, is_board_module)))

  opts <- c(
    as_board_options(options),
    lapply(modules, board_module_options)
  )

  new_board(
    ...,
    options = opts,
    modules = modules,
    class = c(class, "dag_board")
  )
}

#' @param x (Board) object
#' @rdname run_demo_app
#' @export
is_dag_board <- function(x) {
  inherits(x, "dag_board") && is_board(x)
}

#' @rdname run_demo_app
#' @export
dag_board_options <- function() {
  new_board_options(
    new_board_name_option(category = "Board options"),
    if (need_llm_cfg_opts()) new_llm_model_option(category = "Board options"),
    new_n_rows_option(category = "Table options"),
    new_page_size_option(category = "Table options"),
    new_filter_rows_option(category = "Table options"),
    new_thematic_option(category = "Theme options"),
    new_dark_mode_option(
      blockr_option("dark_mode", FALSE),
      category = "Theme options"
    ),
    new_stack_colors_option(category = "Board options"),
    new_show_conditions_option(category = "Board options"),
    new_blocks_position_option(category = "Layout options")
  )
}

#' @export
board_plugins.dag_board <- function(x, which = NULL, ...) {
  res <- plugins(
    preserve_board(ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(
      server = gen_add_rm_link_server(context_menu_items(x)),
      ui = add_rm_link_ui
    ),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    generate_code(server = generate_code_server, ui = generate_code_ui),
    notify_user(),
    edit_block(server = edit_block_server, ui = edit_block_ui)
  )

  if (is.null(which)) {
    return(res)
  }

  res[which]
}

#' @export
serve.dag_board <- function(
  x,
  id = "main",
  board_id = rand_names(),
  plugins = board_plugins(x),
  ...
) {
  ui <- do.call(
    page_fillable,
    c(
      list(
        padding = 0,
        gap = 0,
        shinyjs::useShinyjs(),
        add_busy_load_deps(main_ui(id, x, board_id, plugins))
      ),
      unname(list(...))
    )
  )

  server <- function(input, output, session) {
    main_server(id, x, board_id, plugins)
  }

  shinyApp(add_blockr.ui_deps(ui), server)
}

#' @include context-menu.R
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

#' @include context-menu.R
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

#' @include context-menu.R
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

#' @include context-menu.R
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

#' @include context-menu.R
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

#' @include context-menu.R
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

#' @include context-menu.R
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
