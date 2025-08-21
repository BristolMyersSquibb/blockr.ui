#' @param modules Further modules to pass.
#' @param class Additional class(es).
#' @rdname run_demo_app
#' @export
new_dag_board <- function(..., modules = new_dashboard_module(),
                          options = dag_board_options(),
                          class = character()) {

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

#' @rdname run_demo_app
#' @export
dag_board_options <- function() {
  c(
    default_board_options(),
    new_stack_colors_options(),
    new_auto_snapshot_option()
  )

  n_stacks <- 40
  stacks_color_palette <- "spectral"
  if (nchar(Sys.getenv("N_STACKS_COLORS")) > 0) {
    n_stacks <- Sys.getenv("N_STACKS_COLORS")
  }
  if (nchar(Sys.getenv("STACKS_COLOR_PALETTE")) > 0) {
    stacks_color_palett <- Sys.getenv("STACKS_COLOR_PALETTE")
  }

  snapshot_location <- tempdir()
  if (nchar(Sys.getenv("SNAPSHOT_LOCATION")) > 0) {
    snapshot_location <- Sys.getenv("SNAPSHOT_LOCATION")
  }

  auto_snapshot <- FALSE
  if (nchar(Sys.getenv("AUTO_SNAPSHOT")) > 0) {
    auto_snapshot <- as.logical(Sys.getenv("AUTO_SNAPSHOT"))
  }

  new_board_options(
    dark_mode = "light",
    stacks_colors = hcl.colors(n_stacks, palette = stacks_color_palette),
    dashboard_zoom = 1,
    snapshot = list(
      location = snapshot_location,
      auto = auto_snapshot
    )
  )
}

#' @export
serve.dag_board <- function(x, id = "main", ...) {

  Sys.setenv("blockr_dark_mode" = "light")

  modules <- board_modules(x)

  ctx_menu_items <- unlst(
    c(
      list(
        list(
          create_edge_ctxm,
          remove_node_ctxm,
          remove_edge_ctxm,
          append_node_ctxm,
          create_stack_ctxm,
          remove_stack_ctxm,
          add_block_ctxm
        )
      ),
      lapply(modules, board_module_context_menu)
    )
  )

  plugins <- plugins(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(
      server = gen_add_rm_link_server(ctx_menu_items),
      ui = add_rm_link_ui
    ),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    generate_code(server = generate_code_server, ui = generate_code_ui),
    notify_user()
  )

  ui <- page_fillable(
    padding = 0,
    gap = 0,
    shinyjs::useShinyjs(),
    add_busy_load_deps(main_ui(id, x, plugins)),
    ...
  )

  server <- function(input, output, session) {
    main_server(id, x, plugins, modules)
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
