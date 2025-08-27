#' Create block icon
#'
#' Blocks are categorized. This function
#' returns the corresponding icon for each category.
#' It may be used in different places like in the nodes
#' or in the contextual menu (scoutbar) ...
#'
#' @param category Block category. See \link[blockr.core]{available_blocks}.
#'
#' @keywords internal
blk_icon <- function(category) {
  if (!length(category)) {
    res <- "reddit-alien"
  } else {
    res <- switch(
      category,
      "data" = "table",
      "file" = "file",
      "parse" = "gear",
      "plot" = "chart-line",
      "transform" = "wand-magic",
      "table" = "table",
      "reddit-alien"
    )
  }

  # FIXME: We can't use fontawesome in the scoutbaR
  # due to compatibility issue with the g6R toolbar...
  icon(res)
}

#' Create block choices for scoutbaR widget
#'
#' Utility to populate the scoutbar with block
#' registry information. Create one page per block category
#'
#' @keywords internal
blk_choices <- function() {
  blk_cats <- sort(
    unique(chr_ply(available_blocks(), \(b) attr(b, "category")))
  )

  lapply(blk_cats, \(cat) {
    scout_section(
      label = cat,
      .list = dropNulls(
        unname(
          lapply(available_blocks(), \(choice) {
            if (attr(choice, "category") == cat) {
              scout_action(
                id = sprintf("%s@add_block", attr(choice, "classes")[1]),
                label = attr(choice, "name"),
                description = attr(choice, "description"),
                icon = blk_icon(cat)
              )
            }
          })
        )
      )
    )
  })
}

#' @rdname board_ui
#' @export
options_ui <- function(id, x, ...) {
  opts <- split(x, chr_ply(x, attr, "category"))

  offcanvas_id <- NS(id, "options-offcanvas")

  tagList(
    tags$a(
      class = "btn btn-sm btn-white",
      icon("gear"),
      `data-bs-toggle` = "offcanvas",
      `data-bs-target` = sprintf("#%s", offcanvas_id),
      `aria-controls` = offcanvas_id
    ),
    off_canvas(
      id = offcanvas_id,
      position = "end",
      title = "Board options",
      do.call(
        accordion,
        c(
          list(
            id = NS(id, "board_options"),
            multiple = TRUE,
            open = TRUE,
            class = "accordion-flush"
          ),
          map(
            do.call,
            rep(list(accordion_panel), length(opts)),
            map(
              list,
              title = names(opts),
              lapply(opts, lapply, board_option_ui, id)
            )
          )
        )
      )
    )
  )
}

#' Board header
#'
#' Header layout.
#'
#' @param id Board id.
#' @param board_ui Board ui.
#' @rdname board-layout
#' @keywords internal
board_header <- function(id, board_ui) {
  div(
    class = "d-flex align-items-center justify-content-center mx-5 gap-3",
    board_ui$toolbar_ui$preserve_board$restore,
    div(
      style = "margin-left: auto",
      board_ui$board_options_ui
    )
  )
}

#' Custom board UI
#'
#' @param id Namespace ID.
#' @param x Board.
#' @param plugins UI for board plugins.
#' @param ... Generic consistency.
#' @rdname board_ui
#' @export
board_ui.dag_board <- function(id, x, plugins = list(), ...) {
  plugins <- as_plugins(plugins)

  toolbar_plugins <- c(
    "preserve_board",
    "manage_stacks",
    "generate_code"
  )

  toolbar_plugins <- plugins[intersect(toolbar_plugins, names(plugins))]
  toolbar_ui <- setNames(
    board_ui(id, toolbar_plugins, x),
    names(toolbar_plugins)
  )

  if ("edit_block" %in% names(plugins)) {
    block_plugin <- plugins[["edit_block"]]
  } else {
    block_plugin <- NULL
  }

  my_board_ui <- list(
    toolbar_ui = toolbar_ui,
    notifications = board_ui(id, plugins[["notify_user"]], x),
    board_options_ui = options_ui(id, as_board_options(x))
  )

  # If there are blocks at start, we need to generate the UI
  # There are then put in the offcanvas, waiting to be shown
  # For now, I've omited the plugins[["edit_block"]] but that can
  # be added later on.
  blocks <- lapply(
    board_block_ids(x),
    \(blk_id) {
      block_ui(
        id = id,
        x = x,
        block = board_blocks(x)[blk_id]
      )
    }
  )

  tagList(
    # Offcanvas is used has an hidden element to move block UI whenever
    # we remove and add panels in the dock. This avoids to have
    # to recreate the block UI each time (which causes other issues anyway)
    off_canvas(
      id = paste0(id, "-offcanvas"),
      title = "Board",
      blocks
    ),
    board_header(id, my_board_ui),
    dockViewOutput(
      paste0(id, "-layout"),
      width = "100%",
      height = "100vh"
    ),
    scoutbar(
      sprintf("%s-scoutbar", id),
      placeholder = "What do you want to do?",
      showRecentSearch = TRUE
    )
  )
}

#' Board restoration callback
#'
#' @keywords internal
#' @rdname handlers-utils
board_restore <- function(board, update, session, parent, ...) {
  board_refresh <- get("board_refresh", parent.frame(1))

  observeEvent(
    board_refresh(),
    {
      showNotification(
        "Board restored",
        type = "message",
        duration = 1
      )
      parent$refreshed <- "board"
    },
    ignoreInit = TRUE
  )

  NULL
}

get_block_panels <- function(session) {
  gsub(
    "block-",
    "",
    grep(
      "block",
      get_panels_ids("layout", session),
      value = TRUE
    )
  )
}

cleanup_layout <- function(parent, session) {
  parent$refreshed <- "clean-layout"
  showNotification(
    "Layout cleaned",
    type = "message",
    duration = 1
  )
  block_panels <- get_block_panels(session)
  if (!length(block_panels)) {
    return(NULL)
  }
  remove_block_panels(block_panels)
}

#' App layout
#'
#' @keywords internal
#' @rdname handlers-utils
build_layout <- function(modules, plugins) {
  function(board, update, session, parent, ...) {
    input <- session$input
    output <- session$output
    ns <- session$ns

    # Cleanup existing panels on restore
    observeEvent(
      {
        req(parent$refreshed == "restore-network")
      },
      {
        cleanup_layout(parent, session)
      }
    )

    # Wait that the layout is cleaned so we can create and show
    # any existing block panel
    observeEvent(
      {
        req(
          parent$refreshed == "clean-layout",
          length(get_block_panels(session)) == 0
        )
      },
      {
        parent$refreshed <- "restore-layout"
      }
    )

    # Dynamic trigger that account for restore context
    add_panel_trigger <- reactive({
      if (is.null(parent$refreshed)) {
        req(parent$selected_block, length(parent$selected_block) == 1)
      } else if (parent$refreshed == "restore-layout") {
        req(parent$refreshed == "restore-layout")
      }
    })

    # Add or re-insert block panel ui
    observeEvent(
      add_panel_trigger(),
      {
        create_or_show_block_panel(parent$selected_block, parent, session)
      }
    )

    observeEvent(
      input[["layout_panel-to-remove"]],
      {
        hide_block_panel(input[["layout_panel-to-remove"]], session)
        # Send callback to links plugin to unselect the node
        parent$unselected_block <- gsub(
          "block-",
          "",
          input[["layout_panel-to-remove"]]
        )
      }
    )

    # Remove block panel on block remove
    # We can remove multiple blocks at once
    observeEvent(parent$removed_block, {
      remove_block_panels(parent$removed_block)
    })

    output$layout <- renderDockView({
      # Since board$board is reactive, we need to isolate it
      # so we don't re-render the whole layout each time ...
      isolate({
        dock_view(
          panels = c(
            list(
              panel(
                id = "dag",
                title = "Pipeline overview",
                content = board_ui(
                  ns(NULL),
                  plugins["manage_links"]
                )
              )
            ),
            map(
              panel,
              id = chr_ply(modules, board_module_id),
              title = chr_ply(modules, board_module_title),
              content = lapply(
                modules,
                call_board_module_ui,
                ns(NULL),
                board$board
              ),
              position = board_module_positions(modules)
            )
          ),
          # TBD (make theme function of board options)
          theme = "light"
        )
      })
    })

    # Update theme in real time
    observeEvent(get_board_option_value("dark_mode"), {
      update_dock_view(
        "layout",
        list(theme = get_board_option_value("dark_mode"))
      )
    })

    NULL
  }
}

#' Scoutbar management callback
#'
#' @keywords internal
#' @rdname handlers-utils
manage_scoutbar <- function(board, update, session, parent, ...) {
  input <- session$input
  ns <- session$ns

  # Trigger add block
  observeEvent(
    req(parent$open_scoutbar),
    {
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    }
  )

  # Reset dot_args$parent$append_block is user
  # accidentally close the scoutbar without selecting
  # a block, so that the scoutbar can open again on the
  # next input$append_block or from the links plugin.
  observeEvent(
    input[["scoutbar-open"]],
    {
      if (!input[["scoutbar-open"]]) {
        parent$append_block <- FALSE
        parent$open_scoutbar <- FALSE
        parent$scoutbar <- list()
      }
      parent$scoutbar$is_open <- input[["scoutbar-open"]]
    }
  )

  # Open the scoutbar when append block
  observeEvent(req(parent$append_block), {
    update_scoutbar(
      session,
      "scoutbar",
      revealScoutbar = TRUE
    )
  })

  # Update scoutbar action with snapshots taken in the serialise module
  observeEvent(
    {
      parent$backup_list
    },
    {
      # TBD: this isn't optimal. scoutbaR should
      # be able to allow to append/remove/modify actions
      # instead of having to re-create the whole list.
      new_actions <- list(
        scout_page(
          label = "Add a block",
          .list = blk_choices()
        ),
        scout_page(
          label = "Restore a snapshot",
          .list = lapply(
            list_snapshot_files(board$board_id),
            \(path) {
              infos <- file.info(path)
              scout_action(
                id = sprintf("%s@restore_board", path),
                label = strsplit(
                  path,
                  path.expand(
                    attr(
                      get_board_option_or_default(
                        "snapshot",
                        dag_board_options(),
                        session
                      ),
                      "location"
                    )
                  ),
                  ""
                )[[1]][2],
                description = sprintf(
                  "Created by %s. Date: %s. Size: %s KB",
                  infos[["uname"]],
                  round(infos[["mtime"]], units = "secs"),
                  round(infos[["size"]] / 1000, 1)
                ),
                icon = icon("file")
              )
            }
          )
        )
      )
      # We need to avoid to overwrite the existing actions ...
      update_scoutbar(
        session,
        "scoutbar",
        actions = new_actions
      )
    }
  )

  # Sync value for other modules
  observeEvent(input$scoutbar, {
    tmp <- strsplit(input$scoutbar, "@")[[1]]
    parent$scoutbar <- list(
      action = tmp[2],
      value = tmp[1]
    )
  })

  NULL
}
