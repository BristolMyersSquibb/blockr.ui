#' Create block icon
#'
#' Blocks are categorized. This function
#' returns the corresponding icon for each category.
#' It may be used in different places like in the nodes
#' or in the contextual menu (scoutbar) ...
#'
#' @param category Block category. See \link[blockr.core]{available_blocks}.
#' @param class Additional class to add to the icon, to change the size for example.
#'
#' @keywords internal
blk_icon <- function(category, class = NULL) {
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
  icon(res, class)
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
    tags$button(
      class = "blockr-fab",
      icon("gear"),
      `data-bs-toggle` = "offcanvas",
      `data-bs-target` = sprintf("#%s", offcanvas_id),
      `aria-controls` = offcanvas_id
    ),
    off_canvas(
      id = offcanvas_id,
      position = "end",
      title = "Board options",
      ...,
      hr(),
      do.call(
        accordion,
        c(
          list(
            id = NS(id, "board_options"),
            multiple = TRUE,
            open = FALSE,
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
    board_options_ui = options_ui(
      id,
      as_board_options(x),
      toolbar_ui$preserve_board$restore
    )
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
    my_board_ui$board_options_ui,
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
      parent$refreshed <- "refresh-board"
    },
    ignoreInit = TRUE
  )

  NULL
}

get_block_panels <- function(panels, pattern = "block-") {
  # TBD: pattern is hardcoded because we prefix our panels
  # with block- elsewhere... We could centralise that.
  gsub(
    pattern,
    "",
    grep(
      pattern,
      panels,
      value = TRUE
    )
  )
}

restore_layout <- function(parent, session) {
  # Move any existing block UI from the offcanvas to their panel
  block_panels <- get_block_panels(names(parent$app_layout$panels))
  lapply(block_panels, \(id) {
    dockViewR::select_panel(
      "layout",
      sprintf("block-%s", id)
    )
    # Move block from offcanvas to panel
    show_block_panel(id, session)
  })
  parent$refreshed <- "restore-layout"
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

    # Save layout on change
    observeEvent(
      {
        # Should not trigger on restore, only when the dashboard changes
        req(is.null(parent$refreshed))
        input$layout_state
      },
      {
        parent$app_layout <- input$layout_state
      }
    )

    # Restore layout from snapshot
    observeEvent(
      {
        req(parent$refreshed == "refresh-board")
      },
      {
        # No need to cleanup before
        restore_dock("layout", parent$app_layout)
        parent$refreshed <- "restore-dock"
      }
    )

    # Wait for state to be synchronised
    observeEvent(
      {
        req(
          parent$refreshed == "restore-dock",
          setequal(
            names(input$layout_state$panels),
            names(parent$app_layout$panels)
          )
        )
      },
      {
        # Ensure the default renderer is always on
        # since restoring layout does not manage to preserve
        # the individual panel renderer state.
        dockViewR::update_dock_view(
          "layout",
          list(defaultRenderer = "always")
        )
        restore_layout(parent, session)
      }
    )

    # Add or re-insert block panel ui
    observeEvent(
      req(parent$selected_block, length(parent$selected_block) == 1),
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
      remove_block_panels(parent$removed_block, parent$app_layout$panels)
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
                renderer = "always",
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
              renderer = "always",
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
  observeEvent(
    req(parent$append_block, is.null(parent$ai_chat)),
    {
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    }
  )

  # Update scoutbar action with snapshots taken in the serialise module
  observeEvent(
    {
      parent$backup_list
    },
    {
      # TBD: this isn't optimal. scoutbaR should
      # be able to allow to append/remove/modify actions
      # instead of having to re-create the whole list.
      location <- attr(
        get_board_option_or_default("snapshot", dag_board_options(), session),
        "location"
      )
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
                  path.expand(location)
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

#' @keywords internal
update_blk_code_ui <- function(blk) {
  observeEvent(
    {
      blk_expr <- try(blk$server$expr(), silent = TRUE)
      req(!inherits(blk_expr, "try-error"))
    },
    {
      blk_code <- paste(
        deparse(blk$server$expr()),
        collapse = "\n"
      )

      bslib::accordion_panel_update(
        id = paste0("accordion-", attr(blk, "uid")),
        target = "code",
        tagList(
          pre(code(class = "language-r", blk_code)),
          tags$script(HTML(
            "setTimeout(function() {
              if (typeof Prism !== 'undefined') {
                Prism.highlightAll();
              }
            }, 100);"
          ))
        )
      )
    }
  )
}

#' @keywords internal
update_blk_state_ui <- function(blk) {
  conds <- names(blk$server$cond)

  # Listen to blk$server$cond[["state"]], ...
  lapply(conds, \(nme) {
    observeEvent(blk$server$cond[[nme]], {
      cond <- blk$server$cond[[nme]]
      statuses <- lapply(names(cond), \(status) {
        cl <- switch(
          status,
          "error" = "danger",
          "warning" = "warning",
          "message" = "info"
        )

        msgs <- NULL
        if (length(cond[[status]])) {
          msgs <- tags$div(
            class = sprintf("alert alert-%s", cl),
            HTML(cli::ansi_html(paste(
              unlist(cond[[status]]),
              collapse = "\n"
            )))
          )
        }

        if (!is.null(msgs)) {
          nav_panel(
            class = "p-3",
            title = tagList(
              paste0(firstup(status), "(s)"),
              tags$span(
                class = sprintf(
                  "badge text-bg-%s",
                  cl
                ),
                length(unlist(cond[[status]]))
              )
            ),
            msgs,
          )
        }
      })

      bslib::accordion_panel_update(
        id = paste0("accordion-", attr(blk, "uid")),
        target = "state",
        bslib::navset_pill(!!!statuses)
      )
    })
  })
}

#' Update some pieces of the block UI
#'
#' Some elements of the block UI require server
#' elements to be available. This can't be done
#' from the board_ui as this one is done once.
#'
#' @keywords internal
#' @rdname handlers-utils
update_block_ui <- function(board, update, session, parent, ...) {
  input <- session$input
  ns <- session$ns

  # Register update block UI callbacks for existing blocks
  observeEvent(
    req(!parent$cold_start),
    {
      lapply(
        names(board$blocks),
        \(id) {
          blk <- board$blocks[[id]]
          attr(blk, "uid") <- id
          update_blk_code_ui(blk)
          update_blk_state_ui(blk)
        }
      )
    },
    once = TRUE
  )

  # Each time a block is added, we should register the observers above
  observeEvent(
    parent$added_block,
    {
      blk <- board$blocks[[block_uid(parent$added_block)]]
      attr(blk, "uid") <- block_uid(parent$added_block)
      update_blk_code_ui(blk)
      update_blk_state_ui(blk)
    },
    ignoreInit = TRUE
  )
}
