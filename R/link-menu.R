#' Link menu
#'
#' A bidirectional card-list link picker. The link menu lives inside
#' the sidebar primitive and replaces the legacy selectize-style
#' chooser used by `blockr.dock`'s add-link flow. Each card represents
#' a board **instance** that the new link can attach to from either
#' direction relative to a fixed `anchor` block (the right-clicked
#' block in the dock flow).
#'
#' The link menu is a Shiny module. `link_menu_ui(id, board, anchor)`
#' renders the panel; `link_menu_server(id)` returns a reactive that
#' fires once per commit with the link spec:
#' `list(source, target, link_id, block_input)`. The JS binding
#' composes `source` / `target` from the clicked card's
#' `data-direction` and the panel's `data-anchor`, so the consumer
#' never has to know the orientation up front:
#'
#' * `data-direction = "outgoing"` -> `source = anchor`, `target = card`
#' * `data-direction = "incoming"` -> `source = card`,   `target = anchor`
#'
#' The panel renders up to two category-grouped sections, labelled
#' from the anchor's port-role perspective:
#'
#' * **OUTPUT TO** (anchor -> card): blocks the anchor can link to.
#'   Eligible = every other board block that has at least one free
#'   named input port or is variadic, excluding any that already
#'   reach the anchor (would close a cycle).
#' * **INPUT FROM** (card -> anchor): blocks that can link into the
#'   anchor. Only rendered when the anchor itself has at least one
#'   free named input port or is variadic. Eligible = every other
#'   board block, excluding any that the anchor already reaches.
#'
#' When both pools are empty the panel renders an in-place empty-state
#' instead of returning `NULL`.
#'
#' Internally the panel publishes its committed link spec through the
#' `blockr.ui.linkMenu` `Shiny.InputBinding` (the root element's `id`
#' is `NS(id)("commit")`). The binding's `receiveMessage` accepts a
#' `list(type = "pool-update", eligible = list(outgoing, incoming),
#' link_id_seed)` payload so consumers can keep the menu open across
#' multiple commits and push live eligibility updates without
#' re-rendering - the multi-link session pattern. Use
#' [link_eligible_pools()] to compute the post-commit pool on the
#' consumer side and send it via `session$sendInputMessage()` (see
#' `shiny::session`).
#'
#' @param id Module id. As usual for Shiny modules, pass `NS(id)("...")`
#'   from the parent at the `*_ui()` call site and the bare id to
#'   `*_server()`.
#' @param board The current board state.
#' @param anchor Block id the new link is anchored on (the
#'   right-clicked block in the dock flow). A non-empty character
#'   scalar; must be a member of [blockr.core::board_block_ids()].
#'
#' @return
#' * `link_menu_ui()` returns an [htmltools::tag] with
#'   [link_menu_dep()] attached.
#' * `link_menu_server()` returns a [shiny::reactive] of the committed
#'   link spec (a list), firing once per commit.
#' * `link_menu_dep()` returns an [htmltools::htmlDependency].
#' * `link_eligible_pools()` returns
#'   `list(outgoing = <chr>, incoming = <chr>)` using the same
#'   eligibility rules `link_menu_ui()` uses for its initial render.
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       sidebar_ui("panel", side = "right"),
#'       shiny::actionButton("open", "Open link menu")
#'     ),
#'     server = function(input, output, session) {
#'       board <- blockr.core::new_board(
#'         blockr.core::as_blocks(list(
#'           a = blockr.core::new_dataset_block(),
#'           b = blockr.core::new_head_block()
#'         ))
#'       )
#'       committed <- link_menu_server("menu")
#'       shiny::observeEvent(input$open, {
#'         show_sidebar(
#'           "panel", title = "Connect a",
#'           ui = link_menu_ui(session$ns("menu"), board, anchor = "a")
#'         )
#'       })
#'       shiny::observeEvent(committed(), print(committed()))
#'     }
#'   )
#' }
#'
#' @name link-menu
NULL

#' @rdname link-menu
#' @export
link_menu_ui <- function(id, board, anchor) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  # link_eligible_pools() validates `anchor`; the same check covers
  # link_menu_ui() so we don't double-validate.
  pools <- link_eligible_pools(board, anchor)

  ns <- shiny::NS(id)
  htmltools::attachDependencies(
    link_menu_panel(ns, board, anchor, pools),
    list(block_browser_dep(), link_menu_dep())
  )
}

#' @rdname link-menu
#' @export
link_menu_server <- function(id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::eventReactive(
        input$commit,
        {
          spec <- input$commit
          spec[["nonce"]] <- NULL
          spec
        },
        ignoreNULL = TRUE
      )
    }
  )
}

#' @rdname link-menu
#' @export
link_menu_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-link-menu",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = "css/blockr-link-menu.css",
    script = "js/blockr-link-menu.js",
    all_files = FALSE
  )
}

#' @rdname link-menu
#' @export
link_eligible_pools <- function(board, anchor) {
  stopifnot(is.character(anchor), length(anchor) == 1L, nzchar(anchor))
  validate_anchor(board, anchor)

  blocks <- blockr.core::board_blocks(board)
  links_df <- as.data.frame(blockr.core::board_links(board))

  outgoing <- compute_outgoing_targets(blocks, anchor, links_df)
  incoming <- compute_incoming_sources(blocks, anchor, links_df)

  # Per-target free named inputs, keyed by target block id. OUTGOING
  # cards target the card itself; INCOMING cards all target the
  # anchor. The pool-update push reads this map to refresh each
  # visible card's block-input <select> options after a commit.
  targets <- unique(c(outgoing, if (length(incoming)) anchor))
  free <- stats::setNames(
    lapply(targets, function(id) free_named_inputs(blocks[[id]], id, links_df)),
    targets
  )
  list(outgoing = outgoing, incoming = incoming, free_inputs = free)
}

# ---- eligibility -------------------------------------------------------

validate_anchor <- function(board, anchor) {
  ids <- if (is.null(board)) {
    character()
  } else {
    blockr.core::board_block_ids(board)
  }
  if (!(anchor %in% ids)) {
    rlang::abort(
      paste0(
        "No block with id ", encodeString(anchor, quote = "'"),
        " on the board."
      ),
      class = "blockr_ui_link_menu_unknown_anchor"
    )
  }
  invisible(anchor)
}

# Every non-anchor block with at least one free input port or variadic
# arity, EXCLUDING candidates whose addition as `anchor -> candidate`
# would close a cycle (candidate already reaches anchor through the
# existing link graph). blockr.core has no exported eligibility
# helper that fits, so we reimplement it locally on top of
# `block_inputs()`, `block_arity()`, and the links data frame.
compute_outgoing_targets <- function(blocks, anchor, links_df) {
  cycle_seed <- ancestors_of(anchor, links_df)
  ids <- setdiff(names(blocks), c(anchor, cycle_seed))
  keep <- vapply(
    ids,
    function(id) has_input_capacity(blocks[[id]], id, links_df),
    logical(1L)
  )
  ids[keep]
}

# Every non-anchor block, but ONLY when the anchor itself has a free
# input (or is variadic). EXCLUDES candidates that anchor already
# reaches (adding `candidate -> anchor` would close a cycle).
compute_incoming_sources <- function(blocks, anchor, links_df) {
  if (!has_input_capacity(blocks[[anchor]], anchor, links_df)) {
    return(character())
  }
  cycle_seed <- descendants_of(anchor, links_df)
  setdiff(names(blocks), c(anchor, cycle_seed))
}

# Block ids that reach `start` by following outgoing links (i.e. that
# already have a directed path INTO `start`). Adding `start -> X`
# would create a cycle iff `X` is in this set.
ancestors_of <- function(start, links_df) {
  walk_reachable(start, links_df, from_col = "to", to_col = "from")
}

# Block ids that `start` reaches by following outgoing links. Adding
# `X -> start` would create a cycle iff `X` is in this set.
descendants_of <- function(start, links_df) {
  walk_reachable(start, links_df, from_col = "from", to_col = "to")
}

walk_reachable <- function(start, links_df, from_col, to_col) {
  if (is.null(links_df) || !nrow(links_df) ||
        !all(c(from_col, to_col) %in% names(links_df))) {
    return(character())
  }
  from_vec <- as.character(links_df[[from_col]])
  to_vec <- as.character(links_df[[to_col]])
  visited <- character()
  frontier <- start
  while (length(frontier)) {
    next_nodes <- unique(to_vec[from_vec %in% frontier])
    next_nodes <- setdiff(next_nodes, c(visited, start))
    visited <- c(visited, next_nodes)
    frontier <- next_nodes
  }
  visited
}

# Can `blk` accept one more incoming link? TRUE for variadic; TRUE
# when there's at least one named input not already wired; FALSE
# otherwise.
has_input_capacity <- function(blk, blk_id, links_df) {
  if (is.null(blk)) return(FALSE)
  if (is.na(blockr.core::block_arity(blk))) return(TRUE)
  length(free_named_inputs(blk, blk_id, links_df)) > 0L
}

# Free named inputs = block_inputs(blk) minus the ports already wired
# by incoming links to `blk_id`. Variadic blocks return character() -
# they accept a fresh slot which the consumer generates server-side,
# so no port picker is shown for them.
free_named_inputs <- function(blk, blk_id, links_df) {
  if (is.null(blk)) return(character())
  if (is.na(blockr.core::block_arity(blk))) return(character())
  used <- if (is.null(links_df) || !nrow(links_df) ||
                !all(c("to", "input") %in% names(links_df))) {
    character()
  } else {
    as.character(links_df$input[links_df$to == blk_id])
  }
  setdiff(blockr.core::block_inputs(blk), used)
}

# ---- panel assembly ----------------------------------------------------

link_menu_panel <- function(ns, board, anchor, pools) {
  is_empty <- length(pools$outgoing) == 0L && length(pools$incoming) == 0L

  root_attrs <- list(
    id = ns("commit"),
    class = paste(
      "blockr-link-menu",
      if (is_empty) "is-empty" else ""
    ),
    `data-anchor` = anchor
  )

  do.call(
    shiny::tags$div,
    c(
      root_attrs,
      list(
        shiny::tags$input(
          type = "search",
          class = "blockr-block-browser-search",
          placeholder = "Search...",
          `aria-label` = "Search blocks"
        ),
        shiny::tags$div(
          class = "blockr-link-menu-directions",
          # Order follows the visual left-to-right data flow: sources
          # (INPUT FROM) feed the anchor, the anchor feeds targets
          # (OUTPUT TO). Showing INPUT FROM first reads more naturally.
          if (length(pools$incoming) > 0L) {
            direction_section(
              ns, board, anchor, pools$incoming,
              direction = "incoming", header = "Input from",
              free_inputs = pools$free_inputs
            )
          },
          if (length(pools$outgoing) > 0L) {
            direction_section(
              ns, board, anchor, pools$outgoing,
              direction = "outgoing", header = "Output to",
              free_inputs = pools$free_inputs
            )
          }
        ),
        shiny::tags$div(
          class = "blockr-block-browser-empty",
          paste0(
            "This block can't be linked: no other blocks have free ",
            "inputs, and the anchor itself has no free inputs either."
          )
        )
      )
    )
  )
}

direction_section <- function(ns, board, anchor, pool, direction, header,
                              free_inputs) {
  registry <- blockr.core::available_blocks()
  blocks <- blockr.core::board_blocks(board)

  metas <- lapply(pool, function(blk_id) {
    blk <- blocks[[blk_id]]
    entry <- registry_entry_for(blk, registry)
    label <- blockr.core::block_name(blk) %||% blk_id
    target_id <- if (direction == "outgoing") blk_id else anchor
    list(
      id = blk_id,
      name = if (nzchar(label)) label else blk_id,
      category = entry_attr(entry, "category", ""),
      icon = entry_attr(entry, "icon", ""),
      package = entry_attr(entry, "package", "local"),
      description = entry_attr(entry, "description", ""),
      target_id = target_id,
      target_inputs = free_inputs[[target_id]] %||% character()
    )
  })

  groups <- category_groups(metas)

  shiny::tags$div(
    class = "blockr-link-menu-direction",
    `data-direction` = direction,
    shiny::tags$h4(class = "blockr-link-menu-section-header", header),
    shiny::tags$div(
      class = "blockr-block-browser-categories",
      lapply(names(groups), function(cat) {
        category_section(
          cat, groups[[cat]],
          function(m) link_block_card(m, ns, board, direction)
        )
      })
    )
  )
}

link_block_card <- function(meta, ns, board, direction) {
  shiny::tags$div(
    class = "blockr-block-browser-card blockr-link-menu-card",
    `data-block-type` = meta$id,
    `data-direction` = direction,
    `data-name` = paste(meta$name, meta$id),
    `data-description` = meta$description,
    `data-package` = meta$package,
    `data-category` = meta_category(meta),
    shiny::tags$div(
      class = "blockr-block-browser-card-header",
      shiny::tags$span(
        class = "blockr-block-browser-card-icon",
        if (nzchar(meta$icon)) htmltools::HTML(meta$icon) else NULL
      ),
      shiny::tags$div(
        class = "blockr-link-menu-card-titles",
        shiny::tags$span(
          class = "blockr-block-browser-card-name",
          meta$name
        ),
        shiny::tags$span(
          class = "blockr-link-menu-card-id",
          paste0("id: ", meta$id)
        )
      ),
      shiny::tags$button(
        type = "button",
        class = "blockr-block-browser-card-chevron",
        `aria-label` = "Configure before adding",
        chevron_icon()
      )
    ),
    link_card_advanced(meta, ns, board)
  )
}

# Per-card advanced form: link_id always; block_input only when the
# target end has finite arity > 1 free slots.
link_card_advanced <- function(meta, ns, board) {
  card_ns <- function(suffix) ns(paste0("card_", meta$id, "_", suffix))
  link_id_default <- seed_link_id(board)
  show_port <- length(meta$target_inputs) > 1L

  shiny::tags$div(
    class = "blockr-block-browser-card-advanced",
    field_text(
      class_suffix = "link-id",
      id = card_ns("link_id"),
      label = "Link ID",
      value = link_id_default
    ),
    if (show_port) {
      field_select(
        class_suffix = "block-input",
        id = card_ns("block_input"),
        label = "Block input port",
        options = meta$target_inputs
      )
    },
    shiny::tags$button(
      type = "button",
      class = "blockr-block-browser-card-add",
      "Add link"
    )
  )
}

# ---- helpers -----------------------------------------------------------

# `field_text()`, `field_select()`, and `chevron_icon()` are defined
# in `block-browser.R` and reused here via package scope. The link
# menu shares the `.blockr-block-browser-field-*` class space with
# the block browser, so the markup is identical.

seed_link_id <- function(board) {
  out <- seed_ids(blockr.core::board_link_ids(board), 1L)
  if (length(out) == 0L) "" else out
}
