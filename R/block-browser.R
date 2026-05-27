#' Block browser
#'
#' A card-list block picker designed to live inside the sidebar
#' primitive. Replaces the legacy single-select form used for the add /
#' append / prepend block flows in `blockr.dock`. Each card represents a
#' registered block (one row of [blockr.core::available_blocks()]),
#' grouped by category and filterable with the search bar.
#'
#' Adding a block is single-shot and works two ways:
#'
#' * **Click the card body** to add the block immediately with sensible
#'   defaults (a freshly-generated unique id, the block's default title,
#'   and - for append / prepend - a default link and port).
#' * **Open the card** via its chevron to tweak the id, title, link id
#'   and input port first, then click the in-card add button.
#'
#' The block browser is a Shiny module. `block_browser_ui(id, ...)`
#' renders the panel; `block_browser_server(id)` returns a reactive that
#' fires once per add with the block to create:
#' `list(type, id, title, link_id, block_input, target_input)`. Fields
#' not applicable to the current mode are `NULL`. Because the suggested
#' ids are generated avoiding the board's existing block / link ids,
#' clicking the same card repeatedly (with a pinned sidebar) yields
#' distinct blocks.
#'
#' Internally the panel publishes its value through the
#' `blockr.ui.blockBrowser` `Shiny.InputBinding` (the root element's
#' `id` is `NS(id)("commit")`); the value carries an internal `nonce` so
#' repeat adds re-fire as events. `block_browser_server()` strips the
#' nonce and hands back the clean spec, so consumers never see it.
#'
#' @param id Module id. As usual for Shiny modules, pass `NS(id)("...")`
#'   from the parent at the `*_ui()` call site and the bare id to
#'   `*_server()`.
#' @param board The current board state. Used to look up the trigger
#'   block's input arity (for `mode = "prepend"`) and to seed unique
#'   default ids avoiding the board's existing block / link ids.
#' @param mode One of `"add"`, `"append"`, `"prepend"`. Controls which
#'   per-card fields are shown and what the commit spec carries.
#' @param trigger_id The block the new block attaches to: the source
#'   block for `mode = "append"`, the target block for `mode =
#'   "prepend"`. Drives the panel's context subtitle, and for `prepend`
#'   resolves the target's input arity / slot names (which populate the
#'   `target_input` picker). Ignored for `mode = "add"`.
#'
#' @return
#' * `block_browser_ui()` returns an [htmltools::tag] with
#'   [block_browser_dep()] attached.
#' * `block_browser_server()` returns a [shiny::reactive] of the
#'   committed block spec (a list), firing once per add.
#' * `block_browser_dep()` returns an [htmltools::htmlDependency].
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       sidebar_ui("panel", side = "right"),
#'       shiny::actionButton("open", "Open block browser")
#'     ),
#'     server = function(input, output, session) {
#'       added <- block_browser_server("browser")
#'       shiny::observeEvent(input$open, {
#'         show_sidebar(
#'           "panel",
#'           title = "Add new block",
#'           ui = block_browser_ui(session$ns("browser"), NULL, mode = "add")
#'         )
#'       })
#'       shiny::observeEvent(added(), print(added()))
#'     }
#'   )
#' }
#'
#' @name block-browser
NULL

#' @rdname block-browser
#' @export
block_browser_ui <- function(id, board, mode = c("add", "append", "prepend"),
                             trigger_id = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  mode <- match.arg(mode)
  if (!is.null(trigger_id)) {
    stopifnot(is.character(trigger_id), length(trigger_id) == 1L)
  }

  ns <- shiny::NS(id)
  commit_id <- ns("commit")
  registry <- blockr.core::available_blocks()

  # Target arity is only meaningful for prepend with a known target.
  # NA arity (variadic, e.g. rbind) is encoded as the string "inf".
  target_attrs <- list()
  target_inputs <- character()
  if (mode == "prepend" && !is.null(trigger_id) && !is.null(board)) {
    target_blk <- board_block(board, trigger_id)
    if (!is.null(target_blk)) {
      target_inputs <- blockr.core::block_inputs(target_blk)
      arity <- blockr.core::block_arity(target_blk)
      if (is.na(arity)) {
        target_attrs <- list(`data-target-arity` = "inf")
      } else if (is.numeric(arity) && length(arity) == 1L) {
        target_attrs <- list(`data-target-arity` = as.character(arity))
      }
    }
  }

  metas <- lapply(seq_along(registry), function(i) {
    entry <- registry[[i]]
    list(
      type = entry_attr(entry, "ctor_name", names(registry)[[i]]),
      name = entry_attr(entry, "name", names(registry)[[i]]),
      description = entry_attr(entry, "description", ""),
      category = entry_attr(entry, "category", ""),
      icon = entry_attr(entry, "icon", ""),
      package = entry_attr(entry, "package", "local"),
      ctor = entry
    )
  })

  for (i in seq_along(metas)) {
    metas[[i]]$inputs <- safe_block_inputs(metas[[i]]$ctor)
  }

  # Seed default ids / link ids so they are unique among the cards AND
  # against the board's existing ids. The caller re-renders the browser
  # with the updated board after each add, so successive adds keep
  # suggesting fresh ids.
  n <- length(metas)
  existing_blocks <- safe_board_ids(board, blockr.core::board_block_ids)
  existing_links <- safe_board_ids(board, blockr.core::board_link_ids)
  id_defaults <- if (n > 0L) {
    blockr.core::rand_names(old_names = existing_blocks, n = n)
  } else {
    character()
  }
  link_defaults <- if (n > 0L) {
    blockr.core::rand_names(old_names = existing_links, n = n)
  } else {
    character()
  }
  for (i in seq_along(metas)) {
    metas[[i]]$id_default <- id_defaults[[i]]
    metas[[i]]$link_default <- link_defaults[[i]]
  }

  categories <- split(
    metas,
    vapply(metas, meta_category, character(1L))
  )
  cat_order <- unique(vapply(metas, meta_category, character(1L)))
  uncat <- "Uncategorized" %in% cat_order
  cat_order <- c(setdiff(cat_order, "Uncategorized"),
                 if (uncat) "Uncategorized" else character())

  # The root element IS the Shiny input: its `id` is the commit input id
  # the `blockr.ui.blockBrowser` InputBinding reports against. The action
  # handler observes `input[[commit_id]]`.
  root_attrs <- c(
    list(
      id = commit_id,
      class = "blockr-block-browser",
      `data-mode` = mode
    ),
    target_attrs
  )

  panel <- do.call(
    shiny::tags$div,
    c(
      root_attrs,
      list(
        context_subtitle(mode, trigger_id, board),
        shiny::tags$input(
          type = "search",
          class = "blockr-block-browser-search",
          placeholder = "Search...",
          `aria-label` = "Search blocks"
        ),
        shiny::tags$div(
          class = "blockr-block-browser-categories",
          lapply(cat_order, function(cat) {
            category_section(cat, categories[[cat]], ns, mode, target_inputs)
          })
        ),
        shiny::tags$div(
          class = "blockr-block-browser-empty",
          "No blocks match your search."
        )
      )
    )
  )

  htmltools::attachDependencies(panel, list(block_browser_dep()))
}

#' @rdname block-browser
#' @export
block_browser_server <- function(id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # `input$commit` is the binding's value; the `nonce` it carries
      # makes every add a fresh event, so eventReactive fires once per
      # add. Strip the nonce before handing the spec to the caller.
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

#' @rdname block-browser
#' @export
block_browser_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-block-browser",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = "css/blockr-block-browser.css",
    script = "js/blockr-block-browser.js",
    all_files = FALSE
  )
}

# ---- helpers -----------------------------------------------------------

entry_attr <- function(entry, key, default) {
  val <- attr(entry, key, exact = TRUE)
  if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
}

meta_category <- function(m) {
  if (nzchar(m$category)) m$category else "Uncategorized"
}

# Construct one block instance with no args to read its input slot names.
# Returns character(0) on any error.
safe_block_inputs <- function(ctor) {
  tryCatch(
    {
      blk <- ctor()
      blockr.core::block_inputs(blk)
    },
    error = function(e) character()
  )
}

safe_board_ids <- function(board, getter) {
  if (is.null(board)) return(character())
  tryCatch(getter(board), error = function(e) character())
}

board_block <- function(board, id) {
  if (is.null(board)) return(NULL)
  blocks <- tryCatch(blockr.core::board_blocks(board), error = function(e) NULL)
  if (is.null(blocks)) return(NULL)
  blocks[[id]]
}

context_subtitle <- function(mode, trigger_id, board) {
  if (mode == "add" || is.null(trigger_id)) return(NULL)
  blk <- board_block(board, trigger_id)
  trigger_name <- if (!is.null(blk)) {
    nm <- tryCatch(blockr.core::block_name(blk), error = function(e) NULL)
    if (is.null(nm) || !nzchar(nm)) trigger_id else nm
  } else {
    trigger_id
  }
  verb <- if (mode == "append") "Append from" else "Prepend to"
  shiny::tags$p(
    class = "blockr-block-browser-context",
    verb, " ", shiny::tags$strong(trigger_name)
  )
}

category_section <- function(category, entries, ns, mode, target_inputs) {
  shiny::tags$div(
    class = "blockr-block-browser-category",
    `data-category` = category,
    shiny::tags$h3(category),
    shiny::tags$div(
      class = "blockr-block-browser-cards",
      lapply(entries, function(m) card(m, ns, mode, target_inputs))
    )
  )
}

card <- function(meta, ns, mode, target_inputs) {
  shiny::tags$div(
    class = "blockr-block-browser-card",
    `data-block-type` = meta$type,
    `data-name` = meta$name,
    `data-description` = meta$description,
    `data-package` = meta$package,
    `data-category` = meta_category(meta),
    shiny::tags$div(
      class = "blockr-block-browser-card-header",
      shiny::tags$span(
        class = "blockr-block-browser-card-icon",
        if (nzchar(meta$icon)) htmltools::HTML(meta$icon) else NULL
      ),
      shiny::tags$span(class = "blockr-block-browser-card-name", meta$name),
      shiny::tags$span(
        class = "blockr-block-browser-card-package", meta$package
      ),
      shiny::tags$button(
        type = "button",
        class = "blockr-block-browser-card-chevron",
        `aria-label` = "Configure before adding",
        chevron_icon()
      )
    ),
    shiny::tags$div(
      class = "blockr-block-browser-card-description",
      meta$description
    ),
    card_advanced(meta, ns, mode, target_inputs)
  )
}

card_advanced <- function(meta, ns, mode, target_inputs) {
  field_id <- function(suffix) ns(paste0(meta$type, "_", suffix))
  add_label <- switch(mode,
    add = "Add",
    append = "Append",
    prepend = "Prepend"
  )

  shiny::tags$div(
    class = "blockr-block-browser-card-advanced",
    field_text(
      class_suffix = "id",
      id = field_id("id"),
      label = "Block ID",
      value = meta$id_default
    ),
    field_text(
      class_suffix = "title",
      id = field_id("title"),
      label = "Block title",
      value = "",
      placeholder = meta$name
    ),
    field_text(
      class_suffix = "link-id",
      id = field_id("link_id"),
      label = "Link ID",
      value = meta$link_default
    ),
    field_select(
      class_suffix = "block-input",
      id = field_id("block_input"),
      label = "New block input port",
      options = meta$inputs
    ),
    field_select(
      class_suffix = "target-input",
      id = field_id("target_input"),
      label = "Target input port",
      options = target_inputs
    ),
    shiny::tags$button(
      type = "button",
      class = "blockr-block-browser-card-add",
      add_label
    )
  )
}

field_text <- function(class_suffix, id, label, value, placeholder = NULL) {
  shiny::tags$div(
    class = paste0(
      "blockr-block-browser-field blockr-block-browser-field-", class_suffix
    ),
    shiny::tags$label(`for` = id, label),
    shiny::tags$input(
      type = "text",
      id = id,
      value = value,
      placeholder = placeholder
    )
  )
}

field_select <- function(class_suffix, id, label, options) {
  shiny::tags$div(
    class = paste0(
      "blockr-block-browser-field blockr-block-browser-field-", class_suffix
    ),
    shiny::tags$label(`for` = id, label),
    shiny::tags$select(
      id = id,
      lapply(options, function(opt) {
        shiny::tags$option(value = opt, opt)
      })
    )
  )
}

chevron_icon <- function() {
  shiny::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    fill = "currentColor",
    `aria-hidden` = "true",
    shiny::tags$path(
      d = paste0(
        "M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1",
        " .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z"
      )
    )
  )
}
