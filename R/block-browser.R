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
#' not applicable to the current flow are `NULL`. Because the suggested
#' ids are generated avoiding the board's existing block / link ids,
#' clicking the same card repeatedly (with a pinned sidebar) yields
#' distinct blocks.
#'
#' The flow is selected by the `target` argument: `NULL` (default) adds
#' a standalone block, [append_to()] appends from a source block, and
#' [prepend_to()] prepends into a target block. Because the append /
#' prepend constructors always carry the block they attach to, the flow
#' and its trigger can never disagree.
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
#' @param board The current board state. Used to resolve the target
#'   block's input arity (for prepend) and to seed unique default ids
#'   avoiding the board's existing block / link ids.
#' @param target Where the new block attaches. `NULL` (default) is a
#'   plain add; [append_to()] appends from a source block;
#'   [prepend_to()] prepends into a target block.
#' @param block_id Block id the new block attaches to: a source block
#'   for [append_to()], a target block for [prepend_to()].
#'
#' @return
#' * `block_browser_ui()` returns an [htmltools::tag] with
#'   [block_browser_dep()] attached.
#' * `block_browser_server()` returns a [shiny::reactive] of the
#'   committed block spec (a list), firing once per add.
#' * `append_to()` / `prepend_to()` return a `bb_target` descriptor.
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
#'           ui = block_browser_ui(session$ns("browser"), NULL)
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
block_browser_ui <- function(id, board, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  stopifnot(is.null(target) || inherits(target, "bb_target"))

  ns <- shiny::NS(id)
  mode <- target_mode(target)
  metas <- browser_block_metas(board, mode)
  tgt <- resolve_target(board, target)

  htmltools::attachDependencies(
    browser_panel(ns, metas, mode, tgt),
    list(block_browser_dep())
  )
}

#' @rdname block-browser
#' @export
append_to <- function(block_id) {
  new_bb_target("append", block_id)
}

#' @rdname block-browser
#' @export
prepend_to <- function(block_id) {
  new_bb_target("prepend", block_id)
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

# ---- target descriptor -------------------------------------------------

new_bb_target <- function(mode, block_id) {
  stopifnot(is.character(block_id), length(block_id) == 1L, nzchar(block_id))
  structure(
    list(mode = mode, id = block_id),
    class = c(paste0("bb_target_", mode), "bb_target")
  )
}

# The resulting flow: "add" when there's no target, else the target's
# own mode. The rest of the module branches on this string.
target_mode <- function(target) {
  if (is.null(target)) "add" else target$mode
}

# ---- panel assembly ----------------------------------------------------

# Build the per-block metadata list: registry rows plus the seeded
# unique default ids. Link ids and block-input slots are only computed
# for the flows that render those fields (each `safe_block_inputs()`
# instantiates a block, so we skip the work otherwise).
browser_block_metas <- function(board, mode) {
  registry <- blockr.core::available_blocks()
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

  n <- length(metas)
  need_link <- mode %in% c("append", "prepend")
  need_inputs <- mode == "append"

  id_defaults <- seed_ids(
    safe_board_ids(board, blockr.core::board_block_ids), n
  )
  link_defaults <- if (need_link) {
    seed_ids(safe_board_ids(board, blockr.core::board_link_ids), n)
  } else {
    rep("", n)
  }

  for (i in seq_along(metas)) {
    metas[[i]]$inputs <- if (need_inputs) {
      safe_block_inputs(metas[[i]]$ctor)
    } else {
      character()
    }
    metas[[i]]$id_default <- id_defaults[[i]]
    metas[[i]]$link_default <- link_defaults[[i]]
  }

  metas
}

# Everything the panel needs to know about the trigger block, in one
# place: the context subtitle, the target's input slot names, and the
# `data-target-arity` attribute (prepend only).
resolve_target <- function(board, target) {
  if (is.null(target)) {
    return(list(subtitle = NULL, inputs = character(), attrs = list()))
  }

  blk <- board_block(board, target$id)
  trigger_name <- if (!is.null(blk)) {
    nm <- tryCatch(blockr.core::block_name(blk), error = function(e) NULL)
    if (is.null(nm) || !nzchar(nm)) target$id else nm
  } else {
    target$id
  }
  verb <- if (target$mode == "append") "Append from" else "Prepend to"
  subtitle <- shiny::tags$p(
    class = "blockr-block-browser-context",
    verb, " ", shiny::tags$strong(trigger_name)
  )

  inputs <- character()
  attrs <- list()
  if (target$mode == "prepend" && !is.null(blk)) {
    inputs <- blockr.core::block_inputs(blk)
    arity <- blockr.core::block_arity(blk)
    if (is.na(arity)) {
      attrs <- list(`data-target-arity` = "inf")
    } else if (is.numeric(arity) && length(arity) == 1L) {
      attrs <- list(`data-target-arity` = as.character(arity))
    }
  }

  list(subtitle = subtitle, inputs = inputs, attrs = attrs)
}

browser_panel <- function(ns, metas, mode, tgt) {
  categories <- split(metas, vapply(metas, meta_category, character(1L)))
  cat_order <- unique(vapply(metas, meta_category, character(1L)))
  uncat <- "Uncategorized" %in% cat_order
  cat_order <- c(setdiff(cat_order, "Uncategorized"),
                 if (uncat) "Uncategorized" else character())

  # The root element IS the Shiny input: its `id` is the commit input id
  # the `blockr.ui.blockBrowser` InputBinding reports against, read by
  # `block_browser_server()` as `input$commit`.
  root_attrs <- c(
    list(
      id = ns("commit"),
      class = "blockr-block-browser",
      `data-mode` = mode
    ),
    tgt$attrs
  )

  do.call(
    shiny::tags$div,
    c(
      root_attrs,
      list(
        tgt$subtitle,
        shiny::tags$input(
          type = "search",
          class = "blockr-block-browser-search",
          placeholder = "Search...",
          `aria-label` = "Search blocks"
        ),
        shiny::tags$div(
          class = "blockr-block-browser-categories",
          lapply(cat_order, function(cat) {
            category_section(cat, categories[[cat]], ns, mode, tgt$inputs)
          })
        ),
        shiny::tags$div(
          class = "blockr-block-browser-empty",
          "No blocks match your search."
        )
      )
    )
  )
}

category_section <- function(category, entries, ns, mode, target_inputs) {
  shiny::tags$div(
    class = "blockr-block-browser-category",
    `data-category` = category,
    shiny::tags$h3(category),
    shiny::tags$div(
      class = "blockr-block-browser-cards",
      lapply(entries, function(m) block_card(m, ns, mode, target_inputs))
    )
  )
}

block_card <- function(meta, ns, mode, target_inputs) {
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

# Render only the fields the flow needs (mode is fixed per render, so
# there is no need to render every field and hide the inapplicable ones
# via CSS). `htmltools` drops the NULL children for the omitted fields.
card_advanced <- function(meta, ns, mode, target_inputs) {
  field_id <- function(suffix) ns(paste0(meta$type, "_", suffix))
  show_link <- mode %in% c("append", "prepend")
  show_block_input <- mode == "append"
  show_target_input <- mode == "prepend" && length(target_inputs) > 1L
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
    if (show_link) {
      field_text(
        class_suffix = "link-id",
        id = field_id("link_id"),
        label = "Link ID",
        value = meta$link_default
      )
    },
    if (show_block_input) {
      field_select(
        class_suffix = "block-input",
        id = field_id("block_input"),
        label = "New block input port",
        options = meta$inputs
      )
    },
    if (show_target_input) {
      field_select(
        class_suffix = "target-input",
        id = field_id("target_input"),
        label = "Target input port",
        options = target_inputs
      )
    },
    shiny::tags$button(
      type = "button",
      class = "blockr-block-browser-card-add",
      add_label
    )
  )
}

field_wrapper <- function(class_suffix, id, label, control) {
  shiny::tags$div(
    class = paste0(
      "blockr-block-browser-field blockr-block-browser-field-", class_suffix
    ),
    shiny::tags$label(`for` = id, label),
    control
  )
}

field_text <- function(class_suffix, id, label, value, placeholder = NULL) {
  field_wrapper(
    class_suffix, id, label,
    shiny::tags$input(
      type = "text",
      id = id,
      value = value,
      placeholder = placeholder
    )
  )
}

field_select <- function(class_suffix, id, label, options) {
  field_wrapper(
    class_suffix, id, label,
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

# ---- small helpers -----------------------------------------------------

entry_attr <- function(entry, key, default) {
  val <- attr(entry, key, exact = TRUE)
  if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
}

meta_category <- function(m) {
  if (nzchar(m$category)) m$category else "Uncategorized"
}

# n unique ids avoiding `existing` (and each other); empty for n == 0.
seed_ids <- function(existing, n) {
  if (n > 0L) {
    blockr.core::rand_names(old_names = existing, n = n)
  } else {
    character()
  }
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
