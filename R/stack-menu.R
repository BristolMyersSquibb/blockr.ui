#' Stack menu
#'
#' A multi-select card-list block picker for stacks. The stack menu
#' lives inside the sidebar primitive and replaces the legacy
#' selectize-style chooser used by `blockr.dock`'s add / edit stack
#' flows. Each card represents a board **instance** (a block already on
#' the board); clicking a card toggles its membership in the new stack.
#' A small panel-level form below the cards carries the stack-level
#' parameters (name, color, id).
#'
#' The stack menu is a Shiny module. `stack_menu_ui(id, board, target)`
#' renders the panel; `stack_menu_server(id)` returns a reactive that
#' fires once per confirm with the committed spec:
#' `list(blocks, name, color, id)`. In edit mode (`target = "<stack_id>"`)
#' the spec carries the new selection / name / color and `id = NULL`
#' (the stack id is immutable once a stack exists).
#'
#' The flow is selected by `target`: `NULL` (default) is the *create*
#' flow; a character scalar names the stack being edited. In edit mode
#' the function pre-selects the stack's current blocks, augments the
#' eligible pool with them (so the user can deselect them), and
#' pre-fills the name / color fields. An unknown id raises a clean
#' error.
#'
#' Internally the panel publishes the selection set through the
#' `blockr.ui.stackMenu` `Shiny.InputBinding` (the root element's `id`
#' is `NS(id)("commit")`); the published value carries an internal
#' `nonce` so repeat confirms re-fire as events. The panel-level
#' name / color / id fields are normal Shiny inputs the server reads
#' directly; `stack_menu_server()` strips the nonce and composes the
#' full spec.
#'
#' @param id Module id. As usual for Shiny modules, pass `NS(id)("...")`
#'   from the parent at the `*_ui()` call site and the bare id to
#'   `*_server()`.
#' @param board The current board state. Used to resolve the eligible
#'   pool of board blocks and (in edit mode) to look the target stack
#'   up.
#' @param target Stack to edit. `NULL` (default) selects the *create*
#'   flow; a non-empty character scalar selects the *edit* flow and
#'   names the stack being edited. The parameter name mirrors
#'   [block_browser_ui()].
#'
#' @return
#' * `stack_menu_ui()` returns an [htmltools::tag] with
#'   [stack_menu_dep()] attached.
#' * `stack_menu_server()` returns a [shiny::reactive] of the
#'   committed stack spec (a list), firing once per confirm.
#' * `stack_menu_dep()` returns an [htmltools::htmlDependency].
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       sidebar_ui("panel", side = "right"),
#'       shiny::actionButton("open", "Open stack menu")
#'     ),
#'     server = function(input, output, session) {
#'       committed <- stack_menu_server("menu")
#'       shiny::observeEvent(input$open, {
#'         show_sidebar(
#'           "panel",
#'           title = "New stack",
#'           ui = stack_menu_ui(session$ns("menu"), NULL)
#'         )
#'       })
#'       shiny::observeEvent(committed(), print(committed()))
#'     }
#'   )
#' }
#'
#' @name stack-menu
NULL

#' @rdname stack-menu
#' @export
stack_menu_ui <- function(id, board, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  stopifnot(
    is.null(target) ||
      (is.character(target) && length(target) == 1L && nzchar(target))
  )

  ns <- shiny::NS(id)
  ctx <- resolve_stack_target(board, target)
  metas <- stack_menu_block_metas(board, ctx$pool, ctx$selected)

  htmltools::attachDependencies(
    stack_menu_panel(ns, metas, ctx),
    list(block_browser_dep(), stack_menu_dep())
  )
}

#' @rdname stack-menu
#' @export
stack_menu_server <- function(id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::eventReactive(
        input$commit,
        {
          list(
            blocks = input$commit$blocks %||% character(),
            name = input[["stack_name"]],
            color = input[["stack_color"]],
            id = input[["stack_id"]]
          )
        },
        ignoreNULL = TRUE
      )
    }
  )
}

#' @rdname stack-menu
#' @export
stack_menu_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-stack-menu",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = "css/blockr-stack-menu.css",
    script = "js/blockr-stack-menu.js",
    all_files = FALSE
  )
}

# ---- target resolution ------------------------------------------------

# Resolve everything the panel needs to know about the create / edit
# context in one place: the pool of eligible block ids, the pre-selected
# subset, the pre-filled name / color, and the seeded stack id default.
# The caller (a sidebar or modal) owns its own title chrome; the menu
# does not render context labels.
resolve_stack_target <- function(board, target) {
  pool <- stack_eligible_blocks(board)

  if (is.null(target)) {
    return(list(
      mode = "create",
      pool = pool,
      selected = character(),
      name = seed_stack_name(board),
      color = default_stack_color(),
      stack_id = seed_stack_id(board)
    ))
  }

  stack <- lookup_stack(board, target)
  selected <- as.character(blockr.core::stack_blocks(stack))

  list(
    mode = "edit",
    # Augment the pool with the stack's own blocks so the user can
    # deselect them; otherwise they'd be invisible (not in available).
    pool = union(pool, selected),
    selected = selected,
    name = blockr.core::stack_name(stack) %||% "",
    color = attr(stack, "color", exact = TRUE) %||% default_stack_color(),
    stack_id = NULL
  )
}

lookup_stack <- function(board, target) {
  stacks <- blockr.core::board_stacks(board)
  if (!(target %in% names(stacks))) {
    rlang::abort(
      paste0(
        "No stack with id ", encodeString(target, quote = "'"),
        " on the board."
      ),
      class = "blockr_ui_stack_menu_unknown_target"
    )
  }
  stacks[[target]]
}

# ---- per-block metadata -----------------------------------------------

# Build one card meta per board block in `pool`. The card's
# `data-block-type` carries the board block id (repurposed). Visual
# metadata (category / icon / package / description) is pulled from
# the matching registry entry; the card label uses the block's
# user-visible name so "Dataset 1" / "Dataset 2" read correctly.
stack_menu_block_metas <- function(board, pool, selected) {
  if (length(pool) == 0L) return(list())

  blocks <- if (is.null(board)) list() else blockr.core::board_blocks(board)
  registry <- blockr.core::available_blocks()

  lapply(pool, function(blk_id) {
    blk <- blocks[[blk_id]]
    entry <- registry_entry_for(blk, registry)
    label <- blockr.core::block_name(blk) %||% blk_id
    list(
      type = blk_id,
      name = if (nzchar(label)) label else blk_id,
      description = entry_attr(entry, "description", ""),
      category = entry_attr(entry, "category", ""),
      icon = entry_attr(entry, "icon", ""),
      package = entry_attr(entry, "package", "local"),
      selected = blk_id %in% selected
    )
  })
}

registry_entry_for <- function(blk, registry) {
  if (is.null(blk) || length(registry) == 0L) return(NULL)
  uid <- blockr.core::registry_id_from_block(blk)
  if (length(uid)) registry[[uid]] else NULL
}

# ---- panel assembly ---------------------------------------------------

stack_menu_panel <- function(ns, metas, ctx) {
  groups <- category_groups(metas)
  is_edit <- identical(ctx$mode, "edit")

  shiny::tags$div(
    id = ns("commit"),
    class = "blockr-stack-menu",
    `data-mode` = ctx$mode,
    shiny::tags$h4(
      class = "blockr-stack-menu-section-header",
      "Stack blocks"
    ),
    shiny::tags$input(
      type = "search",
      class = "blockr-block-browser-search",
      placeholder = "Search...",
      `aria-label` = "Search blocks"
    ),
    shiny::tags$div(
      class = "blockr-block-browser-categories",
      lapply(names(groups), function(cat) {
        category_section(cat, groups[[cat]], stack_block_card)
      })
    ),
    shiny::tags$div(
      class = "blockr-block-browser-empty",
      "No blocks match your search."
    ),
    stack_menu_form(ns, ctx, is_edit),
    shiny::tags$button(
      type = "button",
      class = "blockr-stack-menu-confirm",
      if (is_edit) "Update stack" else "Create stack"
    )
  )
}

stack_block_card <- function(meta) {
  card_classes <- "blockr-block-browser-card blockr-stack-menu-card"
  if (isTRUE(meta$selected)) {
    card_classes <- paste(card_classes, "card-selected")
  }
  # The data-name haystack still includes the block id so search hits
  # "src" or "head1" too; data-description / data-package are kept on
  # the element so the shared block-browser search code works, but
  # they are NOT rendered as visible chrome.
  card_attrs <- list(
    class = card_classes,
    `data-block-type` = meta$type,
    `data-name` = paste(meta$name, meta$type),
    `data-description` = meta$description,
    `data-package` = meta$package,
    `data-category` = meta_category(meta)
  )
  if (isTRUE(meta$selected)) {
    card_attrs$`data-selected` <- "true"
  }

  do.call(
    shiny::tags$div,
    c(
      card_attrs,
      list(
        shiny::tags$div(
          class = "blockr-block-browser-card-header",
          shiny::tags$span(
            class = "blockr-block-browser-card-icon",
            if (nzchar(meta$icon)) htmltools::HTML(meta$icon) else NULL
          ),
          shiny::tags$div(
            class = "blockr-stack-menu-card-titles",
            shiny::tags$span(
              class = "blockr-block-browser-card-name",
              meta$name
            ),
            shiny::tags$span(
              class = "blockr-stack-menu-card-id",
              paste0("id: ", meta$type)
            )
          )
        )
      )
    )
  )
}

# Panel-level form. All three fields (name, colour, id) are visible
# top-level in create mode - the sidebar has room for them and the
# auto-seeded id is still useful chrome (users can rename it). Edit
# mode omits the id input entirely (the id is immutable once a stack
# is created). The form is preceded by a small uppercase section
# header (matching the category labels above) and rendered inside a
# subtly-tinted panel so the configuration block reads as distinct
# from the picker.
stack_menu_form <- function(ns, ctx, is_edit) {
  shiny::tagList(
    shiny::tags$h4(
      class = "blockr-stack-menu-section-header",
      "Stack settings"
    ),
    shiny::tags$div(
      class = "blockr-stack-menu-form",
      text_field_tag(ns, "stack_name", "Stack name", ctx$name, "My stack"),
      color_field_tag(ns, ctx$color),
      if (!is_edit) {
        text_field_tag(ns, "stack_id", "Stack id", ctx$stack_id, "stack_id")
      }
    )
  )
}

text_field_tag <- function(ns, key, label, value, placeholder) {
  shiny::tags$div(
    class = "blockr-stack-menu-field",
    shiny::tags$label(`for` = ns(key), label),
    shiny::tags$input(
      id = ns(key),
      type = "text",
      value = value %||% "",
      placeholder = placeholder
    )
  )
}

color_field_tag <- function(ns, value) {
  hex <- value %||% default_stack_color()
  shiny::tags$div(
    class = "blockr-stack-menu-field blockr-stack-menu-color",
    shiny::tags$label(`for` = ns("stack_color"), "Stack color"),
    shiny::tags$div(
      class = "blockr-stack-menu-color-preview",
      shiny::tags$span(class = "blockr-stack-menu-color-swatch"),
      shiny::tags$input(
        id = ns("stack_color"),
        type = "text",
        class = "blockr-stack-menu-hex",
        value = hex,
        spellcheck = "false",
        autocomplete = "off",
        `aria-label` = "Hex colour value"
      )
    ),
    shiny::tags$input(
      type = "range",
      class = "blockr-stack-menu-hue",
      min = "0",
      max = "360",
      step = "1",
      value = "0",
      `aria-label` = "Hue"
    ),
    shiny::tags$input(
      type = "range",
      class = "blockr-stack-menu-lightness",
      min = "20",
      max = "85",
      step = "1",
      value = "60",
      `aria-label` = "Lightness"
    )
  )
}

# ---- small helpers ----------------------------------------------------

# Seed colour for the create flow. The dock can override `stack_color`
# via `updateTextInput()` after the panel is shown if it wants a
# smarter starting point.
default_stack_color <- function() {
  "#66c2a5"
}

seed_stack_id <- function(board) {
  seed_ids(safe_board_ids(board, blockr.core::board_stack_ids), 1L)
}

# A unique, human-readable default name for the create flow. Uses the
# same `rand_names()` generator as the seeded id (adjective_animal, e.g.
# "savoury_midge"), kept distinct from the board's existing stack names
# so it reads as a real, submittable value out of the box.
seed_stack_name <- function(board) {
  existing <- if (is.null(board)) {
    character()
  } else {
    vapply(
      blockr.core::board_stacks(board),
      function(s) blockr.core::stack_name(s) %||% "",
      character(1L),
      USE.NAMES = FALSE
    )
  }
  seed_ids(existing, 1L)
}

# Block ids on the board that are not currently a member of any stack.
# `blockr.core::available_stack_blocks()` computes exactly this when seeded
# with the board's block ids (its default seeds with stack ids instead).
stack_eligible_blocks <- function(board) {
  if (is.null(board)) return(character())
  blockr.core::available_stack_blocks(
    board,
    blocks = blockr.core::board_block_ids(board)
  )
}
