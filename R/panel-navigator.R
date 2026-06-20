#' Panel navigator sidebar
#'
#' A sidebar that lists every panel on a board, grouped by stack, with a
#' per-entry eye toggle reflecting whether the panel is on the current view
#' and tags for the other views it appears on. It mirrors the visual language
#' of the [block-browser] (same card chrome and CSS), but lists *existing*
#' panels rather than block constructors: clicking an entry's eye toggles its
#' membership in the current view.
#'
#' Unlike the block browser, the navigator's content depends on live board /
#' dock state, so its body is rendered per open by the consumer (blockr.dock)
#' and passed to [show_sidebar()]. The consumer assembles the `model`.
#'
#' @param id Module id.
#' @param model A list of groups. Each group is
#'   `list(name=, color=, kind=, entries=)` where `kind` is one of
#'   `"stack"`, `"ungrouped"`, `"extensions"`, and each entry is
#'   `list(id=, type=, title=, subtitle=, icon=, package=, color=,
#'   on_view=, tags=)`.
#'
#' @return `panel_navigator_ui()` returns a [shiny::tag] with the navigator
#'   dependency attached. `panel_navigator_server()` returns a
#'   [shiny::reactive] firing `list(id=, type=, to=)` on each eye toggle,
#'   where `to` is `"add"` or `"remove"`.
#'
#' @name panel-navigator
NULL

#' @rdname panel-navigator
#' @export
panel_navigator_ui <- function(id, model = list()) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  ns <- shiny::NS(id)

  body <- shiny::tags$div(
    id = ns("toggle"),
    class = "blockr-panel-navigator",
    shiny::tags$input(
      type = "search",
      class = "blockr-panel-nav-search",
      placeholder = "Search blocks...",
      `aria-label` = "Search blocks",
      # Suppress browser autofill (Safari otherwise offers its "AutoFill"
      # contact card on these fields).
      autocomplete = "off",
      autocorrect = "off",
      autocapitalize = "off",
      spellcheck = "false"
    ),
    shiny::tags$div(
      class = "blockr-panel-nav-body",
      lapply(model, nav_group_section)
    ),
    shiny::tags$div(
      class = "blockr-panel-nav-empty",
      "No blocks match your search."
    ),
    nav_add_stack_control()
  )

  htmltools::attachDependencies(
    body,
    list(panel_navigator_dep())
  )
}

#' @param board Reactive returning the current board object. When supplied
#'   with `update`, the navigator applies the pure-blockr.core events itself
#'   (stack reassign / reorder, block / stack rename) — the same division of
#'   labour as `stack_menu_server()`. May be `NULL` for the legacy
#'   "surface every event to the caller" behaviour.
#' @param update Board-update callback for the core events above.
#' @rdname panel-navigator
#' @export
panel_navigator_server <- function(id, board = NULL, update = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  board_fn <- if (is.null(board)) {
    function() NULL
  } else if (is.function(board)) {
    board
  } else {
    function() board
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      # `input$toggle` carries a nonce so each interaction re-fires. It is
      # multiplexed by `kind`. The navigator owns the pure-blockr.core
      # events and applies them here via `update`: "assign" (move / reorder
      # a block between stacks), "rename", "rename_stack". The dock-specific
      # ones — "toggle" / "focus" (panel show / hide / reveal) and
      # "add_stack" (needs the dock's coloured stack constructor) — are
      # surfaced through the returned reactive for the consumer to apply.
      if (is.function(update)) {
        shiny::observeEvent(input$toggle, {
          panel_nav_apply_core(input$toggle, board_fn(), update)
        })
      }

      shiny::eventReactive(
        input$toggle,
        {
          ev <- input$toggle
          ev[["nonce"]] <- NULL
          ev
        },
        ignoreNULL = TRUE
      )
    }
  )
}

# Apply the pure-blockr.core navigator events (no dock dependency): stack
# reassign / reorder, or block / stack rename. Dock events are left to the
# consumer. `board` is the current board object.
panel_nav_apply_core <- function(ev, board, update) {
  kind <- ev$kind %||% "toggle"

  if (identical(kind, "assign")) {
    payload <- nav_reassign_payload(board, ev$id, ev$stack %||% "", ev$before)
    if (!is.null(payload)) {
      update(payload)
    }
    return(invisible())
  }

  if (identical(kind, "rename")) {
    if (nzchar(ev$name %||% "") && nzchar(ev$id %||% "")) {
      update(list(
        blocks = list(
          mod = stats::setNames(list(list(block_name = ev$name)), ev$id)
        )
      ))
    }
    return(invisible())
  }

  if (identical(kind, "rename_stack")) {
    if (nzchar(ev$stack %||% "") && nzchar(ev$name %||% "")) {
      update(list(
        stacks = list(
          mod = stats::setNames(list(list(name = ev$name)), ev$stack)
        )
      ))
    }
    return(invisible())
  }

  invisible()
}

# Build the board-update payload for a grip drag: move `block_id` into stack
# `target` ("" = ungrouped) before block `before` (or append), or reorder
# within a stack. Pure blockr.core; the deltas carry only `blocks`, so stack
# name / colour are preserved. Returns `NULL` for a no-op.
nav_reassign_payload <- function(board, block_id, target, before = NULL) {
  all_stacks <- blockr.core::board_stacks(board)

  cur <- NULL
  for (sid in names(all_stacks)) {
    blks <- as.character(blockr.core::stack_blocks(all_stacks[[sid]]))
    if (block_id %in% blks) {
      cur <- sid
      break
    }
  }

  target <- if (identical(target, "")) NULL else target
  before <- if (length(before) && nzchar(before)) before else NULL

  insert_before <- function(vec, id, before) {
    vec <- setdiff(vec, id)
    if (is.null(before) || !before %in% vec) {
      c(vec, id)
    } else {
      append(vec, id, after = match(before, vec) - 1L)
    }
  }

  if (identical(cur, target)) {
    if (is.null(target)) {
      return(NULL)
    }
    blks <- as.character(blockr.core::stack_blocks(all_stacks[[target]]))
    new_blks <- insert_before(blks, block_id, before)
    if (identical(new_blks, blks)) {
      return(NULL)
    }
    return(list(
      stacks = list(mod = stats::setNames(list(list(blocks = new_blks)), target))
    ))
  }

  mod <- list()
  if (!is.null(cur)) {
    mod[[cur]] <- list(
      blocks = setdiff(
        as.character(blockr.core::stack_blocks(all_stacks[[cur]])), block_id
      )
    )
  }
  if (!is.null(target)) {
    mod[[target]] <- list(
      blocks = insert_before(
        as.character(blockr.core::stack_blocks(all_stacks[[target]])),
        block_id, before
      )
    )
  }

  list(stacks = list(mod = mod))
}

#' Shape the grouped navigator model from raw pieces.
#'
#' The dock-specific data adapter (in blockr.dock) gathers per-entry display
#' metadata plus a few live-dock facts and hands them here; this builds the
#' grouped model `panel_navigator_ui()` renders — grouping blocks by stack,
#' flagging current-view visibility, and tagging other views. Pure
#' blockr.core, so the shaping lives next to the UI.
#'
#' @param entries Named-by-id list of per-entry metadata, each
#'   `list(type=, title=, subtitle=, icon=, color=, category=, package=)`.
#' @param stacks Ordered list of `list(id=, name=, color=, block_ids=)`.
#' @param ungrouped Character vector of block ids in no stack.
#' @param extensions Character vector of extension ids.
#' @param visible Character vector of currently-visible object ids.
#' @param views Named (by view id) list of object-id vectors per view.
#' @param view_labels Named character vector: view id -> display label.
#' @param active Active view id.
#'
#' @return A list of groups for [panel_navigator_ui()].
#' @export
panel_nav_model <- function(entries, stacks = list(),
                            ungrouped = character(), extensions = character(),
                            visible = character(), views = list(),
                            view_labels = character(), active = NULL) {
  visible <- as.character(visible)
  others <- setdiff(names(views), active)

  tags_for <- function(id) {
    hit <- others[
      vapply(others, function(v) id %in% views[[v]], logical(1L))
    ]
    unname(view_labels[hit])
  }

  entry_for <- function(id) {
    e <- entries[[id]]
    if (is.null(e)) {
      return(NULL)
    }
    c(e, list(id = id, on_view = id %in% visible, tags = tags_for(id)))
  }
  entries_for <- function(ids) {
    Filter(Negate(is.null), lapply(ids, entry_for))
  }

  groups <- list()
  for (s in stacks) {
    bids <- intersect(s$block_ids, names(entries))
    groups <- c(groups, list(list(
      id = s$id, name = s$name, color = s$color,
      kind = "stack", entries = entries_for(bids)
    )))
  }

  groups <- c(groups, list(list(
    id = "", name = "Ungrouped", color = "",
    kind = "ungrouped", entries = entries_for(as.character(ungrouped))
  )))

  if (length(extensions)) {
    groups <- c(groups, list(list(
      id = "", name = "Extensions", color = "",
      kind = "extensions", entries = entries_for(as.character(extensions))
    )))
  }

  groups
}

# ---- rendering (the "Visible blocks" design, variant B) ----------------
# See _blockr.design/open/panel-navigator/6-visibility-sidebar-variantB.md.

# One group = one coloured stack bar (`ghd`) you click to collapse, listing
# block rows. Accent = the stack colour (via `--accent*` vars); Ungrouped /
# Extensions use a neutral grey. The "N shown" count is how many of the
# group's blocks are currently visible (on the active view).
nav_group_section <- function(group) {
  accent <- if (identical(group$kind, "stack") && nzchar(group$color %||% "")) {
    group$color
  } else {
    "#6b7280"
  }
  style <- sprintf(
    "--accent:%s;--accent-bg:%s;--accent-ink:%s;",
    accent, hex_alpha(accent, 0.13), darken_hex(accent, 0.72)
  )

  # Stacks + Ungrouped accept dropped blocks (reassign / reorder);
  # Extensions do not. `data-stack-id` is the drop target ("" = ungrouped).
  droppable <- group$kind %in% c("stack", "ungrouped")

  shown <- sum(vapply(group$entries, function(e) isTRUE(e$on_view), logical(1L)))

  # Only real stacks are renamable (Ungrouped / Extensions are not user
  # names): a renamable label is the canonical inline-rename widget.
  label <- if (identical(group$kind, "stack")) {
    nav_rename("stack", group$name, "blockr-panel-nav-glab")
  } else {
    shiny::tags$span(class = "blockr-panel-nav-glab", group$name)
  }

  # An empty Ungrouped is hidden (no point showing it), but stays a drop
  # target during a drag so you can still drag a block out of a stack to
  # ungroup it — the CSS reveals it only while the navigator is dragging.
  empty_ungrouped <- identical(group$kind, "ungrouped") &&
    length(group$entries) == 0L

  shiny::tags$div(
    class = paste(
      "blockr-panel-nav-grp open",
      if (droppable) "blockr-panel-nav-dropzone" else "",
      if (empty_ungrouped) "blockr-panel-nav-grp-empty" else ""
    ),
    style = style,
    `data-stack-id` = if (droppable) group$id else NULL,
    `data-kind` = group$kind,
    # The bar: a `div[role=button]` (not `<button>`) so the rename `<input>`
    # can sit inside it; the JS supplies Enter / Space activation.
    shiny::tags$div(
      class = "blockr-panel-nav-ghd",
      role = "button",
      tabindex = "0",
      `aria-expanded` = "true",
      nav_caret_icon(),
      label,
      shiny::tags$span(
        class = "blockr-panel-nav-gvis",
        sprintf("%d shown", shown)
      )
    ),
    shiny::tags$div(
      class = "blockr-panel-nav-gbody",
      shiny::tags$div(
        class = "blockr-panel-nav-rows",
        lapply(group$entries, nav_entry_card)
      )
    )
  )
}

# rgba()/darkened helpers for the per-stack accent. Stack + category colours
# are 6-digit hex (`suggest_new_colors()` / `blk_color()`); anything else
# falls back unchanged.
hex_alpha <- function(hex, alpha) {
  rgb <- hex_rgb(hex)
  if (is.null(rgb)) {
    return(hex)
  }
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

darken_hex <- function(hex, factor) {
  rgb <- hex_rgb(hex)
  if (is.null(rgb)) {
    return(hex)
  }
  sprintf("#%02x%02x%02x", round(rgb[1] * factor), round(rgb[2] * factor),
          round(rgb[3] * factor))
}

hex_rgb <- function(hex) {
  hex <- sub("^#", "", hex)
  if (!grepl("^[0-9a-fA-F]{6}$", hex)) {
    return(NULL)
  }
  c(strtoi(substr(hex, 1, 2), 16L), strtoi(substr(hex, 3, 4), 16L),
    strtoi(substr(hex, 5, 6), 16L))
}

# === Canonical inline-rename widget ====================================
# Double-click the name to rename. A subtle hover highlight hints it is
# editable (no pencil); Enter / blur commit, Esc reverts. A keyboard path
# (focus the row, press F2) covers touch / a11y, where hover alone cannot.
#
# This is the agreed app-wide rename pattern (decided 2026): adapted from
# the minimal blockr.session navbar title, but using double-click +
# hover-highlight + keyboard rather than single-click-hover-only — because
# in a dense list single-click is already taken (toggle / collapse) and a
# hover-only affordance fails touch and keyboard users.
#
# FOLLOW-UP — migrate these other rename sites onto this same widget so the
# whole app speaks one rename language:
#   * blockr.session  R/ui.R:118-160  (navbar workflow title: single-click,
#                     no keyboard / touch path) + inst/assets/* project-navbar
#   * blockr.dock     R/plugin-block.R:35-138  (block-panel header
#                     "Click to rename": hover pencil, single click)
#   * blockr.dock     R/view-ui.R + inst/assets/js/view-binding.js
#                     (view tabs: ALWAYS-visible pencil — drop the pencil)
#   * blockr.ui       R/stack-menu.R  (stack name: a form field, not inline)
#
# `kind` is "block" or "stack" (the navigator commits the matching rename
# event); `extra_class` keeps the host typography (card-name / heading
# label) on the visible text.
nav_rename <- function(kind, text, extra_class = NULL) {
  shiny::tags$span(
    class = "blockr-panel-nav-rename",
    `data-rename-kind` = kind,
    shiny::tags$span(
      class = paste("blockr-panel-nav-rename-text", extra_class),
      title = "Double-click to rename",
      text
    ),
    shiny::tags$input(
      class = "blockr-panel-nav-rename-input",
      type = "text",
      value = text,
      tabindex = "-1",
      `aria-label` = "Rename",
      autocomplete = "off",
      autocorrect = "off",
      autocapitalize = "off",
      spellcheck = "false"
    )
  )
}

nav_caret_icon <- function() {
  htmltools::HTML(
    paste0(
      '<svg class="blockr-panel-nav-caret" viewBox="0 0 24 24" width="15" ',
      'height="15" fill="none" stroke="currentColor" stroke-width="2" ',
      'stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
      '<path d="m9 18 6-6-6-6"/></svg>'
    )
  )
}

nav_grip_icon <- function() {
  htmltools::HTML(
    paste0(
      '<svg viewBox="0 0 24 24" width="14" height="14" fill="currentColor" ',
      'aria-hidden="true"><circle cx="9" cy="6" r="1.4"/>',
      '<circle cx="15" cy="6" r="1.4"/><circle cx="9" cy="12" r="1.4"/>',
      '<circle cx="15" cy="12" r="1.4"/><circle cx="9" cy="18" r="1.4"/>',
      '<circle cx="15" cy="18" r="1.4"/></svg>'
    )
  )
}

# The switch is the ONLY visibility toggle (variant B): a row's pill switch.
# role=switch + aria-checked; the JS toggles on click / Enter / Space.
nav_switch <- function(on) {
  shiny::tags$span(
    class = "blockr-panel-nav-switch",
    role = "switch",
    tabindex = "0",
    `aria-checked` = if (on) "true" else "false",
    `aria-label` = "Show / hide on this view"
  )
}

# A compact footer to create a new (empty) stack: a button that reveals an
# inline name input. Submitting fires an "add_stack" event; the navigator
# re-renders with the new heading, ready as a drop target.
nav_add_stack_control <- function() {
  shiny::tags$div(
    class = "blockr-panel-nav-addstack",
    shiny::tags$button(
      type = "button",
      class = "blockr-panel-nav-addstack-btn",
      "+ New stack"
    ),
    shiny::tags$div(
      class = "blockr-panel-nav-addstack-form",
      shiny::tags$input(
        type = "text",
        class = "blockr-panel-nav-addstack-input",
        placeholder = "Stack name, then Enter",
        autocomplete = "off",
        autocorrect = "off",
        autocapitalize = "off",
        spellcheck = "false"
      )
    )
  )
}

# One block / extension row: grip (hover) + category-tinted icon tile +
# renamable name (+ other-view tags) + the visibility switch. `.on`/`.off`
# reflects current-view membership. Only blocks carry a stack, so only block
# rows are draggable (move between stacks / reorder) and renamable.
nav_entry_card <- function(entry) {
  on_view <- isTRUE(entry$on_view)
  is_block <- identical(entry$type, "block")
  cat_color <- if (nzchar(entry$color %||% "")) entry$color else "#999999"

  name <- if (is_block) {
    nav_rename("block", entry$title, "blockr-panel-nav-name")
  } else {
    shiny::tags$span(class = "blockr-panel-nav-name", entry$title)
  }

  shiny::tags$div(
    class = paste("blockr-panel-nav-row", if (on_view) "on" else "off"),
    draggable = if (is_block) "true" else NULL,
    `data-panel-id` = entry$id,
    `data-panel-type` = entry$type,
    `data-on-view` = if (on_view) "true" else "false",
    `data-search` = tolower(
      paste(entry$title, entry$subtitle, entry$package)
    ),
    if (is_block) {
      shiny::tags$span(
        class = "blockr-panel-nav-grip",
        title = "Drag to move between stacks / reorder",
        `aria-hidden` = "true",
        nav_grip_icon()
      )
    },
    shiny::tags$span(
      class = "blockr-panel-nav-tile",
      style = sprintf(
        "background:%s;color:%s", hex_alpha(cat_color, 0.13), cat_color
      ),
      if (nzchar(entry$icon)) htmltools::HTML(entry$icon) else NULL
    ),
    shiny::tags$span(
      class = "blockr-panel-nav-namewrap",
      name,
      nav_tags_row(entry$tags)
    ),
    nav_switch(on_view)
  )
}

nav_tags_row <- function(tags) {
  if (!length(tags)) {
    return(NULL)
  }
  shiny::tags$span(
    class = "blockr-panel-nav-tags",
    lapply(tags, function(t) {
      shiny::tags$span(class = "blockr-panel-nav-tag", t)
    })
  )
}

panel_navigator_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-panel-navigator",
    version = utils::packageVersion("blockr.ui"),
    package = "blockr.ui",
    src = "assets",
    stylesheet = "css/blockr-panel-navigator.css",
    script = "js/blockr-panel-navigator.js",
    all_files = FALSE
  )
}
