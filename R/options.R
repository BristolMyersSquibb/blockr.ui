new_stack_colors_option <- function(
  n_stacks = blockr_option("n_stacks", 40L),
  color_palette = blockr_option("stacks_palette", "Spectral"),
  ...
) {
  new_board_option(
    id = "stack_colors",
    default = list(n_stacks = n_stacks, color_palette = color_palette),
    ui = function(id) {
      span(
        numericInput(
          NS(id, "n_stacks"),
          "Number of stack colors",
          n_stacks,
          min = 1L,
          step = 1L
        ),
        selectInput(
          NS(id, "color_palette"),
          "Color palette",
          grDevices::hcl.pals(),
          color_palette
        )
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("stack_colors", session),
        {
          opt <- get_board_option_value("stack_colors", session)
          updateNumericInput(
            session,
            "n_stacks",
            value = attr(opt, "n_stacks")
          )
          updateSelectInput(
            session,
            "color_palette",
            selected = attr(opt, "palette")
          )
        }
      )
    },
    update_trigger = c("n_stacks", "color_palette"),
    transform = function(x) {
      structure(
        grDevices::hcl.colors(x$n_stacks, palette = x$color_palette),
        n_stacks = as.integer(x$n_stacks),
        palette = x$color_palette
      )
    },
    ...
  )
}

#' @export
validate_board_option.stack_colors_option <- function(x) {
  val <- board_option_value(NextMethod())

  nst <- attr(val, "n_stacks")
  pal <- attr(val, "palette")

  if (!is_count(nst)) {
    abort(
      "Expecting `n_stacks` to represent a count.",
      class = "board_options_stack_colors_invalid"
    )
  }

  if (!(is_string(pal) && pal %in% grDevices::hcl.pals())) {
    abort(
      "Expecting `color_palette` to represent a single valid color palette.",
      class = "board_options_stack_colors_invalid"
    )
  }

  if (!(is.character(val) && length(val) == nst)) {
    abort(
      paste0(
        "Expecting `stack_colors` to be a character vector of length ",
        nst,
        "."
      ),
      class = "board_options_stack_colors_invalid"
    )
  }

  invisible(x)
}

#' @export
blockr_ser.stack_colors_option <- function(x, option = NULL, ...) {
  val <- coal(option, board_option_value(x))

  NextMethod(
    option = list(
      n_stacks = attr(val, "n_stacks"),
      color_palette = attr(val, "palette")
    )
  )
}

new_snapshot_option <- function(
  auto_save = blockr_option("auto_save", FALSE),
  location = blockr_option("save_location", tempdir()),
  ...
) {
  new_board_option(
    id = "snapshot",
    default = auto_save,
    ui = function(id) {
      bslib::input_switch(
        NS(id, "snapshot"),
        "Enable auto-save",
        auto_save
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("snapshot", session),
        {
          bslib::toggle_switch(
            "snapshot",
            value = get_board_option_value("snapshot", session),
            session = session
          )
        }
      )
    },
    transform = function(x) structure(as.logical(x), location = location),
    ...
  )
}

new_blocks_position_option <- function(
  reference_panel = blockr_option("reference_panel", NULL),
  direction = blockr_option("direction", "within"),
  ...
) {
  new_board_option(
    id = "blocks_position",
    default = list(reference_panel = reference_panel, direction = direction),
    ui = function(id) {
      tagList(
        h4("Blocks position"),
        selectInput(
          NS(id, "reference_panel"),
          "Reference panel",
          choices = list()
        ),
        selectInput(
          NS(id, "direction"),
          "Direction",
          choices = c("within", "above", "below", "left", "right"),
          selected = direction
        )
      )
    },
    server = function(..., session) {
      dock_proxy <- dock_view_proxy("layout", session)

      list(
        observeEvent(
          req(length(get_panels_ids(dock_proxy)) > 0),
          {
            layout_panels <- reference_panel_candidates(dock_proxy)

            updateSelectInput(
              session,
              "reference_panel",
              choices = layout_panels,
              selected = coal(reference_panel, last(layout_panels))
            )
          },
          once = TRUE
        ),
        observeEvent(
          get_board_option_or_null("blocks_position", session),
          {
            opt <- get_board_option_value("blocks_position", session)

            updateSelectInput(
              session,
              "reference_panel",
              choices = reference_panel_candidates(dock_proxy),
              selected = opt$reference_panel
            )

            updateSelectInput(
              session,
              "direction",
              selected = opt$direction
            )
          }
        )
      )
    },
    update_trigger = c("reference_panel", "direction"),
    ...
  )
}

reference_panel_candidates <- function(proxy) {
  grep(
    get_panels_ids(proxy),
    pattern = "^(?!.*block-).*$",
    perl = TRUE,
    value = TRUE
  )
}

#' @export
validate_board_option.blocks_position_option <- function(x) {
  val <- board_option_value(NextMethod())

  if (!is.list(val)) {
    blockr_abort(
      "Expecting `blocks_position_option` to be list.",
      class = "board_options_blocks_position_invalid"
    )
  }

  comps <- c("reference_panel", "direction")

  if (!setequal(names(val), comps)) {
    blockr_abort(
      "Expecting `blocks_position_option` to contain component{?s} {comps}.",
      class = "board_options_blocks_position_invalid"
    )
  }

  ref <- val$reference_panel

  if (!(is.null(ref) || is_string(ref))) {
    blockr_abort(
      "Expecting the `reference_panel` entry of `blocks_position_option` to ",
      "either be `NULL` or string-valued.",
      class = "board_options_blocks_position_invalid"
    )
  }

  opts <- c("within", "above", "below", "left", "right")
  dir <- val$direction

  if (!(is_string(dir) && dir %in% opts)) {
    abort(
      "Expecting the `direction` entry of `blocks_position_option` to be one ",
      "of {opts}.",
      class = "board_options_blocks_position_invalid"
    )
  }

  invisible(x)
}
