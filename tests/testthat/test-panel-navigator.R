test_that("panel_navigator_ui renders stack bars, rows, switches and tags", {
  model <- list(
    list(
      id = "inputs", name = "Inputs", color = "#DF8396", kind = "stack",
      entries = list(
        list(id = "a", type = "block", title = "Dataset", subtitle = "ID: a",
             icon = "", package = "blockr.core", color = "#0072B2",
             category = "input", on_view = TRUE, tags = character()),
        list(id = "b", type = "block", title = "Dataset", subtitle = "ID: b",
             icon = "", package = "blockr.core", color = "#0072B2",
             category = "input", on_view = FALSE, tags = c("Second"))
      )
    ),
    list(
      id = "", name = "Ungrouped", color = "", kind = "ungrouped",
      entries = list(
        list(id = "c", type = "block", title = "Dataset", subtitle = "ID: c",
             icon = "", package = "blockr.core", color = "#0072B2",
             category = "input", on_view = FALSE, tags = character())
      )
    )
  )

  html <- as.character(panel_navigator_ui("nav", model))

  # Root is the input element the binding reports against.
  expect_match(html, 'id="nav-toggle"')
  expect_match(html, "blockr-panel-navigator")

  # One coloured stack bar per group; accent = the stack colour.
  expect_match(html, "blockr-panel-nav-ghd")
  expect_match(html, "--accent:#DF8396", fixed = TRUE)
  expect_match(html, ">Inputs<")
  expect_match(html, ">Ungrouped<")

  # "N shown" = how many of the group's blocks are visible (a only).
  expect_match(html, "1 shown")

  # One row per entry, each with the visibility switch and the data contract.
  expect_equal(length(gregexpr('"blockr-panel-nav-row ', html)[[1]]), 3L)
  expect_equal(length(gregexpr('role="switch"', html)[[1]]), 3L)
  expect_match(html, 'data-panel-id="a"')
  expect_match(html, 'data-on-view="true"')
  expect_match(html, 'data-on-view="false"')

  # Visible row -> .on + switch aria-checked; tag pill for the other view.
  expect_match(html, 'class="blockr-panel-nav-row on"')
  expect_match(html, 'aria-checked="true"')
  expect_match(html, "blockr-panel-nav-tag")
  expect_match(html, ">Second<")
})

test_that("panel_navigator_ui tints the icon tile with the block colour", {
  model <- list(
    list(
      id = "inputs", name = "Inputs", color = "#DF8396", kind = "stack",
      entries = list(
        list(id = "a", type = "block", title = "Dataset", subtitle = "ID: a",
             icon = "", package = "blockr.core", color = "#0072B2",
             category = "input", on_view = TRUE, tags = character())
      )
    )
  )
  html <- as.character(panel_navigator_ui("nav", model))
  # The tile carries the category colour as fill + tinted background.
  expect_match(html, "blockr-panel-nav-tile")
  expect_match(html, "color:#0072B2", fixed = TRUE)
  expect_match(html, "rgba(0,114,178,0.13)", fixed = TRUE)
})

test_that("panel_navigator_ui handles an empty model", {
  ui <- panel_navigator_ui("nav", list())
  html <- as.character(ui)
  expect_match(html, "blockr-panel-navigator")
  expect_no_match(html, "blockr-panel-nav-row")
})

test_that("an empty Ungrouped group is marked hidden (revealed on drag)", {
  empty <- list(
    list(id = "", name = "Ungrouped", color = "", kind = "ungrouped",
         entries = list())
  )
  expect_match(
    as.character(panel_navigator_ui("nav", empty)),
    "blockr-panel-nav-grp-empty"
  )

  filled <- list(
    list(id = "", name = "Ungrouped", color = "", kind = "ungrouped",
         entries = list(
           list(id = "c", type = "block", title = "Dataset", subtitle = "",
                icon = "", package = "blockr.core", color = "#0072B2",
                category = "input", on_view = FALSE, tags = character())
         ))
  )
  expect_no_match(
    as.character(panel_navigator_ui("nav", filled)),
    "blockr-panel-nav-grp-empty"
  )
})

test_that("panel_navigator_ui renders drag-drop targets and add-stack", {
  model <- list(
    list(
      id = "s1", name = "Inputs", color = "#DF8396", kind = "stack",
      entries = list(
        list(id = "a", type = "block", title = "Dataset", subtitle = "ID: a",
             icon = "", package = "blockr.core", color = "#0072B2",
             category = "input", on_view = TRUE, tags = character())
      )
    ),
    list(
      id = "", name = "Ungrouped", color = "", kind = "ungrouped",
      entries = list()
    )
  )

  html <- as.character(panel_navigator_ui("nav", model))

  # Block rows are draggable (grip) onto drop-target groups carrying their
  # stack id ("" for ungrouped).
  expect_match(html, 'draggable="true"')
  expect_match(html, "blockr-panel-nav-grip")
  expect_match(html, "blockr-panel-nav-dropzone")
  expect_match(html, 'data-stack-id="s1"')
  expect_match(html, 'data-stack-id=""')

  # The add-stack control is present.
  expect_match(html, "blockr-panel-nav-addstack")
  expect_match(html, "New stack")
})

test_that("panel_navigator_ui makes block names and stack bars renamable", {
  model <- list(
    list(
      id = "s1", name = "Inputs", color = "#DF8396", kind = "stack",
      entries = list(
        list(id = "a", type = "block", title = "Dataset", subtitle = "ID: a",
             icon = "", package = "blockr.core", color = "#0072B2",
             category = "input", on_view = TRUE, tags = character())
      )
    ),
    list(
      id = "", name = "Extensions", color = "", kind = "extensions",
      entries = list(
        list(id = "dag", type = "ext", title = "Workflow", subtitle = "ID: dag",
             icon = "", package = "blockr.dag", color = "#999999",
             category = "", on_view = FALSE, tags = character())
      )
    )
  )

  html <- as.character(panel_navigator_ui("nav", model))

  # Inline rename widget (double-click) on the block name and the stack bar.
  expect_match(html, 'data-rename-kind="block"')
  expect_match(html, 'data-rename-kind="stack"')
  expect_match(html, "blockr-panel-nav-rename-input")
  expect_match(html, "Double-click to rename")

  # Extensions are NOT renamable: exactly one block + one stack rename zone.
  expect_equal(length(gregexpr('data-rename-kind="', html)[[1]]), 2L)
})

test_that("panel_nav_model groups entries by stack with view state + tags", {
  entries <- list(
    a = list(type = "block", title = "Dataset", subtitle = "ID: a",
             icon = "", color = "#0072B2", category = "input",
             package = "blockr.core"),
    b = list(type = "block", title = "Dataset", subtitle = "ID: b",
             icon = "", color = "#0072B2", category = "input",
             package = "blockr.core"),
    c = list(type = "block", title = "Dataset", subtitle = "ID: c",
             icon = "", color = "#0072B2", category = "input",
             package = "blockr.core"),
    dag = list(type = "ext", title = "Workflow", subtitle = "ID: dag",
               icon = "", color = "#999999", category = "",
               package = "blockr.dag")
  )
  model <- panel_nav_model(
    entries = entries,
    stacks = list(list(id = "s1", name = "Inputs", color = "#DF8396",
                       block_ids = c("a", "b"))),
    ungrouped = "c",
    extensions = "dag",
    visible = c("a"),
    views = list(First = c("a"), Second = c("b")),
    view_labels = c(First = "First", Second = "Second"),
    active = "First"
  )

  expect_identical(
    vapply(model, `[[`, character(1L), "kind"),
    c("stack", "ungrouped", "extensions")
  )
  ents <- setNames(model[[1]]$entries,
                   vapply(model[[1]]$entries, `[[`, character(1L), "id"))
  expect_true(ents[["a"]]$on_view)              # a visible
  expect_identical(ents[["a"]]$tags, character())
  expect_false(ents[["b"]]$on_view)             # b hidden, lives on Second
  expect_identical(ents[["b"]]$tags, "Second")
  expect_identical(model[[3]]$entries[[1]]$id, "dag")
})

test_that("nav_reassign_payload moves, reorders and ungroups", {
  brd <- blockr.core::new_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block(),
               c = new_dataset_block())
  )
  brd <- blockr.core::modify_board_stacks(
    brd,
    add = blockr.core::stacks(
      s1 = blockr.core::new_stack(blocks = c("a", "b"), name = "One"),
      s2 = blockr.core::new_stack(blocks = "c", name = "Two")
    )
  )

  # Move b from s1 into s2, before c -> s2 = (b, c), s1 = (a).
  p <- nav_reassign_payload(brd, "b", "s2", before = "c")
  expect_identical(p$stacks$mod$s1$blocks, "a")
  expect_identical(p$stacks$mod$s2$blocks, c("b", "c"))

  # Reorder within s1: a before... no-op target keeps it a, b. Move a after b.
  p2 <- nav_reassign_payload(brd, "a", "s1", before = NULL)
  expect_identical(p2$stacks$mod$s1$blocks, c("b", "a"))

  # Ungroup c (target "").
  p3 <- nav_reassign_payload(brd, "c", "", before = NULL)
  expect_identical(p3$stacks$mod$s2$blocks, character(0))
  expect_null(p3$stacks$mod$s2$x)  # only s2 modded (source), no target add
})

test_that("panel_nav_apply_core applies rename + reassign via update()", {
  brd <- blockr.core::new_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block())
  )
  brd <- blockr.core::modify_board_stacks(
    brd,
    add = blockr.core::stacks(s1 = blockr.core::new_stack(blocks = "a", name = "One"))
  )
  cap <- NULL
  up <- function(x) cap <<- x

  panel_nav_apply_core(
    list(kind = "rename", type = "block", id = "a", name = "Renamed"), brd, up)
  expect_identical(cap$blocks$mod$a$block_name, "Renamed")

  panel_nav_apply_core(
    list(kind = "rename_stack", stack = "s1", name = "Sources"), brd, up)
  expect_identical(cap$stacks$mod$s1$name, "Sources")

  cap <- NULL
  panel_nav_apply_core(list(kind = "rename", type = "block", id = "a", name = ""),
                       brd, up)
  expect_null(cap)  # empty name is a no-op
})
