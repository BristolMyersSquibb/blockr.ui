test_that("action_menu() hangs the rows, hidden, beside the trigger", {

  menu <- action_menu(
    tool_button(htmltools::HTML("D"), "Download"),
    menu_section("This patient"),
    menu_item(shiny::downloadLink("pptx", "PowerPoint"), meta = ".pptx"),
    NULL,
    menu_divider(),
    menu_item(shiny::actionLink("rm", "Remove"), danger = TRUE)
  )

  html <- htmltools::renderTags(menu)$html

  expect_match(html, 'class="blockr-action-menu" data-align="end"', fixed = TRUE)
  expect_match(html, 'aria-haspopup="menu"', fixed = TRUE)
  expect_match(html, 'aria-expanded="false"', fixed = TRUE)
  expect_match(html, "blockr-tool blockr-action-menu__trigger", fixed = TRUE)
  expect_match(html, 'class="blockr-menu" role="menu" tabindex="-1" hidden',
               fixed = TRUE)
  expect_match(html, 'class="blockr-menu__title" role="presentation">This patient',
               fixed = TRUE)
  expect_match(html, 'class="blockr-menu__divider" role="separator"',
               fixed = TRUE)
  expect_match(html, "blockr-menu__item blockr-menu__item--danger",
               fixed = TRUE)

  # The row marker is for action_menu() only and never reaches the page.
  rows <- htmltools::tagQuery(menu)$find(".blockr-menu")$children()$selectedTags()
  expect_length(rows, 4L)
  expect_false(any(vapply(rows, inherits, logical(1L), "blockr_menu_row")))

  expect_identical(
    htmltools::tagGetAttribute(action_menu(
      tool_button(htmltools::HTML("D"), "Download"),
      menu_divider(),
      align = "start"
    ), "data-align"),
    "start"
  )
})

test_that("action_menu() and tool_button() bring the controls along", {

  names <- function(x) {
    vapply(htmltools::findDependencies(x), `[[`, character(1L), "name")
  }

  expect_true("blockr-ui-js" %in% names(tool_button(htmltools::HTML("D"), "D")))
  expect_true("blockr-blocks-css" %in% names(
    action_menu(htmltools::tags$button("D"), menu_divider())
  ))
})

test_that("menu_item() dresses a Shiny link as a row and keeps it working", {

  row <- menu_item(
    shiny::downloadLink("pptx", "PowerPoint"),
    meta = ".pptx",
    icon = htmltools::HTML("<svg></svg>")
  )

  expect_s3_class(row, "blockr_menu_row")
  expect_identical(htmltools::tagGetAttribute(row, "id"), "pptx")
  expect_identical(htmltools::tagGetAttribute(row, "role"), "menuitem")

  # A downloadLink() brings a tabindex of its own; the row has exactly one.
  expect_identical(htmltools::tagGetAttribute(row, "tabindex"), "-1")
  expect_match(htmltools::tagGetAttribute(row, "class"), "shiny-download-link",
               fixed = TRUE)

  html <- as.character(row)
  expect_match(html, '<span class="blockr-menu__icon"><svg></svg></span>',
               fixed = TRUE)
  expect_match(html, '<span class="blockr-menu__label">PowerPoint</span>',
               fixed = TRUE)
  expect_match(html, '<span class="blockr-menu__meta">.pptx</span>',
               fixed = TRUE)

  btn <- menu_item(htmltools::tags$button(id = "go", "Go"))
  expect_identical(htmltools::tagGetAttribute(btn, "type"), "button")
})

test_that("a disabled row keeps its place and says why", {

  row <- menu_item(shiny::downloadLink("xlsx", "Excel"), meta = ".xlsx",
                   disabled = "Needs the openxlsx package")

  expect_match(htmltools::tagGetAttribute(row, "class"),
               "blockr-menu__item--disabled", fixed = TRUE)
  expect_identical(htmltools::tagGetAttribute(row, "aria-disabled"), "true")
  expect_identical(htmltools::tagGetAttribute(row, "data-blockr-tooltip"),
                   "Needs the openxlsx package")

  ok <- menu_item(shiny::actionLink("go", "Go"))
  expect_null(htmltools::tagGetAttribute(ok, "aria-disabled"))
  expect_null(htmltools::tagGetAttribute(ok, "data-blockr-tooltip"))
})

test_that("tool_button() names itself for the tooltip and for screen readers", {

  btn <- tool_button(htmltools::HTML("D"), "Download", id = "dl")

  expect_identical(htmltools::tagGetAttribute(btn, "class"), "blockr-tool")
  expect_identical(htmltools::tagGetAttribute(btn, "type"), "button")
  expect_identical(htmltools::tagGetAttribute(btn, "aria-label"), "Download")
  expect_identical(htmltools::tagGetAttribute(btn, "data-blockr-tooltip"),
                   "Download")
  expect_identical(htmltools::tagGetAttribute(btn, "id"), "dl")

  expect_error(tool_button(htmltools::HTML("D"), ""))
})

test_that("stray markup is refused", {

  trigger <- tool_button(htmltools::HTML("D"), "Download")

  expect_error(action_menu("Download", menu_divider()), "must be an HTML tag")
  expect_error(action_menu(trigger), "at least one row")
  expect_error(
    action_menu(trigger, shiny::downloadLink("x", "X")),
    "menu_item\\(\\), menu_section\\(\\) or menu_divider\\(\\)"
  )
  expect_error(menu_item(htmltools::tags$div("X")), "an <a> or a <button>")
  expect_error(menu_item("X"), "an <a> or a <button>")
})
