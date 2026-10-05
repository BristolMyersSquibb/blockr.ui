html_of <- function(x) htmltools::renderTags(x)$html

test_that("gear_tray() draws the gear last in the header row, and the tray", {

  tray <- gear_tray(
    "gear",
    tray_section("Format", shiny::selectInput("sep", "Separator", c(",", ";"))),
    tray_section("Skip", shiny::numericInput("skip", "Rows", 0),
                 toggle = "skip_on"),
    tools = tool_button(htmltools::HTML("D"), "Download")
  )
  html <- html_of(tray)

  expect_match(
    html,
    paste0(
      '<button id="gear" type="button" ',
      'class="blockr-gear-btn blockr-ui-gear" aria-controls="gear_tray" ',
      'aria-expanded="false" aria-label="Settings" ',
      'data-blockr-tooltip="Settings"><svg'
    ),
    fixed = TRUE
  )
  # The gear's icon is the shared one.
  expect_match(html, as.character(small_icon("gear")), fixed = TRUE)
  expect_match(html, "blockr-tool.*blockr-gear-btn")
  expect_match(html, paste0('id="gear_tray" class="blockr-settings ',
                            'blockr-settings--beak" role="region" ',
                            'aria-label="Settings"'), fixed = TRUE)
  expect_match(html, '<div class="blockr-settings__title">Format</div>',
               fixed = TRUE)
  expect_match(html, "blockr-settings__title blockr-settings__title--toggle",
               fixed = TRUE)
  # The toggle is Shiny's own checkbox, labelled by the title.
  expect_match(
    html,
    paste0('blockr-settings__title--toggle">\\s*',
           '<div class="form-group shiny-input-container">')
  )
  expect_match(html, 'id="skip_on" type="checkbox"', fixed = TRUE)
  expect_match(html, "<span>Skip</span>", fixed = TRUE)
  expect_length(
    gregexpr('class="blockr-settings__grid"', html, fixed = TRUE)[[1L]], 2L
  )

  names <- vapply(htmltools::findDependencies(tray), `[[`, character(1L),
                  "name")
  expect_true("blockr-shiny-js" %in% names)
})

test_that("a tray of one section has no title, unless it is a toggle", {

  text <- function(id) shiny::textInput(id, toupper(id), updateOn = "blur")

  one <- html_of(gear_tray("g", tray_section("Format", text("a"))))
  expect_no_match(one, "blockr-settings__title")

  loose <- html_of(gear_tray("g", text("a"), htmltools::span("x")))
  expect_no_match(loose, "blockr-settings__title")
  # Loose tags get a grid cell.
  expect_match(loose, '<div class="blockr-settings__field">\\s*<span>x</span>')

  toggled <- html_of(gear_tray("g", tray_section("Skip", text("a"),
                                                 toggle = "on")))
  expect_match(toggled, "blockr-settings__title--toggle", fixed = TRUE)

  expect_error(gear_tray("g"), "no options")
  expect_error(gear_tray("g", tray_section("A", text("a")), text("b")),
               "tray_section")
  expect_error(tray_section(NULL, toggle = "on"), "title")
})

test_that("every field gets a plain cell; the stylesheet sizes it", {

  html <- html_of(gear_tray(
    "g",
    shiny::numericInput("n", "Rows", 1, updateOn = "blur"),
    shiny::textInput("t", "Name", updateOn = "blur")
  ))
  cells <- gregexpr(
    '<div class="blockr-settings__field">\\s*<div class="form-group', html
  )[[1L]]
  expect_length(cells, 2L)

  expect_error(tray_section("A", toggle = "on", value = NA), "value")
})
