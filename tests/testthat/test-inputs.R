html_of <- function(x) htmltools::renderTags(x)$html

# A session that records what update_*() sends.
recording_session <- function() {
  env <- new.env()
  env$sent <- list()
  env$sendInputMessage <- function(inputId, message) { # nolint
    env$sent[[length(env$sent) + 1L]] <- list(id = inputId, message = message)
  }
  env
}

test_that("select_input() writes its settings for the binding", {

  html <- html_of(
    select_input("col", "Column", c("mpg", disp = "Displacement"),
                 selected = "disp")
  )

  expect_match(html, 'class="blockr-settings__field"', fixed = TRUE)
  expect_match(
    html, '<label id="col-label" class="blockr-label">Column</label>',
    fixed = TRUE
  )
  expect_match(html, 'id="col" class="blockr-ui-select"', fixed = TRUE)
  expect_match(html, 'data-multiple="false"', fixed = TRUE)
  expect_match(
    html,
    paste0(
      'data-options="[&quot;mpg&quot;,{&quot;value&quot;:',
      '&quot;Displacement&quot;,&quot;label&quot;:&quot;disp&quot;}]"'
    ),
    fixed = TRUE
  )
  expect_match(html, 'data-selected="[&quot;disp&quot;]"', fixed = TRUE)
  expect_no_match(html, "data-placeholder")

  # One choice and one pick still go as arrays.
  multi <- html_of(select_input("cols", NULL, "mpg", "mpg", multiple = TRUE,
                                placeholder = "Pick"))
  expect_match(multi, 'data-options="[&quot;mpg&quot;]"', fixed = TRUE)
  expect_match(multi, 'data-selected="[&quot;mpg&quot;]"', fixed = TRUE)
  expect_match(multi, "blockr-settings__field--full", fixed = TRUE)
  expect_match(multi, 'data-placeholder="Pick"', fixed = TRUE)
  expect_no_match(multi, "<label")

  expect_match(html_of(select_input("x", "X", c("a", "b"))),
               'data-selected="[]"', fixed = TRUE)

  expect_error(select_input("x", "X", c("a", "b"), c("a", "b")), "one")
})

test_that("a select reports a character vector, or NULL for no pick", {

  expect_identical(select_value(list("a", "b")), c("a", "b"))
  expect_identical(select_value("a"), "a")
  expect_identical(select_value(""), "")
  expect_null(select_value(list()))
  expect_null(select_value(NULL))
})

test_that("text and number fields sit in the commit field", {

  html <- html_of(text_input("name", "Name", "x.csv", placeholder = "file"))

  expect_match(html, '<label id="name-label" class="blockr-label" for="name">',
               fixed = TRUE)
  expect_match(html, '<div class="blockr-commit-field">', fixed = TRUE)
  expect_match(html, 'class="blockr-text-input blockr-ui-text" value="x.csv"',
               fixed = TRUE)

  num <- html_of(number_input("n", "Rows", 10, min = 0, step = 5))
  expect_match(num, "blockr-settings__field--small", fixed = TRUE)
  expect_match(num, 'type="number"', fixed = TRUE)
  expect_match(num, 'value="10" min="0" step="5"', fixed = TRUE)
  expect_no_match(num, "max=")

  expect_no_match(html_of(number_input("n", "Rows")), "value=")
})

test_that("checkbox_input() draws Blockr.checkbox's markup", {

  html <- html_of(checkbox_input("header", "First row is a header", TRUE))

  expect_match(html, '<label class="blockr-checkbox">', fixed = TRUE)
  expect_match(
    html, 'id="header" type="checkbox" class="blockr-ui-checkbox" checked',
    fixed = TRUE
  )
  expect_match(html, '<span class="blockr-checkbox__box"><svg', fixed = TRUE)
  expect_match(html, "blockr-checkbox__label\">First row is a header",
               fixed = TRUE)
  expect_no_match(html_of(checkbox_input("h", "H")), "checked")
})

test_that("segmented_input() takes two or three values", {

  html <- html_of(
    segmented_input("from", "From", c(First = "head", Last = "tail"),
                    size = "xs")
  )

  expect_match(
    html,
    paste0(
      'data-choices="[{&quot;value&quot;:&quot;head&quot;,',
      "&quot;label&quot;:&quot;First&quot;},{&quot;value&quot;:",
      '&quot;tail&quot;,&quot;label&quot;:&quot;Last&quot;}]"'
    ),
    fixed = TRUE
  )
  expect_match(html, 'data-selected="&quot;head&quot;"', fixed = TRUE)
  expect_match(html, 'data-size="xs"', fixed = TRUE)

  expect_error(segmented_input("x", "X", "a"), "two or three")
  expect_error(segmented_input("x", "X", letters[1:4]), "two or three")
  expect_error(segmented_input("x", "X", c("a", "b"), selected = "c"))
})

test_that("the update functions send only what they are given", {

  s <- recording_session()

  update_select_input("col", choices = c("a", b = "B"), selected = "a",
                      session = s)
  update_select_input("col", selected = character(), session = s)
  update_text_input("name", value = "y", session = s)
  update_number_input("n", value = NA, session = s)
  update_number_input("n", value = 2.5, label = "Rows", session = s)
  update_checkbox_input("header", value = FALSE, session = s)
  update_segmented_input("from", selected = "tail", session = s)

  msgs <- lapply(s$sent, `[[`, "message")

  expect_identical(msgs[[1L]], list(
    choices = list("a", list(value = "B", label = "b")),
    selected = list("a")
  ))
  expect_identical(msgs[[2L]], list(selected = list()))
  expect_identical(msgs[[3L]], list(value = "y"))
  expect_identical(msgs[[4L]], list(value = ""))
  expect_identical(msgs[[5L]], list(value = "2.5", label = "Rows"))
  expect_identical(msgs[[6L]], list(value = FALSE))
  expect_identical(msgs[[7L]], list(selected = "tail"))
  expect_identical(s$sent[[1L]]$id, "col")
})

test_that("gear_tray() draws the gear last in the header row, and the tray", {

  tray <- gear_tray(
    "gear",
    tray_section("Format", select_input("sep", "Separator", c(",", ";"))),
    tray_section("Skip", number_input("skip", "Rows", 0), toggle = "skip_on"),
    tools = tool_button(htmltools::HTML("D"), "Download")
  )
  html <- html_of(tray)

  expect_match(
    html,
    paste0(
      '<button id="gear" type="button" ',
      'class="blockr-gear-btn blockr-ui-gear" aria-controls="gear_tray" ',
      'aria-expanded="false" aria-label="Settings" ',
      'data-blockr-tooltip="Settings"></button>'
    ),
    fixed = TRUE
  )
  expect_match(html, "blockr-tool.*blockr-gear-btn")
  expect_match(html, paste0('id="gear_tray" class="blockr-settings ',
                            'blockr-settings--beak" role="region" ',
                            'aria-label="Settings"'), fixed = TRUE)
  expect_match(html, '<div class="blockr-settings__title">Format</div>',
               fixed = TRUE)
  expect_match(html, "blockr-settings__title blockr-settings__title--toggle",
               fixed = TRUE)
  expect_match(html, 'id="skip_on" type="checkbox"', fixed = TRUE)
  expect_match(html, "blockr-checkbox__label\">Skip", fixed = TRUE)
  expect_length(
    gregexpr('class="blockr-settings__grid"', html, fixed = TRUE)[[1L]], 2L
  )

  names <- vapply(htmltools::findDependencies(tray), `[[`, character(1L),
                  "name")
  expect_true("blockr-inputs-js" %in% names)
})

test_that("a tray of one section has no title, unless it is a toggle", {

  one <- html_of(gear_tray("g", tray_section("Format", text_input("a", "A"))))
  expect_no_match(one, "blockr-settings__title")

  loose <- html_of(gear_tray("g", text_input("a", "A"), htmltools::span("x")))
  expect_no_match(loose, "blockr-settings__title")
  # Loose tags get a grid cell.
  expect_match(loose, '<div class="blockr-settings__field">\\s*<span>x</span>')

  toggled <- html_of(gear_tray("g", tray_section("Skip", text_input("a", "A"),
                                                 toggle = "on")))
  expect_match(toggled, "blockr-settings__title--toggle", fixed = TRUE)

  expect_error(gear_tray("g"), "no options")
  expect_error(gear_tray("g", tray_section("A", text_input("a", "A")),
                         text_input("b", "B")), "tray_section")
  expect_error(tray_section(NULL, toggle = "on"), "title")
})
