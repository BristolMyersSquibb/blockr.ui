test_that("controls_dep ships the controls after the tokens", {

  deps <- htmltools::findDependencies(controls_dep())
  names <- blockr.core::chr_xtr(deps, "name")

  # blockr-ui.js defines the namespace Select builds on, and the stylesheets
  # read the tokens, so the order is part of the contract. The theme layer
  # is the app's to attach.
  expect_identical(
    names,
    c("blockr-tokens", "blockr-ui-js", "blockr-blocks-css", "blockr-menu-css",
      "blockr-tooltip-css", "blockr-buttons-css", "blockr-settings-band",
      "blockr-select-js", "blockr-select-css", "blockr-input-js",
      "blockr-input-css", "blockr-inputs-js", "blockr-inputs-css")
  )

  assets <- system.file("assets", package = "blockr.ui")
  files <- blockr.core::unlst(
    c(
      blockr.core::lst_xtr(deps, "script"),
      blockr.core::lst_xtr(deps, "stylesheet")
    )
  )

  expect_true(all(file.exists(file.path(assets, files))))
})

test_that("the controls read only meaning tokens this package defines", {

  # Every stylesheet controls_dep() ships, read off the dependency, so a
  # sheet added to it is held to this at once. A property a component sets
  # for itself (the menu mark's colour, set inline by Blockr.menu) is not a
  # token and is named here.
  local <- "--blockr-menu-mark"
  sheets <- blockr.core::unlst(
    blockr.core::lst_xtr(htmltools::findDependencies(controls_dep()),
                         "stylesheet")
  )
  sheets <- basename(sheets[!grepl("tokens", sheets)])
  # A sheet that only lays things out (blockr-inputs.css) reads no token.
  reads <- vapply(
    sheets,
    function(x) {
      css <- system.file("assets", "css", x, package = "blockr.ui")
      any(grepl("var(--", readLines(css, warn = FALSE), fixed = TRUE))
    },
    logical(1L)
  )

  sites <- token_references("blockr.ui")
  controls <- sites[
    basename(sites$file) %in% sheets & !sites$token %in% local,
  ]
  stray <- grepl(palette_token, controls$token) |
    controls$token %in% names(legacy_tokens())

  expect_setequal(unique(basename(controls$file)), sheets[reads])
  expect_identical(unique(controls$token[is.na(controls$value)]), character())
  expect_identical(unique(controls$token[stray]), character())
})

test_that("text edited in place gets its cursor wherever blockr-ui.js goes", {

  sheets_of <- function(x) {
    deps <- htmltools::findDependencies(x)
    blockr.core::unlst(
      lapply(deps, function(d) {
        file.path(system.file(d$src$file, package = "blockr.ui"), d$stylesheet)
      })
    )
  }
  cursor <- function(sheets) {
    any(grepl(
      "[data-blockr-editable]",
      blockr.core::chr_ply(sheets, read_css),
      fixed = TRUE
    ))
  }

  expect_true(cursor(sheets_of(controls_dep())))
  expect_true(
    cursor(sheets_of(build_html_table(data.frame(a = 1), total_rows = 1L)))
  )
})

test_that("markup built in R writes no native title", {

  # The spec's rule: no native `title` tooltips; a name shows in the light
  # card. blockr-ui.js takes over any that reach the page, but its own markup
  # should not send one. Every helper here, at its most decorated.
  long <- strrep("Subject-Level Analysis ", 5L)
  df <- data.frame(x = c(-1, NA, 2), y = c("a", "b", strrep("long ", 30L)))
  attr(df$x, "label") <- long

  html <- paste(
    htmltools::renderTags(htmltools::tagList(
      build_html_table(
        df, 3L,
        sort_state = list(col = "x", dir = "desc"),
        table_label = long
      ),
      action_menu(
        tool_button(htmltools::HTML("D"), "Download"),
        menu_section("Files"),
        menu_item(shiny::downloadLink("csv", "CSV"), meta = shortcut("Mod+S")),
        menu_item(shiny::downloadLink("xlsx", "Excel"), disabled = long)
      )
    ))$html
  )

  expect_no_match(html, "\\stitle=", perl = TRUE)
})
