test_that("shortcut() writes the Mac form and the other", {

  html <- as.character(shortcut("Mod+Shift+S"))

  expect_match(html, '<span class="blockr-shortcut__mac">⌘⇧S</span>',
               fixed = TRUE)
  expect_match(html, '<span class="blockr-shortcut__other">Ctrl+Shift+S</span>',
               fixed = TRUE)

  expect_identical(shortcut_text(c("Mod", "Enter"), TRUE), "⌘↵")
  expect_identical(shortcut_text(c("Mod", "Enter"), FALSE), "Ctrl+↵")
  expect_identical(shortcut_text("Esc", FALSE), "Esc")
})

test_that("a menu row takes a shortcut as its meta", {

  row <- as.character(
    menu_item(shiny::actionLink("save", "Save"), meta = shortcut("Mod+S"))
  )

  expect_match(row, "blockr-menu__meta", fixed = TRUE)
  expect_match(row, "blockr-shortcut__other\">Ctrl+S", fixed = TRUE)
})

test_that("controls_dep() carries the rules that pick a hint's form", {

  sheets <- blockr.core::unlst(
    lapply(htmltools::findDependencies(controls_dep()), function(d) {
      file.path(system.file(d$src$file, package = "blockr.ui"), d$stylesheet)
    })
  )
  css <- paste(blockr.core::chr_ply(sheets, read_css), collapse = "\n")

  expect_match(css, ".blockr-mac .blockr-shortcut__other", fixed = TRUE)
  expect_match(css, ".blockr-shortcut__mac {", fixed = TRUE)
})

test_that("R and blockr-ui.js write a hint the same way", {

  # One table for both: tests/js/shortcut.test.js holds Blockr.keys() to it.
  cases <- utils::read.delim(
    test_path("fixtures", "shortcuts.tsv"),
    colClasses = "character",
    encoding = "UTF-8"
  )

  for (i in seq_len(nrow(cases))) {
    parts <- strsplit(cases$keys[i], "+", fixed = TRUE)[[1L]]
    expect_identical(shortcut_text(parts, TRUE), enc2utf8(cases$mac[i]))
    expect_identical(shortcut_text(parts, FALSE), enc2utf8(cases$other[i]))
  }
})
