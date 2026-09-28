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

test_that("the hint picks its form where blockr.dplyr's stylesheet wins", {

  # A board with blockr.dplyr blocks resolves blockr-blocks-css to that
  # package's older copy, so the rules that pick a form live elsewhere.
  older <- htmltools::htmlDependency(
    "blockr-blocks-css", "99.0.0",
    src = c(file = withr::local_tempdir()),
    stylesheet = "blockr-blocks.css"
  )
  deps <- htmltools::resolveDependencies(
    c(htmltools::findDependencies(shortcut("Mod+S")), list(older)),
    resolvePackageDir = FALSE
  )
  ours <- Filter(function(d) identical(d$package, "blockr.ui"), deps)
  sheets <- blockr.core::unlst(
    lapply(ours, function(d) {
      file.path(system.file(d$src$file, package = "blockr.ui"), d$stylesheet)
    })
  )
  css <- paste(blockr.core::chr_ply(sheets, read_css), collapse = "\n")

  expect_match(css, ".blockr-mac .blockr-shortcut__other", fixed = TRUE)
  expect_match(css, ".blockr-shortcut__mac {", fixed = TRUE)
})
