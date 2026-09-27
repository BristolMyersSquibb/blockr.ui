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
