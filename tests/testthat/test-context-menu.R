test_that("context menu", {
  ctx <- context_menu_items()

  node <- build_context_menu(ctx, target = list(type = "node"))

  expect_type(node, "list")
  expect_named(node, NULL)

  expect_setequal(
    chr_xtr(node, "value"),
    c("create_edge", "remove_block", "append_block")
  )

  edge <- build_context_menu(ctx, target = list(type = "edge"))

  expect_type(edge, "list")
  expect_named(edge, NULL)

  expect_setequal(
    chr_xtr(edge, "value"),
    "remove_edge"
  )

  canv <- build_context_menu(ctx, target = list(type = "canvas"))

  expect_type(canv, "list")
  expect_named(canv, NULL)

  expect_setequal(
    chr_xtr(canv, "value"),
    c("create_stack", "add_block")
  )

  comb <- build_context_menu(ctx, target = list(type = "combo"))

  expect_type(comb, "list")
  expect_named(comb, NULL)

  expect_setequal(
    chr_xtr(comb, "value"),
    "remove_stack"
  )
})
