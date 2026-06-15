# html_table_render(): the renderUI loop, instance-unique inputs, and the
# multi-preview independence regression test.

test_that("html_table_render reacts to sort and page inputs", {
  df <- data.frame(x = c(30, 10, 20), stringsAsFactors = FALSE)

  server <- function(input, output, session) {
    output$result <- html_table_render(df, session, page_size = 2L)
  }

  shiny::testServer(server, {
    html <- as.character(htmltools::as.tags(output$result$html))

    # initial: natural order, page 1
    expect_match(html, "data-current-page=\"1\"")
    expect_true(regexpr("30", html) < regexpr("10", html))

    # sort ascending (instance id derives from the output name "result")
    session$setInputs(result_table_sort = list(col = "x", dir = "asc"))
    html <- as.character(htmltools::as.tags(output$result$html))
    expect_true(regexpr(">10<", html) < regexpr(">20<", html))
    expect_match(html, "blockr-sort-asc")

    # page 2 keeps the sort
    session$setInputs(result_table_page = 2)
    html <- as.character(htmltools::as.tags(output$result$html))
    expect_match(html, "data-current-page=\"2\"")
    expect_match(html, ">30<")
  })
})

test_that("two previews in one session sort independently", {
  df1 <- data.frame(x = c(3, 1, 2))
  df2 <- data.frame(y = c(30, 10, 20))

  server <- function(input, output, session) {
    output$first <- html_table_render(df1, session, page_size = 5L)
    output$second <- html_table_render(df2, session, page_size = 5L)
  }

  shiny::testServer(server, {
    # sorting the first output must not reorder the second
    session$setInputs(first_table_sort = list(col = "x", dir = "asc"))

    html1 <- as.character(htmltools::as.tags(output$first$html))
    html2 <- as.character(htmltools::as.tags(output$second$html))

    expect_true(regexpr(">1<", html1) < regexpr(">3<", html1))
    # second keeps natural order (30 before 10)
    expect_true(regexpr(">30<", html2) < regexpr(">10<", html2))
    # and each container advertises its own input ids
    expect_match(html1, "data-sort-input=\"[^\"]*first_table_sort\"")
    expect_match(html2, "data-sort-input=\"[^\"]*second_table_sort\"")
  })
})

test_that("render errors are caught and shown inline", {
  bad <- structure(list(), class = "tbl_lazy") # lazy w/o dbplyr connection

  server <- function(input, output, session) {
    output$result <- html_table_render(bad, session)
  }

  shiny::testServer(server, {
    html <- as.character(htmltools::as.tags(output$result$html))
    expect_match(html, "blockr-table-error")
  })
})

test_that("the table-level label is rendered from attr(result, 'label')", {
  df <- data.frame(x = 1)
  attr(df, "label") <- "My Table"

  server <- function(input, output, session) {
    output$result <- html_table_render(df, session)
  }

  shiny::testServer(server, {
    html <- as.character(htmltools::as.tags(output$result$html))
    expect_match(html, "My Table")
  })
})
