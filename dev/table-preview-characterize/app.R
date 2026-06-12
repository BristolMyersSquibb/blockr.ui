# Characterization app for the table-preview JS behavior.
#
# Pins current behavior (sort cycle, pagination, horizontal scroll restore,
# column width lock) before/after the JS rewrite - drive it with Playwright
# and compare results. Serve on 3838 (the devcontainer's forwarded port).

options(shiny.port = 3838, shiny.launch.browser = FALSE)

pkgload::load_all("/workspace/blockr.ui", quiet = TRUE)
library(shiny)

set.seed(7)
wide <- as.data.frame(setNames(
  lapply(1:24, function(i) round(rnorm(23, mean = i * 10), 2)),
  paste0("measurement_col_", sprintf("%02d", 1:24))
))
wide$measurement_col_03[c(2, 5, 9)] <- NA
attr(wide$measurement_col_01, "label") <- "First measurement (units)"
attr(wide$measurement_col_02, "label") <- "Second measurement with a very long label"
attr(wide, "label") <- "Wide characterization table"

ui <- fluidPage(
  tags$h4("first: wide table (scroll + sort + pagination)"),
  uiOutput("first"),
  tags$hr(),
  tags$h4("second: iris (independence)"),
  uiOutput("second")
)

server <- function(input, output, session) {
  output$first <- html_table_render(wide, session, page_size = 5L)
  output$second <- html_table_render(
    iris[1:12, , drop = FALSE], session, page_size = 5L
  )
}

shinyApp(ui, server)
