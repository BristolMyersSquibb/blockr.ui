# Table preview on a dock board
#
# A blockr.dock board (with the blockr.dag extension) where every data /
# transform block previews through the shared HTML table preview that lives
# in blockr.ui (build_html_table / table_page / html_table_render). The
# preview is opted in via blockr.extra's `blockr.html_table_preview` option,
# which swaps it in for DT on data and transform blocks.
#
#   [data: ADSL] --> [select cols] --> [arrange]
#
# ADSL is wide and label-rich: scroll a preview horizontally, sort a column
# (the scroll position survives the re-render), page through, and note the
# ADaM labels under the column names. Every block preview sorts and pages
# independently.

options(blockr.html_table_preview = TRUE)

library(shiny)
library(blockr.core)
library(blockr.ui)
library(blockr.dplyr)
library(blockr.dock)
library(blockr.dag)
library(blockr.extra)

stopifnot(requireNamespace("safetyData", quietly = TRUE))
adsl <- safetyData::adam_adsl

board <- new_dock_board(
  blocks = c(
    data = new_static_block(adsl),
    cols = new_select_block(
      state = list(
        columns = list(
          "USUBJID", "SITEID", "TRT01P", "AGE", "SEX",
          "BMIBL", "WEIGHTBL", "HEIGHTBL"
        ),
        exclude = FALSE,
        distinct = FALSE
      )
    ),
    sorted = new_arrange_block()
  ),
  links = links(
    from = c("data", "cols"),
    to = c("cols", "sorted")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

serve(board, "table-preview")
