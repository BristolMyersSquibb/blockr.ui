library(blockr.core)
library(blockr.dplyr)
library(blockr.io)
library(blockr.sdtm)
library(blockr.ai)
library(blockr.gt)
library(blockr.ui)
library(blockr.ggplot)

serve(
  new_dag_board(
    blocks = list(
      a = new_dataset_block("penguins", package = "palmerpenguins"),
      b = new_scatter_plot_block(
        x = "body_mass_g",
        y = "bill_depth_mm",
        color = "species"
      )
    ),
    links = list(
      ab = new_link("a", "b")
    ),
    modules = list(
      dash = new_dashboard_module(),
      chat = new_chat_module()
    )
  ),
  "main"
)
