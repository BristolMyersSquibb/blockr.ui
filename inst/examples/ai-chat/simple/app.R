library(blockr.core)
library(blockr.ui)

serve(
  new_dag_board(
    modules = list(
      dash = new_dashboard_module(),
      chat = new_chat_module()
    )
  ),
  "main"
)
