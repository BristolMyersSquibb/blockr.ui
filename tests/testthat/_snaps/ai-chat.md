# chat_mod_ui works

    Code
      app_request()
    Output
      $action
      [1] "add_block"
      
      $data
      $data$name
      [1] "dataset_block"
      
      $data$append
      [1] FALSE
      
      $data$parms
      $data$parms$dataset
      [1] "iris"
      
      $data$parms$package
      [1] "datasets"
      
      
      

---

    Code
      app_request()
    Output
      $action
      [1] "remove_block"
      
      $data
      [1] "iris_data"
      

---

    Code
      app_request()
    Output
      $action
      [1] "remove_block"
      
      $data
      [1] "iris_data"
      
      $error
      [1] "Block with id iris_data does not exist."
      

