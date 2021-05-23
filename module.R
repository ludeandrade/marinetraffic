#### --- module

# Dropdown Menu UI
dropDownUI <- function(id, label) {
  flow_layout(
    min_cell_width = "10px",
    max_cell_width = "200px",
    column_gap = "20px",
    row_gap = "20px",
    selectInput(inputId = NS(id,"dropDown1"), label = label[1], choices = "-"),
    selectInput(inputId = NS(id,"dropDown2"), label = label[2], choices = "-") 
  )
}

# Dropdown Menu Server
dropDownServer <- function(id, select) {
  moduleServer(id, function(input, output, session) {
    
    choices1 <- data[[select[1]]] %>% unique() %>% sort()
    updateSelectInput(session, inputId = "dropDown1", choices = choices1)
    
    observeEvent(input[["dropDown1"]], {
      choices2 <- data[[select[2]]][data[[select[1]]]==input[["dropDown1"]]] %>% unique() %>% sort()
      updateSelectInput(session, inputId = "dropDown2", choices = choices2)
    })
  })
}