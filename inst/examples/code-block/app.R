devtools::load_all()
library(shiny)

stack <- new_stack(
  new_dataset_block,
  new_code_transform_block
)

ui <- fluidPage(
  theme = bslib::bs_theme(5L),
  generate_ui(stack)
)

server <- function(input, output, session) {
  generate_server(stack)
}

shinyApp(ui, server)
