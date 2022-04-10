library(shiny)

ui <- fluidPage(
  sliderInput("number", "Select a number:",
              min = 0, max = 100, value = 0, 
              step = 5, animate = TRUE)
)
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
