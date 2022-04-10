library(shiny)
library(ggplot2)
ui <- fluidPage(
  fluidRow(
    column(4, 
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "µ", value = 0, step = 0.1),
           numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4, 
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "µ", value = 0, step = 0.1),
           numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
  ),
  
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
  
)
server <- function(input, output) {
  x1 <- reactive({rnorm(input$n1, input$mean1, input$sd1)})
  x2 <- reactive({rnorm(input$n2, input$mean2, input$sd2)})
  df <- reactive({data.frame(
    x = c(x1(), x2()),
    g = c(rep("x1", length(x1())), rep("x2", length(x2())))
  )})
  
  output$hist <- renderPlot({
 
    ggplot(df()) +
      geom_histogram(aes(x, colour = g, fill = g)) 
    #hist(x1, x2, binwidth = input$binwidth, xlim = input$range)
  })
  
  output$ttest <- renderPrint({
   # x1 <- rnorm(input$n1, input$mean1, input$sd1)
   # x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    t.test(x1(), x2())
  })
}
shinyApp(ui = ui, server = server)
