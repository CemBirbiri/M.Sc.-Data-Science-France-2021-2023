##############################
#
# QUESTION-2  / Assignment-4
#
#############################

#
# UFUK CEM BIRBIRI
#
#
#############################
# 
# 
#Create an app that compare 2 two simulated datasets with a plot 
#and a hypothesis test (tips. Histogram and ttest)
#
#############################

library(ggplot2)
library(shiny)
library(dplyr)
library(hrbrthemes)


plot_histogram <- function(dataset1, dataset2) {
  #Convert datasets to a dataframe
  data <- data.frame(
    type = c( rep("variable 1", length(dataset1)), rep("variable 2", length(dataset2)) ),
    value = c( dataset1, dataset2 )
  )

  #Plot them
  p <- data %>%
    ggplot( aes(x=value, fill=type))
  p+
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    labs(fill="")+
    ggtitle("Comparison of two datasets. Means are 4 and 5 respectively")
}



t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  # use sprintf() to format t.test() results compactly
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}



ui <- fluidPage(
  fluidRow(
    column(4, 
           "Please select the number of datapoints:",
           numericInput("n1", label = "n", value = 1000, min = 1),
           
    ),
    column(4, 
           "Please select the number of datapoints:",
           numericInput("n2", label = "n", value = 1000, min = 1),
           
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)


server <- function(input, output, session) {
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1,4)
    x2 <- rnorm(input$n2,5)

    plot_histogram(x1,x2)
  }, res = 96)
  
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, 4)
    x2 <- rnorm(input$n2, 5)
    
    t_test(x1, x2)
  })
}

shinyApp(ui=ui, server=server)