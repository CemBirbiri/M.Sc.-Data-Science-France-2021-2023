##############################
#
# Exercise-1 / Question-1
#
#############################
#
# UFUK CEM BIRBIRI
#
#
#############################
# Question:
#Download the following dataset:
# https://drive.google.com/drive/folders/1doMk8cQUMsQ7DK3yeY-shbhDl76K-a3d?usp=sharing
#  Create a Shiny App that looks like
#  the one in the picture.
#  Set the following choices for countries:
#    - CANADA
#  - FRANCE
#  - ITALY
# - UNITED STATES OF AMERICA
#
#############################


library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)



#The entire UI is HTML
ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarPanel(
    sliderInput(inputId = "priceInput", label= "Price", min = 0, max = 100,
            value = c(25, 40), pre = "$"),
    radioButtons(inputId ="typeInput", label="Product type",
                            choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                            selected = "WINE"),
    selectInput(inputId="countryInput", "Country",
                           choices = c("CANADA", "FRANCE", "ITALY"))
  ),
  mainPanel(
    plotOutput(outputId = "coolplot"),
    verbatimTextOutput(outputId = "description"),
    br(),br(),
    dataTableOutput(outputId ="results")
  )
)



server <- function(input, output, session) {
  #Since 'coolplot' was defined as a plotOutput, we must use the renderPlot function, 
  #and we must create a plot inside the renderPlot function.
  output$coolplot <- renderPlot({
       filtered <-bcl %>%filter(Price >= input$priceInput[1],
                              Price <= input$priceInput[2],
                              Type == input$typeInput,
                              Country == input$countryInput)
       ggplot(filtered, aes(Alcohol_Content)) +
          geom_histogram(col="black", 
                         fill="#69b3a2")})
  #Since it’s a table output, we should use the renderTable() function. 
  #Shiny will know that it needs to display it as a table because it’s defined 
  #as a tableOutput
  output$description <- renderPrint({
    filtered <-bcl %>%filter(Price >= input$priceInput[1],
                             Price <= input$priceInput[2],
                             Type == input$typeInput,
                             Country == input$countryInput) 
    summary(filtered$Alcohol_Content)
    }
    
  )
  output$results <- renderDataTable({
       filtered <-bcl %>%filter(Price >= input$priceInput[1],
                               Price <= input$priceInput[2],
                               Type == input$typeInput,
                               Country == input$countryInput)
       filtered})
  
}
shinyApp(ui = ui, server = server)