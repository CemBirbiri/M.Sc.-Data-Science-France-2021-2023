##############################
#
# Exercise-1 / Question-2
#
#############################
#
# UFUK CEM BIRBIRI
#
#
#############################
# Question:
#Create the same Shiny App using reactive{}
#function when defining data in server.
#In this case the choices will include all countries in the
#dataset, that will be shown sorted in an alphabetic
#order.
#
#############################


library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

countries <- bcl["Country"]
countries <- unique(countries)
countries <-countries[order(countries$Country),]


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
                #choices = c(countries))
  ),
  mainPanel(
    plotOutput(outputId = "coolplot"),
    verbatimTextOutput(outputId = "description"),
    br(),br(),
    dataTableOutput(outputId ="results")
  )
)



server <- function(input, output, session) {
  filtered <- reactive({bcl %>%filter(Price >= input$priceInput[1],
                               Price <= input$priceInput[2],
                               Type == input$typeInput,
                               Country == input$countryInput)
                        })
  #Since 'coolplot' was defined as a plotOutput, we must use the renderPlot function, 
  #and we must create a plot inside the renderPlot function.
  output$coolplot <- renderPlot({
    filtered_data<-filtered()
    ggplot(filtered_data, aes(Alcohol_Content)) +
      geom_histogram(col="black", 
                     fill="#69b3a2")})
  #Since it’s a table output, we should use the renderTable() function. 
  #Shiny will know that it needs to display it as a table because it’s defined 
  #as a tableOutput
  output$description <- renderPrint({
    filtered_data<-filtered() 
    summary(filtered_data$Alcohol_Content)
  })
  output$results <- renderDataTable({
    filtered_data<-filtered() 
    filtered_data})
  
  observe({print(input$priceInput)})
  
}
shinyApp(ui = ui, server = server)