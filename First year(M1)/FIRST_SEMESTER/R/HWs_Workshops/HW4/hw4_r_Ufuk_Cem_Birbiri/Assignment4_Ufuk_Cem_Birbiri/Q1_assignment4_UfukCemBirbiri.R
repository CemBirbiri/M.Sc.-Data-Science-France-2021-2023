##############################
#
# QUESTION-1  / Assignment-4
#
#############################
#
# UFUK CEM BIRBIRI
#
#
#############################
# 
#Create a slider input to select values between 0 and 100 where the interval 
#between each selectable value on
#the slider is 5. Then, add animation to the input widget so when 
#the user presses play the input widget scrolls through automatically.
#
#############################


#install.packages("shiny")
library(ggplot2)
library(shiny)
library(dplyr)
library(hrbrthemes)




ui <- fluidPage(
  sliderInput("number", "Select a number:",
              min = 0, max = 100, value = 0, 
              step = 5, animate = TRUE)
)

server <- function(input, output){ }

shinyApp(server=server,ui=ui)
