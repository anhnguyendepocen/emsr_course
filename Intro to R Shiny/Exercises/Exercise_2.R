# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============

rm(list = ls())

## Exercise 2


# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

# we need the function shiny installed, this loads it from the library.
library(shiny)             


#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluidpage function

  
            numericInput(inputId = "x",      # id of input, used in server (https://shiny.rstudio.com/gallery/widget-gallery.html)
                   label = "number",  # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 100000),                # maximum value allowed

# a) Add a second numeric input, y, with initial value of 200 and a range of 0 - 400



            
      textOutput(outputId = "printvalue")                    # heading (results table)                
      

  
) # close UI fluidpage


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output){   # server = function with two inputs
  
# b) Take the sum of x and y and show the result as as a text output.  
                 
                 #--- CREATE NUMBER IN SERVER ---#
                 output$printvalue <- renderText({
                   
                 
                   
                 }) # render Text end.
                 
  
  
} # Server end





## ----- run app------

shinyApp(ui, server)