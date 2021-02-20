# ==============
# Making Health Economic Models Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield & Dark Peak Analytics
# contact: info@darkpeakanalytics.com
# ==============


# Exercise 1. Create a new Shiny app that contains the text "Hello <your name>".

# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

# we need the function shiny installed, this loads it from the library.
library(shiny)             


#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluid-page function
  

) # close UI fluidpage


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output){   
  
  # server function

  
} # Server end





## ----- run app------

shinyApp(ui = ui, server = server)