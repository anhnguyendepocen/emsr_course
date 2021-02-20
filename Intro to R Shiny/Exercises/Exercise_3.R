# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============

rm(list = ls())

# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

# we need the function shiny installed, this loads it from the library.
library(shiny)             


#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluidpage function

  titlePanel("More complex model"),   # title of app
  
# a)  In the Sidebar Layout page add two inputs:  
#      i)  Slider input, z, with a initial value of 25 and a range on 10 - 80 (Hint: use the sliderInput function)
#      ii) Action button which is referred to as: run_model .
  
  # SIDEBAR
  sidebarLayout(    # indicates layout is going to be a sidebar-layout
    
    sidebarPanel( # open sidebar panel
      
      numericInput(inputId = "x",           # id of input, used in server
                   label = "Number x",          # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 100000),              # maximum value allowed
                   
      numericInput(inputId = "y",      # id of input, used in server
                   label = "Number Y",        # label next to numeric input
                   value = 200,              # initial value
                   min = 0,                   # minimum value allowed
                   max = 400)                # maximum value allowed

      
    ),  # close sidebarPanel
    
    mainPanel(            # open main panel
      
      h3("Results") ,  
    
      textOutput(outputId = "printvalue")     # text output                
      
    ) # close mainpanel    
    
  ) # close sidebarlayout
  
) # close UI fluid page


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output){   # server = function with two inputs
  
#  b) Create a function in the server that calculates the difference 
#     between the maximum & mean of the three numbers (x, y, z) and returns a single number.
  
  
  fun_shiny <- function(x,y,z){
    
   
    
  }
  
# c) Output the result as a text output when the action button is pressed.
  
  
  #--- CREATE NUMBER IN SERVER ---#
  output$printvalue <- renderText({
    
    
    
  }) # render Text end.

} # Server end





## ----- run app------

shinyApp(ui, server)
