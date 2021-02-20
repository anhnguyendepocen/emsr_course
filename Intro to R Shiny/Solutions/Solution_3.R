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
                   label = "Number X",          # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 100000),              # maximum value allowed
                   
      numericInput(inputId = "y",      # id of input, used in server
                   label = "Number Y",        # label next to numeric input
                   value = 200,              # initial value
                   min = 0,                   # minimum value allowed
                   max = 400),                # maximum value allowed

      sliderInput(inputId = "z",  # id of input, used in server
                  label = "Number Z",      # label next to numeric input
                  value = 25,                 # initial value
                  min = 10,                   # minimum value allowed
                  max = 80),                  # maximum value allowed

      
      actionButton(inputId = "run_model",     # id of action button, used in server
                   label   = "Run model")     # action button label (on button)
      
      
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
    
    max_minus_mean = max(c(x,y,z)) - mean(c(x,y,z))
    
    max_minus_mean
    
  }
  
# c) Output the result as a text output when the action button is pressed.
  
  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = F, {
                 
                 num = fun_shiny(x = input$x,
                                 y = input$y,
                                 z = input$z)
                 

  #--- CREATE NUMBER IN SERVER ---#
  output$printvalue <- renderText({
    
    paste("The difference between the maximum and the mean of x, y, and z is:", round(num,1))
    
    
     }) # render Text end.
  
  }) # Observe Event End  

} # Server end





## ----- run app------

shinyApp(ui, server)
